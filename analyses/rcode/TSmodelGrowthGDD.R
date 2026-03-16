# CoringTreespotters growth model
# CRD 18 December 2025

# Goal: build a model to understand the relationship between growth and growing degree days using the tree cores collected in at the Arnold Arboretum in the spring of 2025

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 
options(digits = 3)

# Load library 
library(ggplot2)
library(rstan)
library(future)
library(shinystan)
library(wesanderson)
library(patchwork)

# stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")


if (length(grep("christophe_rouleau-desrochers", getwd())) > 0) {
  setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")
} else if (length(grep("lizzie", getwd())) > 0) {
  setwd("/Users/lizzie/Documents/git/projects/others/coringtreespotters/wildchrokie/analyses")
} else  {
  setwd("/home/crouleau/wildchrokie/analyses")
}

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)
# my function to extract parameters
source('rcode/utilExtractParam.R')

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Most restricted amount of data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# emp2 <- read.csv("output/empiricalDataMAIN.csv")
# read empirical data with max phenology observations instead of min
emp <- read.csv("output/empiricalDataMAIN.csv")
# remove NAs
emp2 <- emp[!is.na(emp$pgsGDD5) & !is.na(emp$lengthMM), ]

# transform my groups to numeric values
emp2$spp_num <- match(emp2$latbi, unique(emp2$latbi))
emp2$treeid_num <- match(emp2$id, unique(emp2$id))

# some checks
table(emp2$latbi, emp2$spp_num)
table(emp2$id, emp2$latbi)
table(emp2$treeid_num, emp2$spp_num)

# transform data in vectors
y <- emp2$lengthMM # ring width in mm
N <- nrow(emp2)
Nspp <- length(unique(emp2$spp_num))
species <- as.numeric(as.character(emp2$spp_num))
treeid <- as.numeric(emp2$treeid_num)
Ntreeid <- length(unique(treeid))

# different response variables
gdd <- emp2$pgsGDD5/200
gsl <- as.numeric(emp2$pgsGSL)/10
sos <- emp2$leafout/10
eos <- emp2$coloredLeaves/10

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit model GDD
rstan_options(auto_write = TRUE)
gddmodel <- stan_model("stan/TSmodelGrowthGDD.stan")
fitgdd <- sampling(gddmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gdd"),
                   warmup = 1000, iter=2000, 
                   chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDD")

# Fit model GSL
rstan_options(auto_write = TRUE)
gslmodel <- stan_model("stan/TSmodelGrowthGSL.stan")
fitgsl <- sampling(gslmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gsl"),
                   warmup = 1000, iter = 2000, 
                   chains = 4)
saveRDS(fitgsl, "output/stanOutput/fitGrowthGSL")

# Fit model SOS
rstan_options(auto_write = TRUE)
sosmodel <- stan_model("stan/TSmodelGrowthSOS.stan")
fitsos <- sampling(sosmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid",
                                      "sos"),
                   warmup = 1000, iter = 2000,
                   chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOS")

# Fit model EOS
rstan_options(auto_write = TRUE)
eosmodel <- stan_model("stan/TSmodelGrowthEOS.stan")
fiteos <- sampling(eosmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid",
                                      "eos"),
                   warmup = 1000, iter = 2000,
                   chains=4)
saveRDS(fiteos, "output/stanOutput/fitGrowthEOS")

# diagnostics
# diagnostics <- util$extract_hmc_diagnostics(fit) 
# util$check_all_hmc_diagnostics(diagnostics)
# samples <- util$extract_expectand_vals(fit)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot GDD fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
##### Recover parameters #####
df_fitgdd <- as.data.frame(fitgdd)

# full posterior
columns <- colnames(df_fitgdd)[!grepl("prior", colnames(df_fitgdd))]
sigma_df <- df_fitgdd[, columns[grepl("sigma", columns)]]
bspp_df <- df_fitgdd[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitgdd[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fitgdd[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

# posterior summaries
sigma_df2  <- extract_params(df_fitgdd, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fitgdd, "bsp", "fit_bspp", 
                             "spp", "bsp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", 
                             "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z|sigma", treeid))
aspp_df2   <- extract_params(df_fitgdd, "aspp", "fit_aspp", 
                             "spp", "aspp\\[(\\d+)\\]")


##### Plot posterior vs priors for gdd fit #####
pdf(file = "figures/empiricalData/gddModelPriorVSPosterior.pdf", width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 2))

# a
plot(density(df_fitgdd[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0,0.5))
lines(density(df_fitgdd[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitgdd[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,2))
lines(density(df_fitgdd[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitgdd[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0,2))
lines(density(df_fitgdd[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitgdd[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", xlim = c(-20, 20), ylim = c(0, 0.15))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitgdd[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot GSL fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
##### Recover parameters #####
df_fitgsl <- as.data.frame(fitgsl)

# full posterior
columns <- colnames(df_fitgsl)[!grepl("prior", colnames(df_fitgsl))]
sigma_df <- df_fitgsl[, columns[grepl("sigma", columns)]]
bspp_df <- df_fitgsl[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitgsl[, grepl("treeid", columns) & 
                         !grepl("z|sigma", columns)]
aspp_df <- df_fitgsl[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

# posterior summaries
sigma_df2  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fitgsl, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z|sigma", treeid))
aspp_df2   <- extract_params(df_fitgsl, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("prior", treeid))

##### Plot posterior vs priors for GSL fit #####
pdf(file = "figures/empiricalData/gslModelPriorVSPosterior.pdf", width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 2))

# a
plot(density(df_fitgsl[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0,0.5))
lines(density(df_fitgsl[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitgsl[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,2))
lines(density(df_fitgsl[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitgsl[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0,2))
lines(density(df_fitgsl[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitgsl[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", xlim = c(-20, 20), ylim = c(0, 0.15))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitgsl[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot SOS fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
##### Recover parameters #####
df_fitsos <- as.data.frame(fitsos)

# full posterior
columns <- colnames(df_fitsos)[!grepl("prior", colnames(df_fitsos))]
sigma_df <- df_fitsos[, columns[grepl("sigma", columns)]]
bspp_df <- df_fitsos[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitsos[, grepl("treeid", columns) & 
                         !grepl("z|sigma", columns)]
aspp_df <- df_fitsos[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

# posterior summaries
sigma_df2_sos  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_sos   <- extract_params(df_fitsos, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_sos <- extract_params(df_fitsos, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_sos <- subset(treeid_df2_sos, !grepl("z|sigma", treeid))
aspp_df2_sos <- extract_params(df_fitsos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df2_sos <- subset(treeid_df2_sos, !grepl("prior", treeid))

##### Plot posterior vs priors for sos fit #####
pdf(file = "figures/empiricalData/sosModelPriorVSPosterior.pdf", width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 2))

# a
plot(density(df_fitsos[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0,0.5))
lines(density(df_fitsos[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitsos[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,2))
lines(density(df_fitsos[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitsos[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0,2))
lines(density(df_fitsos[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitsos[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", xlim = c(-20, 20), ylim = c(0, 0.15))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitsos[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot EOS fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
##### Recover parameters #####
df_fiteos <- as.data.frame(fiteos)

# full posterior
columns <- colnames(df_fiteos)[!grepl("prior", colnames(df_fiteos))]
sigma_df <- df_fiteos[, columns[grepl("sigma", columns)]]
bspp_df <- df_fiteos[, columns[grepl("bsp", columns)]]
treeid_df <- df_fiteos[, grepl("treeid", columns) & 
                         !grepl("z|sigma", columns)]
aspp_df <- df_fiteos[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

# posterior summaries
sigma_df2_eos  <- extract_params(df_fiteos, "sigma", "mean", "sigma")
bspp_df2_eos   <- extract_params(df_fiteos, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_eos <- extract_params(df_fiteos, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_eos <- subset(treeid_df2_eos, !grepl("z|sigma", treeid))
aspp_df2_eos   <- extract_params(df_fiteos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df2_eos <- subset(treeid_df2, !grepl("prior", treeid))

##### Plot posterior vs priors for eos fit #####
pdf(file = "figures/empiricalData/eosModelPriorVSPosterior.pdf", width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 2))

# a
plot(density(df_fiteos[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0,0.5))
lines(density(df_fiteos[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fiteos[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,2))
lines(density(df_fiteos[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fiteos[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0,2))
lines(density(df_fiteos[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fiteos[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", xlim = c(-20, 20), ylim = c(0, 0.15))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fiteos[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FULL DATA ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit model SOS --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
empsos <- emp[!is.na(emp$leafout) & !is.na(emp$lengthMM),]

# transform my groups to numeric values
empsos$spp_num <- match(empsos$latbi, unique(empsos$latbi))
empsos$treeid_num <- match(empsos$id, unique(empsos$id))

# transform data in vectors for gsl
y <- empsos$lengthMM # ring width in mm
N <- nrow(empsos)
Nspp <- length(unique(empsos$spp_num))
species <- as.numeric(as.character(empsos$spp_num))
treeid <- as.numeric(empsos$treeid_num)
Ntreeid <- length(unique(treeid))
sos <- empsos$leafout/10

rstan_options(auto_write = TRUE)
sosmodel <- stan_model("stan/TSmodelGrowthSOS.stan")
fitsosfull <- sampling(sosmodel, data = c("N","y",
                                          "Nspp","species",
                                          "Ntreeid", "treeid",
                                          "sos"),
                       warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOSFull")

# Fit model EOS --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
empeos <- emp[!is.na(emp$coloredLeaves) & !is.na(emp$lengthMM),]

# transform my groups to numeric values
empeos$spp_num <- match(empeos$latbi, unique(empeos$latbi))
empeos$treeid_num <- match(empeos$id, unique(empeos$id))

# transform data in vectors for gsl
y <- empeos$lengthMM # ring width in mm
N <- nrow(empeos)
Nspp <- length(unique(empeos$spp_num))
species <- as.numeric(as.character(empeos$spp_num))
treeid <- as.numeric(empeos$treeid_num)
Ntreeid <- length(unique(treeid))
eos <- empeos$coloredLeaves/10

rstan_options(auto_write = TRUE)
eosmodel <- stan_model("stan/TSmodelGrowthEOS.stan")
fiteosfull <- sampling(eosmodel, data = c("N","y",
                                          "Nspp","species",
                                          "Ntreeid", "treeid",
                                          "eos"),
                       warmup = 1000, iter = 2000,
                       chains=4)
saveRDS(fiteos, "output/stanOutput/fitGrowthEOSFull")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Recover and plot parameters SOS restricted vs full #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# SOS restricted
df_fitsos <- as.data.frame(fitsosfull)

# full posterior
columns <- colnames(df_fitsos)[!grepl("prior", colnames(df_fitsos))]
sigma_df <- df_fitsos[, columns[grepl("sigma", columns)]]
bspp_df <- df_fitsos[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitsos[, grepl("treeid", columns) & 
                         !grepl("z|sigma", columns)]
aspp_df <- df_fitsos[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

# posterior summaries
sigma_df2_sos_full  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_sos_full   <- extract_params(df_fitsos, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_sos_full <- extract_params(df_fitsos, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_sos_full <- subset(treeid_df2_sos_full, !grepl("z|sigma", treeid))
treeid_df2_sos_full <- subset(treeid_df2_sos_full, !grepl("prior", treeid))
aspp_df2_sos_full   <- extract_params(df_fitsos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")

# Open device
jpeg("figures/empiricalData/SOSFullVSRestricted.jpeg", width = 6, height = 6, units = "in", res = 300)
par(mfrow = c(2,2))

plot(sigma_df2_sos$mean, sigma_df2_sos_full$mean,
     xlab = "restricted", ylab = "full", main = "", type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_sos_full$mean_per25, sigma_df2_sos_full$mean_per75)),
     xlim = range(c(sigma_df2_sos$mean_per25, sigma_df2_sos$mean_per75)))
arrows(x0 = sigma_df2_sos$mean, y0 = sigma_df2_sos_full$mean_per25,
       x1 = sigma_df2_sos$mean, y1 = sigma_df2_sos_full$mean_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_sos$mean_per25, y0 = sigma_df2_sos_full$mean,
       x1 = sigma_df2_sos$mean_per75, y1 = sigma_df2_sos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")

points(sigma_df2_sos$mean, sigma_df2_sos_full$mean,
       pch = 16, col = "#046C9A", cex = 1.5)

abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)
points(sigma_df2_sos$mean, sigma_df2_sos_full$mean, pch = 16, col = "#046C9A", cex = 1.5)
text(sigma_df2_sos$mean_per75, sigma_df2_sos_full$mean_per25, labels = sigma_df2_sos$sigma, pos = c(3,3), cex = 0.75)

# bspp
plot(bspp_df2_sos$fit_bspp, bspp_df2_sos_full$fit_bspp,
     xlab = "restricted", ylab = "full", main = "bspp", type = "n", frame = FALSE,
     ylim = range(c(bspp_df2_sos_full$fit_bspp_per25, bspp_df2_sos_full$fit_bspp_per75)),
     xlim = range(c(bspp_df2_sos$fit_bspp_per25, bspp_df2_sos$fit_bspp_per75)))
arrows(x0 = bspp_df2_sos$fit_bspp, y0 = bspp_df2_sos_full$fit_bspp_per25,
       x1 = bspp_df2_sos$fit_bspp, y1 = bspp_df2_sos_full$fit_bspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_sos$fit_bspp_per25, y0 = bspp_df2_sos_full$fit_bspp,
       x1 = bspp_df2_sos$fit_bspp_per75, y1 = bspp_df2_sos_full$fit_bspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_sos$fit_bspp, bspp_df2_sos_full$fit_bspp,
       pch = 16, col = "#046C9A", cex = 1.5)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)

# aspp
plot(aspp_df2_sos$fit_aspp, aspp_df2_sos_full$fit_aspp,
     xlab = "restricted", ylab = "full", main = "aspp", type = "n", frame = FALSE,
     ylim = range(c(aspp_df2_sos_full$fit_aspp_per25, aspp_df2_sos_full$fit_aspp_per75)),
     xlim = range(c(aspp_df2_sos$fit_aspp_per25, aspp_df2_sos$fit_aspp_per75)))
arrows(x0 = aspp_df2_sos$fit_aspp, y0 = aspp_df2_sos_full$fit_aspp_per25,
       x1 = aspp_df2_sos$fit_aspp, y1 = aspp_df2_sos_full$fit_aspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = aspp_df2_sos$fit_aspp_per25, y0 = aspp_df2_sos_full$fit_aspp,
       x1 = aspp_df2_sos$fit_aspp_per75, y1 = aspp_df2_sos_full$fit_aspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(aspp_df2_sos$fit_aspp, aspp_df2_sos_full$fit_aspp,
       pch = 16, col = "#046C9A", cex = 1.5)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)

###### Plot treeid ######
treeid_df2_sos_full <- subset(treeid_df2_sos_full, !treeid %in% setdiff(treeid_df2_sos_full$treeid, treeid_df2_sos$treeid))
plot(treeid_df2_sos$fit_atreeid, treeid_df2_sos_full$fit_atreeid,
     xlab = "restricted", ylab = "full", main = "atreeid", type = "n", frame = FALSE,
     ylim = range(c(treeid_df2_sos_full$fit_atreeid_per25, treeid_df2_sos_full$fit_atreeid_per75)),
     xlim = range(c(treeid_df2_sos$fit_atreeid_per25, treeid_df2_sos$fit_atreeid_per75)))
arrows(x0 = treeid_df2_sos$fit_atreeid, y0 = treeid_df2_sos_full$fit_atreeid_per25,
       x1 = treeid_df2_sos$fit_atreeid, y1 = treeid_df2_sos_full$fit_atreeid_per75,
       angle = 90, code = 3, length = 0, lwd = 0.8, col = "darkgray")
arrows(x0 = treeid_df2_sos$fit_atreeid_per25, y0 = treeid_df2_sos_full$fit_atreeid,
       x1 = treeid_df2_sos$fit_atreeid_per75, y1 = treeid_df2_sos_full$fit_atreeid,
       angle = 90, code = 3, length = 0, lwd = 0.8, col = "darkgray")
points(treeid_df2_sos$fit_atreeid, treeid_df2_sos_full$fit_atreeid,
       pch = 16, col = "#046C9A", cex = 0.8)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)

dev.off()

# check the outlier for aspp
spp1full <- subset(empsos, spp_num == 1)
spp1rest <- subset(emp2, spp_num == 1)
fulleosleafout <- aggregate(leafout ~ treeid_num, spp1full, FUN = length)

restreosleafout <- aggregate(leafout ~ treeid_num, spp1rest, FUN = length)
sum(fulleosleafout$leafout)- sum(restreosleafout$leafout)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Recover and plot parameters EOS restricted vs full #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# EOS restricted
df_fiteos <- as.data.frame(fiteosfull)

# full posterior
columns <- colnames(df_fiteos)[!grepl("prior", colnames(df_fiteos))]
sigma_df <- df_fiteos[, columns[grepl("sigma", columns)]]
bspp_df <- df_fiteos[, columns[grepl("bsp", columns)]]
treeid_df <- df_fiteos[, grepl("treeid", columns) & 
                         !grepl("z|sigma", columns)]
aspp_df <- df_fiteos[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

# posterior summaries
sigma_df2_eos_full  <- extract_params(df_fiteos, "sigma", "mean", "sigma")
bspp_df2_eos_full   <- extract_params(df_fiteos, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_eos_full <- extract_params(df_fiteos, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_eos_full <- subset(treeid_df2_eos_full, !grepl("z|sigma", treeid))
aspp_df2_eos_full   <- extract_params(df_fiteos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df2_eos_full <- subset(treeid_df2_eos_full, !grepl("prior", treeid))

# Open device
jpeg("figures/empiricalData/EOSFullVSRestricted.jpeg", width = 6, height = 6, units = "in", res = 300)
par(mfrow = c(2,2))

plot(sigma_df2_eos$mean, sigma_df2_eos_full$mean,
     xlab = "restricted", ylab = "full", main = "", type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_eos_full$mean_per25, sigma_df2_eos_full$mean_per75)),
     xlim = range(c(sigma_df2_eos$mean_per25, sigma_df2_eos$mean_per75)))
arrows(x0 = sigma_df2_eos$mean, y0 = sigma_df2_eos_full$mean_per25,
       x1 = sigma_df2_eos$mean, y1 = sigma_df2_eos_full$mean_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_eos$mean_per25, y0 = sigma_df2_eos_full$mean,
       x1 = sigma_df2_eos$mean_per75, y1 = sigma_df2_eos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")

points(sigma_df2_eos$mean, sigma_df2_eos_full$mean,
       pch = 16, col = "#046C9A", cex = 1.5)

abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)
points(sigma_df2_eos$mean, sigma_df2_eos_full$mean, pch = 16, col = "#046C9A", cex = 1.5)
text(sigma_df2_eos$mean_per75, sigma_df2_eos_full$mean_per25, labels = sigma_df2_eos$sigma, pos = c(3,3), cex = 0.75)

# bspp
plot(bspp_df2_eos$fit_bspp, bspp_df2_eos_full$fit_bspp,
     xlab = "restricted", ylab = "full", main = "bspp", type = "n", frame = FALSE,
     ylim = range(c(bspp_df2_eos_full$fit_bspp_per25, bspp_df2_eos_full$fit_bspp_per75)),
     xlim = range(c(bspp_df2_eos$fit_bspp_per25, bspp_df2_eos$fit_bspp_per75)))
arrows(x0 = bspp_df2_eos$fit_bspp, y0 = bspp_df2_eos_full$fit_bspp_per25,
       x1 = bspp_df2_eos$fit_bspp, y1 = bspp_df2_eos_full$fit_bspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_eos$fit_bspp_per25, y0 = bspp_df2_eos_full$fit_bspp,
       x1 = bspp_df2_eos$fit_bspp_per75, y1 = bspp_df2_eos_full$fit_bspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_eos$fit_bspp, bspp_df2_eos_full$fit_bspp,
       pch = 16, col = "#046C9A", cex = 1.5)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)

# aspp
plot(aspp_df2_eos$fit_aspp, aspp_df2_eos_full$fit_aspp,
     xlab = "restricted", ylab = "full", main = "aspp", type = "n", frame = FALSE,
     ylim = range(c(aspp_df2_eos_full$fit_aspp_per25, aspp_df2_eos_full$fit_aspp_per75)),
     xlim = range(c(aspp_df2_eos$fit_aspp_per25, aspp_df2_eos$fit_aspp_per75)))
arrows(x0 = aspp_df2_eos$fit_aspp, y0 = aspp_df2_eos_full$fit_aspp_per25,
       x1 = aspp_df2_eos$fit_aspp, y1 = aspp_df2_eos_full$fit_aspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = aspp_df2_eos_full$fit_aspp_per25, y0 = aspp_df2_eos_full$fit_aspp,
       x1 = aspp_df2_eos$fit_aspp_per75, y1 = aspp_df2_eos_full$fit_aspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(aspp_df2_eos$fit_aspp, aspp_df2_eos_full$fit_aspp,
       pch = 16, col = "#046C9A", cex = 1.5)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)

###### Plot treeid ######
treeid_df2_eos_full <- subset(treeid_df2_eos_full, !treeid %in% setdiff(treeid_df2_eos_full$treeid, treeid_df2_eos$treeid))
plot(treeid_df2_eos$fit_atreeid, treeid_df2_eos_full$fit_atreeid,
     xlab = "restricted", ylab = "full", main = "atreeid", type = "n", frame = FALSE,
     ylim = range(c(treeid_df2_eos_full$fit_atreeid_per25, treeid_df2_eos_full$fit_atreeid_per75)),
     xlim = range(c(treeid_df2_eos$fit_atreeid_per25, treeid_df2_eos$fit_atreeid_per75)))
arrows(x0 = treeid_df2_eos$fit_atreeid, y0 = treeid_df2_eos_full$fit_atreeid_per25,
       x1 = treeid_df2_eos$fit_atreeid, y1 = treeid_df2_eos_full$fit_atreeid_per75,
       angle = 90, code = 3, length = 0, lwd = 0.8, col = "darkgray")
arrows(x0 = treeid_df2_eos$fit_atreeid_per25, y0 = treeid_df2_eos_full$fit_atreeid,
       x1 = treeid_df2_eos$fit_atreeid_per75, y1 = treeid_df2_eos_full$fit_atreeid,
       angle = 90, code = 3, length = 0, lwd = 0.8, col = "darkgray")
points(treeid_df2_eos$fit_atreeid, treeid_df2_eos_full$fit_atreeid,
       pch = 16, col = "#046C9A", cex = 0.8)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
jpeg(
  filename = "figures/modelGrowthGDD/retrodictiveCheckHist.jpeg",
  width = 2400,      
  height = 2400,
  res = 300          
)
util$plot_hist_quantiles(samples, "y_rep", 
                         -5, # lower x axis limit
                         15, # upper x axis limit
                         0.5, # binning
                         baseline_values = y,
                         xlab = "Ring width (mm)")
dev.off()
