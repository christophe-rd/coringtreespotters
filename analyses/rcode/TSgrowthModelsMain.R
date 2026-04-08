# CoringTreespotters growth model
# CRD 18 December 2025

# Goal: build a model to understand the relationship between growth and growing degree days using the tree cores collected in at the Arnold Arboretum in the spring of 2025

# housekeeping
# rm(list=ls())
# options(stringsAsFactors = FALSE)
# options(max.print = 150)
# options(digits = 3)

# Load library 
library(rstan)
library(patchwork)
library(wesanderson)
# stan options
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

runmodels <- FALSE
runzscoredmodels <- FALSE
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Most restricted amount of data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# empts <- read.csv("output/empiricalDataMAIN.csv")
# read empirical data with max phenology observations instead of mingit status
empts <- read.csv("output/empiricalDataMAIN.csv")
# log ring width
empts$loglength <- log(empts$lengthMM)

empfullsosts <- empts[!is.na(empts$leafout) & !is.na(empts$lengthMM) & empts$year != 2015,]
empfulleosts <- empts[!is.na(empts$coloredLeaves) & !is.na(empts$lengthMM) & empts$year != 2015,]
gddyr <- read.csv("output/gddByYear.csv")

# remove NAs
empts <- empts[!is.na(empts$pgsGDD5) & !is.na(empts$lengthMM) & empts$year != 2015, ]

# scale gdd to how many gdd are in 10 average spring days
temp<- subset(gddyr, doy <151 & doy > 120)
temp$mingddperiod <- ave(temp$GDD_5, temp$year, FUN = min)
temp$gdddiff <- temp$GDD_5 - temp$mingddperiod

temp <- temp[order(temp$year, temp$doy), ]

temp$bin10 <- ave(temp$doy, temp$year, FUN = function(x) ceiling((x - min(x) + 1) / 10))
gdd_10day <- aggregate(gdddiff ~ year + bin10, data = temp, max)
gddscale <- mean(gdd_10day$gdddiff)

# transform my groups to numeric values
empts$spp_num <- match(empts$latbi, unique(empts$latbi))
empts$treeid_num <- match(empts$id, unique(empts$id))

# some checks
table(empts$latbi, empts$spp_num)
table(empts$id, empts$latbi)
table(empts$treeid_num, empts$spp_num)

# transform data in vectors
y <- empts$loglength # ring width in mm
N <- nrow(empts)
Nspp <- length(unique(empts$spp_num))
species <- as.numeric(as.character(empts$spp_num))
treeid <- as.numeric(empts$treeid_num)
Ntreeid <- length(unique(treeid))

# different response variables
gdd <- empts$pgsGDD5 / tsgddscale
gsl <- as.numeric(empts$pgsGSL) / 10
sos <- empts$leafout / 5
eos <- empts$coloredLeaves / 10

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if(runmodels){
# Fit model GDD
gddmodel <- stan_model("stan/TSmodelGrowthGDD.stan")
fitgdd <- sampling(gddmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gdd"),
                   warmup = 1000, iter=2000, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDD")

diagnostics <- util$extract_hmc_diagnostics(fitgdd)
util$check_all_hmc_diagnostics(diagnostics)

# Fit model GSL
gslmodel <- stan_model("stan/TSmodelGrowthGSL.stan")
fitgsl <- sampling(gslmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gsl"),
                   warmup = 1000, iter = 2000, chains = 4)
saveRDS(fitgsl, "output/stanOutput/fitGrowthGSL")

# Fit model SOS
sosmodel <- stan_model("stan/TSmodelGrowthSOS.stan")
fitsos <- sampling(sosmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid",
                                      "sos"),
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOS")

# Fit model EOS
eosmodel <- stan_model("stan/TSmodelGrowthEOS.stan")
fiteos <- sampling(eosmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid",
                                      "eos"),
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fiteos, "output/stanOutput/fitGrowthEOS")

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
pdf(file = "figures/growthModelsMain/diagnostics/gddModelPriorVSPosterior.pdf", width = 8, height = 10)

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
     xlab = "aspp", xlim = c(-20, 20))
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
sigma_df2_gsl  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2_gsl   <- extract_params(df_fitgsl, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_gsl <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_gsl <- subset(treeid_df2_gsl, !grepl("z|sigma", treeid))
aspp_df2_gsl   <- extract_params(df_fitgsl, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df2_gsl <- subset(treeid_df2_gsl, !grepl("prior", treeid))

##### Plot posterior vs priors for GSL fit #####
pdf(file = "figures/growthModelsMain/diagnostics/gslModelPriorVSPosterior.pdf", width = 8, height = 10)

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
pdf(file = "figures/growthModelsMain/diagnostics/sosModelPriorVSPosterior.pdf", width = 8, height = 10)

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
treeid_df2_eos <- subset(treeid_df2_eos, !grepl("prior", treeid))

##### Plot posterior vs priors for eos fit #####
pdf(file = "figures/growthModelsMain/diagnostics/eosModelPriorVSPosterior.pdf", width = 8, height = 10)

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
# Fit model SOS 
# transform my groups to numeric values
empfullsosts$spp_num <- match(empfullsosts$latbi, unique(empfullsosts$latbi))
empfullsosts$treeid_num <- match(empfullsosts$id, unique(empfullsosts$id))

# transform data in vectors for gsl
y <- empfullsosts$loglength # ring width in mm
N <- nrow(empfullsosts)
Nspp <- length(unique(empfullsosts$spp_num))
species <- as.numeric(as.character(empfullsosts$spp_num))
treeid <- as.numeric(empfullsosts$treeid_num)
Ntreeid <- length(unique(treeid))
sos <- empfullsosts$leafout / 5


sosmodel <- stan_model("stan/TSmodelGrowthSOS.stan")
fitsosfull <- sampling(sosmodel, data = c("N","y",
                                          "Nspp","species",
                                          "Ntreeid", "treeid",
                                          "sos"),
                       warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOSFull")

# Fit model EOS
# transform my groups to numeric values
empfulleosts$spp_num <- match(empfulleosts$latbi, unique(empfulleosts$latbi))
empfulleosts$treeid_num <- match(empfulleosts$id, unique(empfulleosts$id))

# transform data in vectors for gsl
y <- empfulleosts$loglength # ring width in mm
N <- nrow(empfulleosts)
Nspp <- length(unique(empfulleosts$spp_num))
species <- as.numeric(as.character(empfulleosts$spp_num))
treeid <- as.numeric(empfulleosts$treeid_num)
Ntreeid <- length(unique(treeid))
eos <- empfulleosts$coloredLeaves/10

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

# check the outlier for aspp
spp1full <- subset(empfullsosts, spp_num == 1)
spp1rest <- subset(empts, spp_num == 1)
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

jpeg("figures/growthModelsMain/FullVSRestricted.jpeg", width = 9, height = 6, units = "in", res = 300)
par(mfrow = c(2,3), oma = c(0, 2, 0, 0))

plot(sigma_df2_sos$mean, sigma_df2_sos_full$mean,
     xlab = "restricted", ylab = "full", main = "sigmas", type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_sos_full$mean_per25, sigma_df2_sos_full$mean_per75)),
     xlim = range(c(sigma_df2_sos$mean_per25, sigma_df2_sos$mean_per75)))
arrows(x0 = sigma_df2_sos$mean, y0 = sigma_df2_sos_full$mean_per25,
       x1 = sigma_df2_sos$mean, y1 = sigma_df2_sos_full$mean_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_sos$mean_per25, y0 = sigma_df2_sos_full$mean,
       x1 = sigma_df2_sos$mean_per75, y1 = sigma_df2_sos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_sos$mean, sigma_df2_sos_full$mean,
       pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_sos$mean, sigma_df2_sos_full$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
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
       pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

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
       pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)


# add label
mtext("(a)", side = 2, outer = TRUE, at = 0.95, font = 2, las = 1, line = 0.5)

# EOS
plot(sigma_df2_eos$mean, sigma_df2_eos_full$mean,
     xlab = "restricted", ylab = "full", main = "sigmas", type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_eos_full$mean_per25, sigma_df2_eos_full$mean_per75)),
     xlim = range(c(sigma_df2_eos$mean_per25, sigma_df2_eos$mean_per75)))
arrows(x0 = sigma_df2_eos$mean, y0 = sigma_df2_eos_full$mean_per25,
       x1 = sigma_df2_eos$mean, y1 = sigma_df2_eos_full$mean_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_eos$mean_per25, y0 = sigma_df2_eos_full$mean,
       x1 = sigma_df2_eos$mean_per75, y1 = sigma_df2_eos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_eos$mean, sigma_df2_eos_full$mean,
       pch = 16, col = "#d39822", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_eos$mean, sigma_df2_eos_full$mean, pch = 16, col = "#d39822", cex = 1.5)
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
       pch = 16, col = "#d39822", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# aspp
plot(aspp_df2_eos$fit_aspp, aspp_df2_eos_full$fit_aspp,
     xlab = "restricted", ylab = "full", main = "aspp", type = "n", frame = FALSE,
     ylim = range(c(aspp_df2_eos_full$fit_aspp_per25, aspp_df2_eos_full$fit_aspp_per75)),
     xlim = range(c(aspp_df2_eos$fit_aspp_per25, aspp_df2_eos$fit_aspp_per75)))
arrows(x0 = aspp_df2_eos$fit_aspp, y0 = aspp_df2_eos_full$fit_aspp_per25,
       x1 = aspp_df2_eos$fit_aspp, y1 = aspp_df2_eos_full$fit_aspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = aspp_df2_eos$fit_aspp_per25, y0 = aspp_df2_eos_full$fit_aspp,
       x1 = aspp_df2_eos$fit_aspp_per75, y1 = aspp_df2_eos_full$fit_aspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(aspp_df2_eos$fit_aspp, aspp_df2_eos_full$fit_aspp,
       pch = 16, col = "#d39822", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# add label
mtext("(b)", side = 2, outer = TRUE, at = 0.42, font = 2, las = 1, line = 0.5)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
samples <- util$extract_expectand_vals(fitsos)
jpeg(
  filename = "figures/growthModelsMain/diagnostics/retrodictiveCheckHist.jpeg",
  width = 2400, height = 2400, res = 300          
)
util$plot_hist_quantiles(samples, "y_rep", 
                         -3, # lower x axis limit
                         5, # upper x axis limit
                         0.2, # binning
                         baseline_values = y,
                         xlab = "Ring width (mm)")
dev.off()

}


# === === === === === === === === === === === === === === === === === === === #
# === === === === === === === === === === === === === === === === === === === #

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Z-SCORED ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if (runzscoredmodels) {
gdd <- (empts$pgsGDD5 - mean(empts$pgsGDD5)) / sd(empts$pgsGDD5)
gsl <- (empts$pgsGSL - mean(empts$pgsGSL)) / sd(empts$pgsGSL)
sos <- (empts$leafout - mean(empts$leafout)) / sd(empts$leafout)
eos <- (empts$coloredLeaves - mean(empts$coloredLeaves)) / sd(empts$coloredLeaves)

# Fit model GDD
gddmodel <- stan_model("stan/TSmodelGrowthGDD.stan")
fitgdd <- sampling(gddmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gdd"),
                   warmup = 1000, iter=2000, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDDZscored")

diagnostics <- util$extract_hmc_diagnostics(fitgdd)
util$check_all_hmc_diagnostics(diagnostics)

# Fit model GSL
gslmodel <- stan_model("stan/TSmodelGrowthGSL.stan")
fitgsl <- sampling(gslmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gsl"),
                   warmup = 1000, iter = 2000, chains = 4)
saveRDS(fitgsl, "output/stanOutput/fitGrowthGSLZscored")

# Fit model SOS
sosmodel <- stan_model("stan/TSmodelGrowthSOS.stan")
fitsos <- sampling(sosmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid",
                                      "sos"),
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOSZscored")

# Fit model EOS
eosmodel <- stan_model("stan/TSmodelGrowthEOS.stan")
fiteos <- sampling(eosmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid",
                                      "eos"),
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fiteos, "output/stanOutput/fitGrowthEOSZscored")

}