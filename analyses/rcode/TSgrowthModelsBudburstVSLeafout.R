# CoringTreespotters growth model
# CRD 2 April 2026

# fit the model GS with budburst and compare vs leafout and check if ring porous vs diffuse react differently

# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
options(max.print = 150)
options(digits = 3)

# Load library 
library(rstan)
library(patchwork)

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

runmodels <- TRUE

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Most restricted amount of data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
emp <- read.csv("output/empiricalDataMAIN.csv")

# log ring width
emp$loglength <- log(emp$lengthMM)

empfullsos <- emp[!is.na(emp$budburst) & !is.na(emp$loglength),]
nrow(empfullsos)
empfulleos <- emp[!is.na(emp$coloredLeaves) & !is.na(emp$loglength),]

gddyr <- read.csv("output/gddByYear.csv")

# remove NAs
emp <- emp[!is.na(emp$fgsGDD5) & !is.na(emp$loglength), ]
emp$fgsGSL <- emp$coloredLeaves - emp$budburst

# scale gdd to how many gdd are in 10 average spring days
temp <- subset(gddyr, doy <151 & doy > 120)
temp$mingddperiod <- ave(temp$GDD_5, temp$year, FUN = min)
temp$gdddiff <- temp$GDD_5 - temp$mingddperiod

temp <- temp[order(temp$year, temp$doy), ]

temp$bin10 <- ave(temp$doy, temp$year, FUN = function(x) ceiling((x - min(x) + 1) / 10))
gdd_10day <- aggregate(gdddiff ~ year + bin10, data = temp, max)
gddscale <- mean(gdd_10day$gdddiff)

# transform my groups to numeric values
emp$spp_num <- match(emp$latbi, unique(emp$latbi))
emp$treeid_num <- match(emp$id, unique(emp$id))

# transform data in vectors
y <- emp$loglength # ring width in mm
N <- nrow(emp)
Nspp <- length(unique(emp$spp_num))
species <- as.numeric(as.character(emp$spp_num))
treeid <- as.numeric(emp$treeid_num)
Ntreeid <- length(unique(treeid))

# different response variables
gdd <- emp$fgsGDD5 / gddscale
gsl <- as.numeric(emp$fgsGSL) / 10
sos <- emp$budburst / 5

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if(runmodels){

# Fit model GDD
gddmodel <- stan_model("stan/TSmodelGrowthGDD.stan")
fitgdd <- sampling(gddmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gdd"),
                   warmup = 1000, iter=2000, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDD_budburst")

diagnostics <- util$extract_hmc_diagnostics(fitgdd)
util$check_all_hmc_diagnostics(diagnostics)

# Fit model GSL
gslmodel <- stan_model("stan/TSmodelGrowthGSL.stan")
fitgsl <- sampling(gslmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gsl"),
                   warmup = 1000, iter = 2000, chains = 4)
saveRDS(fitgsl, "output/stanOutput/fitGrowthGSL_budburst")

# Fit model SOS
sosmodel <- stan_model("stan/TSmodelGrowthSOS.stan")
fitsos <- sampling(sosmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid",
                                      "sos"),
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOS_budburst")

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
sigma_df2_bb  <- extract_params(df_fitgdd, "sigma", "mean", "sigma")
bspp_df2_bb   <- extract_params(df_fitgdd, "bsp", "fit_bspp", 
                             "spp", "bsp\\[(\\d+)\\]")
treeid_df2_bb <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", 
                             "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_bb <- subset(treeid_df2_bb, !grepl("z|sigma", treeid))
aspp_df2_bb   <- extract_params(df_fitgdd, "aspp", "fit_aspp", 
                             "spp", "aspp\\[(\\d+)\\]")


##### Plot posterior vs priors for gdd fit #####
pdf(file = "figures/growthModelsMain/budburstVSLeafout/gddModelPriorVSPosterior.pdf", width = 8, height = 10)

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
sigma_df2_bb_gsl  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2_bb_gsl   <- extract_params(df_fitgsl, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_bb_gsl <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_bb_gsl <- subset(treeid_df2_bb_gsl, !grepl("z|sigma", treeid))
aspp_df2_bb_gsl   <- extract_params(df_fitgsl, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df2_bb_gsl <- subset(treeid_df2_bb_gsl, !grepl("prior", treeid))

##### Plot posterior vs priors for GSL fit #####
pdf(file = "figures/growthModelsMain/budburstVSLeafout/gslModelPriorVSPosterior.pdf", width = 8, height = 10)

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
sigma_df2_bb_sos  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_bb_sos   <- extract_params(df_fitsos, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_bb_sos <- extract_params(df_fitsos, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_bb_sos <- subset(treeid_df2_bb_sos, !grepl("z|sigma", treeid))
aspp_df2_bb_sos <- extract_params(df_fitsos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df2_bb_sos <- subset(treeid_df2_bb_sos, !grepl("prior", treeid))

##### Plot posterior vs priors for sos fit #####
pdf(file = "figures/growthModelsMain/budburstVSLeafout/sosModelPriorVSPosterior.pdf", width = 8, height = 10)

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
# Recover models with Leafout ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# GDD posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fit <- readRDS("output/stanOutput/fitGrowthGDD")
df_fit <- as.data.frame(fit)

# full posterior
columns <- colnames(df_fit)
sigma_df <- df_fit[, columns[grepl("sigma", columns)]]
bspp_df <- df_fit[, columns[grepl("bspp", columns)]]
treeid_df <- df_fit[, grepl("treeid", columns) & 
                      !grepl("z", columns) &
                      !grepl("sigma", columns)]
aspp_df <- df_fit[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

# posterior summaries
sigma_df2  <- extract_params(df_fit, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fit, "bsp", "fit_bspp", "spp", "bspp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fit, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z", treeid) & !grepl("sigma", treeid))
aspp_df2   <- extract_params(df_fit, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")

treeid_df2$treeid_name <- emp$id[match(treeid_df2$treeid, emp$treeid_num)]
aspp_df2$spp_name <- emp$latbi[match(aspp_df2$spp, emp$spp_num)]
bspp_df2$spp_name <- emp$latbi[match(bspp_df2$spp, emp$spp_num)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# GSL posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fitgsl <- readRDS("output/stanOutput/fitGrowthGSL")

df_fitgsl <- as.data.frame(fitgsl)

# full posterior
columns <- colnames(df_fitgsl)[!grepl("prior", colnames(df_fitgsl))]
sigma_df_gsl <- df_fitgsl[, columns[grepl("sigma", columns)]]
bspp_df_gsl <- df_fitgsl[, columns[grepl("bsp", columns)]]
treeid_df_gsl <- df_fitgsl[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df_gsl <- df_fitgsl[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df_gsl) <- 1:ncol(bspp_df_gsl)
colnames(treeid_df_gsl) <- 1:ncol(treeid_df_gsl)
colnames(aspp_df_gsl) <- 1:ncol(aspp_df_gsl)

# posterior summaries
sigma_df2_gsl  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2_gsl   <- extract_params(df_fitgsl, "bsp", "fit_bspp", 
                                 "spp", "bspp\\[(\\d+)\\]")
treeid_df2_gsl <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", 
                                 "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_gsl <- subset(treeid_df2_gsl, !grepl("z|sigma", treeid))
aspp_df2_gsl   <- extract_params(df_fitgsl, "aspp", "fit_aspp", 
                                 "spp", "aspp\\[(\\d+)\\]")

treeid_df2_gsl$treeid_name <- emp$id[match(treeid_df2_gsl$treeid, emp$treeid_num)]
bspp_df2_gsl$spp_name <- emp$latbi[match(bspp_df2_gsl$spp, emp$spp_num)]
aspp_df2_gsl$spp_name <- emp$latbi[match(aspp_df2_gsl$spp, emp$spp_num)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# SOS posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fitsos <- readRDS("output/stanOutput/fitGrowthSOS")

df_fitsos <- as.data.frame(fitsos)

# full posterior
columns <- colnames(df_fitsos)[!grepl("prior", colnames(df_fitsos))]
sigma_df_sos <- df_fitsos[, columns[grepl("sigma", columns)]]
bspp_df_sos <- df_fitsos[, columns[grepl("bsp", columns)]]
treeid_df_sos <- df_fitsos[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df_sos <- df_fitsos[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df_sos) <- 1:ncol(bspp_df_sos)
colnames(treeid_df_sos) <- 1:ncol(treeid_df_sos)
colnames(aspp_df_sos) <- 1:ncol(aspp_df_sos)

# posterior summaries
sigma_df2_sos  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_sos   <- extract_params(df_fitsos, "bsp", "fit_bspp", 
                                 "spp", "bspp\\[(\\d+)\\]")
treeid_df2_sos <- extract_params(df_fitsos, "atreeid", "fit_atreeid", 
                                 "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_sos <- subset(treeid_df2_sos, !grepl("z|sigma", treeid))
aspp_df2_sos   <- extract_params(df_fitsos, "aspp", "fit_aspp", 
                                 "spp", "aspp\\[(\\d+)\\]")

treeid_df2_sos$treeid_name <- emp$id[match(treeid_df2_sos$treeid, emp$treeid_num)]
bspp_df2_sos$spp_name <- emp$latbi[match(bspp_df2_sos$spp, emp$spp_num)]
aspp_df2_sos$spp_name <- emp$latbi[match(aspp_df2_sos$spp, emp$spp_num)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Figure for model comparison ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
jpeg("figures/growthModelsMain/budburstVSLeafout/DporousVSRporous.jpeg", width = 7, height = 6, units = "in", res = 300)

# define colors
wood_colors <- c("Diffuse-porous" = "#4e9af1",   # blue
                 "Ring-porous"    = "#e07b3f")     # orange

# GDD
bspp_df2_bb$spp_name <- emp$latbi[match(bspp_df2_bb$spp, emp$spp_num)]
treeid_df2$spp_name <- emp$latbi[match(treeid_df2$treeid, emp$treeid_num)]
treeid_df2_bb$spp_name<- emp$latbi[match(treeid_df2_bb$treeid, emp$treeid_num)]

# GSL
bspp_df2_bb_gsl$spp_name<- emp$latbi[match(bspp_df2_bb_gsl$spp,emp$spp_num)]
treeid_df2_gsl$spp_name <- emp$latbi[match(treeid_df2_gsl$treeid, emp$treeid_num)]
treeid_df2_bb_gsl$spp_name<- emp$latbi[match(treeid_df2_bb_gsl$treeid,emp$treeid_num)]

# SOS
bspp_df2_bb_sos$spp_name <- emp$latbi[match(bspp_df2_bb_sos$spp, emp$spp_num)]
treeid_df2_sos$spp_name <- emp$latbi[match(treeid_df2_sos$treeid, emp$treeid_num)]
treeid_df2_bb_sos$spp_name <- emp$latbi[match(treeid_df2_bb_sos$treeid, emp$treeid_num)]

porousness <- c(
  "Tilia americana"        = "Diffuse-porous",
  "Populus deltoides"      = "Diffuse-porous",
  "Betula nigra"           = "Diffuse-porous",
  "Betula alleghaniensis"  = "Diffuse-porous",
  "Acer saccharum"         = "Diffuse-porous",
  "Acer rubrum"            = "Diffuse-porous",
  "Aesculus flava"         = "Diffuse-porous",
  "Quercus rubra"          = "Ring-porous",
  "Quercus alba"           = "Ring-porous",
  "Carya glabra"           = "Ring-porous",
  "Carya ovata"            = "Ring-porous"
)

bspp_df2$wood <- porousness[bspp_df2$spp_name]
bspp_df2_bb$wood <- porousness[bspp_df2_bb$spp_name]
treeid_df2$wood <- porousness[treeid_df2$spp_name]
treeid_df2_bb$wood <- porousness[treeid_df2_bb$spp_name]

bspp_df2_gsl$wood <- porousness[bspp_df2_gsl$spp_name]
bspp_df2_bb_gsl$wood <- porousness[bspp_df2_bb_gsl$spp_name]
treeid_df2_gsl$wood <- porousness[treeid_df2_gsl$spp_name]
treeid_df2_bb_gsl$wood <- porousness[treeid_df2_bb_gsl$spp_name]

bspp_df2_sos$wood <- porousness[bspp_df2_sos$spp_name]
bspp_df2_bb_sos$wood <- porousness[bspp_df2_bb_sos$spp_name]
treeid_df2_sos$wood <- porousness[treeid_df2_sos$spp_name]
treeid_df2_bb_sos$wood <- porousness[treeid_df2_bb_sos$spp_name]

par(mfrow = c(3,3), oma = c(0, 2, 0, 4))

# GDD --- --- --- --- --- ---
plot(sigma_df2_bb$mean, sigma_df2$mean,
     xlab = "budburst", ylab = "leafout", main = "sigmas", type = "n", frame = FALSE,
     ylim = range(c(sigma_df2$mean_per25, sigma_df2$mean_per75)),
     xlim = range(c(sigma_df2_bb$mean_per25, sigma_df2_bb$mean_per75)))
arrows(x0 = sigma_df2_bb$mean, y0 = sigma_df2$mean_per25,
       x1 = sigma_df2_bb$mean, y1 = sigma_df2$mean_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_bb$mean_per25, y0 = sigma_df2$mean,
       x1 = sigma_df2_bb$mean_per75, y1 = sigma_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_bb$mean, sigma_df2$mean,
       pch = 16, col = "black", cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_bb$mean, sigma_df2$mean, pch = 16, 
       col = wood_colors[sigma_df2$wood], cex = 1.2)
text(sigma_df2_bb$mean_per75, sigma_df2$mean_per25, labels = sigma_df2_bb$sigma, pos = c(3,3), cex = 0.75)

mtext("a) GDD", side = 3, adj = -0.5, font = 2, cex = 0.9)

# bspp
plot(bspp_df2_bb$fit_bspp, bspp_df2$fit_bspp,
     xlab = "budburst", ylab = "leafout", main = "bspp", type = "n", frame = FALSE,
     ylim = range(c(bspp_df2$fit_bspp_per25, bspp_df2$fit_bspp_per75)),
     xlim = range(c(bspp_df2_bb$fit_bspp_per25, bspp_df2_bb$fit_bspp_per75)))
arrows(x0 = bspp_df2_bb$fit_bspp, y0 = bspp_df2$fit_bspp_per25,
       x1 = bspp_df2_bb$fit_bspp, y1 = bspp_df2$fit_bspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_bb$fit_bspp_per25, y0 = bspp_df2$fit_bspp,
       x1 = bspp_df2_bb$fit_bspp_per75, y1 = bspp_df2$fit_bspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_bb$fit_bspp, bspp_df2$fit_bspp,
       pch = 16, col = wood_colors[bspp_df2$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# atreeid
plot(treeid_df2_bb$fit_atreeid, treeid_df2$fit_atreeid,
     xlab = "budburst", ylab = "leafout", main = "atreeid", type = "n", frame = FALSE,
     ylim = range(c(treeid_df2$fit_atreeid_per25, treeid_df2$fit_atreeid_per75)),
     xlim = range(c(treeid_df2_bb$fit_atreeid_per25, treeid_df2_bb$fit_atreeid_per75)))
arrows(x0 = treeid_df2_bb$fit_atreeid, y0 = treeid_df2$fit_atreeid_per25,
       x1 = treeid_df2_bb$fit_atreeid, y1 = treeid_df2$fit_atreeid_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = treeid_df2_bb$fit_atreeid_per25, y0 = treeid_df2$fit_atreeid,
       x1 = treeid_df2_bb$fit_atreeid_per75, y1 = treeid_df2$fit_atreeid,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(treeid_df2_bb$fit_atreeid, treeid_df2$fit_atreeid,
       pch = 16, col = wood_colors[treeid_df2$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# GSL --- --- --- --- --- ---
plot(sigma_df2_bb_gsl$mean, sigma_df2_gsl$mean,
     xlab = "budburst", ylab = "leafout", main = "sigmas", type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_gsl$mean_per25, sigma_df2_gsl$mean_per75)),
     xlim = range(c(sigma_df2_bb_gsl$mean_per25, sigma_df2_bb_gsl$mean_per75)))
arrows(x0 = sigma_df2_bb_gsl$mean, y0 = sigma_df2_gsl$mean_per25,
       x1 = sigma_df2_bb_gsl$mean, y1 = sigma_df2_gsl$mean_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_bb_gsl$mean_per25, y0 = sigma_df2_gsl$mean,
       x1 = sigma_df2_bb_gsl$mean_per75, y1 = sigma_df2_gsl$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_bb_gsl$mean, sigma_df2_gsl$mean,
       pch = 16, col = "black", cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_bb_gsl$mean, sigma_df2_gsl$mean, pch = 16, col = wood_colors[sigma_df2_gsl$wood], cex = 1.2)
text(sigma_df2_bb_gsl$mean_per75, sigma_df2_gsl$mean_per25, labels = sigma_df2_bb_gsl$sigma, pos = c(3,3), cex = 0.75)

mtext("b) GSL", side = 3, adj = -0.5, font = 2, cex = 0.9)

# bspp
plot(bspp_df2_bb_gsl$fit_bspp, bspp_df2_gsl$fit_bspp,
     xlab = "budburst", ylab = "leafout", main = "bspp", type = "n", frame = FALSE,
     ylim = range(c(bspp_df2_gsl$fit_bspp_per25, bspp_df2_gsl$fit_bspp_per75)),
     xlim = range(c(bspp_df2_bb_gsl$fit_bspp_per25, bspp_df2_bb_gsl$fit_bspp_per75)))
arrows(x0 = bspp_df2_bb_gsl$fit_bspp, y0 = bspp_df2_gsl$fit_bspp_per25,
       x1 = bspp_df2_bb_gsl$fit_bspp, y1 = bspp_df2_gsl$fit_bspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_bb_gsl$fit_bspp_per25, y0 = bspp_df2_gsl$fit_bspp,
       x1 = bspp_df2_bb_gsl$fit_bspp_per75, y1 = bspp_df2_gsl$fit_bspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_bb_gsl$fit_bspp, bspp_df2_gsl$fit_bspp,
       pch = 16, col = wood_colors[bspp_df2_gsl$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# atreeid
plot(treeid_df2_bb_gsl$fit_atreeid, treeid_df2_gsl$fit_atreeid,
     xlab = "budburst", ylab = "leafout", main = "atreeid", type = "n", frame = FALSE,
     ylim = range(c(treeid_df2_gsl$fit_atreeid_per25, treeid_df2_gsl$fit_atreeid_per75)),
     xlim = range(c(treeid_df2_bb_gsl$fit_atreeid_per25, treeid_df2_bb_gsl$fit_atreeid_per75)))
arrows(x0 = treeid_df2_bb_gsl$fit_atreeid, y0 = treeid_df2_gsl$fit_atreeid_per25,
       x1 = treeid_df2_bb_gsl$fit_atreeid, y1 = treeid_df2_gsl$fit_atreeid_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = treeid_df2_bb_gsl$fit_atreeid_per25, y0 = treeid_df2_gsl$fit_atreeid,
       x1 = treeid_df2_bb_gsl$fit_atreeid_per75, y1 = treeid_df2_gsl$fit_atreeid,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(treeid_df2_bb_gsl$fit_atreeid, treeid_df2_gsl$fit_atreeid,
       pch = 16, col = wood_colors[treeid_df2_gsl$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# SOS  --- --- --- --- --- ---
plot(sigma_df2_bb_sos$mean, sigma_df2_sos$mean,
     xlab = "budburst", ylab = "leafout", main = "sigmas", type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_sos$mean_per25, sigma_df2_sos$mean_per75)),
     xlim = range(c(sigma_df2_bb_sos$mean_per25, sigma_df2_bb_sos$mean_per75)))
arrows(x0 = sigma_df2_bb_sos$mean, y0 = sigma_df2_sos$mean_per25,
       x1 = sigma_df2_bb_sos$mean, y1 = sigma_df2_sos$mean_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_bb_sos$mean_per25, y0 = sigma_df2_sos$mean,
       x1 = sigma_df2_bb_sos$mean_per75, y1 = sigma_df2_sos$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_bb_sos$mean, sigma_df2_sos$mean,
       pch = 16, col = "black", cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_bb_sos$mean, sigma_df2_sos$mean, pch = 16, col = wood_colors[sigma_df2_sos$wood], cex = 1.2)
text(sigma_df2_bb_sos$mean_per75, sigma_df2_sos$mean_per25, labels = sigma_df2_bb_sos$sigma, pos = c(3,3), cex = 0.75)

mtext("c) SOS", side = 3, adj = -0.5, font = 2, cex = 0.9)

# bspp
plot(bspp_df2_bb_sos$fit_bspp, bspp_df2_sos$fit_bspp,
     xlab = "budburst", ylab = "leafout", main = "bspp", type = "n", frame = FALSE,
     ylim = range(c(bspp_df2_sos$fit_bspp_per25, bspp_df2_sos$fit_bspp_per75)),
     xlim = range(c(bspp_df2_bb_sos$fit_bspp_per25, bspp_df2_bb_sos$fit_bspp_per75)))
arrows(x0 = bspp_df2_bb_sos$fit_bspp, y0 = bspp_df2_sos$fit_bspp_per25,
       x1 = bspp_df2_bb_sos$fit_bspp, y1 = bspp_df2_sos$fit_bspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_bb_sos$fit_bspp_per25, y0 = bspp_df2_sos$fit_bspp,
       x1 = bspp_df2_bb_sos$fit_bspp_per75, y1 = bspp_df2_sos$fit_bspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_bb_sos$fit_bspp, bspp_df2_sos$fit_bspp,
       pch = 16, col = wood_colors[bspp_df2_sos$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# atreeid
plot(treeid_df2_bb_sos$fit_atreeid, treeid_df2_sos$fit_atreeid,
     xlab = "budburst", ylab = "leafout", main = "atreeid", type = "n", frame = FALSE,
     ylim = range(c(treeid_df2_sos$fit_atreeid_per25, treeid_df2_sos$fit_atreeid_per75)),
     xlim = range(c(treeid_df2_bb_sos$fit_atreeid_per25, treeid_df2_bb_sos$fit_atreeid_per75)))
arrows(x0 = treeid_df2_bb_sos$fit_atreeid, y0 = treeid_df2_sos$fit_atreeid_per25,
       x1 = treeid_df2_bb_sos$fit_atreeid, y1 = treeid_df2_sos$fit_atreeid_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = treeid_df2_bb_sos$fit_atreeid_per25, y0 = treeid_df2_sos$fit_atreeid,
       x1 = treeid_df2_bb_sos$fit_atreeid_per75, y1 = treeid_df2_sos$fit_atreeid,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(treeid_df2_bb_sos$fit_atreeid, treeid_df2_sos$fit_atreeid,
       pch = 16, col = wood_colors[treeid_df2_sos$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# legend once, in outer margin on the right
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = names(wood_colors), col = wood_colors,
       pch = 16, pt.cex = 1.2, bty = "n", title = "Wood anatomy", xpd = TRUE)
dev.off()


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
