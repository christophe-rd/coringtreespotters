# CoringTreespotters growth model
# CRD 18 December 2025

# Goal: build a model to understand the relationship between growth and growing degree days using the tree cores collected in at the Arnold Arboretum in the spring of 2025

# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
options(max.print = 150)
options(digits = 3)

# Load library 
library(rstan)

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
source('/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/rcode/tools.R')

runmodels <- F
runzscoredmodels <- F
runfulldata <- FALSE
runmodelnoayear <- FALSE
fitmodelprvsyr <- FALSE
runbudburst <- FALSE
fit2xpriors <- FALSE
runmodelSOSonEOS <- F
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Most restricted amount of data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# empts <- read.csv("output/empiricalDataMAIN.csv")
# read empirical data with max phenology observations instead of mingit status
empts <- read.csv("output/empiricalDataMAIN.csv")
gddyr <- read.csv("output/gddByYear.csv")

empts$loglength <- log(empts$lengthMM)

empts <- subset(empts, year != 2015)

empfullsosts <- empts[!is.na(empts$leafout) & !is.na(empts$loglength),]
empfulleosts <- empts[!is.na(empts$coloredLeaves) & !is.na(empts$loglength),]

empts <- empts[!is.na(empts$pgsGDD5) & !is.na(empts$lengthMM),]

# add calendar days
empts$loCal <- format(
  as.Date(empts$leafout - 1,
          origin = paste0(empts$year, "-01-01")),
  "%d-%b"
)
empts$clCal <- format(
  as.Date(empts$coloredLeaves - 1,
          origin = paste0(empts$year, "-01-01")),
  "%d-%b"
)

lineplotseqlength <- 10
# transform my groups to numeric values
empts$spp_num <- match(empts$latbi, unique(empts$latbi))
empts$treeid_num <- match(empts$id, unique(empts$id))
empts$year_num <- match(empts$year, unique(empts$year))

# order by tree id
treeid_spp <- unique(empts[, c("treeid_num", "spp_num", "id", "latbi")])

treeid_spp_ordered <- treeid_spp[order(treeid_spp$treeid_num), ]


# scale gdd to how many gdd are in 10 average spring days
temp<- subset(gddyr, doy <151 & doy > 120)
temp$mingddperiod <- ave(temp$GDD_5, temp$year, FUN = min)
temp$gdddiff <- temp$meanTempC - 5

temp <- temp[order(temp$year, temp$doy), ]

temp$bin7 <- ave(temp$doy, temp$year, FUN = function(x) ceiling((x - min(x) + 1) / 7))
gdd_7day <- aggregate(gdddiff ~ year + bin7, data = temp, sum)
tsgddscale <- mean(gdd_7day$gdddiff)

gddseq <- seq(min(empts$pgsGDD5), max(empts$pgsGDD5), length.out = lineplotseqlength)

# data list for gdd

dgdd <- list(
  y = empts$loglength,
  N = nrow(empts),
  Nspp = length(unique(empts$spp_num)),
  species = as.numeric(as.character(empts$spp_num)),
  treeid = as.numeric(empts$treeid_num),
  Ntreeid = length(unique(as.numeric(empts$treeid_num))),
  year = as.numeric(empts$year_num),
  Nyear = length(unique(empts$year_num)),
  treeid_species = treeid_spp_ordered$spp_num,
  Ntreeid_per_spp = as.integer(table(treeid_spp_ordered$spp_num)),
  gdd = (empts$pgsGDD5) / tsgddscale,
  gddseq = gddseq,
  tsgddscale = tsgddscale,
  Ngddseq = length(gddseq)
)
dgdd

# Set model GSL data
gslscale <- 7
gsl <- empts$pgsGSL / gslscale
gslseq <-  seq(min(empts$pgsGSL), max(empts$pgsGSL), length.out = lineplotseqlength)

# data list for GSL
dgsl <- list(
  y = empts$loglength,
  N = nrow(empts),
  Nspp = length(unique(empts$spp_num)),
  species = as.numeric(as.character(empts$spp_num)),
  treeid = as.numeric(empts$treeid_num),
  Ntreeid = length(unique(as.numeric(empts$treeid_num))),
  year = as.numeric(empts$year_num),
  Nyear = length(unique(empts$year_num)),
  treeid_species = treeid_spp_ordered$spp_num,
  Ntreeid_per_spp = as.integer(table(treeid_spp_ordered$spp_num)),
  gsl = empts$pgsGSL / gslscale,
  gslseq = gslseq,
  gslscale = gslscale,
  Ngslseq = length(gslseq)
)

sosscale <- 7
sos <- empts$leafout / sosscale
sosseq <-  seq(min(empts$leafout), max(empts$leafout), length.out = lineplotseqlength)

# data list for sos
dsos <- list(
  y = empts$loglength,
  N = nrow(empts),
  Nspp = length(unique(empts$spp_num)),
  species = as.numeric(as.character(empts$spp_num)),
  treeid = as.numeric(empts$treeid_num),
  Ntreeid = length(unique(as.numeric(empts$treeid_num))),
  year = as.numeric(empts$year_num),
  Nyear = length(unique(empts$year_num)),
  treeid_species = treeid_spp_ordered$spp_num,
  Ntreeid_per_spp = as.integer(table(treeid_spp_ordered$spp_num)),
  sos = empts$leafout / sosscale,
  sosseq = sosseq,
  sosscale = sosscale,
  Nsosseq = length(sosseq)
)

eosscale <- 7
eos <- empts$coloredLeaves / eosscale
eosseq <-  seq(min(empts$coloredLeaves), max(empts$coloredLeaves), length.out = lineplotseqlength)

# data list for eos
deos <- list(
  y = empts$loglength,
  N = nrow(empts),
  Nspp = length(unique(empts$spp_num)),
  species = as.numeric(as.character(empts$spp_num)),
  treeid = as.numeric(empts$treeid_num),
  Ntreeid = length(unique(as.numeric(empts$treeid_num))),
  year = as.numeric(empts$year_num),
  Nyear = length(unique(empts$year_num)),
  treeid_species = treeid_spp_ordered$spp_num,
  Ntreeid_per_spp = as.integer(table(treeid_spp_ordered$spp_num)),
  eos = empts$coloredLeaves / eosscale,
  eosseq = eosseq,
  eosscale = eosscale,
  Neosseq = length(eosseq)
)
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if(runmodels){
# Fit model GDD
gddmodel <- stan_model("stan/TSmodelGrowthGDD.stan")
fitgdd <- sampling(gddmodel, data = dgdd,
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDD")

diagnostics <- util$extract_hmc_diagnostics(fitgdd)
util$check_all_hmc_diagnostics(diagnostics)

# Fit model GSL
gslmodel <- stan_model("stan/TSmodelGrowthGSL.stan")
fitgsl <- sampling(gslmodel, data = dgsl,
                   warmup = 1000, iter = 2000, chains = 4)
saveRDS(fitgsl, "output/stanOutput/fitGrowthGSL")

# Fit model SOS
sosmodel <- stan_model("stan/TSmodelGrowthSOS.stan")
fitsos <- sampling(sosmodel, data = dsos,
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOS")

# Fit model EOS
eosmodel <- stan_model("stan/TSmodelGrowthEOS.stan")
fiteos <- sampling(eosmodel, data = deos,
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fiteos, "output/stanOutput/fitGrowthEOS")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot GDD fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fitgdd <- readRDS("output/stanOutput/fitGrowthGDD")
fitgsl <- readRDS("output/stanOutput/fitGrowthGSL")
fitsos <- readRDS("output/stanOutput/fitGrowthSOS")
fiteos <- readRDS("output/stanOutput/fitGrowthEOS")

# Setup color palette across all plots
pal <- wes_palette("AsteroidCity1")[3:4]

##### Recover parameters #####
df_fitgdd <- as.data.frame(fitgdd)

# full posterior arrays for multi-line extraction
columns <- colnames(df_fitgdd)[!grepl("prior", colnames(df_fitgdd))]
bspp_df <- df_fitgdd[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitgdd[, grepl("atreeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fitgdd[, columns[grepl("aspp", columns)]]
ayear_df <- df_fitgdd[, columns[grepl("ayear", columns) & !grepl("mean", columns)]]

# change colnames to indexes for loop tracing
colnames(bspp_df)  <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df)   <- 1:ncol(aspp_df)
colnames(ayear_df)  <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2  <- extract_params(df_fitgdd, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fitgdd, "bspp", "fit_bspp", "spp", "bspp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z|sigma", id))
aspp_df2   <- extract_params(df_fitgdd, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2  <- extract_params(df_fitgdd, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2  <- subset(ayear_df2, !grepl("mean", year))
a_df2      <- extract_params(df_fitgdd, "a", "fit_a",
                             "grandmean", "a\\[(\\d+)\\]")
a_df2      <- subset(a_df2, grandmean == "a")

# save csvs
write.csv(sigma_df2,  "output/GM_GDDparam_sigma.csv",  row.names = FALSE)
write.csv(bspp_df2,   "output/GM_GDDparam_bspp.csv",   row.names = FALSE)
write.csv(treeid_df2, "output/GM_GDDparam_treeid.csv", row.names = FALSE)
write.csv(aspp_df2,   "output/GM_GDDparam_aspp.csv",   row.names = FALSE)
write.csv(ayear_df2,  "output/GM_GDDparam_ayear.csv",  row.names = FALSE)
write.csv(a_df2,      "output/GM_GDDparam_a.csv",      row.names = FALSE)

##### Plot posterior vs priors for gdd fit #####
pdf(file = "figures/growthModelsMain/diagnostics/gddModelPriorVSPosterior.pdf", width = 8, height = 10)
par(mfrow = c(3, 2))

# a
plot(density(df_fitgdd[, "a_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_a", xlab = "a", ylim = c(0, 0.5))
lines(density(df_fitgdd[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitgdd[, "sigma_atreeid_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_atreeid", xlab = "sigma_atreeid", ylim = c(0, 2))
lines(density(df_fitgdd[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitgdd[, "sigma_y_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_y", xlab = "sigma_y", ylim = c(0, 2))
lines(density(df_fitgdd[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitgdd[, "aspp_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_aspp", xlab = "aspp", xlim = c(-20, 20))
for (col in colnames(aspp_df)) { lines(density(aspp_df[, col]), col = pal[2], lwd = 1) } 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitgdd[, "bsp_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_bsp", xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) { lines(density(bspp_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fitgdd[, "ayear_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_ayear", xlab = "ayear", xlim = c(-3, 3), ylim = c(0, 1.5))
for (col in colnames(ayear_df)) { lines(density(ayear_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot GSL fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
##### Recover parameters #####
df_fitgsl <- as.data.frame(fitgsl)

# full posterior arrays for multi-line extraction
columns <- colnames(df_fitgsl)[!grepl("prior", colnames(df_fitgsl))]
bspp_df <- df_fitgsl[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitgsl[, grepl("atreeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fitgsl[, columns[grepl("aspp", columns)]]
ayear_df <- df_fitgsl[, columns[grepl("ayear", columns) & !grepl("mean", columns)]]

# change colnames to indexes for loop tracing
colnames(bspp_df)  <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df)   <- 1:ncol(aspp_df)
colnames(ayear_df)  <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2_gsl  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2_gsl   <- extract_params(df_fitgsl, "bspp", "fit_bspp", "spp", "bspp\\[(\\d+)\\]")
treeid_df2_gsl <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2_gsl <- subset(treeid_df2_gsl, !grepl("z|sigma", id))
aspp_df2_gsl   <- extract_params(df_fitgsl, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2_gsl  <- extract_params(df_fitgsl, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2_gsl  <- subset(ayear_df2_gsl, !grepl("mean", year))
a_df2_gsl      <- extract_params(df_fitgsl, "a", "fit_a",
                                 "grandmean", "a\\[(\\d+)\\]")
a_df2_gsl      <- subset(a_df2_gsl, grandmean == "a")

# save csvs
write.csv(sigma_df2_gsl,  "output/GM_GSLparam_sigma.csv",  row.names = FALSE)
write.csv(bspp_df2_gsl,   "output/GM_GSLparam_bspp.csv",   row.names = FALSE)
write.csv(treeid_df2_gsl, "output/GM_GSLparam_treeid.csv", row.names = FALSE)
write.csv(aspp_df2_gsl,   "output/GM_GSLparam_aspp.csv",   row.names = FALSE)
write.csv(ayear_df2_gsl,  "output/GM_GSLparam_ayear.csv",  row.names = FALSE)
write.csv(a_df2_gsl,      "output/GM_GSLparam_a.csv",      row.names = FALSE)

##### Plot posterior vs priors for GSL fit #####
pdf(file = "figures/growthModelsMain/diagnostics/gslModelPriorVSPosterior.pdf", width = 8, height = 10)
par(mfrow = c(3, 2))

# a
plot(density(df_fitgsl[, "a_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_a", xlab = "a", ylim = c(0, 0.5))
lines(density(df_fitgsl[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitgsl[, "sigma_atreeid_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_atreeid", xlab = "sigma_atreeid", ylim = c(0, 2))
lines(density(df_fitgsl[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitgsl[, "sigma_y_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_y", xlab = "sigma_y", ylim = c(0, 2))
lines(density(df_fitgsl[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitgsl[, "aspp_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_aspp", xlab = "aspp", xlim = c(-20, 20), ylim = c(0, 0.15))
for (col in colnames(aspp_df)) { lines(density(aspp_df[, col]), col = pal[2], lwd = 1) } 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitgsl[, "bsp_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_bsp", xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) { lines(density(bspp_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fitgsl[, "ayear_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_ayear", xlab = "ayear", xlim = c(-3, 3), ylim = c(0, 1.5))
for (col in colnames(ayear_df)) { lines(density(ayear_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot SOS fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
##### Recover parameters #####
df_fitsos <- as.data.frame(fitsos)

# full posterior arrays for multi-line extraction
columns <- colnames(df_fitsos)[!grepl("prior", colnames(df_fitsos))]
bspp_df <- df_fitsos[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitsos[, grepl("atreeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fitsos[, columns[grepl("aspp", columns)]]
ayear_df <- df_fitsos[, columns[grepl("ayear", columns) & !grepl("mean", columns)]]

# change colnames to indexes for loop tracing
colnames(bspp_df)  <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df)   <- 1:ncol(aspp_df)
colnames(ayear_df)  <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2_sos  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_sos   <- extract_params(df_fitsos, "bspp", "fit_bspp", "spp", "bspp\\[(\\d+)\\]")
treeid_df2_sos <- extract_params(df_fitsos, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2_sos <- subset(treeid_df2_sos, !grepl("z|sigma", id))
aspp_df2_sos   <- extract_params(df_fitsos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2_sos  <- extract_params(df_fitsos, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2_sos  <- subset(ayear_df2_sos, !grepl("mean", year))
a_df2_sos      <- extract_params(df_fitsos, "a", "fit_a",
                                 "grandmean", "a\\[(\\d+)\\]")
a_df2_sos      <- subset(a_df2_sos, grandmean == "a")

# save csvs
write.csv(sigma_df2_sos,  "output/GM_SOSparam_sigma.csv",  row.names = FALSE)
write.csv(bspp_df2_sos,   "output/GM_SOSparam_bspp.csv",   row.names = FALSE)
write.csv(treeid_df2_sos, "output/GM_SOSparam_treeid.csv", row.names = FALSE)
write.csv(aspp_df2_sos,   "output/GM_SOSparam_aspp.csv",   row.names = FALSE)
write.csv(ayear_df2_sos,  "output/GM_SOSparam_ayear.csv",  row.names = FALSE)
write.csv(a_df2_sos,      "output/GM_SOSparam_a.csv",      row.names = FALSE)

##### Plot posterior vs priors for sos fit #####
pdf(file = "figures/growthModelsMain/diagnostics/sosModelPriorVSPosterior.pdf", width = 8, height = 10)
par(mfrow = c(3, 2))

# a
plot(density(df_fitsos[, "a_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_a", xlab = "a", ylim = c(0, 0.5))
lines(density(df_fitsos[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitsos[, "sigma_atreeid_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_atreeid", xlab = "sigma_atreeid", ylim = c(0, 2))
lines(density(df_fitsos[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitsos[, "sigma_y_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_y", xlab = "sigma_y", ylim = c(0, 2))
lines(density(df_fitsos[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitsos[, "aspp_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_aspp", xlab = "aspp", xlim = c(-20, 20), ylim = c(0, 0.15))
for (col in colnames(aspp_df)) { lines(density(aspp_df[, col]), col = pal[2], lwd = 1) } 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitsos[, "bsp_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_bsp", xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) { lines(density(bspp_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fitsos[, "ayear_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_ayear", xlab = "ayear", xlim = c(-3, 3), ylim = c(0, 1.5))
for (col in colnames(ayear_df)) { lines(density(ayear_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot EOS fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
##### Recover parameters #####
df_fiteos <- as.data.frame(fiteos)

# full posterior arrays for multi-line extraction
columns <- colnames(df_fiteos)[!grepl("prior", colnames(df_fiteos))]
bspp_df <- df_fiteos[, columns[grepl("bsp", columns)]]
treeid_df <- df_fiteos[, grepl("atreeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fiteos[, columns[grepl("aspp", columns)]]
ayear_df <- df_fiteos[, columns[grepl("ayear", columns) & !grepl("mean", columns)]]

# change colnames to indexes for loop tracing
colnames(bspp_df)  <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df)   <- 1:ncol(aspp_df)
colnames(ayear_df)  <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2_eos  <- extract_params(df_fiteos, "sigma", "mean", "sigma")
bspp_df2_eos   <- extract_params(df_fiteos, "bspp", "fit_bspp", "spp", "bspp\\[(\\d+)\\]")
treeid_df2_eos <- extract_params(df_fiteos, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2_eos <- subset(treeid_df2_eos, !grepl("z|sigma", id))
aspp_df2_eos   <- extract_params(df_fiteos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2_eos  <- extract_params(df_fiteos, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2_eos  <- subset(ayear_df2_eos, !grepl("mean", year))
a_df2_eos      <- extract_params(df_fiteos, "a", "fit_a",
                                 "grandmean", "a\\[(\\d+)\\]")
a_df2_eos      <- subset(a_df2_eos, grandmean == "a")

# save csvs
write.csv(sigma_df2_eos,  "output/GM_EOSparam_sigma.csv",  row.names = FALSE)
write.csv(bspp_df2_eos,   "output/GM_EOSparam_bspp.csv",   row.names = FALSE)
write.csv(treeid_df2_eos, "output/GM_EOSparam_treeid.csv", row.names = FALSE)
write.csv(aspp_df2_eos,   "output/GM_EOSparam_aspp.csv",   row.names = FALSE)
write.csv(ayear_df2_eos,  "output/GM_EOSparam_ayear.csv",  row.names = FALSE)
write.csv(a_df2_eos,      "output/GM_EOSparam_a.csv",      row.names = FALSE)

##### Plot posterior vs priors for eos fit #####
pdf(file = "figures/growthModelsMain/diagnostics/eosModelPriorVSPosterior.pdf", width = 8, height = 10)
par(mfrow = c(3, 2))

# a
plot(density(df_fiteos[, "a_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_a", xlab = "a", ylim = c(0, 0.5))
lines(density(df_fiteos[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fiteos[, "sigma_atreeid_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_atreeid", xlab = "sigma_atreeid", ylim = c(0, 2))
lines(density(df_fiteos[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fiteos[, "sigma_y_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_y", xlab = "sigma_y", ylim = c(0, 2))
lines(density(df_fiteos[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fiteos[, "aspp_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_aspp", xlab = "aspp", xlim = c(-20, 20), ylim = c(0, 0.15))
for (col in colnames(aspp_df)) { lines(density(aspp_df[, col]), col = pal[2], lwd = 1) } 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fiteos[, "bsp_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_bsp", xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) { lines(density(bspp_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fiteos[, "ayear_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_ayear", xlab = "ayear", xlim = c(-3, 3), ylim = c(0, 1.5))
for (col in colnames(ayear_df)) { lines(density(ayear_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
diagnostics_gdd <- util$extract_hmc_diagnostics(fitgdd)
diagnostics_gsl <- util$extract_hmc_diagnostics(fitgsl)
diagnostics_sos <- util$extract_hmc_diagnostics(fitsos)
diagnostics_eos <- util$extract_hmc_diagnostics(fiteos)
samples_gdd <- util$extract_expectand_vals(fitgdd)
samples_gsl <- util$extract_expectand_vals(fitgsl)
samples_sos <- util$extract_expectand_vals(fitsos)
samples_eos <- util$extract_expectand_vals(fiteos)

jpeg(filename = "figures/growthModelsMain/diagnostics/retrodictiveCheckHist.jpeg",
     width = 2400, height = 2400, res = 300)
util$plot_hist_quantiles(samples_gdd, "y_rep", 
                         -2, # lower x axis limit
                         4, # upper x axis limit
                         0.3, # binning
                         baseline_values = dgdd$y,
                         xlab = "log(Ring width)")
dev.off()


# Hist by species
# GDD
jpeg(filename = "figures/growthModelsMain/diagnostics/retrodictiveHistGDD.jpeg",
     width = 3600, height = 2000, res = 300)
par(mfrow = c(3, 4))
for (s in unique(dgdd$species)) { # s = 2
  idxs <- which(dgdd$species == s)
  samples_sub <- samples_gdd[grep(paste0("^y_rep\\[(", paste(idxs, collapse="|"), ")\\]$"), names(samples_gdd))]
  util$plot_hist_quantiles(samples_sub,
                           "y_rep",
                           -2,
                           4,
                           0.4,
                           baseline_values = dgdd$y[idxs],
                           xlab = "log(ring width)",
                           main = unique(empts$latbi[which(empts$spp_num == s)]))
}
dev.off()

# GSL
jpeg(filename = "figures/growthModelsMain/diagnostics/retrodictiveHistGSL.jpeg",
     width = 3600, height = 2000, res = 300)
par(mfrow = c(3, 4))
for (s in unique(dgsl$species)) { # s = 2
  idxs <- which(dgsl$species == s)
  samples_sub <- samples_gsl[grep(paste0("^y_rep\\[(", paste(idxs, collapse="|"), ")\\]$"), names(samples_gsl))]
  util$plot_hist_quantiles(samples_sub,
                           "y_rep",
                           -2,
                           4,
                           0.3,
                           baseline_values = dgsl$y[idxs],
                           xlab = "log(ring width)",
                           main = unique(empts$latbi[which(empts$spp_num == s)]))
}
dev.off()


# SOS
jpeg(filename = "figures/growthModelsMain/diagnostics/retrodictiveHistSOS.jpeg",
     width = 3600, height = 2000, res = 300)
par(mfrow = c(3, 4))
for (s in unique(dsos$species)) { # s = 2
  idxs <- which(dsos$species == s)
  samples_sub <- samples_sos[grep(paste0("^y_rep\\[(", paste(idxs, collapse="|"), ")\\]$"), names(samples_sos))]
  util$plot_hist_quantiles(samples_sub,
                           "y_rep",
                           -2,
                           4,
                           0.3,
                           baseline_values = dsos$y[idxs],
                           xlab = "log(ring width)",
                           main = unique(empts$latbi[which(empts$spp_num == s)]))
}
dev.off()

# EOS
jpeg(filename = "figures/growthModelsMain/diagnostics/retrodictiveHistEOS.jpeg",
     width = 3600, height = 2000, res = 300)
par(mfrow = c(3, 4))
for (s in unique(deos$species)) { # s = 2
  idxs <- which(deos$species == s)
  samples_sub <- samples_eos[grep(paste0("^y_rep\\[(", paste(idxs, collapse="|"), ")\\]$"), names(samples_eos))]
  util$plot_hist_quantiles(samples_sub,
                           "y_rep",
                           -2,
                           4,
                           0.3,
                           baseline_values = deos$y[idxs],
                           xlab = "log(ring width)",
                           main = unique(empts$latbi[which(empts$spp_num == s)]))
}
dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FULL DATA ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit model SOS 
# transform my groups to numeric values
empfullsosts$treeid_num <- match(empfullsosts$id, unique(empfullsosts$id))
empfullsosts$spp_num <- match(empfullsosts$latbi, unique(empfullsosts$latbi))
empfullsosts$year_num <- match(empfullsosts$year, unique(empfullsosts$year))

# order by tree id
treeid_spp <- unique(empfullsosts[, c("treeid_num", "spp_num", "id", "latbi")])

treeid_spp_ordered <- treeid_spp[order(treeid_spp$treeid_num), ]

sosscale <- 7
sos <- empfullsosts$leafout / sosscale
sosseq <-  seq(min(empfullsosts$leafout), max(empfullsosts$leafout), length.out = lineplotseqlength)

# data list for sos
dsos <- list(
  y = empfullsosts$loglength,
  N = nrow(empfullsosts),
  Nspp = length(unique(empfullsosts$spp_num)),
  species = as.numeric(as.character(empfullsosts$spp_num)),
  treeid = as.numeric(empfullsosts$treeid_num),
  Ntreeid = length(unique(as.numeric(empfullsosts$treeid_num))),
  year = as.numeric(empfullsosts$year_num),
  Nyear = length(unique(empfullsosts$year_num)),
  treeid_species = treeid_spp_ordered$spp_num,
  Ntreeid_per_spp = as.integer(table(treeid_spp_ordered$spp_num)),
  sos = empfullsosts$leafout / sosscale,
  sosseq = sosseq,
  sosscale = sosscale,
  Nsosseq = length(sosseq)
)

# Fit model SOS 
# transform my groups to numeric values
empfulleosts$spp_num <- match(empfulleosts$latbi, unique(empfulleosts$latbi))
empfulleosts$treeid_num <- match(empfulleosts$id, unique(empfulleosts$id))
empfulleosts$year_num <- match(empfulleosts$year, unique(empfulleosts$year))


eos <- empfulleosts$coloredLeaves / eosscale
eosseq <-  seq(min(empfulleosts$coloredLeaves), max(empfulleosts$coloredLeaves), length.out = lineplotseqlength)

# data list for eos
deos <- list(
  y = empfulleosts$loglength,
  N = nrow(empfulleosts),
  Nspp = length(unique(empfulleosts$spp_num)),
  species = as.numeric(as.character(empfulleosts$spp_num)),
  treeid = as.numeric(empfulleosts$treeid_num),
  Ntreeid = length(unique(as.numeric(empfulleosts$treeid_num))),
  year = as.numeric(empfulleosts$year_num),
  Nyear = length(unique(empfulleosts$year_num)),
  treeid_species = treeid_spp_ordered$spp_num,
  Ntreeid_per_spp = as.integer(table(treeid_spp_ordered$spp_num)),
  eos = empfulleosts$coloredLeaves / eosscale,
  eosseq = eosseq,
  eosscale = eosscale,
  Neosseq = length(eosseq)
)

if(runfulldata) {
sosmodel <- stan_model("stan/TSmodelGrowthSOS.stan")
fitsosfull <- sampling(sosmodel, data = dsos,
                       warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsosfull, "output/stanOutput/fitGrowthSOSFull")

# Fit model EOS
eosmodel <- stan_model("stan/TSmodelGrowthEOS.stan")
fiteosfull <- sampling(eosmodel, data = deos,
                       warmup = 1000, iter = 2000, chains=4)
saveRDS(fiteosfull, "output/stanOutput/fitGrowthEOSFull")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Recover and plot parameters SOS restricted vs full #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
fitsosfull <- readRDS("output/stanOutput/fitGrowthSOSFull")
fiteosfull <- readRDS("output/stanOutput/fitGrowthEOSFull")

# SOS restricted
df_fitsos <- as.data.frame(fitsosfull)

# posterior summaries
sigma_df2_sos_full  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_sos_full   <- extract_params(df_fitsos, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_sos_full <- extract_params(df_fitsos, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2_sos_full <- subset(treeid_df2_sos_full, !grepl("prior|z|sigma", id))
aspp_df2_sos_full   <- extract_params(df_fitsos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2_sos_full  <- extract_params(df_fitsos, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2_sos_full <- subset(ayear_df2_sos_full, !grepl("mean", year))

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

# posterior summaries
sigma_df2_eos_full  <- extract_params(df_fiteos, "sigma", "mean", "sigma")
bspp_df2_eos_full   <- extract_params(df_fiteos, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_eos_full <- extract_params(df_fiteos, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2_eos_full <- subset(treeid_df2_eos_full, !grepl("z|sigma", id))
aspp_df2_eos_full   <- extract_params(df_fiteos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df2_eos_full <- subset(treeid_df2_eos_full, !grepl("prior", id))
ayear_df2_eos_full   <- extract_params(df_fitsos, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2_eos_full <- subset(ayear_df2_eos_full, !grepl("mean", year))

# Recover restricted
# SOS 
sigma_df2_sos  <- read.csv("output/GM_SOSparam_sigma.csv")
bspp_df2_sos   <- read.csv("output/GM_SOSparam_bspp.csv")
treeid_df2_sos <- read.csv("output/GM_SOSparam_treeid.csv")
aspp_df2_sos   <- read.csv("output/GM_SOSparam_aspp.csv")
ayear_df2_sos  <- read.csv("output/GM_SOSparam_ayear.csv")

treeid_df2_sos$treeid_name <- empts$id[match(treeid_df2_sos$id, empts$treeid_num)]
bspp_df2_sos$spp_name <- empts$latbi[match(bspp_df2_sos$spp, empts$spp_num)]
aspp_df2_sos$spp_name <- empts$latbi[match(aspp_df2_sos$spp, empts$spp_num)]
ayear_df2_sos$year_name <- empts$year[match(ayear_df2_sos$year, empts$year_num)]

# EOS 
sigma_df2_eos  <- read.csv("output/GM_EOSparam_sigma.csv")
bspp_df2_eos   <- read.csv("output/GM_EOSparam_bspp.csv")
treeid_df2_eos <- read.csv("output/GM_EOSparam_treeid.csv")
aspp_df2_eos   <- read.csv("output/GM_EOSparam_aspp.csv")
ayear_df2_eos  <- read.csv("output/GM_EOSparam_ayear.csv")

treeid_df2_eos$treeid_name <- empts$id[match(treeid_df2_eos$id, empts$treeid_num)]
bspp_df2_eos$spp_name <- empts$latbi[match(bspp_df2_eos$spp, empts$spp_num)]
aspp_df2_eos$spp_name <- empts$latbi[match(aspp_df2_eos$spp, empts$spp_num)]
ayear_df2_eos$year_name <- empts$year[match(ayear_df2_eos$year, empts$year_num)]


pdf("figures/growthModelsMain/FullVSRestricted.pdf", width = 9, height = 6)
par(mfrow = c(2, 4), oma = c(2, 2, 2, 0))


plot(sigma_df2_sos$mean, sigma_df2_sos_full$mean,
     xlab = "Restricted dataset", ylab = "Full dataset", 
     main = bquote(sigma),
     type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_sos_full$p25, sigma_df2_sos_full$p75)),
     xlim = range(c(sigma_df2_sos$p25, sigma_df2_sos$p75)))
arrows(x0 = sigma_df2_sos$mean, y0 = sigma_df2_sos_full$p25,
       x1 = sigma_df2_sos$mean, y1 = sigma_df2_sos_full$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_sos$p25, y0 = sigma_df2_sos_full$mean,
       x1 = sigma_df2_sos$p75, y1 = sigma_df2_sos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_sos$mean, sigma_df2_sos_full$mean,
       pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_sos$mean, sigma_df2_sos_full$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
text(sigma_df2_sos$p25, sigma_df2_sos_full$p75,
     labels = parse(text = c("sigma[atreeid]", "sigma[y]")),
     pos = c(4,4), cex = 1)
# bspp
plot(bspp_df2_sos$mean, bspp_df2_sos_full$mean,
     xlab = "Restricted dataset", ylab = "Full dataset", 
     main = lapply("species", function(s) bquote(beta[.(s)])), 
     type = "n", frame = FALSE,
     ylim = range(c(bspp_df2_sos_full$p25, bspp_df2_sos_full$p75)),
     xlim = range(c(bspp_df2_sos$p25, bspp_df2_sos$p75)))
arrows(x0 = bspp_df2_sos$mean, y0 = bspp_df2_sos_full$p25,
       x1 = bspp_df2_sos$mean, y1 = bspp_df2_sos_full$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_sos$p25, y0 = bspp_df2_sos_full$mean,
       x1 = bspp_df2_sos$p75, y1 = bspp_df2_sos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_sos$mean, bspp_df2_sos_full$mean,
       pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# aspp
plot(aspp_df2_sos$mean, aspp_df2_sos_full$mean,
     xlab = "Restricted dataset", ylab = "Full dataset", 
     main = lapply("species", function(s) bquote(alpha[.(s)])), 
     type = "n", frame = FALSE, 
     ylim = range(c(aspp_df2_sos_full$p25, aspp_df2_sos_full$p75)),
     xlim = range(c(aspp_df2_sos$p25, aspp_df2_sos$p75)))
arrows(x0 = aspp_df2_sos$mean, y0 = aspp_df2_sos_full$p25,
       x1 = aspp_df2_sos$mean, y1 = aspp_df2_sos_full$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = aspp_df2_sos$p25, y0 = aspp_df2_sos_full$mean,
       x1 = aspp_df2_sos$p75, y1 = aspp_df2_sos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(aspp_df2_sos$mean, aspp_df2_sos_full$mean,
       pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# aspp
plot(ayear_df2_sos$mean, ayear_df2_sos_full$mean,
     xlab = "Restricted dataset", ylab = "Full dataset", 
     main = lapply("year", function(s) bquote(alpha[.(s)])), 
     type = "n", frame = FALSE, 
     ylim = range(c(ayear_df2_sos_full$p25, ayear_df2_sos_full$p75)),
     xlim = range(c(ayear_df2_sos$p25, ayear_df2_sos$p75)))
arrows(x0 = ayear_df2_sos$mean, y0 = ayear_df2_sos_full$p25,
       x1 = ayear_df2_sos$mean, y1 = ayear_df2_sos_full$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = ayear_df2_sos$p25, y0 = ayear_df2_sos_full$mean,
       x1 = ayear_df2_sos$p75, y1 = ayear_df2_sos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(ayear_df2_sos$mean, ayear_df2_sos_full$mean,
       pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)


# EOS
plot(sigma_df2_eos$mean, sigma_df2_eos_full$mean,
     xlab = "Restricted dataset", ylab = "Full dataset", 
     main = bquote(sigma),
     type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_eos_full$p25, sigma_df2_eos_full$p75)),
     xlim = range(c(sigma_df2_eos$p25, sigma_df2_eos$p75)))
arrows(x0 = sigma_df2_eos$mean, y0 = sigma_df2_eos_full$p25,
       x1 = sigma_df2_eos$mean, y1 = sigma_df2_eos_full$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_eos$p25, y0 = sigma_df2_eos_full$mean,
       x1 = sigma_df2_eos$p75, y1 = sigma_df2_eos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_eos$mean, sigma_df2_eos_full$mean,
       pch = 16, col = "#d39822", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_eos$mean, sigma_df2_eos_full$mean, pch = 16, col = "#d39822", cex = 1.5)
text(sigma_df2_sos$p25, sigma_df2_sos_full$p75,
     labels = parse(text = c("sigma[atreeid]", "sigma[y]")),
     pos = c(4,4), cex = 1)

# bspp
plot(bspp_df2_eos$mean, bspp_df2_eos_full$mean,
     xlab = "Restricted dataset", ylab = "Full dataset", 
     main = lapply("species", function(s) bquote(beta[.(s)])),
     type = "n", frame = FALSE,
     ylim = range(c(bspp_df2_eos_full$p25, bspp_df2_eos_full$p75)),
     xlim = range(c(bspp_df2_eos$p25, bspp_df2_eos$p75)))
arrows(x0 = bspp_df2_eos$mean, y0 = bspp_df2_eos_full$p25,
       x1 = bspp_df2_eos$mean, y1 = bspp_df2_eos_full$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_eos$p25, y0 = bspp_df2_eos_full$mean,
       x1 = bspp_df2_eos$p75, y1 = bspp_df2_eos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_eos$mean, bspp_df2_eos_full$mean,
       pch = 16, col = "#d39822", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# aspp
plot(aspp_df2_eos$mean, aspp_df2_eos_full$mean,
     xlab = "Restricted dataset", ylab = "Full dataset", 
     main = lapply("species", function(s) bquote(alpha[.(s)])),
     type = "n", frame = FALSE,
     ylim = range(c(aspp_df2_eos_full$p25, aspp_df2_eos_full$p75)),
     xlim = range(c(aspp_df2_eos$p25, aspp_df2_eos$p75)))
arrows(x0 = aspp_df2_eos$mean, y0 = aspp_df2_eos_full$p25,
       x1 = aspp_df2_eos$mean, y1 = aspp_df2_eos_full$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = aspp_df2_eos$p25, y0 = aspp_df2_eos_full$mean,
       x1 = aspp_df2_eos$p75, y1 = aspp_df2_eos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(aspp_df2_eos$mean, aspp_df2_eos_full$mean,
       pch = 16, col = "#d39822", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# ayear
plot(ayear_df2_eos$mean, ayear_df2_eos_full$mean,
     xlab = "Restricted dataset", ylab = "Full dataset", 
     main = lapply("year", function(s) bquote(alpha[.(s)])),
     type = "n", frame = FALSE,
     ylim = range(c(ayear_df2_eos_full$p25, ayear_df2_eos_full$p75)),
     xlim = range(c(ayear_df2_eos$p25, ayear_df2_eos$p75)))
arrows(x0 = ayear_df2_eos$mean, y0 = ayear_df2_eos_full$p25,
       x1 = ayear_df2_eos$mean, y1 = ayear_df2_eos_full$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = ayear_df2_eos$p25, y0 = ayear_df2_eos_full$mean,
       x1 = ayear_df2_eos$p75, y1 = ayear_df2_eos_full$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(ayear_df2_eos$mean, ayear_df2_eos_full$mean,
       pch = 16, col = "#d39822", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

mtext("(a) Start of season (SOS)",
      side = 3, outer = TRUE, at = 0, adj = 0, font = 2, las = 1, line = -0.5)
mtext("(b) End of season",
      side = 3, outer = TRUE, at = 0, adj = 0, font = 2, las = 1, line = -22)
dev.off()

}


}
# === === === === === === === === === === === === === === === === === === === #
# === === === === === === === === === === === === === === === === === === === #

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Z-SCORED ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if (runzscoredmodels) {
  
# Fit model GDD
dgdd$covariate <- (empts$pgsGDD5 - mean(empts$pgsGDD5)) / sd(empts$pgsGDD5)
gddmodel <- stan_model("stan/TSmodelGrowth_z.stan")
fitgdd <- sampling(gddmodel, data = dgdd,
                   warmup = 1000, iter=2000, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDDZscored")

diagnostics <- util$extract_hmc_diagnostics(fitgdd)
util$check_all_hmc_diagnostics(diagnostics)

# Fit model GSL
dgsl$covariate <- (empts$pgsGSL - mean(empts$pgsGSL)) / sd(empts$pgsGSL)
gslmodel <- stan_model("stan/TSmodelGrowth_z.stan")
fitgsl <- sampling(gslmodel, data = dgsl,
                   warmup = 1000, iter = 2000, chains = 4)
saveRDS(fitgsl, "output/stanOutput/fitGrowthGSLZscored")

# Fit model SOS
dsos$covariate <- (empts$leafout - mean(empts$leafout)) / sd(empts$leafout)
sosmodel <- stan_model("stan/TSmodelGrowth_z.stan")
fitsos <- sampling(sosmodel, data = dsos,
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOSZscored")

# Fit model EOS
deos$covariate <- (empts$coloredLeaves - mean(empts$coloredLeaves)) / 
  sd(empts$coloredLeaves)
eosmodel <- stan_model("stan/TSmodelGrowth_z.stan")
fiteos <- sampling(eosmodel, data = deos,
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fiteos, "output/stanOutput/fitGrowthEOSZscored")

# Plot GDD fit
fitgdd <- readRDS("output/stanOutput/fitGrowthGDDZscored")
fitgsl <- readRDS("output/stanOutput/fitGrowthGSLZscored")
fitsos <- readRDS("output/stanOutput/fitGrowthSOSZscored")
fiteos <- readRDS("output/stanOutput/fitGrowthEOSZscored")

df_fitgdd <- as.data.frame(fitgdd)

# full posterior
columns <- colnames(df_fitgdd)[!grepl("prior", colnames(df_fitgdd))]
sigma_df <- df_fitgdd[, columns[grepl("sigma", columns)]]
bspp_df <- df_fitgdd[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitgdd[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fitgdd[, columns[grepl("aspp", columns)]]
ayear_df2   <- extract_params(df_fitgdd, "ayear", "fit_ayear",
                              "year", "ayear\\[(\\d+)\\]")
ayear_df <- df_fitgdd[, columns[grepl("ayear", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)
colnames(ayear_df) <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2  <- extract_params(df_fitgdd, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fitgdd, "bsp", "fit_bspp", 
                             "spp", "bsp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", 
                             "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z|sigma", treeid))
aspp_df2   <- extract_params(df_fitgdd, "aspp", "fit_aspp", 
                             "spp", "aspp\\[(\\d+)\\]")
a_df2      <- extract_params(df_fitgdd, "a", "fit_a",
                             "grandmean", "a\\[(\\d+)\\]")
a_df2      <- subset(a_df2, grandmean == "a")

# save csvs
write.csv(sigma_df2,  "output/GM_GDDparam_Z_sigma.csv",  row.names = FALSE)
write.csv(bspp_df2,   "output/GM_GDDparam_Z_bspp.csv",   row.names = FALSE)
write.csv(treeid_df2, "output/GM_GDDparam_Z_treeid.csv", row.names = FALSE)
write.csv(aspp_df2,   "output/GM_GDDparam_Z_aspp.csv",   row.names = FALSE)
write.csv(ayear_df2,  "output/GM_GDDparam_Z_ayear.csv",  row.names = FALSE)
write.csv(a_df2,      "output/GM_GDDparam_Z_a.csv",      row.names = FALSE)

##### Plot posterior vs priors for gdd fit #####
pdf(file = "figures/growthModelsMain/diagnostics/ZgddModelPriorVSPosterior.pdf", width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 2))

# a
plot(density(df_fitgdd[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0, 1))
lines(density(df_fitgdd[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitgdd[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,4))
lines(density(df_fitgdd[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitgdd[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0, 4))
lines(density(df_fitgdd[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitgdd[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", 
     # xlim = c(-5, 5), 
     ylim = c(0, 1))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fitgdd[, "ayear_prior"]),
     col = pal[1], lwd = 2,
     main = "priorVSposterior_ayear",
     xlab = "ayear",
     ylim = c(0, 1))
for (col in colnames(ayear_df)) {
  lines(density(ayear_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitgdd[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 5))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()

# Plot GSL fit
df_fitgsl <- as.data.frame(fitgsl)

# full posterior
columns <- colnames(df_fitgsl)[!grepl("prior", colnames(df_fitgsl))]
sigma_df <- df_fitgsl[, columns[grepl("sigma", columns)]]
bspp_df <- df_fitgsl[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitgsl[, grepl("treeid", columns) & 
                         !grepl("z|sigma", columns)]
aspp_df <- df_fitgsl[, columns[grepl("aspp", columns)]]
ayear_df <- df_fitgsl[, columns[grepl("ayear", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)
colnames(ayear_df) <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fitgsl, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("prior|z|sigma", treeid))
aspp_df2   <- extract_params(df_fitgsl, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2  <- extract_params(df_fitgsl, "ayear", "fit_ayear",
                             "year", "ayear\\[(\\d+)\\]")
a_df2_gsl  <- extract_params(df_fitgsl, "a", "fit_a",
                             "grandmean", "a\\[(\\d+)\\]")
a_df2_gsl  <- subset(a_df2_gsl, grandmean == "a")

# save csvs
write.csv(sigma_df2,  "output/GM_GSLparam_Z_sigma.csv",  row.names = FALSE)
write.csv(bspp_df2,   "output/GM_GSLparam_Z_bspp.csv",   row.names = FALSE)
write.csv(treeid_df2, "output/GM_GSLparam_Z_treeid.csv", row.names = FALSE)
write.csv(aspp_df2,   "output/GM_GSLparam_Z_aspp.csv",   row.names = FALSE)
write.csv(ayear_df2,  "output/GM_GSLparam_Z_ayear.csv",  row.names = FALSE)
write.csv(a_df2_gsl,  "output/GM_GSLparam_Z_a.csv",      row.names = FALSE)

##### Plot posterior vs priors for GSL fit #####
pdf(file = "figures/growthModelsMain/diagnostics/ZgslModelPriorVSPosterior.pdf", width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 2))

# a
plot(density(df_fitgsl[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0, 1))
lines(density(df_fitgsl[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitgsl[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,4))
lines(density(df_fitgsl[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitgsl[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0, 4))
lines(density(df_fitgsl[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitgsl[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", 
     # xlim = c(-5, 5), 
     ylim = c(0, 1))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fitgsl[, "ayear_prior"]),
     col = pal[1], lwd = 2,
     main = "priorVSposterior_ayear",
     xlab = "ayear",
     ylim = c(0, 1))
for (col in colnames(ayear_df)) {
  lines(density(ayear_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitgsl[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 5))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()

# Plot SOS fit 
# Recover parameters
df_fitsos <- as.data.frame(fitsos)

# full posterior
columns <- colnames(df_fitsos)[!grepl("prior", colnames(df_fitsos))]
sigma_df <- df_fitsos[, columns[grepl("sigma", columns)]]
bspp_df <- df_fitsos[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitsos[, grepl("treeid", columns) & 
                         !grepl("z|sigma", columns)]
aspp_df <- df_fitsos[, columns[grepl("aspp", columns)]]
ayear_df <- df_fitsos[, columns[grepl("ayear", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)
colnames(ayear_df) <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2_sos  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_sos   <- extract_params(df_fitsos, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_sos <- extract_params(df_fitsos, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_sos <- subset(treeid_df2_sos, !grepl("prior|z|sigma", treeid))
aspp_df2_sos   <- extract_params(df_fitsos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2_sos  <- extract_params(df_fitsos, "ayear", "fit_ayear",
                                 "year", "ayear\\[(\\d+)\\]")
a_df2_sos      <- extract_params(df_fitsos, "a", "fit_a",
                                 "grandmean", "a\\[(\\d+)\\]")
a_df2_sos      <- subset(a_df2_sos, grandmean == "a")

# save csvs
write.csv(sigma_df2_sos,  "output/GM_SOS_param_Z_sigma.csv",  row.names = FALSE)
write.csv(bspp_df2_sos,   "output/GM_SOS_param_Z_bspp.csv",   row.names = FALSE)
write.csv(treeid_df2_sos, "output/GM_SOS_param_Z_treeid.csv", row.names = FALSE)
write.csv(aspp_df2_sos,   "output/GM_SOS_param_Z_aspp.csv",   row.names = FALSE)
write.csv(ayear_df2_sos,  "output/GM_SOS_param_Z_ayear.csv",  row.names = FALSE)
write.csv(a_df2_sos,      "output/GM_SOS_param_Z_a.csv",      row.names = FALSE)

##### Plot posterior vs priors for sos fit #####
pdf(file = "figures/growthModelsMain/diagnostics/ZsosModelPriorVSPosterior.pdf", width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 2))

# a
plot(density(df_fitsos[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0, 1))
lines(density(df_fitsos[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitsos[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,4))
lines(density(df_fitsos[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitsos[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0, 4))
lines(density(df_fitsos[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitsos[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", 
     # xlim = c(-5, 5), 
     ylim = c(0, 1))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fitsos[, "ayear_prior"]),
     col = pal[1], lwd = 2,
     main = "priorVSposterior_ayear",
     xlab = "ayear",
     ylim = c(0, 1))
for (col in colnames(ayear_df)) {
  lines(density(ayear_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitsos[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 5))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()

# Plot EOS fit
# Recover parameters
df_fiteos <- as.data.frame(fiteos)

# full posterior
columns <- colnames(df_fiteos)[!grepl("prior", colnames(df_fiteos))]
sigma_df <- df_fiteos[, columns[grepl("sigma", columns)]]
bspp_df <- df_fiteos[, columns[grepl("bsp", columns)]]
treeid_df <- df_fiteos[, grepl("treeid", columns) & 
                         !grepl("z|sigma", columns)]
aspp_df <- df_fiteos[, columns[grepl("aspp", columns)]]
ayear_df <- df_fiteos[, columns[grepl("ayear", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)
colnames(ayear_df) <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2_eos  <- extract_params(df_fiteos, "sigma", "mean", "sigma")
bspp_df2_eos   <- extract_params(df_fiteos, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_eos <- extract_params(df_fiteos, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_eos <- subset(treeid_df2_eos, !grepl("prior|z|sigma", treeid))
aspp_df2_eos   <- extract_params(df_fiteos, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2_eos  <- extract_params(df_fiteos, "ayear", "fit_ayear",
                                 "year", "ayear\\[(\\d+)\\]")
a_df2_eos      <- extract_params(df_fiteos, "a", "fit_a",
                                 "grandmean", "a\\[(\\d+)\\]")
a_df2_eos      <- subset(a_df2_eos, grandmean == "a")

# save csvs
write.csv(sigma_df2_eos,  "output/GM_EOS_param_Z_sigma.csv",  row.names = FALSE)
write.csv(bspp_df2_eos,   "output/GM_EOS_param_Z_bspp.csv",   row.names = FALSE)
write.csv(treeid_df2_eos, "output/GM_EOS_param_Z_treeid.csv", row.names = FALSE)
write.csv(aspp_df2_eos,   "output/GM_EOS_param_Z_aspp.csv",   row.names = FALSE)
write.csv(ayear_df2_eos,  "output/GM_EOS_param_Z_ayear.csv",  row.names = FALSE)
write.csv(a_df2_eos,      "output/GM_EOS_param_Z_a.csv",      row.names = FALSE)

##### Plot posterior vs priors for eos fit #####
pdf(file = "figures/growthModelsMain/diagnostics/ZeosModelPriorVSPosterior.pdf", width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 2))

# a
plot(density(df_fiteos[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0, 1))
lines(density(df_fiteos[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fiteos[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,4))
lines(density(df_fiteos[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fiteos[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0, 4))
lines(density(df_fiteos[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fiteos[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", 
     # xlim = c(-5, 5), 
     ylim = c(0, 1))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fiteos[, "ayear_prior"]),
     col = pal[1], lwd = 2,
     main = "priorVSposterior_ayear",
     xlab = "ayear",
     ylim = c(0, 1))
for (col in colnames(ayear_df)) {
  lines(density(ayear_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fiteos[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 5))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()

}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Compare model with and without ayear ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Fit model GDD WITHOUT ayear #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
if(runmodelnoayear){
gddmodel <- stan_model("stan/TSmodelGrowthGDD_noayear.stan")
fitgdd <- sampling(gddmodel, data = dgdd,
                   warmup = 1000, iter=2000, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDD_noayear")
fitgdd <- readRDS("output/stanOutput/fitGrowthGDD_noayear")
# Recover parameters
df_fitgdd <- as.data.frame(fitgdd)

sigma_df2_noayr  <- extract_params(df_fitgdd, "sigma", "mean", "sigma")
bspp_df2_noayr   <- extract_params(df_fitgdd, "bsp", "fit_bspp", 
                                   "spp", "bsp\\[(\\d+)\\]")
treeid_df2_noayr <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", 
                                   "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_noayr <- subset(treeid_df2_noayr, !grepl("z|sigma", treeid))
aspp_df2_noayr   <- extract_params(df_fitgdd, "aspp", "fit_aspp", 
                                   "spp", "aspp\\[(\\d+)\\]")

# Recover model with ayear
fitgdd <- readRDS("output/stanOutput/fitGrowthGDD")
df_fitgdd <- as.data.frame(fitgdd)
# posterior summaries
sigma_df2  <- extract_params(df_fitgdd, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fitgdd, "bspp", "fit_bspp", "spp", "bspp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z|sigma", id))
aspp_df2   <- extract_params(df_fitgdd, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2  <- extract_params(df_fitgdd, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2  <- subset(ayear_df2, !grepl("mean", year))

# Open device
jpeg("figures/growthModelsMain/withoutVSnoayr.jpeg", width = 9, height = 6, units = "in", res = 300)
par(mfrow = c(2,2), oma = c(0, 2, 0, 0))

# sigma
plot(sigma_df2_noayr$mean, sigma_df2$mean,
     xlab = "no year intercept", ylab = "with year intercept", main = "sigmas", type = "n", frame = FALSE,
     ylim = range(c(sigma_df2$p25, sigma_df2$p75)),
     xlim = range(c(sigma_df2_noayr$p25, sigma_df2_noayr$p75+0.2)))
arrows(x0 = sigma_df2_noayr$mean, y0 = sigma_df2$p25,
       x1 = sigma_df2_noayr$mean, y1 = sigma_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_noayr$p25, y0 = sigma_df2$mean,
       x1 = sigma_df2_noayr$p75, y1 = sigma_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_noayr$mean, sigma_df2$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_noayr$mean, sigma_df2$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
text(sigma_df2_noayr$p75, sigma_df2$p25, labels = sigma_df2_noayr$sigma, pos = c(3,3), cex = 0.75)

# bspp
plot(bspp_df2_noayr$mean, bspp_df2$mean,
     xlab = "no year intercept", ylab = "with year intercept", main = "bspp", type = "n", frame = FALSE,
     ylim = range(c(bspp_df2$p25, bspp_df2$p75)),
     xlim = range(c(bspp_df2_noayr$p25, bspp_df2_noayr$p75)))
arrows(x0 = bspp_df2_noayr$mean, y0 = bspp_df2$p25,
       x1 = bspp_df2_noayr$mean, y1 = bspp_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_noayr$p25, y0 = bspp_df2$mean,
       x1 = bspp_df2_noayr$p75, y1 = bspp_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_noayr$mean, bspp_df2$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# aspp
plot(aspp_df2_noayr$mean, aspp_df2$mean,
     xlab = "no year intercept", ylab = "with year intercept", main = "aspp", type = "n", frame = FALSE,
     ylim = range(c(aspp_df2$p25, aspp_df2$p75)),
     xlim = range(c(aspp_df2_noayr$p25, aspp_df2_noayr$p75)))
arrows(x0 = aspp_df2_noayr$mean, y0 = aspp_df2$p25,
       x1 = aspp_df2_noayr$mean, y1 = aspp_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = aspp_df2_noayr$p25, y0 = aspp_df2$mean,
       x1 = aspp_df2_noayr$p75, y1 = aspp_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(aspp_df2_noayr$mean, aspp_df2$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# atreeid
plot(treeid_df2_noayr$mean, treeid_df2$mean,
     xlab = "no year intercept", ylab = "with year intercept", main = "atreeid", type = "n", frame = FALSE,
     ylim = range(c(treeid_df2$p25, treeid_df2$p75)),
     xlim = range(c(treeid_df2_noayr$p25, treeid_df2_noayr$p75)))
arrows(x0 = treeid_df2_noayr$mean, y0 = treeid_df2$p25,
       x1 = treeid_df2_noayr$mean, y1 = treeid_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1, col = "darkgray")
arrows(x0 = treeid_df2_noayr$p25, y0 = treeid_df2$mean,
       x1 = treeid_df2_noayr$p75, y1 = treeid_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1, col = "darkgray")
points(treeid_df2_noayr$mean, treeid_df2$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)
dev.off()
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit model previous year ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if(fitmodelprvsyr){
# add a year diff index 
empts <- read.csv("output/empiricalDataMAIN.csv")
years <- 2016:2024
empts$yeardiff <- NA
for (i in years) {
  empts$yeardiff[empts$year == i] <- empts$year[empts$year == i] - 1
}

# remove NAs
empts$idyear <- paste0(empts$id, "_", empts$year)
empts$idyearprvs <- paste0(empts$id, "_", empts$yeardiff)
empts$gddprvsyr <- empts$pgsGDD5[match(empts$idyearprvs, empts$idyear)]
empts[, c("id", "year", "pgsGDD5", "gddprvsyr")]

empts2 <- empts[!is.na(empts$pgsGDD5) & !is.na(empts$gddprvsyr)
               & !is.na(empts$lengthMM),]

# transform my groups to numeric values
empts2$spp_num <- match(empts2$latbi, unique(empts2$latbi))
empts2$treeid_num <- match(empts2$id, unique(empts2$id))
empts2$year_num <- match(empts2$year, unique(empts2$year))

dgddyr <- list(
  y = log(empts2$lengthMM),
  N = nrow(empts2),
  Nspp = length(unique(empts2$spp_num)),
  species = as.numeric(as.character(empts2$spp_num)),
  treeid = as.numeric(empts2$treeid_num),
  Ntreeid = length(unique(as.numeric(empts2$treeid_num))),
  year = as.numeric(empts2$year_num),
  Nyear = length(unique(empts2$year_num)),
  gdd = empts2$pgsGDD5/176,
  gddyr = empts2$gddprvsyr / 176
)

# Fit model GDD with previous year GDD
gddmodel <- stan_model("stan/TSmodelGrowthPreviousYear.stan")
fit <- sampling(gddmodel, data = dgddyr, iter = 2000, chains = 4, 
                control = list(max_treedepth = 12))
saveRDS(fit, "output/stanOutput/fitGrowthPreviousYear")
diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)
samples <- util$extract_expectand_vals(fit)

if(FALSE){
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Retrodictive checks #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
fit <- readRDS("output/stanOutput/fitGrowthPreviousYear")
jpeg(
  filename = "figures/growthPreviousYearModel/retrodictiveCheckHistPrvsYr.jpeg",
  width = 2400, height = 2400, res = 300)
util$plot_hist_quantiles(samples, "y_rep", 
                         -2, # lower x axis limit
                         5, # upper x axis limit
                         0.2, # binning
                         baseline_values = data$y,
                         xlab = "log(ring width")
dev.off()

# discs by species
jpeg(
  filename = "figures/growthPreviousYearModel/retrodictiveDiskSpp.jpeg",
  width = 3600, height = 2400, res = 300)
par(mfrow = c(3,4))
for (s in unique(data$species)) { # s = 1
  idxs <- which(data$species == s)
  util$plot_disc_pushforward_quantiles(samples,
                                       paste0("y_rep[", idxs, "]"),
                                       baseline_values = data$y[idxs],
                                       ylab = "Ring width",
                                       main = paste("Spp", s))
}
dev.off()
# discs by year
par(mfrow = c(1,data$Nyear))
for (y in unique(data$year)) { # s = 1
  idxs <- which(data$year == y)
  util$plot_disc_pushforward_quantiles(samples,
                                       paste0("y_rep[", idxs, "]"),
                                       baseline_values = data$y[idxs],
                                       ylab = "Leafout",
                                       main = paste("Year", y))
}
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Plot posterior vs priors for gdd fit #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
fit <- readRDS("output/stanOutput/fitGrowthPreviousYear")

# Setup color palette across all plots
pal <- wes_palette("AsteroidCity1")[3:4]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Recover parameters #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
df_fit <- as.data.frame(fit)

# full posterior arrays for multi-line extraction
columns <- colnames(df_fit)[!grepl("prior", colnames(df_fit))]
bspp_df <- df_fit[, columns[grepl("bsp", columns) & !grepl("yr", columns)]]
bsppyr_df <- df_fit[, columns[grepl("bspyr", columns)]]
treeid_df <- df_fit[, grepl("atreeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fit[, columns[grepl("aspp", columns)]]
ayear_df <- df_fit[, columns[grepl("ayear", columns) & !grepl("mean", columns)]]

# change colnames to indexes for loop tracing
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(bsppyr_df) <- 1:ncol(bsppyr_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)
colnames(ayear_df) <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2  <- extract_params(df_fit, "sigma", "mean", "sigma")
bspp_df2_curr <- extract_params(df_fit, "bsp", "fit_bsp", "spp", "bsp\\[(\\d+)\\]")
bspp_df2_curr <- subset(bspp_df2_curr, !grepl("yr", spp))
bspp_df2_prvs <- extract_params(df_fit, "bspyr", "fit_bsp", "spp", "bspyr\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fit, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z|sigma", id))
aspp_df2   <- extract_params(df_fit, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2  <- extract_params(df_fit, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2  <- subset(ayear_df2, !grepl("mean", year))

# save csvs
write.csv(sigma_df2,  "output/GM_GDDparam_sigma_prvsYr.csv",  row.names = FALSE)
write.csv(bspp_df2_curr, "output/GM_GDDparam_bspp_prvsYr.csv",   row.names = FALSE)
write.csv(bspp_df2_prvs, "output/GM_GDDparam_bsppYr_prvsYr.csv",   row.names = FALSE)
write.csv(treeid_df2, "output/GM_GDDparam_treeid_prvsYr.csv", row.names = FALSE)
write.csv(aspp_df2,   "output/GM_GDDparam_aspp_prvsYr.csv",   row.names = FALSE)
write.csv(ayear_df2,  "output/GM_GDDparam_ayear.csv",  row.names = FALSE)

jpeg("figures/growthPreviousYearModel/gddModelPriorVSPosteriorPrvsYr.jpeg", 
     width =2400, height = 2400, res =300)
pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 3))

# a
plot(density(df_fit[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0,0.1))
lines(density(df_fit[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fit[, "sigma_atreeid_prior"]),
     col = pal[1], lwd = 2,
     main = "priorVSposterior_sigma_atreeid",
     xlab = "sigma_atreeid", ylim = c(0,2))
lines(density(df_fit[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fit[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0,2))
lines(density(df_fit[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fit[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", xlim = c(-60, 60), ylim = c(0, 0.1))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fit[, "ayear_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_ayear", 
     xlab = "ayear", xlim = c(-10, 10), ylim = c(0, 2))
for (col in colnames(ayear_df)) {
  lines(density(ayear_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fit[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bspyr
plot(density(df_fit[, "bspyr_prior"]),
     col = pal[1], lwd = 2,
     main = "priorVSposterior_bspPreviousYr",
     xlab = "bspPreviousYr", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) {
  lines(density(bsppyr_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit model budburst vs leafout ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if(runbudburst){
empts <- read.csv("output/empiricalDataMAIN.csv")

# remove entries for which we dont have budburst
empts$loglength <- log(empts$lengthMM)

nrow(empts[!is.na(empts$fgsGDD5) & !is.na(empts$loglength),])
nrow(empts[!is.na(empts$pgsGDD5) & !is.na(empts$loglength),])
emptscomp <- empts[!is.na(empts$fgsGDD5) & !is.na(empts$loglength),]
emptscomp <- subset(emptscomp, year != 2015)

lineplotseqlength <- 10

# transform my groups to numeric values
emptscomp$spp_num <- match(emptscomp$latbi, unique(emptscomp$latbi))
emptscomp$treeid_num <- match(emptscomp$id, unique(emptscomp$id))
emptscomp$year_num <- match(emptscomp$year, unique(emptscomp$year))

# order by tree id
treeid_spp <- unique(emptscomp[, c("treeid_num", "spp_num", "id", "latbi")])

treeid_spp_ordered <- treeid_spp[order(treeid_spp$treeid_num), ]
gddseq <- seq(min(emptscomp$fgsGDD5), max(emptscomp$fgsGDD5), length.out = lineplotseqlength)

dgdd <- list(
  y = emptscomp$loglength,
  N = nrow(emptscomp),
  Nspp = length(unique(emptscomp$spp_num)),
  species = as.numeric(as.character(emptscomp$spp_num)),
  treeid = as.numeric(emptscomp$treeid_num),
  Ntreeid = length(unique(as.numeric(emptscomp$treeid_num))),
  year = as.numeric(emptscomp$year_num),
  Nyear = length(unique(emptscomp$year_num)),
  treeid_species = treeid_spp_ordered$spp_num,
  Ntreeid_per_spp = as.integer(table(treeid_spp_ordered$spp_num)),
  gddseq = gddseq,
  tsgddscale = tsgddscale,
  Ngddseq = length(gddseq)
)
dgdd

# Set model GSL data
gslscale <- 7
emptscomp$fgsGSL <- emptscomp$coloredLeaves - emptscomp$budburst
gslseq <-  seq(min(emptscomp$fgsGSL), max(emptscomp$fgsGSL), length.out = lineplotseqlength)

# data list for GSL
dgsl <- list(
  y = emptscomp$loglength,
  N = nrow(emptscomp),
  Nspp = length(unique(emptscomp$spp_num)),
  species = as.numeric(as.character(emptscomp$spp_num)),
  treeid = as.numeric(emptscomp$treeid_num),
  Ntreeid = length(unique(as.numeric(emptscomp$treeid_num))),
  year = as.numeric(emptscomp$year_num),
  Nyear = length(unique(emptscomp$year_num)),
  treeid_species = treeid_spp_ordered$spp_num,
  Ntreeid_per_spp = as.integer(table(treeid_spp_ordered$spp_num)),
  gslseq = gslseq,
  gslscale = gslscale,
  Ngslseq = length(gslseq)
)

sosscale <- 7
sos <- emptscomp$budburst / sosscale
sosseq <-  seq(min(emptscomp$budburst), max(emptscomp$budburst), length.out = lineplotseqlength)

# data list for sos
dsos <- list(
  y = emptscomp$loglength,
  N = nrow(emptscomp),
  Nspp = length(unique(emptscomp$spp_num)),
  species = as.numeric(as.character(emptscomp$spp_num)),
  treeid = as.numeric(emptscomp$treeid_num),
  Ntreeid = length(unique(as.numeric(emptscomp$treeid_num))),
  year = as.numeric(emptscomp$year_num),
  Nyear = length(unique(emptscomp$year_num)),
  treeid_species = treeid_spp_ordered$spp_num,
  Ntreeid_per_spp = as.integer(table(treeid_spp_ordered$spp_num)),
  sosseq = sosseq,
  sosscale = sosscale,
  Nsosseq = length(sosseq)
)

if(FALSE) {
# translate model
model <- stan_model("stan/TSmodelGrowth_z.stan")

# Fit model GDD
dgdd$covariate <- (emptscomp$fgsGDD5 - mean(emptscomp$fgsGDD5)) / sd(emptscomp$fgsGDD5)
fitgdd <- sampling(model, data = dgdd, warmup = 1000, iter=2000, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDD_budburst")
diagnostics <- util$extract_hmc_diagnostics(fitgdd)
util$check_all_hmc_diagnostics(diagnostics)

# Fit model GSL
dgsl$covariate <- (emptscomp$fgsGSL - mean(emptscomp$fgsGSL)) / sd(emptscomp$fgsGSL)
fitgsl <- sampling(model, data = dgsl, warmup = 1000, iter = 2000, chains = 4)
saveRDS(fitgsl, "output/stanOutput/fitGrowthGSL_budburst")

# Fit model SOS
dsos$covariate <- (emptscomp$budburst - mean(emptscomp$budburst)) / sd(emptscomp$budburst)
fitsos <- sampling(model, data = dsos, warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOS_budburst")
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Recover models with budburst #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
fitgdd <- readRDS("output/stanOutput/fitGrowthGDD_budburst")
fitgsl <- readRDS("output/stanOutput/fitGrowthGSL_budburst")
fitsos <- readRDS("output/stanOutput/fitGrowthSOS_budburst")

df_fit    <- as.data.frame(fitgdd)
df_fitgsl <- as.data.frame(fitgsl)
df_fitsos <- as.data.frame(fitsos)

# posterior summaries
sigma_df2_bb  <- extract_params(df_fit, "sigma", "mean", "sigma")
bspp_df2_bb   <- extract_params(df_fit, "bsp", "fit_bsp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2_bb <- extract_params(df_fit, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_bb <- subset(treeid_df2_bb, !grepl("z", treeid) & !grepl("sigma", treeid))
aspp_df2_bb   <- extract_params(df_fit, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2_bb  <- extract_params(df_fit, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")

treeid_df2_bb$treeid_name <- empts$id[match(treeid_df2_bb$treeid, emptscomp$treeid_num)]
aspp_df2_bb$spp_name <- emptscomp$latbi[match(aspp_df2_bb$spp, emptscomp$spp_num)]
bspp_df2_bb$spp_name <- emptscomp$latbi[match(bspp_df2_bb$spp, emptscomp$spp_num)]
ayear_df2_bb$year_name <- emptscomp$year[match(ayear_df2_bb$year, emptscomp$year_num)]

# GSL posterior recovery
# posterior summaries
sigma_df2_bb_gsl  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2_bb_gsl   <- extract_params(df_fitgsl, "bsp", "fit_bsp", 
                                    "spp", "bsp\\[(\\d+)\\]")
treeid_df2_bb_gsl <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", 
                                    "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_bb_gsl <- subset(treeid_df2_bb_gsl, !grepl("z|sigma", treeid))
aspp_df2_bb_gsl   <- extract_params(df_fitgsl, "aspp", "fit_aspp", 
                                    "spp", "aspp\\[(\\d+)\\]")
ayear_df2_bb_gsl   <- extract_params(df_fit, "ayear", "fit_ayear", 
                                 "year", "ayear\\[(\\d+)\\]")

treeid_df2_bb_gsl$treeid_name <- emptscomp$id[match(treeid_df2_bb_gsl$treeid, emptscomp$treeid_num)]
bspp_df2_bb_gsl$spp_name <- emptscomp$latbi[match(bspp_df2_bb_gsl$spp, emptscomp$spp_num)]
aspp_df2_bb_gsl$spp_name <- emptscomp$latbi[match(aspp_df2_bb_gsl$spp, emptscomp$spp_num)]
ayear_df2_bb_gsl$year_name <- emptscomp$year[match(ayear_df2_bb$year, emptscomp$year_num)]

# SOS posterior recovery
# posterior summaries
sigma_df2_bb_sos  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_bb_sos   <- extract_params(df_fitsos, "bsp", "fit_bsp", 
                                    "spp", "bsp\\[(\\d+)\\]")
treeid_df2_bb_sos <- extract_params(df_fitsos, "atreeid", "fit_atreeid", 
                                    "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_bb_sos <- subset(treeid_df2_bb_sos, !grepl("z|sigma", treeid))
aspp_df2_bb_sos   <- extract_params(df_fitsos, "aspp", "fit_aspp", 
                                    "spp", "aspp\\[(\\d+)\\]")
ayear_df2_bb_sos  <- extract_params(df_fit, "ayear", "fit_ayear", 
                                 "year", "ayear\\[(\\d+)\\]")

treeid_df2_bb_sos$treeid_name <- emptscomp$id[match(treeid_df2_bb_sos$treeid, emptscomp$treeid_num)]
bspp_df2_bb_sos$spp_name <- emptscomp$latbi[match(bspp_df2_bb_sos$spp, emptscomp$spp_num)]
aspp_df2_bb_sos$spp_name <- emptscomp$latbi[match(aspp_df2_bb_sos$spp, emptscomp$spp_num)]
ayear_df2_bb_sos$year_name <- emptscomp$year[match(ayear_df2_bb_sos$year, emptscomp$year_num)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Recover models with Leafout #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
sigma_df2  <- read.csv("output/GM_GDDparam_Z_sigma.csv")
bspp_df2   <- read.csv("output/GM_GDDparam_Z_bspp.csv")
treeid_df2 <- read.csv("output/GM_GDDparam_Z_treeid.csv")
aspp_df2   <- read.csv("output/GM_GDDparam_Z_aspp.csv")
ayear_df2  <- read.csv("output/GM_GDDparam_Z_ayear.csv")

treeid_df2$treeid_name <- emptscomp$id[match(treeid_df2$treeid, emptscomp$treeid_num)]
bspp_df2$spp_name <- emptscomp$latbi[match(bspp_df2$spp, emptscomp$spp_num)]
aspp_df2$spp_name <- emptscomp$latbi[match(aspp_df2$spp, emptscomp$spp_num)]
ayear_df2$year_name <- emptscomp$year[match(ayear_df2$year, emptscomp$year_num)]

# GSL
sigma_df2_gsl  <- read.csv("output/GM_GSLparam_Z_sigma.csv")
bspp_df2_gsl   <- read.csv("output/GM_GSLparam_Z_bspp.csv")
treeid_df2_gsl <- read.csv("output/GM_GSLparam_Z_treeid.csv")
aspp_df2_gsl   <- read.csv("output/GM_GSLparam_Z_aspp.csv")
ayear_df2_gsl  <- read.csv("output/GM_GSLparam_Z_ayear.csv")

treeid_df2_gsl$treeid_name <- emptscomp$id[match(treeid_df2_gsl$treeid, emptscomp$treeid_num)]
bspp_df2_gsl$spp_name <- emptscomp$latbi[match(bspp_df2_gsl$spp, emptscomp$spp_num)]
aspp_df2_gsl$spp_name <- emptscomp$latbi[match(aspp_df2_gsl$spp, emptscomp$spp_num)]
ayear_df2_gsl$year_name <- emptscomp$year[match(ayear_df2_gsl$year, emptscomp$year_num)]

# SOS 
sigma_df2_sos  <- read.csv("output/GM_SOS_param_Z_sigma.csv")
bspp_df2_sos   <- read.csv("output/GM_SOS_param_Z_bspp.csv")
treeid_df2_sos <- read.csv("output/GM_SOS_param_Z_treeid.csv")
aspp_df2_sos   <- read.csv("output/GM_SOS_param_Z_aspp.csv")
ayear_df2_sos  <- read.csv("output/GM_SOS_param_Z_ayear.csv")

treeid_df2_sos$treeid_name <- emptscomp$id[match(treeid_df2_sos$treeid, emptscomp$treeid_num)]
bspp_df2_sos$spp_name <- emptscomp$latbi[match(bspp_df2_sos$spp, emptscomp$spp_num)]
aspp_df2_sos$spp_name <- emptscomp$latbi[match(aspp_df2_sos$spp, emptscomp$spp_num)]
ayear_df2_sos$year_name <- emptscomp$year[match(ayear_df2_sos$year, emptscomp$year_num)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Figure for model comparison #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# define colors
wood_colors <- c("Diffuse-porous" = "#4e9af1",   # blue
                 "Ring-porous"    = "#e07b3f")     # orange

# GDD
bspp_df2_bb$spp_name <- emptscomp$latbi[match(bspp_df2_bb$spp, emptscomp$spp_num)]
treeid_df2$spp_name <- emptscomp$latbi[match(treeid_df2$treeid, emptscomp$treeid_num)]
treeid_df2_bb$spp_name<- emptscomp$latbi[match(treeid_df2_bb$treeid, emptscomp$treeid_num)]

# GSL
bspp_df2_bb_gsl$spp_name<- emptscomp$latbi[match(bspp_df2_bb_gsl$spp,emptscomp$spp_num)]
treeid_df2_gsl$spp_name <- emptscomp$latbi[match(treeid_df2_gsl$treeid, emptscomp$treeid_num)]
treeid_df2_bb_gsl$spp_name<- emptscomp$latbi[match(treeid_df2_bb_gsl$treeid,emptscomp$treeid_num)]

# SOS
bspp_df2_bb_sos$spp_name <- emptscomp$latbi[match(bspp_df2_bb_sos$spp, emptscomp$spp_num)]
treeid_df2_sos$spp_name <- emptscomp$latbi[match(treeid_df2_sos$treeid, emptscomp$treeid_num)]
treeid_df2_bb_sos$spp_name <- emptscomp$latbi[match(treeid_df2_bb_sos$treeid, emptscomp$treeid_num)]

porousness <- c(
  "T. americana"      = "Diffuse-porous",
  "P. deltoides"      = "Diffuse-porous",
  "B. nigra"          = "Diffuse-porous",
  "B. alleghaniensis" = "Diffuse-porous",
  "A. saccharum"      = "Diffuse-porous",
  "A. rubrum"         = "Diffuse-porous",
  "Ae. flava"         = "Diffuse-porous",
  "Q. rubra"          = "Ring-porous",
  "Q. alba"           = "Ring-porous",
  "C. glabra"         = "Ring-porous",
  "C. ovata"          = "Ring-porous"
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
pdf("figures/growthModelsMain/budburstVSLeafout/DporousVSRporous.pdf", width = 7, height = 6)
par(mfrow = c(3,3), mar = c(4, 4, 4, 4)) 

# GDD --- --- --- --- --- ---
plot(sigma_df2_bb$mean, sigma_df2$mean,
     xlab = "Budburst as SOS", ylab = "Leafout as SOS", 
     type = "n", frame = FALSE,
     ylim = range(c(sigma_df2$p25, sigma_df2$p75)),
     xlim = range(c(sigma_df2_bb$p25, sigma_df2_bb$p75)))
arrows(x0 = sigma_df2_bb$mean, y0 = sigma_df2$p25,
       x1 = sigma_df2_bb$mean, y1 = sigma_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_bb$p25, y0 = sigma_df2$mean,
       x1 = sigma_df2_bb$p75, y1 = sigma_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_bb$mean, sigma_df2$mean,
       pch = 16, col = "black", cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_bb$mean, sigma_df2$mean, pch = 16, 
       col = wood_colors[sigma_df2$wood], cex = 1.2)
text(sigma_df2_sos$p25, sigma_df2_sos_full$p75,
     labels = parse(text = c("sigma[atreeid]", "sigma[y]")),
     pos = c(4,4), cex = 1)
mtext(bquote(sigma), side = 3, line = -0.1, adj = 0.5, font = 2, cex = 1.2)
mtext("(a) GDD", side = 3, adj = 0, line = 1, font = 2, cex = 0.9)

# bspp
plot(bspp_df2_bb$mean, bspp_df2$mean,
     xlab = "Budburst as SOS", ylab = "Leafout as SOS", 
     type = "n", frame = FALSE,
     ylim = range(c(bspp_df2$p25, bspp_df2$p75)),
     xlim = range(c(bspp_df2_bb$p25, bspp_df2_bb$p75)))
arrows(x0 = bspp_df2_bb$mean, y0 = bspp_df2$p25,
       x1 = bspp_df2_bb$mean, y1 = bspp_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_bb$p25, y0 = bspp_df2$mean,
       x1 = bspp_df2_bb$p75, y1 = bspp_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_bb$mean, bspp_df2$mean,
       pch = 16, col = wood_colors[bspp_df2$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
mtext(expression(beta[species]), 
      side = 3, line = 0.5, adj = 0.5, font = 2, cex = 1.2)

# atreeid
plot(treeid_df2_bb$mean, treeid_df2$mean,
     xlab = "Budburst as SOS", ylab = "Leafout as SOS", 
     type = "n", frame = FALSE,
     ylim = range(c(treeid_df2$p25, treeid_df2$p75)),
     xlim = range(c(treeid_df2_bb$p25, treeid_df2_bb$p75)))
arrows(x0 = treeid_df2_bb$mean, y0 = treeid_df2$p25,
       x1 = treeid_df2_bb$mean, y1 = treeid_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = treeid_df2_bb$p25, y0 = treeid_df2$mean,
       x1 = treeid_df2_bb$p75, y1 = treeid_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(treeid_df2_bb$mean, treeid_df2$mean,
       pch = 16, col = wood_colors[treeid_df2$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
mtext(expression(alpha[treeid]), 
      side = 3, line = 0.5, adj = 0.5, font = 2, cex = 1.2)


# GSL --- --- --- --- --- ---
plot(sigma_df2_bb_gsl$mean, sigma_df2_gsl$mean,
     xlab = "Budburst as SOS", ylab = "Leafout as SOS", 
     type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_gsl$p25, sigma_df2_gsl$p75)),
     xlim = range(c(sigma_df2_bb_gsl$p25, sigma_df2_bb_gsl$p75)))
arrows(x0 = sigma_df2_bb_gsl$mean, y0 = sigma_df2_gsl$p25,
       x1 = sigma_df2_bb_gsl$mean, y1 = sigma_df2_gsl$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_bb_gsl$p25, y0 = sigma_df2_gsl$mean,
       x1 = sigma_df2_bb_gsl$p75, y1 = sigma_df2_gsl$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_bb_gsl$mean, sigma_df2_gsl$mean,
       pch = 16, col = "black", cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_bb_gsl$mean, sigma_df2_gsl$mean, pch = 16, col = wood_colors[sigma_df2_gsl$wood], cex = 1.2)
text(sigma_df2_bb_gsl$p25, sigma_df2_gsl$p75,
     labels = parse(text = c("sigma[atreeid]", "sigma[y]")),
     pos = c(4,4), cex = 1)
mtext(bquote(sigma), side = 3, line = -0.1, adj = 0.5, font = 2, cex = 1.2)
mtext("(b) GSL", side = 3, adj = 0, line = 1, font = 2, cex = 0.9)

# bspp
plot(bspp_df2_bb_gsl$mean, bspp_df2_gsl$mean,
     xlab = "Budburst as SOS", ylab = "Leafout as SOS", 
     type = "n", frame = FALSE,
     ylim = range(c(bspp_df2_gsl$p25, bspp_df2_gsl$p75)),
     xlim = range(c(bspp_df2_bb_gsl$p25, bspp_df2_bb_gsl$p75)))
arrows(x0 = bspp_df2_bb_gsl$mean, y0 = bspp_df2_gsl$p25,
       x1 = bspp_df2_bb_gsl$mean, y1 = bspp_df2_gsl$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_bb_gsl$p25, y0 = bspp_df2_gsl$mean,
       x1 = bspp_df2_bb_gsl$p75, y1 = bspp_df2_gsl$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_bb_gsl$mean, bspp_df2_gsl$mean,
       pch = 16, col = wood_colors[bspp_df2_gsl$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
mtext(expression(beta[species]), 
      side = 3, line = 0.5, adj = 0.5, font = 2, cex = 1.2)


# atreeid
plot(treeid_df2_bb_gsl$mean, treeid_df2_gsl$mean,
     xlab = "Budburst as SOS", ylab = "Leafout as SOS", 
     type = "n", frame = FALSE,
     ylim = range(c(treeid_df2_gsl$p25, treeid_df2_gsl$p75)),
     xlim = range(c(treeid_df2_bb_gsl$p25, treeid_df2_bb_gsl$p75)))
arrows(x0 = treeid_df2_bb_gsl$mean, y0 = treeid_df2_gsl$p25,
       x1 = treeid_df2_bb_gsl$mean, y1 = treeid_df2_gsl$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = treeid_df2_bb_gsl$p25, y0 = treeid_df2_gsl$mean,
       x1 = treeid_df2_bb_gsl$p75, y1 = treeid_df2_gsl$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(treeid_df2_bb_gsl$mean, treeid_df2_gsl$mean,
       pch = 16, col = wood_colors[treeid_df2_gsl$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
mtext(expression(alpha[treeid]), 
      side = 3, line = 0.5, adj = 0.5, font = 2, cex = 1.2)


# SOS  --- --- --- --- --- ---
plot(sigma_df2_bb_sos$mean, sigma_df2_sos$mean,
     xlab = "Budburst as SOS", ylab = "Leafout as SOS",  
     type = "n", frame = FALSE,
     ylim = range(c(sigma_df2_sos$p25, sigma_df2_sos$p75)),
     xlim = range(c(sigma_df2_bb_sos$p25, sigma_df2_bb_sos$p75)))
arrows(x0 = sigma_df2_bb_sos$mean, y0 = sigma_df2_sos$p25,
       x1 = sigma_df2_bb_sos$mean, y1 = sigma_df2_sos$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_bb_sos$p25, y0 = sigma_df2_sos$mean,
       x1 = sigma_df2_bb_sos$p75, y1 = sigma_df2_sos$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_bb_sos$mean, sigma_df2_sos$mean,
       pch = 16, col = "black", cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_bb_sos$mean, sigma_df2_sos$mean, pch = 16, 
       col = wood_colors[sigma_df2_sos$wood], cex = 1.2)
text(sigma_df2_bb_sos$p25, sigma_df2_sos$p75,
     labels = parse(text = c("sigma[atreeid]", "sigma[y]")),
     pos = c(4,4), cex = 1)
mtext(bquote(sigma), side = 3, line = -0.1, adj = 0.5, font = 2, cex = 1.2)
mtext("(c) SOS", side = 3, adj = 0, line = 1, font = 2, cex = 0.9)

# bspp
plot(bspp_df2_bb_sos$mean, bspp_df2_sos$mean,
     xlab = "Budburst as SOS", ylab = "Leafout as SOS",  type = "n", frame = FALSE,
     ylim = range(c(bspp_df2_sos$p25, bspp_df2_sos$p75)),
     xlim = range(c(bspp_df2_bb_sos$p25, bspp_df2_bb_sos$p75)))
arrows(x0 = bspp_df2_bb_sos$mean, y0 = bspp_df2_sos$p25,
       x1 = bspp_df2_bb_sos$mean, y1 = bspp_df2_sos$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_bb_sos$p25, y0 = bspp_df2_sos$mean,
       x1 = bspp_df2_bb_sos$p75, y1 = bspp_df2_sos$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_bb_sos$mean, bspp_df2_sos$mean,
       pch = 16, col = wood_colors[bspp_df2_sos$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
mtext(expression(beta[species]), 
      side = 3, line = 0.5, adj = 0.5, font = 2, cex = 1.2)

# atreeid
plot(treeid_df2_bb_sos$mean, treeid_df2_sos$mean,
     xlab = "Budburst as SOS", ylab = "Leafout as SOS",  type = "n", frame = FALSE,
     ylim = range(c(treeid_df2_sos$p25, treeid_df2_sos$p75)),
     xlim = range(c(treeid_df2_bb_sos$p25, treeid_df2_bb_sos$p75)))
arrows(x0 = treeid_df2_bb_sos$mean, y0 = treeid_df2_sos$p25,
       x1 = treeid_df2_bb_sos$mean, y1 = treeid_df2_sos$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = treeid_df2_bb_sos$p25, y0 = treeid_df2_sos$mean,
       x1 = treeid_df2_bb_sos$p75, y1 = treeid_df2_sos$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(treeid_df2_bb_sos$mean, treeid_df2_sos$mean, 
       pch = 16, col = wood_colors[treeid_df2_sos$wood], cex = 1.2)
abline(0, 1, lty = 2, col = "black", lwd = 2)
mtext(expression(alpha[treeid]), 
      side = 3, line = 0.5, adj = 0.5, font = 2, cex = 1.2)

# legend once, in outer margin on the right
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 6, 1, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = names(wood_colors), col = wood_colors,
       pch = 16, pt.cex = 1.2, bty = "n", title = "Wood anatomy", xpd = TRUE)
dev.off()
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# 2X PRIORS ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if (fit2xpriors){
# Ill do this with 3x the priors so I don't have to change 3 scripts
genericmodel <- stan_model("stan/TSmodelGrowthGDD_largerPriors.stan")

# Fit model GDD
gddz <- (empts$pgsGDD5 - mean(empts$pgsGDD5)) / sd(empts$pgsGDD5)
dgddz <- dgdd[1:10]
dgddz$covariate <- gddz

fitgdd <- sampling(genericmodel, data = dgddz,
                   warmup = 1000, iter=2000, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDDZscored_largerPriors")

# Fit model GSL
gslz <- (empts$pgsGSL - mean(empts$pgsGSL)) / sd(empts$pgsGSL)
dgslz <- dgdd[1:10]
dgslz$covariate <- gslz

fitgsl <- sampling(genericmodel, data = dgslz,
                   warmup = 1000, iter = 2000, chains = 4)
saveRDS(fitgsl, "output/stanOutput/fitGrowthGSLZscored_largerPriors")

# Fit model SOS
sosz <- (empts$leafout - mean(empts$leafout)) / sd(empts$leafout)
dsosz <- dgdd[1:10]
dsosz$covariate <- sosz

fitsos <- sampling(genericmodel, data = dsosz,
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fitsos, "output/stanOutput/fitGrowthSOSZscored_largerPriors")

# Fit model EOS
eosz <- (empts$coloredLeaves - mean(empts$coloredLeaves)) / sd(empts$coloredLeaves)
deosz <- dgdd[1:10]
deosz$covariate <- eosz

fiteos <- sampling(genericmodel, data = deosz,
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fiteos, "output/stanOutput/fitGrowthEOSZscored_largerPriors")
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# EOS as a function of SOS ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if(runmodelSOSonEOS){
dco <- list(
  y = empts$coloredLeaves/7,
  N = nrow(empts),
  Nspp = length(unique(empts$spp_num)),
  species = as.numeric(as.character(empts$spp_num)),
  treeid = as.numeric(empts$treeid_num),
  Ntreeid = length(unique(as.numeric(empts$treeid_num))),
  year = as.numeric(empts$year_num),
  Nyear = length(unique(empts$year_num)),
  sos = empts$leafout/7
)
dco

# fit carry over
modeldco <- stan_model("stan/TSmodelGrowth_zEOSonSOS.stan")
fitco <- sampling(modeldco, data = dco,
                   warmup = 1000, iter = 2000, chains=4)
saveRDS(fitco, "output/stanOutput/fitSOSonEOS")

# Recover stuff
fitco <- readRDS("output/stanOutput/fitSOSonEOS")

# Setup color palette across all plots
pal <- wes_palette("AsteroidCity1")[3:4]

##### Recover parameters #####
df_fitco <- as.data.frame(fitco)

# full posterior arrays for multi-line extraction
columns <- colnames(df_fitco)[!grepl("prior", colnames(df_fitco))]
bspp_df <- df_fitco[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitco[, grepl("atreeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fitco[, columns[grepl("aspp", columns)]]
ayear_df <- df_fitco[, columns[grepl("ayear", columns) & !grepl("mean", columns)]]

# change colnames to indexes for loop tracing
colnames(bspp_df)  <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df)   <- 1:ncol(aspp_df)
colnames(ayear_df)  <- 1:ncol(ayear_df)

# posterior summaries
sigma_df2  <- extract_params(df_fitco, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fitco, "bsp", "fit_bsp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fitco, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z|sigma", id))
aspp_df2   <- extract_params(df_fitco, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
ayear_df2  <- extract_params(df_fitco, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2  <- subset(ayear_df2, !grepl("mean", year))
a_df2      <- extract_params(df_fitco, "a", "fit_a",
                             "grandmean", "a\\[(\\d+)\\]")
a_df2      <- subset(a_df2, grandmean == "a")

# save csvs
write.csv(sigma_df2,  "output/SOSonEOS_param_sigma.csv",  row.names = FALSE)
write.csv(bspp_df2,   "output/SOSonEOS_param_bspp.csv",   row.names = FALSE)
write.csv(treeid_df2, "output/SOSonEOS_param_treeid.csv", row.names = FALSE)
write.csv(aspp_df2,   "output/SOSonEOS_param_aspp.csv",   row.names = FALSE)
write.csv(ayear_df2,  "output/SOSonEOS_param_ayear.csv",  row.names = FALSE)
write.csv(a_df2,      "output/SOSonEOS_param_a.csv",      row.names = FALSE)

##### Plot posterior vs priors for gdd fit #####
pdf(file = "figures/SOSonEOS/SOSonEOSPriorVSPosterior.pdf", width = 8, height = 10)
par(mfrow = c(3, 2))

# a
plot(density(df_fitco[, "a_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_a", xlab = "a", ylim = c(0, 0.5))
lines(density(df_fitco[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitco[, "sigma_atreeid_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_atreeid", xlab = "sigma_atreeid", ylim = c(0, 2))
lines(density(df_fitco[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitco[, "sigma_y_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_sigma_y", xlab = "sigma_y", ylim = c(0, 2))
lines(density(df_fitco[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitco[, "aspp_prior"]), col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", xlab = "aspp", xlim = c(-30, 30), ylim = c(0, 0.1))
for (col in colnames(aspp_df)) { lines(density(aspp_df[, col]), col = pal[2], lwd = 1) } 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitco[, "bsp_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_bsp", xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) { lines(density(bspp_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(df_fitco[, "ayear_prior"]), col = pal[1], lwd = 2, main = "priorVSposterior_ayear", xlab = "ayear", xlim = c(-10, 10), ylim = c(0, 0.5))
for (col in colnames(ayear_df)) { lines(density(ayear_df[, col]), col = pal[2], lwd = 1) }
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()
}
