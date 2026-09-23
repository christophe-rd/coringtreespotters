# CoringTreespotters growth model
# CRD 23 September 2026

# Goal: Given the effect size of the growth responses to the species using empirical data, calculate what would be the sample size required to see an effect

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

wrmUp <- 2000
itrns <- 4000

runmodels <- F

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
gddmodel <- stan_model("stan/TSmodelGrowthGDD_noPP.stan")
fitgdd <- sampling(gddmodel, data = dgdd,
                   warmup = wrmUp, iter = itrns, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDD")

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
bspp_df <- df_fitgdd[, columns[grepl("bsp", columns) & !grepl("z|sigma", columns)]]
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
bspp_df2   <- subset(bspp_df2, !grepl("z|sigma", spp))
treeid_df2 <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", "id", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z|sigma", id))
aspp_df2   <- extract_params(df_fitgdd, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
aspp_df2   <- subset(aspp_df2, !grepl("sigma", spp))
ayear_df2  <- extract_params(df_fitgdd, "ayear", "fit_ayear", "year", "ayear\\[(\\d+)\\]")
ayear_df2  <- subset(ayear_df2, !grepl("mean", year))
a_df2      <- extract_params(df_fitgdd, "a", "fit_a",
                             "grandmean", "a\\[(\\d+)\\]")
a_df2      <- subset(a_df2, grandmean == "a")

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

