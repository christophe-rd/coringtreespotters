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

temp$bin7 <- ave(temp$doy, temp$year, 
                 FUN = function(x) ceiling((x - min(x) + 1) / 7))
gdd_7day <- aggregate(gdddiff ~ year + bin7, data = temp, sum)
tsgddscale <- mean(gdd_7day$gdddiff)

gddseq <- seq(min(empts$pgsGDD5), max(empts$pgsGDD5), 
              length.out = lineplotseqlength)

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
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot GDD fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fitgdd <- readRDS("output/stanOutput/fitGrowthGDD_noPP")
fitgsl <- readRDS("output/stanOutput/fitGrowthGSL_noPP")
fitsos <- readRDS("output/stanOutput/fitGrowthSOS_noPP")
fiteos <- readRDS("output/stanOutput/fitGrowthEOS_noPP")

# Setup color palette across all plots
pal <- wes_palette("AsteroidCity1")[3:4]

##### Recover parameters #####
df_fitgdd <- as.data.frame(fitgdd)

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

bspp_df2$spp_name <- empts$latbi[match(bspp_df2$spp, empts$spp_num)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Simulate data ----
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
set.seed(123)
# compile the 1-species model once, it is fit separately to each species
mod <- stan_model("stan/simOneSpp.stan")


## Get some GDD from the empirical data ----------------------------------------
acsa <- subset(empts, latbi == "A. saccharum")

qual <- subset(empts, latbi == "Q. alba")
tame <- subset(empts, latbi == "T. americana")

mu <- c(mean(acsa$pgsGDD5, na.rm = TRUE),
        mean(qual$pgsGDD5, na.rm = TRUE),
        mean(tame$pgsGDD5, na.rm = TRUE))

names(mu) <- c("A. saccharum",
               "Q. alba",
               "T. americana")

sd <- c(sd(acsa$pgsGDD5, na.rm = TRUE),
        sd(qual$pgsGDD5, na.rm = TRUE),
        sd(tame$pgsGDD5, na.rm = TRUE))

names(sd) <- c("A. saccharum",
               "Q. alba",
               "T. americana")

# compare gdd sim vs empirical
# hist(rnorm(1000, mean = mu, sd = sd) / tsgddscale)
# hist(tame$pgsGDD5 / tsgddscale)

# convert to scaled gdds to simplify downstream code
mu <- mu / tsgddscale
sd <- sd / tsgddscale

# number of species
nspp <- length(mu)

# parameter values, with one slope per species
a <- 0.5
b <- bspp_df2$mean[match(names(mu), bspp_df2$spp_name)]
names(b) <- names(mu)

sigma_y <- 0.4
sigma_atreeid <- 0.5
sigma_year <- 0.5

# fixed number of years (rings) per tree
nyear <- 10

# year effects, shared by all trees and species and kept the same across sample sizes
ayear <- rnorm(nyear, 0, sigma_year)

# sequence of number of trees per species
ntree_vec <- seq(4, 100, by = 5)

# matrices to store posterior summaries of b: rows = number of trees, columns = species
mea <- matrix(NA, nrow = length(ntree_vec), ncol = nspp)
q05 <- matrix(NA, nrow = length(ntree_vec), ncol = nspp)
q25 <- matrix(NA, nrow = length(ntree_vec), ncol = nspp)
q75 <- matrix(NA, nrow = length(ntree_vec), ncol = nspp)
q95 <- matrix(NA, nrow = length(ntree_vec), ncol = nspp)

# set warmup and iterations
wrmUp <- 1000
itrns <- 2000

# one simulated dataset and one fit per number of trees and species
for (j in seq_along(ntree_vec)) { # j = 1
  ntree <- ntree_vec[j]
  N <- ntree * nyear
  treeid <- rep(1:ntree, each = nyear)
  year <- rep(1:nyear, times = ntree)
  
  for (s in 1:nspp) { # s = 1
    # gdd according to tame mu and sd for now
    gdd <- rnorm(N, mu[s], sd[s])
    atreeid <- rnorm(ntree, 0, sigma_atreeid)
    y <- a + atreeid[treeid] + ayear[year] + 
      b[s] * gdd + rnorm(N, 0, sigma_y)
    
    mdl.data <- list(N = N,
                     Ntreeid = ntree,
                     treeid = treeid,
                     Nyear = nyear,
                     year = year,
                     gdd = gdd - mean(gdd),
                     y = y)
    
    # sample, and only keep the b parameter
    fit <- sampling(mod, data = mdl.data, chains = 4, 
                    warmup = wrmUp, iter = itrns, pars = "b")
    bpost <- rstan::extract(fit, pars = "b")$b
    
    mea[j, s] <- mean(bpost)
    q05[j, s] <- quantile(bpost, 0.05)
    q25[j, s] <- quantile(bpost, 0.25)
    q75[j, s] <- quantile(bpost, 0.75)
    q95[j, s] <- quantile(bpost, 0.95)
  }
}

# plot posterior mean with 50% and 90% UI, one panel per species
par(mfrow = c(1, nspp))
for (s in 1:nspp) { # s = 1
  plot(NA, xlim = range(ntree_vec), ylim = c(-0.05, 0.1),
       xlab = "Number of trees", ylab = "Estimated slope",
       main = paste0(names(mu[s]), " (b = ", round(b[s], 2), ")"))
  abline(h = 0, lty = 1)
  abline(h = b[s], lty = 3, col = tscolslatbi[names(mu)[s]], lwd = 2)
  segments(ntree_vec, q05[, s], ntree_vec, q95[, s], col = tscolslatbi[names(mu)[s]], lwd = 1)
  segments(ntree_vec, q25[, s], ntree_vec, q75[, s], col = tscolslatbi[names(mu)[s]], lwd = 2.5)
  points(ntree_vec, mea[, s], pch = 16, col = tscolslatbi[names(mu)[s]])
}

# smallest number of trees where the 90% interval excludes zero, per species
ntree_min <- rep(NA, nspp)
for (s in 1:nspp) {
  ntree_min[s] <- ntree_vec[which(q05[, s] > 0 | q95[, s] < 0)[1]]
}
ntree_min
