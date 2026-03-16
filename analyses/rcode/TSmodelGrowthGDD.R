# CoringTreespotters growth model
# CRD 18 December 2025

# Goal: build a model to understand the relationship between growth and growing degree days using the tree cores collected in at the Arnold Arboretum in the spring of 2025

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 
options(digits = 3)

# stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

# Load library 
library(ggplot2)
library(rstan)
library(future)
library(shinystan)
library(wesanderson)
library(patchwork)

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
# EMPIRICAL DATA ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# emp <- read.csv("output/empiricalDataMAIN.csv")
# read empirical data with max phenology observations instead of min
emp <- read.csv("output/empiricalDataMAIN_max.csv")
nrow(emp[!is.na(emp$pgsGDD10AVG), ]) - nrow(emp[!is.na(emp$pgsGDD10), ])
nrow(emp[!is.na(emp$pgsGDD5), ])

# remove NAs
emp <- emp[!is.na(emp$pgsGDD5), ]

# transform my groups to numeric values
emp$spp_num <- match(emp$symbol, unique(emp$symbol))
emp$treeid_num <- match(emp$id, unique(emp$id))

# some checks
table(emp$symbol, emp$spp_num)
table(emp$id, emp$symbol)
table(emp$treeid_num, emp$spp_num)


# transform data in vectors
y <- emp$lengthMM # ring width in mm
N <- nrow(emp)
gdd <- emp$pgsGDD5/200
Nspp <- length(unique(emp$spp_num))
species <- as.numeric(as.character(emp$spp_num))
treeid <- as.numeric(emp$treeid_num)
Ntreeid <- length(unique(treeid))

# check that everything is fine
table(treeid,species)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
rstan_options(auto_write = TRUE)
fit <- stan("stan/TSmodelGrowthGDD.stan", 
            data=c("N","y",
                   "Nspp","species",
                   "Ntreeid", "treeid", 
                   "gdd"),
            warmup = 1000, iter = 2000, chains=4)
saveRDS(fit, "output/stanOutput/fit_modelGrowth")

# diagnostics
diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)
samples <- util$extract_expectand_vals(fit)

# check tree id parameterization
if (FALSE) {
  atreeid <- names(samples)[grepl("atreeid", names(samples))]
  atreeid <- atreeid[!grepl("sigma", atreeid)]
  # atreeid <- atreeid[sample(length(unique(atreeid)), 9)]
pdf("figures/troubleshootingModelGrowthGDD/atreeidParameterization.pdf",
    width = 14, height = 18)
# jpeg("figures/atreeidParameterization.jpeg", 
     # width = 2000, height = 3000,
     # units = "px", res = 300)
util$plot_div_pairs(atreeid, "sigma_atreeid", samples, diagnostics, transforms = list("sigma_atreeid" = 1))
dev.off()
}

# === === === === === === === === === === === === #
##### Recover parameters from the posterior ##### 
# === === === === === === === === === === === === #
df_fit <- as.data.frame(fit)

sigma_df2  <- extract_params(df_fit, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fit, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fit, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z", treeid))
aspp_df2   <- extract_params(df_fit, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")

# === === === === === === === === #
##### Plot posterior vs prior #####
# === === === === === === === === #

###### Plot a prior vs posterior ######
a_posterior <- df_fit[, colnames(df_fit) %in% "a"]

a_prior <- rnorm(1e4, 2, 3)

priora <- ggplot() +
  geom_density(data = data.frame(a = a_prior),
               aes(x = a, colour = "Prior at N(2,3)"),
               linewidth = 1) +
  geom_density(data = data.frame(value = a_posterior),
               aes(x = value, colour = "Posterior"),
               linewidth = 1) +
  labs(title = "priorVSposterior_a",
       x = "a", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()
priora
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

###### Plot sigmas prior vs posterior ######
sigma_long <- reshape(
  sigma_df,
  direction = "long",
  varying = list(names(sigma_df)),
  v.names = "value",
  timevar = "parameter",
  times = names(sigma_df),
  idvar = "draw"
)
sigma_long

sigma_long$prior <- NA
sigma_long$prior[which(sigma_long$parameter == "sigma_atreeid")] <- 
  rnorm(nrow(sigma_df), 0, 2)
sigma_long$prior[which(sigma_long$parameter == "sigma_y")] <- rnorm(
  nrow(sigma_df), 0, 1)

priorsigmas <- ggplot(sigma_long) +
  geom_density(aes(x = prior, colour = "Prior sigma_atreeid  at N(0, 2)
Prior sigma_y at N(0, 1)"),
               linewidth = 0.8) +
  geom_density(aes(x = value, colour = "Posterior"),
               linewidth = 0.8) +
  facet_wrap(~parameter) + 
  labs(title = "priorVSposterior_sigmas",
       x = "", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()
priorsigmas

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Plot atreeid prior vs posterior ######
treeid_long <- reshape(
  treeid_df,
  direction = "long",
  varying = list(names(treeid_df)),
  v.names = "value",
  timevar = "treeid",
  times = names(treeid_df),
  idvar = "draw"
)
treeid_long

# simulate priors
hyperparameter_draws <- 8000
parameter_draws <- 1000
n_sigmatreeid <- 200

# set to prior values
sigmatreeid_vec <- abs(rnorm(n_sigmatreeid, 0, 1))

prior_treeid <- rep(NA, parameter_draws*length(sigmatreeid_vec))

for (i in 1: length(sigmatreeid_vec)) {
  prior_treeid[((i - 1)*parameter_draws + 1):(i*parameter_draws)] <- rnorm(parameter_draws, 0, sigmatreeid_vec[i])
}
prior_treeid

# sub of some treeids for plotting
subtreeid <- subset(treeid_long, treeid %in% sample(treeid_long$treeid, 5))

prioratreeid <- ggplot() +
  geom_density(data = data.frame(prior_treeid = prior_treeid),
               aes(x = prior_treeid, colour = "Prior"),
               linewidth = 0.8) +
  geom_density(data = subtreeid,
               aes(x = value, colour = "Posterior"),
               linewidth = 0.8) +
  facet_wrap(~treeid) + 
  labs(title = "priorVSposterior_treeid",
       x = "treeid", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()
prioratreeid

###### Plot aspp prior vs posterior ######
# convert posterior distribution to long format
aspp_long <- reshape(
  aspp_df,
  direction = "long",
  varying = list(names(aspp_df)),
  v.names = "value",
  timevar = "spp",
  times = names(aspp_df),
  idvar = "draw"
)
aspp_long

# aspp prior
aspp_prior <- rnorm(1e4, 0, 6)

prioraspp <- ggplot() +
  geom_density(data = data.frame(aspp_prior = aspp_prior),
               aes(x = aspp_prior, colour = "Prior at N(0, 6)"),
               linewidth = 0.8) +
  geom_density(data = aspp_long,
               aes(x = value, colour = "Posterior", group = spp),
               linewidth = 0.5) +
  # facet_wrap(~spp) + 
  labs(title = "priorVSposterior_aspp",
       x = "aspp", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  xlim(c(-20, 20)) +
  theme_minimal()
prioraspp

###### Plot bsp prior vs posterior ######
# convert posterior distribution to long format
bsp_long <- reshape(
  bspp_df,
  direction = "long",
  varying = list(names(bspp_df)),
  v.names = "value",
  timevar = "spp",
  times = names(bspp_df),
  idvar = "draw"
)
bsp_long

# aspp prior
bsp_prior <- rnorm(1e4, 0, 1)

priorbsp <- ggplot() +
  geom_density(data = data.frame(bsp_prior = bsp_prior),
               aes(x = bsp_prior, colour = "Prior at N(0, 1)"),
               linewidth = 0.8) +
  geom_density(data = bsp_long,
               aes(x = value, colour = "Posterior", group = spp),
               linewidth = 0.5) +
  # facet_wrap(~spp) + 
  labs(title = "priorVSposterior_bsp",
       x = "bsp", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()
priorbsp

#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
priorcombined <- (priorsigmas) / (priora) / (prioraspp)  / (priorbsp)
ggsave("figures/troubleshootingModelGrowthGDD/priorVSposteriorCombined.jpeg", priorcombined, width = 8, height = 12, units = "in", dpi = 300)
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

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
