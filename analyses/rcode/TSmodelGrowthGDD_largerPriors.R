# CoringTreespotters growth model WITH LARGER PRIORS
# CRD 18 December 2025

# Goal: build a model to understand the relationship between growth and growing degree days using the tree cores collected in at the Arnold Arboretum in the spring of 2025

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 
options(mc.cores = parallel::detectCores())
options(digits = 3)
# quartz()

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

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# EMPIRICAL DATA ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
emp <- read.csv("output/empiricalDataMAIN.csv")

# transform my groups to numeric values
emp$spp_num <- match(emp$symbol, unique(emp$symbol))
emp$treeid_num <- match(emp$id, unique(emp$id))

# some checks
table(emp$symbol, emp$spp_num)
table(emp$id, emp$symbol)
table(emp$treeid_num, emp$spp_num)

# remove NAs
emp <- emp[!is.na(emp$pgsGDD), ]

# transform data in vectors
y <- emp$lengthMM # ring width in mm
N <- nrow(emp)
gdd <- emp$pgsGDD/200
Nspp <- length(unique(emp$spp_num))
species <- as.numeric(as.character(emp$spp_num))
treeid <- as.numeric(emp$treeid_num)
Ntreeid <- length(unique(treeid))

# check that everything is fine
table(treeid,species)
table(treeid)
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
rstan_options(auto_write = TRUE)
fit <- stan("stan/TSmodelGrowthGDD_largerPriors.stan", 
            data=c("N","y",
                   "Nspp","species",
                   "Ntreeid", "treeid", 
                   "gdd"),
            iter=2000, chains=4, cores=4)

# diagnostics
diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
atreeid <- names(samples)[grepl("atreeid", names(samples))]
atreeid <- atreeid[!grepl("sigma", atreeid)]
# atreeid <- atreeid[sample(length(unique(atreeid)), 9)]

# check tree id parameterization
if (FALSE) {
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
df_fit_lp <- as.data.frame(fit)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover sigmas ######
unique(colnames(df_fit_lp))
sigma_cols_lp <- colnames(df_fit_lp)[grepl("sigma", colnames(df_fit_lp))]

sigma_df_lp <- df_fit_lp[, colnames(df_fit_lp) %in% sigma_cols_lp]

sigma_df2_lp <- data.frame(
  sigma = character(ncol(sigma_df_lp)),
  mean = numeric(ncol(sigma_df_lp)),  
  per5 = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
sigma_df2_lp

for (i in 1:ncol(sigma_df_lp)) { # i = 1
  sigma_df2_lp$sigma[i] <- colnames(sigma_df_lp)[i]         
  sigma_df2_lp$mean[i] <- round(mean(sigma_df_lp[[i]]),3)  
  sigma_df2_lp$per5[i] <- round(quantile(sigma_df_lp[[i]], probs = 0.05), 3)
  sigma_df2_lp$per25[i] <- round(quantile(sigma_df_lp[[i]], probs = 0.25), 3)
  sigma_df2_lp$per75[i] <- round(quantile(sigma_df_lp[[i]], probs = 0.75), 3)
  sigma_df2_lp$per95[i] <- round(quantile(sigma_df_lp[[i]], probs = 0.95), 3)
}
sigma_df2_lp

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover b spp ######
bspp_cols_lp <- colnames(df_fit_lp)[grepl("bsp", colnames(df_fit_lp))]
# remove sigma_bspp for now
# bspp_cols <- bspp_cols[2:length(bspp_cols)]

bspp_df_lp <- df_fit_lp[, colnames(df_fit_lp) %in% bspp_cols_lp]
# change their names
colnames(bspp_df_lp) <- sub("bsp\\[(\\d+)\\]", "\\1", colnames(bspp_df_lp))
#empty spp df
bspp_df2_lp <- data.frame(
  spp = character(ncol(bspp_df_lp)),
  fit_bspp = numeric(ncol(bspp_df_lp)),  
  fit_bspp_per5 = NA, 
  fit_bspp_per25 = NA,
  fit_bspp_per75 = NA,
  fit_bspp_per95 = NA
)
for (i in 1:ncol(bspp_df_lp)) { # i = 1
  bspp_df2_lp$spp[i] <- colnames(bspp_df_lp)[i]         
  bspp_df2_lp$fit_bspp[i] <- round(mean(bspp_df_lp[[i]]),3)  
  bspp_df2_lp$fit_bspp_per5[i] <- round(quantile(bspp_df_lp[[i]], probs = 0.05), 3)
  bspp_df2_lp$fit_bspp_per25[i] <- round(quantile(bspp_df_lp[[i]], probs = 0.25), 3)
  bspp_df2_lp$fit_bspp_per75[i] <- round(quantile(bspp_df_lp[[i]], probs = 0.75), 3)
  bspp_df2_lp$fit_bspp_per95[i] <- round(quantile(bspp_df_lp[[i]], probs = 0.95), 3)
}
bspp_df2_lp


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover treeid ######

# grab treeid 
treeid_cols_lp <- colnames(df_fit_lp)[grepl("atreeid", colnames(df_fit_lp))]
treeid_cols_lp <- treeid_cols_lp[!grepl("zatreeid", treeid_cols_lp)]
treeid_cols_lp <- treeid_cols_lp[!grepl("sigma", treeid_cols_lp)]

treeid_df_lp <- df_fit_lp[, colnames(df_fit_lp) %in% treeid_cols_lp]

# change their names
colnames(treeid_df_lp) <- sub("atreeid\\[(\\d+)\\]", "\\1", colnames(treeid_df_lp))
# empty treeid dataframe
treeid_df2_lp <- data.frame(
  treeid = character(ncol(treeid_df_lp)),
  fit_atreeid = numeric(ncol(treeid_df_lp)),  
  fit_atreeid_per5 = NA, 
  fit_atreeid_per25 = NA,
  fit_atreeid_per75 = NA,
  fit_atreeid_per95 = NA
)
for (i in 1:ncol(treeid_df_lp)) { # i = 1
  treeid_df2_lp$treeid[i] <- colnames(treeid_df_lp)[i]         
  treeid_df2_lp$fit_atreeid[i] <- round(mean(treeid_df_lp[[i]]),3)  
  treeid_df2_lp$fit_atreeid_per5[i] <- round(quantile(treeid_df_lp[[i]], probs = 0.05), 3)
  treeid_df2_lp$fit_atreeid_per25[i] <- round(quantile(treeid_df_lp[[i]], probs = 0.25), 3)
  treeid_df2_lp$fit_atreeid_per75[i] <- round(quantile(treeid_df_lp[[i]], probs = 0.75), 3)
  treeid_df2_lp$fit_atreeid_per95[i] <- round(quantile(treeid_df_lp[[i]], probs = 0.95), 3)
}
treeid_df2_lp

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover a spp  ######
aspp_cols_lp <- colnames(df_fit_lp)[grepl("aspp", colnames(df_fit_lp))]

aspp_df_lp <- df_fit_lp[, colnames(df_fit_lp) %in% aspp_cols_lp]
# change their names
colnames(aspp_df_lp) <- sub("aspp\\[(\\d+)\\]", "\\1", colnames(aspp_df_lp))
#empty aspp df
aspp_df2_lp <- data.frame(
  spp = character(ncol(aspp_df_lp)),
  fit_aspp = numeric(ncol(aspp_df_lp)),  
  fit_aspp_per5 = NA, 
  fit_aspp_per25 = NA,
  fit_aspp_per75 = NA,
  fit_aspp_per95 = NA
)
for (i in 1:ncol(aspp_df_lp)) { # i = 1
  aspp_df2_lp$spp[i] <- colnames(aspp_df_lp)[i]         
  aspp_df2_lp$fit_aspp[i] <- round(mean(aspp_df_lp[[i]]),3)  
  aspp_df2_lp$fit_aspp_per5[i] <- round(quantile(aspp_df_lp[[i]], probs = 0.05), 3)
  aspp_df2_lp$fit_aspp_per25[i] <- round(quantile(aspp_df_lp[[i]], probs = 0.25), 3)
  aspp_df2_lp$fit_aspp_per75[i] <- round(quantile(aspp_df_lp[[i]], probs = 0.75), 3)
  aspp_df2_lp$fit_aspp_per95[i] <- round(quantile(aspp_df_lp[[i]], probs = 0.95), 3)
}
aspp_df2_lp

# === === === === === === === === #
##### Plot posterior vs prior #####
# === === === === === === === === #

###### Plot a prior vs posterior ######
a_posterior <- df_fit_lp[, colnames(df_fit_lp) %in% "a"]

a_prior <- rnorm(1e4, 2, 9)

priora <- ggplot() +
  geom_density(data = data.frame(a = a_prior),
               aes(x = a, colour = "Prior at N(2, 9)"),
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
sigma_long$prior[which(sigma_long$parameter == "sigma_atreeid")] <- rnorm(8e3, 0, 6)
sigma_long$prior[which(sigma_long$parameter == "sigma_y")] <- rnorm(8e3, 0, 3)

priorsigmas <- ggplot(sigma_long) +
  geom_density(aes(x = prior, colour = "Prior sigma_atreeid  at N(0, 6)
Prior sigma_y at N(0, 3)"),
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
sigmatreeid_vec <- abs(rnorm(n_sigmatreeid, 0, 6))

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
aspp_prior <- rnorm(1e4, 0, 18)

prioraspp <- ggplot() +
  geom_density(data = data.frame(aspp_prior = aspp_prior),
               aes(x = aspp_prior, colour = "Prior at N(0, 18)"),
               linewidth = 0.8) +
  geom_density(data = aspp_long,
               aes(x = value, colour = "Posterior", group = spp),
               linewidth = 0.5) +
  # facet_wrap(~spp) + 
  labs(title = "priorVSposterior_aspp",
       x = "aspp", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  xlim(c(-50, 50)) +
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
bsp_prior <- rnorm(1e4, 0, 3)

priorbsp <- ggplot() +
  geom_density(data = data.frame(bsp_prior = bsp_prior),
               aes(x = bsp_prior, colour = "Prior at N(0, 3)"),
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
ggsave("figures/troubleshootingModelGrowthGDD/priorVSposteriorCombined_LP.jpeg", priorcombined, width = 8, height = 12, units = "in", dpi = 300)
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
jpeg(
  filename = "figures/modelGrowthGDD/retrodictiveCheckHist_LP.jpeg",
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

# Add normal Priors ####
fit <- readRDS("output/stanOutput/fit")

df_fit <- as.data.frame(fit)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover sigmas ######
unique(colnames(df_fit))
sigma_cols <- colnames(df_fit)[grepl("sigma", colnames(df_fit))]

sigma_df <- df_fit[, colnames(df_fit) %in% sigma_cols]

sigma_df2 <- data.frame(
  sigma = character(ncol(sigma_df)),
  mean = numeric(ncol(sigma_df)),  
  per5 = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
sigma_df2

for (i in 1:ncol(sigma_df)) { # i = 1
  sigma_df2$sigma[i] <- colnames(sigma_df)[i]         
  sigma_df2$mean[i] <- round(mean(sigma_df[[i]]),3)  
  sigma_df2$per5[i] <- round(quantile(sigma_df[[i]], probs = 0.05), 3)
  sigma_df2$per25[i] <- round(quantile(sigma_df[[i]], probs = 0.25), 3)
  sigma_df2$per75[i] <- round(quantile(sigma_df[[i]], probs = 0.75), 3)
  sigma_df2$per95[i] <- round(quantile(sigma_df[[i]], probs = 0.95), 3)
}
sigma_df2

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover b spp ######
bspp_cols <- colnames(df_fit)[grepl("bsp", colnames(df_fit))]
# remove sigma_bspp for now
# bspp_cols <- bspp_cols[2:length(bspp_cols)]

bspp_df <- df_fit[, colnames(df_fit) %in% bspp_cols]
# change their names
colnames(bspp_df) <- sub("bsp\\[(\\d+)\\]", "\\1", colnames(bspp_df))
#empty spp df
bspp_df2 <- data.frame(
  spp = character(ncol(bspp_df)),
  fit_bspp = numeric(ncol(bspp_df)),  
  fit_bspp_per5 = NA, 
  fit_bspp_per25 = NA,
  fit_bspp_per75 = NA,
  fit_bspp_per95 = NA
)
for (i in 1:ncol(bspp_df)) { # i = 1
  bspp_df2$spp[i] <- colnames(bspp_df)[i]         
  bspp_df2$fit_bspp[i] <- round(mean(bspp_df[[i]]),3)  
  bspp_df2$fit_bspp_per5[i] <- round(quantile(bspp_df[[i]], probs = 0.05), 3)
  bspp_df2$fit_bspp_per25[i] <- round(quantile(bspp_df[[i]], probs = 0.25), 3)
  bspp_df2$fit_bspp_per75[i] <- round(quantile(bspp_df[[i]], probs = 0.75), 3)
  bspp_df2$fit_bspp_per95[i] <- round(quantile(bspp_df[[i]], probs = 0.95), 3)
}
bspp_df2


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover treeid ######

# grab treeid 
treeid_cols <- colnames(df_fit)[grepl("atreeid", colnames(df_fit))]
treeid_cols <- treeid_cols[!grepl("zatreeid", treeid_cols)]
treeid_cols <- treeid_cols[!grepl("sigma", treeid_cols)]

treeid_df <- df_fit[, colnames(df_fit) %in% treeid_cols]

# change their names
colnames(treeid_df) <- sub("atreeid\\[(\\d+)\\]", "\\1", colnames(treeid_df))
# empty treeid dataframe
treeid_df2 <- data.frame(
  treeid = character(ncol(treeid_df)),
  fit_atreeid = numeric(ncol(treeid_df)),  
  fit_atreeid_per5 = NA, 
  fit_atreeid_per25 = NA,
  fit_atreeid_per75 = NA,
  fit_atreeid_per95 = NA
)
for (i in 1:ncol(treeid_df)) { # i = 1
  treeid_df2$treeid[i] <- colnames(treeid_df)[i]         
  treeid_df2$fit_atreeid[i] <- round(mean(treeid_df[[i]]),3)  
  treeid_df2$fit_atreeid_per5[i] <- round(quantile(treeid_df[[i]], probs = 0.05), 3)
  treeid_df2$fit_atreeid_per25[i] <- round(quantile(treeid_df[[i]], probs = 0.25), 3)
  treeid_df2$fit_atreeid_per75[i] <- round(quantile(treeid_df[[i]], probs = 0.75), 3)
  treeid_df2$fit_atreeid_per95[i] <- round(quantile(treeid_df[[i]], probs = 0.95), 3)
}
treeid_df2

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover a spp  ######
aspp_cols <- colnames(df_fit)[grepl("aspp", colnames(df_fit))]

aspp_df <- df_fit[, colnames(df_fit) %in% aspp_cols]
# change their names
colnames(aspp_df) <- sub("aspp\\[(\\d+)\\]", "\\1", colnames(aspp_df))
#empty aspp df
aspp_df2 <- data.frame(
  spp = character(ncol(aspp_df)),
  fit_aspp = numeric(ncol(aspp_df)),  
  fit_aspp_per5 = NA, 
  fit_aspp_per25 = NA,
  fit_aspp_per75 = NA,
  fit_aspp_per95 = NA
)
for (i in 1:ncol(aspp_df)) { # i = 1
  aspp_df2$spp[i] <- colnames(aspp_df)[i]         
  aspp_df2$fit_aspp[i] <- round(mean(aspp_df[[i]]),3)  
  aspp_df2$fit_aspp_per5[i] <- round(quantile(aspp_df[[i]], probs = 0.05), 3)
  aspp_df2$fit_aspp_per25[i] <- round(quantile(aspp_df[[i]], probs = 0.25), 3)
  aspp_df2$fit_aspp_per75[i] <- round(quantile(aspp_df[[i]], probs = 0.75), 3)
  aspp_df2$fit_aspp_per95[i] <- round(quantile(aspp_df[[i]], probs = 0.95), 3)
}
aspp_df2


# PLOT COMPARISON PRIORS ####
sigma_df2
sigma_df2_lp2 <- sigma_df2_lp
colnames(sigma_df2_lp2)[2:ncol(sigma_df2_lp2)] <- paste(colnames(sigma_df2_lp2)[2:ncol(sigma_df2_lp2)],"lp", sep = "_")

sigmaforplot <- merge(sigma_df2, sigma_df2_lp2, by = "sigma")

sigma_priorcomp <- ggplot(sigmaforplot, aes(x = mean, y = mean_lp)) +
  geom_errorbar(aes(ymin = per25_lp, ymax = per75_lp),
                width = 0, linewidth = 1, color = "darkgray", alpha = 1) +
  geom_errorbar(aes(xmin = per25, xmax = per75),
                width = 0, linewidth = 1, color = "darkgray", alpha = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "#B40F20", linewidth = 1) +
  geom_point(color = "#046C9A", size = 3) +
  ggrepel::geom_text_repel(aes(label = sigma), size = 3) +
  labs(x = "regular priors", y = "larger priors",
       title = "sigma") +
  theme_minimal()
sigma_priorcomp

# bsp
bspp_df2_lp2 <- bspp_df2_lp
colnames(bspp_df2_lp2)[2:ncol(bspp_df2_lp2)] <- paste(colnames(bspp_df2_lp2)[2:ncol(bspp_df2_lp2)],"lp", sep = "_")

bsppforplot <- merge(bspp_df2, bspp_df2_lp2, by = "spp")

bspp_priorcomp <- ggplot(bsppforplot, aes(x = fit_bspp, y = fit_bspp_lp)) +
  geom_errorbar(aes(ymin = fit_bspp_per25_lp, ymax = fit_bspp_per75_lp),
                width = 0, linewidth = 1, color = "darkgray", alpha = 1) +
  geom_errorbar(aes(xmin = fit_bspp_per25, xmax = fit_bspp_per75),
                width = 0, linewidth = 1, color = "darkgray", alpha = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "#B40F20", linewidth = 1) +
  geom_point(color = "#046C9A", size = 3) +
  labs(x = "regular priors", y = "larger priors",
       title = "bspp") +
  theme_minimal()
bspp_priorcomp



# aspp
aspp_df2_lp2 <- aspp_df2_lp
colnames(aspp_df2_lp2)[2:ncol(aspp_df2_lp2)] <- paste(colnames(aspp_df2_lp2)[2:ncol(aspp_df2_lp2)],"lp", sep = "_")

asppforplot <- merge(aspp_df2, aspp_df2_lp2, by = "spp")

aspp_priorcomp <- ggplot(asppforplot, aes(x = fit_aspp, y = fit_aspp_lp)) +
  geom_errorbar(aes(ymin = fit_aspp_per25_lp, ymax = fit_aspp_per75_lp),
                width = 0, linewidth = 1, color = "darkgray", alpha = 1) +
  geom_errorbar(aes(xmin = fit_aspp_per25, xmax = fit_aspp_per75),
                width = 0, linewidth = 1, color = "darkgray", alpha = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "#B40F20", linewidth = 1) +
  geom_point(color = "#046C9A", size = 3) +
  labs(x = "regular priors", y = "larger priors",
       title = "aspp") +
  theme_minimal()
aspp_priorcomp

# atreeid
treeid_df2_lp2 <- treeid_df2_lp
colnames(treeid_df2_lp2)[2:ncol(treeid_df2_lp2)] <- paste(colnames(treeid_df2_lp2)[2:ncol(treeid_df2_lp2)],"lp", sep = "_")

atreeidforplot <- merge(treeid_df2, treeid_df2_lp2, by = "treeid")

atreeid_priorcomp <- ggplot(atreeidforplot, aes(x = fit_atreeid, y = fit_atreeid_lp)) +
  geom_errorbar(aes(ymin = fit_atreeid_per25_lp, ymax = fit_atreeid_per75_lp),
                width = 0, linewidth = 0.5, color = "darkgray", alpha = 1) +
  geom_errorbar(aes(xmin = fit_atreeid_per25, xmax = fit_atreeid_per75),
                width = 0, linewidth = 0.5, color = "darkgray", alpha = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "#B40F20", linewidth = 1) +
  geom_point(color = "#046C9A", size = 2) +
  labs(x = "regular priors", y = "larger priors",
       title = "atreeid") +
  theme_minimal()
atreeid_priorcomp

priorcomp <- (sigma_priorcomp + bspp_priorcomp) / 
  (aspp_priorcomp + atreeid_priorcomp)
ggsave("figures/troubleshootingModelGrowthGDD/priorComparisons.jpeg", priorcomp, width = 8, height = 10, units = "in", dpi = 300)


# Quick random plot ####
# Gdd on the x axis and growth on y ####
aspp_df2$a <- mean(df_fit[,"a"])
aspp_df2$a_asp <- aspp_df2$a + aspp_df2$fit_aspp
aspp_df2$bsp <- bspp_df2$fit_bspp

colnames(aspp_df2)[colnames(aspp_df2) == "spp"] <- "spp_num"

emp2 <- emp
emp2 <- merge(emp2, aspp_df2[, c("spp_num", "a", "bsp", "a_asp")], 
              by = "spp_num")
# plot lines
ggplot(emp2) +
  geom_point(aes(x = pgsGDD/200, y = lengthMM, colour = commonName)) +
  geom_abline(aes(intercept = a_asp, slope = bsp, colour = commonName), 
              linewidth = 0.5) +
  labs(title = "", x = "pgsGDD", y = "ring width in mm") +
  # scale_colour_manual(values = wes_palette("AsteroidCity1")) +
  # facet_wrap(~ spp) +
  xlim(0, 12) +
  theme_minimal()

# negative values around scaled 11 so 
11 * 200

