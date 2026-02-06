# CoringTreespotters growth model
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
# REAL VALUES ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
emp <- read.csv("output/empiricalDataMAIN.csv")

# transform my groups to numeric values
emp$spp_num <- match(emp$symbol, unique(emp$symbol))
emp$treeid_num <- match(emp$id, unique(emp$id))

# some checks
table(emp$symbol, emp$spp_num)
table(emp$id, emp$symbol)
table(emp$treeid_num, emp$spp_num)

ggplot(emp, aes(x = pgsGDD10, y = pgsGDD10AVG)) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = real, y = "averaged leaf colour data/spp + year", title = "atreeid") +
  theme_minimal()

# remove NAs
empabs <- emp[!is.na(emp$pgsGDD10), ]

# transform data in vectors
y <- empabs$lengthMM # ring width in mm
N <- nrow(empabs)
gdd <- empabs$pgsGDD10/200
Nspp <- length(unique(empabs$spp_num))
species <- as.numeric(as.character(empabs$spp_num))
treeid <- as.numeric(empabs$treeid_num)
Ntreeid <- length(unique(treeid))

rstan_options(auto_write = TRUE)
fit <- stan("stan/TSmodelGrowthGDD.stan", 
            data=c("N","y",
                   "Nspp","species",
                   "Ntreeid", "treeid", 
                   "gdd"),
            iter=4000, chains=4, cores=4)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AVG VALUES ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# remove NAs
empavg <- emp[!is.na(emp$pgsGDD10AVG), ]

# transform data in vectors
y <- empavg$lengthMM # ring width in mm
N <- nrow(empavg)
gdd <- empavg$pgsGDD10AVG/200
Nspp <- length(unique(empavg$spp_num))
species <- as.numeric(as.character(empavg$spp_num))
treeid <- as.numeric(empavg$treeid_num)
Ntreeid <- length(unique(treeid))

rstan_options(auto_write = TRUE)
fitavg <- stan("stan/TSmodelGrowthGDD.stan", 
            data=c("N","y",
                   "Nspp","species",
                   "Ntreeid", "treeid", 
                   "gdd"),
            iter=4000, chains=4, cores=4)

# === === === === === === === === === === === === #
##### Recover parameters from the posterior REAL##### 
# === === === === === === === === === === === === #
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

# === === === === === === === === === === === === #
##### Recover parameters from the posterior AVG ##### 
# === === === === === === === === === === === === #
df_fit_avg <- as.data.frame(fitavg)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover sigmas ######
unique(colnames(df_fit_avg))
sigma_cols_avg <- colnames(df_fit_avg)[grepl("sigma", colnames(df_fit_avg))]

sigma_df_avg <- df_fit_avg[, colnames(df_fit_avg) %in% sigma_cols_avg]

sigma_df2_avg <- data.frame(
  sigma = character(ncol(sigma_df_avg)),
  mean = numeric(ncol(sigma_df_avg)),  
  per5 = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
sigma_df2_avg

for (i in 1:ncol(sigma_df_avg)) { # i = 1
  sigma_df2_avg$sigma[i] <- colnames(sigma_df_avg)[i]         
  sigma_df2_avg$mean[i] <- round(mean(sigma_df_avg[[i]]),3)  
  sigma_df2_avg$per5[i] <- round(quantile(sigma_df_avg[[i]], probs = 0.05), 3)
  sigma_df2_avg$per25[i] <- round(quantile(sigma_df_avg[[i]], probs = 0.25), 3)
  sigma_df2_avg$per75[i] <- round(quantile(sigma_df_avg[[i]], probs = 0.75), 3)
  sigma_df2_avg$per95[i] <- round(quantile(sigma_df_avg[[i]], probs = 0.95), 3)
}
sigma_df2_avg

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover b spp ######
bspp_cols_avg <- colnames(df_fit_avg)[grepl("bsp", colnames(df_fit_avg))]
# remove sigma_bspp for now
# bspp_cols_avg <- bspp_cols_avg[2:length(bspp_cols_avg)]

bspp_df_avg <- df_fit_avg[, colnames(df_fit_avg) %in% bspp_cols_avg]
# change their names
colnames(bspp_df_avg) <- sub("bsp\\[(\\d+)\\]", "\\1", colnames(bspp_df_avg))
#empty spp df
bspp_df2_avg <- data.frame(
  spp = character(ncol(bspp_df_avg)),
  fit_bspp = numeric(ncol(bspp_df_avg)),  
  fit_bspp_per5 = NA, 
  fit_bspp_per25 = NA,
  fit_bspp_per75 = NA,
  fit_bspp_per95 = NA
)
for (i in 1:ncol(bspp_df_avg)) { # i = 1
  bspp_df2_avg$spp[i] <- colnames(bspp_df_avg)[i]         
  bspp_df2_avg$fit_bspp[i] <- round(mean(bspp_df_avg[[i]]),3)  
  bspp_df2_avg$fit_bspp_per5[i] <- round(quantile(bspp_df_avg[[i]], probs = 0.05), 3)
  bspp_df2_avg$fit_bspp_per25[i] <- round(quantile(bspp_df_avg[[i]], probs = 0.25), 3)
  bspp_df2_avg$fit_bspp_per75[i] <- round(quantile(bspp_df_avg[[i]], probs = 0.75), 3)
  bspp_df2_avg$fit_bspp_per95[i] <- round(quantile(bspp_df_avg[[i]], probs = 0.95), 3)
}
bspp_df2_avg


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover treeid ######

# grab treeid 
treeid_cols_avg <- colnames(df_fit_avg)[grepl("atreeid", colnames(df_fit_avg))]
treeid_cols_avg <- treeid_cols_avg[!grepl("zatreeid", treeid_cols_avg)]
treeid_cols_avg <- treeid_cols_avg[!grepl("sigma", treeid_cols_avg)]

treeid_df_avg <- df_fit_avg[, colnames(df_fit_avg) %in% treeid_cols_avg]

# change their names
colnames(treeid_df_avg) <- sub("atreeid\\[(\\d+)\\]", "\\1", colnames(treeid_df_avg))
# empty treeid dataframe
treeid_df2_avg <- data.frame(
  treeid = character(ncol(treeid_df_avg)),
  fit_atreeid = numeric(ncol(treeid_df_avg)),  
  fit_atreeid_per5 = NA, 
  fit_atreeid_per25 = NA,
  fit_atreeid_per75 = NA,
  fit_atreeid_per95 = NA
)
for (i in 1:ncol(treeid_df_avg)) { # i = 1
  treeid_df2_avg$treeid[i] <- colnames(treeid_df_avg)[i]         
  treeid_df2_avg$fit_atreeid[i] <- round(mean(treeid_df_avg[[i]]),3)  
  treeid_df2_avg$fit_atreeid_per5[i] <- round(quantile(treeid_df_avg[[i]], probs = 0.05), 3)
  treeid_df2_avg$fit_atreeid_per25[i] <- round(quantile(treeid_df_avg[[i]], probs = 0.25), 3)
  treeid_df2_avg$fit_atreeid_per75[i] <- round(quantile(treeid_df_avg[[i]], probs = 0.75), 3)
  treeid_df2_avg$fit_atreeid_per95[i] <- round(quantile(treeid_df_avg[[i]], probs = 0.95), 3)
}
treeid_df2_avg

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover a spp  ######
aspp_cols_avg <- colnames(df_fit_avg)[grepl("aspp", colnames(df_fit_avg))]

aspp_df_avg <- df_fit_avg[, colnames(df_fit_avg) %in% aspp_cols_avg]
# change their names
colnames(aspp_df_avg) <- sub("aspp\\[(\\d+)\\]", "\\1", colnames(aspp_df_avg))
#empty aspp df
aspp_df2_avg <- data.frame(
  spp = character(ncol(aspp_df_avg)),
  fit_aspp = numeric(ncol(aspp_df_avg)),  
  fit_aspp_per5 = NA, 
  fit_aspp_per25 = NA,
  fit_aspp_per75 = NA,
  fit_aspp_per95 = NA
)
for (i in 1:ncol(aspp_df_avg)) { # i = 1
  aspp_df2_avg$spp[i] <- colnames(aspp_df_avg)[i]         
  aspp_df2_avg$fit_aspp[i] <- round(mean(aspp_df_avg[[i]]),3)  
  aspp_df2_avg$fit_aspp_per5[i] <- round(quantile(aspp_df_avg[[i]], probs = 0.05), 3)
  aspp_df2_avg$fit_aspp_per25[i] <- round(quantile(aspp_df_avg[[i]], probs = 0.25), 3)
  aspp_df2_avg$fit_aspp_per75[i] <- round(quantile(aspp_df_avg[[i]], probs = 0.75), 3)
  aspp_df2_avg$fit_aspp_per95[i] <- round(quantile(aspp_df_avg[[i]], probs = 0.95), 3)
}
aspp_df2_avg

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# PLOTS ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot comparison sigmas ####
colnames(sigma_df2_avg)[2:ncol(sigma_df2_avg)] <- paste(colnames(sigma_df2_avg)[2:ncol(sigma_df2_avg)], "avg", sep = "_")

sigmaforplot <- merge(sigma_df2_avg, sigma_df2, by = "sigma")

sigmaplot <- ggplot(sigmaforplot, aes(x = mean_avg, y = mean)) +
  geom_errorbar(aes(xmin = per25_avg, xmax = per75_avg), 
                width = 0, linewidth = 0.7, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = per25, ymax = per75), 
                width = 0, linewidth = 0.7, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "Primary growing seasonGDD with averaged leaf  
color date", y = "Primary growing season GDD 
with real leaf color date", title = "sigma") +
  theme_minimal()
sigmaplot


# Plot comparison bspp ####
colnames(bspp_df2_avg)[2:ncol(bspp_df2_avg)] <- paste(colnames(bspp_df2_avg)[2:ncol(bspp_df2_avg)], "avg", sep = "_")

bsppforplot <- merge(bspp_df2_avg, bspp_df2, by = "spp")

bsppplot <- ggplot(bsppforplot, aes(x = fit_bspp_avg, y = fit_bspp)) +
  geom_errorbar(aes(xmin = fit_bspp_per25_avg, xmax = fit_bspp_per75_avg), 
                width = 0, linewidth = 0.7, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_bspp_per25, ymax = fit_bspp_per75), 
                width = 0, linewidth = 0.7, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "Primary growing seasonGDD with averaged leaf 
color date", y = "Primary growing season GDD 
with real leaf color date", title = "bspp") +
  theme_minimal()
bsppplot

# Plot comparison aspp ####
colnames(aspp_df2_avg)[2:ncol(aspp_df2_avg)] <- paste(colnames(aspp_df2_avg)[2:ncol(aspp_df2_avg)], "avg", sep = "_")

asppforplot <- merge(aspp_df2_avg, aspp_df2, by = "spp")

asppplot <- ggplot(asppforplot, aes(x = fit_aspp_avg, y = fit_aspp)) +
  geom_errorbar(aes(xmin = fit_aspp_per25_avg, xmax = fit_aspp_per75_avg), 
                width = 0, linewidth = 0.7, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_aspp_per25, ymax = fit_aspp_per75), 
                width = 0, linewidth = 0.7, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "Primary growing seasonGDD with averaged leaf  
color date", y = "Primary growing season GDD 
with real leaf color date", title = "aspp") +
  theme_minimal()
asppplot

# Plot comparison atreeid ####
colnames(treeid_df2_avg)[2:ncol(treeid_df2_avg)] <- paste(colnames(treeid_df2_avg)[2:ncol(treeid_df2_avg)], "avg", sep = "_")

treeidforplot <- merge(treeid_df2_avg, treeid_df2, by = "treeid")

treeidplot <- ggplot(treeidforplot, aes(x = fit_atreeid_avg, y = fit_atreeid)) +
  geom_errorbar(aes(xmin = fit_atreeid_per25_avg, xmax = fit_atreeid_per75_avg), 
                width = 0, linewidth = 0.7, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_atreeid_per25, ymax = fit_atreeid_per75), 
                width = 0, linewidth = 0.7, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "Primary Growing Season GDD with averaged leaf  
color date", y = "Primary growing season GDD 
with real leaf color date", title = "atreeid") +
  theme_minimal()
treeidplot

combined <- (sigmaplot + treeidplot) /
  (asppplot + bsppplot)
ggsave("figures/modelGrowthGDD/comparisonGDD/avgvsrealLeafcolordates.jpeg", combined, width = 8, height = 6, units = "in", dpi = 300)

