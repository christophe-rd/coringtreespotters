# CoringTreespotters growth model
# CRD 5 February 2026

# Goal: compare growth responses gdd 5 vs gdd10 baseline values

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
emp <- emp[!is.na(emp$pgsGDD10), ]

# transform data in vectors
y <- emp$lengthMM # ring width in mm
N <- nrow(emp)
gdd5 <- emp$pgsGDD5/200
gdd10 <- emp$pgsGDD10/200
Nspp <- length(unique(emp$spp_num))
species <- as.numeric(as.character(emp$spp_num))
treeid <- as.numeric(emp$treeid_num)
Ntreeid <- length(unique(treeid))

# check that everything is fine
table(treeid,species)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
rstan_options(auto_write = TRUE)
fitgdd5 <- stan("stan/TSmodelGrowthGDD5.stan", 
            data=c("N","y",
                   "Nspp","species",
                   "Ntreeid", "treeid", 
                   "gdd5"),
            iter=4000, chains=4, cores=4)


fitgdd10 <- stan("stan/TSmodelGrowthGDD10.stan", 
                data=c("N","y",
                       "Nspp","species",
                       "Ntreeid", "treeid", 
                       "gdd10"),
                iter=4000, chains=4, cores=4)
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
##### Recover parameters from the posterior GDD5 ##### 
# === === === === === === === === === === === === #
df_fit5 <- as.data.frame(fitgdd5)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover sigmas ######
sigma_cols5 <- colnames(df_fit5)[grepl("sigma", colnames(df_fit5))]

sigma_df_5 <- df_fit5[, colnames(df_fit5) %in% sigma_cols5]

sigma_df2_5 <- data.frame(
  sigma = character(ncol(sigma_df_5)),
  mean = numeric(ncol(sigma_df_5)),  
  per5 = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
sigma_df2_5

for (i in 1:ncol(sigma_df_5)) { # i = 1
  sigma_df2_5$sigma[i] <- colnames(sigma_df_5)[i]         
  sigma_df2_5$mean[i] <- round(mean(sigma_df_5[[i]]),3)  
  sigma_df2_5$per5[i] <- round(quantile(sigma_df_5[[i]], probs = 0.05), 3)
  sigma_df2_5$per25[i] <- round(quantile(sigma_df_5[[i]], probs = 0.25), 3)
  sigma_df2_5$per75[i] <- round(quantile(sigma_df_5[[i]], probs = 0.75), 3)
  sigma_df2_5$per95[i] <- round(quantile(sigma_df_5[[i]], probs = 0.95), 3)
}
sigma_df2_5

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover b spp ######
bspp_cols5 <- colnames(df_fit5)[grepl("bsp", colnames(df_fit5))]
# remove sigma_bspp for now
# bspp_cols5 <- bspp_cols5[2:length(bspp_cols5)]

bspp_df5 <- df_fit5[, colnames(df_fit5) %in% bspp_cols5]
# change their names
colnames(bspp_df5) <- sub("bsp\\[(\\d+)\\]", "\\1", colnames(bspp_df5))
#empty spp df
bspp_df2_5 <- data.frame(
  spp = character(ncol(bspp_df5)),
  fit_bspp = numeric(ncol(bspp_df5)),  
  fit_bspp_per5 = NA, 
  fit_bspp_per25 = NA,
  fit_bspp_per75 = NA,
  fit_bspp_per95 = NA
)
for (i in 1:ncol(bspp_df5)) { # i = 1
  bspp_df2_5$spp[i] <- colnames(bspp_df5)[i]         
  bspp_df2_5$fit_bspp[i] <- round(mean(bspp_df5[[i]]),3)  
  bspp_df2_5$fit_bspp_per5[i] <- round(quantile(bspp_df5[[i]], probs = 0.05), 3)
  bspp_df2_5$fit_bspp_per25[i] <- round(quantile(bspp_df5[[i]], probs = 0.25), 3)
  bspp_df2_5$fit_bspp_per75[i] <- round(quantile(bspp_df5[[i]], probs = 0.75), 3)
  bspp_df2_5$fit_bspp_per95[i] <- round(quantile(bspp_df5[[i]], probs = 0.95), 3)
}
bspp_df2_5


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover treeid ######

# grab treeid 
treeid_cols5 <- colnames(df_fit5)[grepl("atreeid", colnames(df_fit5))]
treeid_cols5 <- treeid_cols5[!grepl("zatreeid", treeid_cols5)]
treeid_cols5 <- treeid_cols5[!grepl("sigma", treeid_cols5)]

treeid_df5 <- df_fit5[, colnames(df_fit5) %in% treeid_cols5]

# change their names
colnames(treeid_df5) <- sub("atreeid\\[(\\d+)\\]", "\\1", colnames(treeid_df5))
# empty treeid dataframe
treeid_df2_5 <- data.frame(
  treeid = character(ncol(treeid_df5)),
  fit_atreeid = numeric(ncol(treeid_df5)),  
  fit_atreeid_per5 = NA, 
  fit_atreeid_per25 = NA,
  fit_atreeid_per75 = NA,
  fit_atreeid_per95 = NA
)
for (i in 1:ncol(treeid_df5)) { # i = 1
  treeid_df2_5$treeid[i] <- colnames(treeid_df5)[i]         
  treeid_df2_5$fit_atreeid[i] <- round(mean(treeid_df5[[i]]),3)  
  treeid_df2_5$fit_atreeid_per5[i] <- round(quantile(treeid_df5[[i]], probs = 0.05), 3)
  treeid_df2_5$fit_atreeid_per25[i] <- round(quantile(treeid_df5[[i]], probs = 0.25), 3)
  treeid_df2_5$fit_atreeid_per75[i] <- round(quantile(treeid_df5[[i]], probs = 0.75), 3)
  treeid_df2_5$fit_atreeid_per95[i] <- round(quantile(treeid_df5[[i]], probs = 0.95), 3)
}
treeid_df2_5

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover a spp  ######
aspp_cols5 <- colnames(df_fit5)[grepl("aspp", colnames(df_fit5))]

aspp_df5 <- df_fit5[, colnames(df_fit5) %in% aspp_cols5]
# change their names
colnames(aspp_df5) <- sub("aspp\\[(\\d+)\\]", "\\1", colnames(aspp_df5))
#empty aspp df
aspp_df2_5 <- data.frame(
  spp = character(ncol(aspp_df5)),
  fit_aspp = numeric(ncol(aspp_df5)),  
  fit_aspp_per5 = NA, 
  fit_aspp_per25 = NA,
  fit_aspp_per75 = NA,
  fit_aspp_per95 = NA
)
for (i in 1:ncol(aspp_df5)) { # i = 1
  aspp_df2_5$spp[i] <- colnames(aspp_df5)[i]         
  aspp_df2_5$fit_aspp[i] <- round(mean(aspp_df5[[i]]),3)  
  aspp_df2_5$fit_aspp_per5[i] <- round(quantile(aspp_df5[[i]], probs = 0.05), 3)
  aspp_df2_5$fit_aspp_per25[i] <- round(quantile(aspp_df5[[i]], probs = 0.25), 3)
  aspp_df2_5$fit_aspp_per75[i] <- round(quantile(aspp_df5[[i]], probs = 0.75), 3)
  aspp_df2_5$fit_aspp_per95[i] <- round(quantile(aspp_df5[[i]], probs = 0.95), 3)
}
aspp_df2_5

# === === === === === === === === === === === === #
##### Recover parameters from the posterior GDD ##### 
# === === === === === === === === === === === === #
df_fit10 <- as.data.frame(fitgdd10)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover sigmas ######
sigma_cols10 <- colnames(df_fit10)[grepl("sigma", colnames(df_fit10))]

sigma_df_10 <- df_fit10[, colnames(df_fit10) %in% sigma_cols10]

sigma_df2_10 <- data.frame(
  sigma = character(ncol(sigma_df_10)),
  mean = numeric(ncol(sigma_df_10)),  
  per5 = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
sigma_df2_10

for (i in 1:ncol(sigma_df_10)) { # i = 1
  sigma_df2_10$sigma[i] <- colnames(sigma_df_10)[i]         
  sigma_df2_10$mean[i] <- round(mean(sigma_df_10[[i]]),3)  
  sigma_df2_10$per5[i] <- round(quantile(sigma_df_10[[i]], probs = 0.05), 3)
  sigma_df2_10$per25[i] <- round(quantile(sigma_df_10[[i]], probs = 0.25), 3)
  sigma_df2_10$per75[i] <- round(quantile(sigma_df_10[[i]], probs = 0.75), 3)
  sigma_df2_10$per95[i] <- round(quantile(sigma_df_10[[i]], probs = 0.95), 3)
}
sigma_df2_10

###### Recover b spp ######
bspp_cols10 <- colnames(df_fit10)[grepl("bsp", colnames(df_fit10))]
# remove sigma_bspp for now
# bspp_cols10 <- bspp_cols10[2:length(bspp_cols10)]

bspp_df10 <- df_fit10[, colnames(df_fit10) %in% bspp_cols10]
# change their names
colnames(bspp_df10) <- sub("bsp\\[(\\d+)\\]", "\\1", colnames(bspp_df10))
#empty spp df
bspp_df2_10 <- data.frame(
  spp = character(ncol(bspp_df10)),
  fit_bspp = numeric(ncol(bspp_df10)),  
  fit_bspp_per5 = NA, 
  fit_bspp_per25 = NA,
  fit_bspp_per75 = NA,
  fit_bspp_per95 = NA
)
for (i in 1:ncol(bspp_df10)) { # i = 1
  bspp_df2_10$spp[i] <- colnames(bspp_df10)[i]         
  bspp_df2_10$fit_bspp[i] <- round(mean(bspp_df10[[i]]),3)  
  bspp_df2_10$fit_bspp_per5[i] <- round(quantile(bspp_df10[[i]], probs = 0.05), 3)
  bspp_df2_10$fit_bspp_per25[i] <- round(quantile(bspp_df10[[i]], probs = 0.25), 3)
  bspp_df2_10$fit_bspp_per75[i] <- round(quantile(bspp_df10[[i]], probs = 0.75), 3)
  bspp_df2_10$fit_bspp_per95[i] <- round(quantile(bspp_df10[[i]], probs = 0.95), 3)
}
bspp_df2_10


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover treeid ######

# grab treeid 
treeid_cols10 <- colnames(df_fit10)[grepl("atreeid", colnames(df_fit10))]
treeid_cols10 <- treeid_cols10[!grepl("zatreeid", treeid_cols10)]
treeid_cols10 <- treeid_cols10[!grepl("sigma", treeid_cols10)]

treeid_df10 <- df_fit10[, colnames(df_fit10) %in% treeid_cols10]

# change their names
colnames(treeid_df10) <- sub("atreeid\\[(\\d+)\\]", "\\1", colnames(treeid_df10))
# empty treeid dataframe
treeid_df2_10 <- data.frame(
  treeid = character(ncol(treeid_df10)),
  fit_atreeid = numeric(ncol(treeid_df10)),  
  fit_atreeid_per5 = NA, 
  fit_atreeid_per25 = NA,
  fit_atreeid_per75 = NA,
  fit_atreeid_per95 = NA
)
for (i in 1:ncol(treeid_df10)) { # i = 1
  treeid_df2_10$treeid[i] <- colnames(treeid_df10)[i]         
  treeid_df2_10$fit_atreeid[i] <- round(mean(treeid_df10[[i]]),3)  
  treeid_df2_10$fit_atreeid_per5[i] <- round(quantile(treeid_df10[[i]], probs = 0.05), 3)
  treeid_df2_10$fit_atreeid_per25[i] <- round(quantile(treeid_df10[[i]], probs = 0.25), 3)
  treeid_df2_10$fit_atreeid_per75[i] <- round(quantile(treeid_df10[[i]], probs = 0.75), 3)
  treeid_df2_10$fit_atreeid_per95[i] <- round(quantile(treeid_df10[[i]], probs = 0.95), 3)
}
treeid_df2_10

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover a spp  ######
aspp_cols10 <- colnames(df_fit10)[grepl("aspp", colnames(df_fit10))]

aspp_df10 <- df_fit10[, colnames(df_fit10) %in% aspp_cols10]
# change their names
colnames(aspp_df10) <- sub("aspp\\[(\\d+)\\]", "\\1", colnames(aspp_df10))
#empty aspp df
aspp_df2_10 <- data.frame(
  spp = character(ncol(aspp_df10)),
  fit_aspp = numeric(ncol(aspp_df10)),  
  fit_aspp_per5 = NA, 
  fit_aspp_per25 = NA,
  fit_aspp_per75 = NA,
  fit_aspp_per95 = NA
)
for (i in 1:ncol(aspp_df10)) { # i = 1
  aspp_df2_10$spp[i] <- colnames(aspp_df10)[i]         
  aspp_df2_10$fit_aspp[i] <- round(mean(aspp_df10[[i]]),3)  
  aspp_df2_10$fit_aspp_per5[i] <- round(quantile(aspp_df10[[i]], probs = 0.05), 3)
  aspp_df2_10$fit_aspp_per25[i] <- round(quantile(aspp_df10[[i]], probs = 0.25), 3)
  aspp_df2_10$fit_aspp_per75[i] <- round(quantile(aspp_df10[[i]], probs = 0.75), 3)
  aspp_df2_10$fit_aspp_per95[i] <- round(quantile(aspp_df10[[i]], probs = 0.95), 3)
}
aspp_df2_10


# Plot comparison sigmas ####
colnames(sigma_df2_5)[2:ncol(sigma_df2_5)] <- paste(colnames(sigma_df2_5)[2:ncol(sigma_df2_5)], "gdd5", sep = "_")

sigmaforplot <- merge(sigma_df2_5, sigma_df2_10, by = "sigma")

sigmaplot <- ggplot(sigmaforplot, aes(x = mean_gdd5, y = mean)) +
  geom_errorbar(aes(xmin = per25_gdd5, xmax = per75_gdd5), 
                width = 0, linewidth = 1, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = per25, ymax = per75), 
                width = 0, linewidth = 1, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "gdd 5", y = "gdd 10", title = "sigma") +
  theme_minimal()
sigmaplot


# Plot comparison bspp ####
colnames(bspp_df2_5)[2:ncol(bspp_df2_5)] <- paste(colnames(bspp_df2_5)[2:ncol(bspp_df2_5)], "gdd5", sep = "_")

bsppforplot <- merge(bspp_df2_5, bspp_df2_10, by = "spp")

bsppplot <- ggplot(bsppforplot, aes(x = fit_bspp_gdd5, y = fit_bspp)) +
  geom_errorbar(aes(xmin = fit_bspp_per25_gdd5, xmax = fit_bspp_per75_gdd5), 
                width = 0, linewidth = 1, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_bspp_per25, ymax = fit_bspp_per75), 
                width = 0, linewidth = 1, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "gdd 5", y = "gdd 10", title = "bspp") +
  theme_minimal()
bsppplot
# Plot comparison aspp ####
colnames(aspp_df2_5)[2:ncol(aspp_df2_5)] <- paste(colnames(aspp_df2_5)[2:ncol(aspp_df2_5)], "gdd5", sep = "_")

asppforplot <- merge(aspp_df2_5, aspp_df2_10, by = "spp")

asppplot <- ggplot(asppforplot, aes(x = fit_aspp_gdd5, y = fit_aspp)) +
  geom_errorbar(aes(xmin = fit_aspp_per25_gdd5, xmax = fit_aspp_per75_gdd5), 
                width = 0, linewidth = 1, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_aspp_per25, ymax = fit_aspp_per75), 
                width = 0, linewidth = 1, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "gdd 5", y = "gdd 10", title = "aspp") +
  theme_minimal()
asppplot

# Plot comparison aspp ####
colnames(treeid_df2_5)[2:ncol(treeid_df2_5)] <- paste(colnames(treeid_df2_5)[2:ncol(treeid_df2_5)], "gdd5", sep = "_")

treeidforplot <- merge(treeid_df2_5, treeid_df2_10, by = "treeid")

treeidplot <- ggplot(treeidforplot, aes(x = fit_atreeid_gdd5, y = fit_atreeid)) +
  geom_errorbar(aes(xmin = fit_atreeid_per25_gdd5, xmax = fit_atreeid_per75_gdd5), 
                width = 0, linewidth = 1, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_atreeid_per25, ymax = fit_atreeid_per75), 
                width = 0, linewidth = 1, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "gdd 5", y = "gdd 10", title = "atreeid") +
  theme_minimal()
treeidplot

combined <- (sigmaplot + treeidplot) /
  (asppplot + bsppplot)
ggsave("figures/modelGrowthGDD/comparisonGDD/GDD5vsGDD10.jpeg", combined, width = 8, height = 6, units = "in", dpi = 300)

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
