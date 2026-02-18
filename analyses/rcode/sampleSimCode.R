# CRD 17 Feb 2026
# 1:1 plot demo code

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 

# Load library 
library(ggplot2)
library(rstan)
library(patchwork)

# stan settings
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

# set wd
if (length(grep("christophe_rouleau-desrochers", getwd())) > 0) {
  setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")
} else if (length(grep("lizzie", getwd())) > 0) {
  setwd("/Users/lizzie/Documents/git/projects/others/coringtreespotters/wildchrokie/analyses")
} else  {
  setwd("/home/crouleau/wildchrokie/analyses")
}

# === === === === === === === === === === === === === === === === 
#### SIMULATE DATA ####
# === === === === === === === === === === === === === === === ===
# set parameters
set.seed(124)
a <- 2
sigma_y <- 0.2
sigma_asp <- 0.6
sigma_atreeid <- 0.25
sigma_bsp <- 0.3

n_spp <- 5 # number of species
n_perspp <- 6 # number of individuals per species
n_treeid <- n_perspp * n_spp # number of treeid
n_meas <- 10 # repeated measurements per id
N <- n_treeid * n_meas # total number of measurements

# get replicated treeid
treeid <- rep(1:n_treeid, each = n_meas)
# non replicated treeid
treeidnonrep <- rep(1:n_perspp, times = n_spp)
# replicated spp
spp <- rep(rep(1:n_spp, each = n_perspp), each = n_meas) 
# non replicated spp
spp_nonrep <- rep(1:n_spp, each = n_perspp)
# quick check 

sim <- data.frame(
  spp = spp,
  treeid = treeid
)

# get intercept values for each species
asp <- rnorm(n_spp, 0, sigma_asp)
atreeid <- rnorm(n_treeid, 0, sigma_atreeid)

# get slope values for each speciess
bsp <- rnorm(n_spp, 0, sigma_bsp)

# Add my parameters to the df
sim$atreeid <- atreeid[treeid]
sim$asp <- asp[sim$spp]
sim$bsp <- bsp[sim$spp]

# add the rest of the boring stuff 
sim$a <- a
sim$sigma_y <- sigma_y
sim$sigma_atreeid <- sigma_atreeid
sim$sigma_asp <- sigma_asp
sim$error <- rnorm(N, 0, sigma_y)
sim$gdd <- rnorm(N, 1800, 100)
sim$gddcons <- sim$gdd/200

# adding both options of tree rings
sim$ringwidth <- 
  sim$asp + 
  sim$atreeid + 
  sim$a +
  (sim$bsp*sim$gddcons)+
  sim$error

# prepare grouping factors
sim$spp <- factor(sim$spp)
sim$treeid <- factor(sim$treeid)

sim$a_asp <- sim$a + sim$asp

# === === === === === #
##### Run model #####
# === === === === === #
y <- sim$ringwidth
N <- nrow(sim)
gdd <- sim$gddcons
Nspp <- length(unique(sim$spp))
species <- as.numeric(as.character(sim$spp))
treeid <- treeid
Ntreeid <- length(unique(treeid))

fit <- stan("stan/TSmodelGrowthGDD.stan", 
            data=c("N","y",
                   "Nspp","species",
                   "Ntreeid", "treeid", 
                   "gdd"),
            iter=1000, chains=4,
            save_dso = FALSE)

# === === === === === === === === === === === === #
##### Recover parameters from the posterior #####
# === === === === === === === === === === === === #
df_fit <- as.data.frame(fit)

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

sigma_df2$sim_sigma <- c(sigma_atreeid, sigma_y)


###### Recover b spp ######
bspp_cols <- colnames(df_fit)[grepl("bsp", colnames(df_fit))]
# remove sigma_aspp for now
bspp_cols <- bspp_cols[2:length(bspp_cols)]

bspp_df <- df_fit[, colnames(df_fit) %in% bspp_cols]
# change their names
colnames(bspp_df) <- sub("bspp\\[(\\d+)\\]", "\\1", colnames(bspp_df))
#empty spp df
bspp_df2 <- data.frame(
  spp = character(ncol(bspp_df)),
  fit_bsp = numeric(ncol(bspp_df)),  
  fit_bsp_per5 = NA, 
  fit_bsp_per25 = NA,
  fit_bsp_per75 = NA,
  fit_bsp_per95 = NA
)
for (i in 1:ncol(bspp_df)) { # i = 1
  bspp_df2$spp[i] <- colnames(bspp_df)[i]         
  bspp_df2$fit_bsp[i] <- round(mean(bspp_df[[i]]),3)  
  bspp_df2$fit_bsp_per5[i] <- round(quantile(bspp_df[[i]], probs = 0.05), 3)
  bspp_df2$fit_bsp_per25[i] <- round(quantile(bspp_df[[i]], probs = 0.25), 3)
  bspp_df2$fit_bsp_per75[i] <- round(quantile(bspp_df[[i]], probs = 0.75), 3)
  bspp_df2$fit_bsp_per95[i] <- round(quantile(bspp_df[[i]], probs = 0.95), 3)
}


###### Recover treeid ######
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


###### Recover a spp  ######
aspp_cols <- colnames(df_fit)[grepl("asp", colnames(df_fit))]
aspp_cols <- aspp_cols[!grepl("zasp", aspp_cols)]
aspp_cols <- aspp_cols[!grepl("sigma", aspp_cols)]

aspp_df <- df_fit[, colnames(df_fit) %in% aspp_cols]
# change their names
colnames(aspp_df) <- sub("aspp\\[(\\d+)\\]", "\\1", colnames(aspp_df))
#empty aspp df
aspp_df2 <- data.frame(
  spp = character(ncol(aspp_df)),
  fit_asp = numeric(ncol(aspp_df)),  
  fit_asp_per5 = NA, 
  fit_asp_per25 = NA,
  fit_asp_per75 = NA,
  fit_asp_per95 = NA
)
for (i in 1:ncol(aspp_df)) { # i = 1
  aspp_df2$spp[i] <- colnames(aspp_df)[i]         
  aspp_df2$fit_asp[i] <- round(mean(aspp_df[[i]]),3)  
  aspp_df2$fit_asp_per5[i] <- round(quantile(aspp_df[[i]], probs = 0.05), 3)
  aspp_df2$fit_asp_per25[i] <- round(quantile(aspp_df[[i]], probs = 0.25), 3)
  aspp_df2$fit_asp_per75[i] <- round(quantile(aspp_df[[i]], probs = 0.75), 3)
  aspp_df2$fit_asp_per95[i] <- round(quantile(aspp_df[[i]], probs = 0.95), 3)
}

# === === === === === === === #
##### Plot parameter recovery #####
# === === === === === === === #

###### Plot sigmas ######
sigma_simXfit_plot <- ggplot(sigma_df2, aes(x = sim_sigma, y = mean)) +
  geom_errorbar(aes(ymin = per25, ymax = per75),
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 1) +
  geom_errorbar(aes(ymin = per5, ymax = per95),
                width = 0, linewidth = 0.5, color = "darkgray", alpha = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "red", linewidth = 1) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(aes(label = sigma), size = 3) +
  labs(x = "sim sigma", y = "fit sigma",
       title = "") +
  theme_minimal()
sigma_simXfit_plot

###### Plot b spp ######
bspptoplot <- merge(
  sim[!duplicated(sim$spp), 
      c("spp", "bsp")], 
  bspp_df2[!duplicated(bspp_df2$spp), 
           c("spp", "fit_bsp", "fit_bsp_per25", "fit_bsp_per75", "fit_bsp_per5", "fit_bsp_per95")], 
  by = "spp"
)
bspptoplot

bsp_simXfit_plot <- ggplot(bspptoplot, aes(x = bsp, y = fit_bsp)) +
  geom_errorbar(aes(ymin = fit_bsp_per5, ymax = fit_bsp_per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = fit_bsp_per25, ymax = fit_bsp_per75), 
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 0.7) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "sim bsp", y = "fit bsp", title = "") +
  theme_minimal()
bsp_simXfit_plot

###### Plot treeid ######
# add sim to fit treeid df
treeidtoplot <- merge(
  sim[!duplicated(sim$treeid), 
      c("treeid", "atreeid")], 
  treeid_df2[!duplicated(treeid_df2$treeid), 
             c("treeid", "fit_atreeid", 
               "fit_atreeid_per5", 
               "fit_atreeid_per25",
               "fit_atreeid_per75",
               "fit_atreeid_per95")], 
  by = "treeid"
)
treeidtoplot
# plot treeid
atreeid_simXfit_plot <- ggplot(treeidtoplot, aes(x = atreeid, y = fit_atreeid)) +
  geom_errorbar(aes(ymin = fit_atreeid_per25, ymax = fit_atreeid_per75), 
                width = 0, linewidth = 1, color = "darkgray", alpha=0.8) +
  geom_errorbar(aes(ymin = fit_atreeid_per5, ymax = fit_atreeid_per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.8) +
  geom_point(size = 2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "sim atreeid", y = "fit atreeid", title = "") +
  theme_minimal()
atreeid_simXfit_plot

###### Plot a spp ######
aspptoplot <- merge(
  sim[!duplicated(sim$spp), 
      c("spp", "asp")], 
  aspp_df2[!duplicated(aspp_df2$spp), 
           c("spp", "fit_asp", 
             "fit_asp_per5", 
             "fit_asp_per25", 
             "fit_asp_per75", 
             "fit_asp_per95")], 
  by = "spp"
)
aspptoplot

asp_simXfit_plot <- ggplot(aspptoplot, aes(x = asp, y = fit_asp)) +
  geom_errorbar(aes(ymin = fit_asp_per5, ymax = fit_asp_per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.9) +
  geom_errorbar(aes(ymin = fit_asp_per25, ymax = fit_asp_per75), 
                width = 0, linewidth = 1.5,  color = "darkgray", alpha=0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "sim asp", y = "fit asp", title = "") +
  geom_point(size = 2) +
  theme_minimal()
asp_simXfit_plot

##### Combined sim plots #####
combined <- (sigma_simXfit_plot + atreeid_simXfit_plot) /
  (bsp_simXfit_plot + asp_simXfit_plot)
combined

