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

# runSimData <- TRUE
# if (runSimData) {
# === === === === === === === === === === === === === === === === 
#### SIMULATED DATA ####
# === === === === === === === === === === === === === === === ===

# set parameters
set.seed(124)
a <- 5
b <- 0.4
sigma_y <- 0.2
sigma_asp <- 0.4
sigma_atreeid <- 0.25
sigma_bsp <- 0.1

n_spp <- 10 # number of species
n_perspp <- 4 # number of individuals per species
n_treeid <- n_perspp * n_spp # number of treeid
n_meas <- 10 # repeated measurements per id
N <- n_treeid * n_meas # total number of measurements
N

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
sim$b <- b
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
  (sim$b*sim$gddcons) + 
  (sim$bsp*sim$gddcons)+
  sim$error

# prepare grouping factors
sim$spp <- factor(sim$spp)
sim$treeid <- factor(sim$treeid)

sim$a_asp <- sim$a + sim$asp
sim$b_bsp <- sim$b + sim$bsp

# check sim data
ggplot(sim) +
  geom_histogram(aes(ringwidth, color = spp, fill = spp), 
                 binwidth = 1) +
  # geom_vline(aes(xintercept = a_asite, linetype = "Site mean"),
  #            linewidth = 0.9, alpha = 0.8, color = "black") +
  geom_vline(aes(xintercept = a_asp, linetype = "Species mean"),
             linewidth = 0.9, color = "black") +
  geom_vline(aes(xintercept = a, linetype = "Grand mean"),
             linewidth = 1.2, color = "black") +
  facet_wrap(~spp) +
  labs(y = "",
       title = "gdd at leafout",
       linetype = "line type") +
  # scale_color_manual(values = wes_palette("AsteroidCity1")) +
  # scale_fill_manual(values = wes_palette("AsteroidCity1")) +
  scale_linetype_manual(values = c(
    "Grand mean" = "solid",
    "Site mean" = "dotted",
    "Species mean" = "dashed"
  )) +
  theme_minimal() +
  theme(
    legend.key.height = unit(4, "lines")
  )


# line plots
ggplot(sim) +
  geom_point(aes(x = gddcons, y = ringwidth, colour = spp)) +
  geom_abline(aes(intercept = a_asp, slope = b_bsp, colour = spp), 
              linewidth = 0.5) +
  labs(title = "", x = "pgsGDD", y = "ring width in mm") +
  # scale_colour_manual(values = wes_palette("AsteroidCity1")) +
  # facet_wrap(~ spp) +
  theme_minimal()

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
table(treeid)

rstan_options(auto_write = TRUE)

fit <- stan("stan/TSmodelGrowthGDD.stan", 
                    data=c("N","y",
                           "Nspp","species",
                           "Ntreeid", "treeid", 
                           "gdd"),
                    iter=4000, chains=4, cores=4)

# saveRDS(fit, "output/stanOutput/GDDleafout/fit")
# fit <- readRDS("output/stanOutput/GDDleafout/fit")


# Diagnostics ####
diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)

# asp
asp <- names(samples)[grepl("asp", names(samples))]
asp <- asp[!grepl("sigma", asp)][1:4]

jpeg("figures/simData/aspParameterization.jpg", width = 2000, height = 2000,
     units = "px", res = 300)
util$plot_div_pairs(asp, "sigma_asp", samples, diagnostics, transforms = list("sigma_asp" = 1))
dev.off()

# jpeg("figures/gddLeafout_empData/asiteParameterization.jpg", width = 2000, height = 2000, 
#      units = "px", res = 300)
# util$plot_div_pairs(asite, "sigma_asite", samples, diagnostics, transforms = list("sigma_asite" = 1))
# dev.off()

# atreeid
atreeid <- names(samples)[!grepl("zatreeid", names(samples))]
atreeid <- atreeid[!grepl("sigma", atreeid)]
atreeid <- atreeid[sample(length(unique(atreeid)), 21)]
pdf("figures/simData/atreeidParameterization.pdf", width = 6, height = 18)
util$plot_div_pairs(atreeid, "sigma_atreeid", samples, diagnostics, transforms = list("sigma_atreeid" = 1))
dev.off()


# === === === === === === === === === === === === #
##### Recover parameters from the posterior #####
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

sigma_df2$sim_sigma <- c( 
  # sigma_bsp,
                         sigma_atreeid, 
                         sigma_asp, 
                         sigma_y)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Recover b spp ######
bspp_cols <- colnames(df_fit)[grepl("bsp", colnames(df_fit))]
# remove sigma_aspp for now
bspp_cols <- bspp_cols[2:length(bspp_cols)]

bspp_df <- df_fit[, colnames(df_fit) %in% bspp_cols]
# change their names
colnames(bspp_df) <- sub("bsp\\[(\\d+)\\]", "\\1", colnames(bspp_df))
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
aspp_cols <- colnames(df_fit)[grepl("asp", colnames(df_fit))]
aspp_cols <- aspp_cols[!grepl("zasp", aspp_cols)]
aspp_cols <- aspp_cols[!grepl("sigma", aspp_cols)]

aspp_df <- df_fit[, colnames(df_fit) %in% aspp_cols]
# change their names
colnames(aspp_df) <- sub("asp\\[(\\d+)\\]", "\\1", colnames(aspp_df))
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

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### Plot sigmas ######
sigma_simXfit_plot <- ggplot(sigma_df2, aes(x = sim_sigma, y = mean)) +
  geom_errorbar(aes(ymin = per25, ymax = per75),
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 1) +
  geom_errorbar(aes(ymin = per5, ymax = per95),
                width = 0, linewidth = 0.5, color = "darkgray", alpha = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "#B40F20", linewidth = 1) +
  geom_point(color = "#046C9A", size = 3) +
  ggrepel::geom_text_repel(aes(label = sigma), size = 3) +
  labs(x = "sim sigma", y = "fit sigma",
       title = "") +
  theme_minimal()
sigma_simXfit_plot
ggsave("figures/simData/sigma_simXfit_plot.jpeg", sigma_simXfit_plot, width = 6, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
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
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "sim bsp", y = "fit bsp", title = "") +
  theme_minimal()
bsp_simXfit_plot
# ggsave!
ggsave("figures/simData/bsp_simXfit_plot2.jpeg", bsp_simXfit_plot, width = 6, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
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
  geom_errorbar(aes(ymin = fit_atreeid_per5, ymax = fit_atreeid_per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.8) +
  geom_point(color = "#046C9A", size = 2, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "sim atreeid", y = "fit atreeid", title = "") +
  theme_minimal()
atreeid_simXfit_plot
# ggsave!
ggsave("figures/simData/atreeid_simXfit_plot.jpeg", atreeid_simXfit_plot, width = 6, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
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
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "sim asp", y = "fit asp", title = "") +
  geom_point(color = "#046C9A", size = 2) +
  theme_minimal()
asp_simXfit_plot
# ggsave!
ggsave("figures/simData/asp_simXfit_plot.jpeg", asp_simXfit_plot, width = 6, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

}

##### Diagnostics #####
# === === === === === === === === === === === === === === === === 

# === === === === === === === === === === === === === === === === 
###### Priors VS Posterior ######
# === === === === === === === === === === === === === === === === 
# prior predictive checks. Simulating prior values from the values set in the model block
hyperparameter_draws <- 8000
parameter_draws <- 1000

###### Priors sigma_bsp ######
sigma_bsp_draw <- abs(rnorm(hyperparameter_draws, 0, 0.2))   
ggplot() +
  geom_density(data = data.frame(sigma_bsp_draw = sigma_bsp_draw),
               aes(x = sigma_bsp_draw, colour = "Prior"),
               linewidth = 0.8) +
  geom_density(data = sigma_df,
               aes(x = sigma_bsp, colour = "Posterior"),
               linewidth = 0.8) +
  labs(title = "priorVSposterior_bsp",
       x = "BSP", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()
ggsave("figures/simData/priorsPredictiveChecks/priorVSposterior_sigma_bsp.jpeg", width = 10, height = 8, units = "in", dpi = 300)

sigma_asp_draw <- abs(rnorm(draws, 0, 0.5))
sigma_atree_draw <- abs(rnorm(draws, 0, 0.05))
sigma_y_draw <- abs(rnorm(draws, 0, 5))

######Priors bsp ######
n_sigma_bsp <- 200

# set to prior values
sigma_bsp_vec <- abs(rnorm(n_sigma_bsp, 0, 0.2))

prior_bsp <- rep(NA, parameter_draws*length(sigma_bsp_vec))

for (i in 1: length(sigma_bsp_vec)) {
  prior_bsp[((i - 1)*parameter_draws + 1):(i*parameter_draws)] <- rnorm(parameter_draws, 0, sigma_bsp_vec[i])
}
prior_bsp

# Get the posterior distribution
bspp_df3 <- bspp_df

bspp_df3$draw <- rownames(bspp_df3)

colnames(bspp_df3) <- c(paste0("spp", 1:10), "draw") 
bspp_df3

long_post_bspp <- reshape(
  bspp_df3,
  direction = "long",
  varying = paste0("spp", 1:10),
  v.names = "post_bsp",
  idvar = "draw",
  timevar = "spp"
)
long_post_bspp

ggplot() +
  geom_density(data = data.frame(prior_bsp = prior_bsp),
               aes(x = prior_bsp, colour = "Prior"),
               linewidth = 0.8) +
  geom_density(data = long_post_bspp,
               aes(x = post_bsp, colour = "Posterior", group = spp),
               linewidth = 0.3) +
  labs(title = "priorVSposterior_bsp",
       x = "BSP", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()
ggsave("figures/simData/priorsPredictiveChecks/priorVSposterior_bsp.jpeg", width = 8, height = 6, units = "in", dpi = 300)

# now add row for prior_bsp
prior_bsp <- rnorm(nrow(sigma_df), 0, sigma_df$prior_sigma_bsp)

# convert each parameter to long format
sigma_long_bsp <- data.frame(
  value  = c(sigma_df$post_sigma_bsp, sigma_df$prior_sigma_bsp),
  source = rep(c("post_sigma_bsp", "prior_sigma_bsp"), 
               each = nrow(sigma_df))
)

ggplot(sigma_long_bsp, aes(x = value, color = source)) +
  geom_density(alpha = 0.3) +
  labs(color = "Parameter", fill = "Parameter") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()

# Sigma asp
sigma_long_asp <- data.frame(
  value  = c(sigma_df$post_sigma_asp, sigma_df$prior_sigma_asp),
  source = rep(c("post_sigma_asp", "prior_sigma_asp"), 
               each = nrow(sigma_df))
)

ggplot(sigma_long_asp, aes(x = value, color = source, fill = source)) +
  geom_density(alpha = 0.3) +
  labs(color = "Parameter", fill = "Parameter") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  scale_fill_manual(values = wes_palette("AsteroidCity1")[3:4])+
  theme_minimal()

# Sigma atreeid
sigma_long_atreeid <- data.frame(
  value  = c(sigma_df$post_sigma_atreeid, sigma_df$prior_sigma_atreeid),
  source = rep(c("post_sigma_atreeid", "prior_sigma_atreeid"), 
               each = nrow(sigma_df))
)

ggplot(sigma_long_atreeid, aes(x = value, color = source, fill = source)) +
  geom_density(alpha = 0.3) +
  labs(color = "Parameter", fill = "Parameter") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  scale_fill_manual(values = wes_palette("AsteroidCity1")[3:4])+
  theme_minimal()

# sigma_y
sigma_long_atreeid <- data.frame(
  value  = c(sigma_df$post_sigma_atreeid, sigma_df$prior_sigma_y),
  source = rep(c("post_sigma_atreeid", "prior_sigma_atreeid"), 
               each = nrow(sigma_df))
)

ggplot(sigma_long_atreeid, aes(x = value, color = source, fill = source)) +
  geom_density(alpha = 0.3) +
  labs(color = "Parameter", fill = "Parameter") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  scale_fill_manual(values = wes_palette("AsteroidCity1")[3:4])+
  theme_minimal()
