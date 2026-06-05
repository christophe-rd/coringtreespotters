# Coringtreespotters model of previous years condition on following year's growth
# CRD 22 May 2026

# Goal: check if the condition of the previous year on current year's growth

# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
options(max.print = 150) 
options(mc.cores = parallel::detectCores())
options(digits = 3)

# Load library 
library(ggplot2)
library(rstan)
library(wesanderson)

if (length(grep("christophe_rouleau-desrochers", getwd())) > 0) {
  setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")
} else if (length(grep("lizzie", getwd())) > 0) {
  setwd("/Users/lizzie/Documents/git/projects/others/christophe/coringtreespotters/analyses")
} else  {
  setwd("/home/crouleau/coringtreespotters/analyses")
}

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)
# my function to extract parameters
source('/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/rcode/tools.R')

empts <- read.csv("output/empiricalDataMAIN.csv")
# rw <- read.csv("output/wildchrokieRingWidth.csv")
gdd <- read.csv("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/output/gddByYear.csv")


empts$latbi[empts$latbi == "Acer rubrum"]           <- "A. rubrum"
empts$latbi[empts$latbi == "Acer saccharum"]        <- "A. saccharum"
empts$latbi[empts$latbi == "Aesculus flava"]        <- "Ae. flava"
empts$latbi[empts$latbi == "Betula alleghaniensis"] <- "B. alleghaniensis"
empts$latbi[empts$latbi == "Betula nigra"]          <- "B. nigra"
empts$latbi[empts$latbi == "Carya glabra"]          <- "C. glabra"
empts$latbi[empts$latbi == "Carya ovata"]           <- "C. ovata"
empts$latbi[empts$latbi == "Populus deltoides"]     <- "P. deltoides"
empts$latbi[empts$latbi == "Quercus alba"]          <- "Q. alba"
empts$latbi[empts$latbi == "Quercus rubra"]         <- "Q. rubra"
empts$latbi[empts$latbi == "Tilia americana"]       <- "T. americana"


runmodels <- F
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Most restricted amount of data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# calculate gdd by year between 1st May (121) and 1st august (244)  
gddsub <- subset(gdd, doy >121 & doy < 244)
agg <- aggregate(GDD_5 ~ year, gddsub, FUN = min)
gddsub$mingdd <- agg$GDD_5[match(gddsub$year, agg$year)]
gddsub$diff <- gddsub$GDD_5 - gddsub$mingdd
agg2 <- aggregate(diff ~ year, gddsub, FUN = max)

# fit only two years of real data
years <- 2015:2024
# add a year diff index 
empts$yeardiff <- NA
for (i in years) {
  empts$yeardiff[empts$year == i] <- empts$year[empts$year == i] - 1
}

# remove NAs
empts$idyear <- paste0(empts$id, "_", empts$year)
empts$idyearprvs <- paste0(empts$id, "_", empts$yeardiff)
empts$gddprvsyr <- empts$pgsGDD5[match(empts$idyearprvs, empts$idyear)]
empts[, c("id", "year", "pgsGDD5", "gddprvsyr")]

empts <- empts[!is.na(empts$pgsGDD5) & !is.na(empts$gddprvsyr)
               & !is.na(empts$lengthMM),]
nrow(empts)
# transform data in vectors for GDD
# data <- list(
#   y = log(rw$lengthMM),
#   N = nrow(rw),
#   Nspp = length(unique(rw$spp_num)),
#   Nsite = length(unique(rw$site_num)),
#   site = as.numeric(as.character(rw$site_num)),
#   species = as.numeric(as.character(rw$spp_num)),
#   treeid = as.numeric(rw$treeid_num),
#   Ntreeid = length(unique(as.numeric(rw$treeid_num))),
#   gdd = rw$gddcurrentyr - 1800,
#   gddyr = rw$gddpreviousyr -1800
#   # gdd = (rw$gddcurrentyr - mean(rw$gddcurrentyr)) / sd(rw$gddcurrentyr),
#   # gddyr = (rw$gddpreviousyr - mean(rw$gddpreviousyr)) / sd(rw$gddpreviousyr)
# )

empts$spp_num <- match(empts$latbi, unique(empts$latbi))
empts$treeid_num <- match(empts$id, unique(empts$id))

data <- list(
  y = log(empts$lengthMM),
  N = nrow(empts),
  Nspp = length(unique(empts$spp_num)),
  species = as.numeric(as.character(empts$spp_num)),
  treeid = as.numeric(empts$treeid_num),
  Ntreeid = length(unique(as.numeric(empts$treeid_num))),
  gdd = empts$pgsGDD5/176,
  gddyr = empts$gddprvsyr / 176
  # gdd = (empts$gddcurrentyr - mean(empts$gddcurrentyr)) / sd(empts$gddcurrentyr),
  # gddyr = (empts$gddpreviousyr - mean(empts$gddpreviousyr)) / sd(empts$gddpreviousyr)
)
data


# Fit model GDD 
if(runmodels) {
gddmodel <- stan_model("stan/TSmodelGrowthPreviousYear.stan")
fit <- sampling(gddmodel, data = data, iter = 2000, chains = 4)
saveRDS(fit, "output/stanOutput/fitGrowthPreviousYear")
fit <- readRDS("output/stanOutput/fitGrowthPreviousYear")
diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)
samples <- util$extract_expectand_vals(fit)


# Diagnostics ####
# check aspp
aspp <- paste0("aspp[", 1:4, "]")
util$plot_div_pairs(aspp, aspp, samples, diagnostics)

# check bspp
bspp <- paste0("bsp[", 1:4, "]")
util$plot_div_pairs(bspp, bspp, samples, diagnostics)

# check bsppyr
bsppyr <- paste0("bspyr[", 1:4, "]")
util$plot_div_pairs(bsppyr, bsppyr, samples, diagnostics)

library(ggplot2)
ggplot(empts) + 
  geom_smooth(aes(x = gddprvsyr, y = lengthMM)) + 
  geom_point(aes(x = gddprvsyr, y = lengthMM)) 

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# fit <- readRDS("output/stanOutput/fitGrowthPreviousYear")
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


##### Plot posterior vs priors for gdd fit #####
# pdf(file = "figures/empiricalData/gddModelPriorVSPosteriorPrvsYr.pdf", width = 8, height = 10)
df_fit <- as.data.frame(fit)

columns <- colnames(df_fit)[!grepl("prior", colnames(df_fit))]
sigma_df <- df_fit[, columns[grepl("sigma", columns)]]
bspp_df <- df_fit[, columns[grepl("bsp", columns) & !grepl("yr", columns)]]
bsppyr_df <- df_fit[, columns[grepl("bspyr", columns)]]
# treeid_df <- df_fit[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fit[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(bsppyr_df) <- 1:ncol(bsppyr_df)
# colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

jpeg("figures/growthPreviousYearModel/gddModelPriorVSPosteriorPrvsYr.jpeg", 
     width =2400, height = 3600, res =300)
pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(2, 3))

# a
plot(density(df_fit[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0,0.1))
lines(density(df_fit[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# # sigma_atreeid
# plot(density(df_fit[, "sigma_atreeid_prior"]), 
#      col = pal[1], lwd = 2, 
#      main = "priorVSposterior_sigma_atreeid", 
#      xlab = "sigma_atreeid", ylim = c(0,2))
# lines(density(df_fit[, "sigma_atreeid"]), col = pal[2], lwd = 2)
# legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

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
# Compare bspp vs bsppyr ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fit <- readRDS("output/stanOutput/fitGrowthPreviousYear")
df_fit <- as.data.frame(fit)
# posterior summaries
bspp_df2_current <- extract_params(df_fit, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
bspp_df2_current <- subset(bspp_df2_current, spp %in% bspp_df2_current$spp[!grepl("yr", bspp_df2_current$spp)])
bspp_df2_previous <- extract_params(df_fit, "bspyr", "fit_bspp", "spp", "bspyr\\[(\\d+)\\]")

bspp_df2_current$spp_name <- empts$latbi[match(bspp_df2_current$spp, empts$spp)]
bspp_df2_previous$spp_name <- empts$latbi[match(bspp_df2_previous$spp, empts$spp)]

jpeg("figures/growthPreviousYearModel/bsppCurrentVSpreviousYR.jpeg", width = 6, height = 9, units = "in", res = 300)
par(mfrow = c(2,1), mar = c(4, 2, 2, 1))
n_spp <- length(unique(bspp_df2_current$spp))
y_pos <- rev(1:11)

# Current year
plot(bspp_df2_current$mean, y_pos,
     xlim = c(-0.4, 0.6), ylim = c(0.5, n_spp + 0.5), 
     xlab = "slope current year", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_current$p5,  y_pos, bspp_df2_current$p95, y_pos,
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_current$p25, y_pos, bspp_df2_current$p75, y_pos,
         col = tscolslatbi, lwd = 3)
mtext("(a) Current year", side = 3, adj = 0, font = 2, cex = 1.3)

legend("topright",
       legend = bspp_df2_previous$spp_name,
       col    = tscolslatbi[bspp_df2_previous$spp_name],
       pch = 16, pt.cex = 1.2, title = "Species", bty = "n")

# Row 2: Previous year
plot(bspp_df2_previous$mean, y_pos,
     xlim = c(-0.4, 0.6), ylim = c(0.5, n_spp + 0.5),
     xlab = "slope previous year", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_previous$p5,  y_pos, bspp_df2_previous$p95, y_pos,
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_previous$p25, y_pos, bspp_df2_previous$p75, y_pos,
         col = tscolslatbi, lwd = 3)
mtext("(b) Previous year", side = 3, adj = 0, font = 2, cex = 1.3)
dev.off()

##### Compare model output with and without slope on prvs year #####
sigma_df2_yr  <- extract_params(df_fit, "sigma", "mean", "sigma")
bspp_df2_yr   <- extract_params(df_fit, "bsp", "fit_bspp", 
                                   "spp", "bsp\\[(\\d+)\\]")
bspp_df2_yr <- subset(bspp_df2_yr, !grepl("yr", spp))
treeid_df2_yr <- extract_params(df_fit, "atreeid", "fit_atreeid", 
                                   "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_yr <- subset(treeid_df2_yr, !grepl("z|sigma", treeid))
aspp_df2_yr   <- extract_params(df_fit, "aspp", "fit_aspp", 
                                   "spp", "aspp\\[(\\d+)\\]")

# Recover fitgdd without partial pooling
fitgdd <- readRDS("output/stanOutput/fitGrowthGDD")

##### Recover parameters for parameters without bspp on previouys year #####
df_fitgdd <- as.data.frame(fitgdd)

sigma_df2  <- extract_params(df_fitgdd, "sigma", "mean", "sigma")
bspp_df2   <- extract_params(df_fitgdd, "bsp", "fit_bspp", 
                             "spp", "bsp\\[(\\d+)\\]")
treeid_df2 <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", 
                             "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2 <- subset(treeid_df2, !grepl("z|sigma", treeid))
aspp_df2   <- extract_params(df_fitgdd, "aspp", "fit_aspp", 
                             "spp", "aspp\\[(\\d+)\\]")

# Open device
jpeg("figures/growthPreviousYearModel/bsppVSbspyr.jpeg", width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2,2), oma = c(0, 2, 0, 0))

# sigma
plot(sigma_df2_yr$mean, sigma_df2$mean,
     xlab = "with bsp on prvs year", ylab = "no bsp on prvs year", main = "sigmas", type = "n", frame = FALSE,
     ylim = range(c(sigma_df2$p25, sigma_df2$p75)),
     xlim = range(c(sigma_df2_yr$p25, sigma_df2_yr$p75+0.2)))
arrows(x0 = sigma_df2_yr$mean, y0 = sigma_df2$p25,
       x1 = sigma_df2_yr$mean, y1 = sigma_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df2_yr$p25, y0 = sigma_df2$mean,
       x1 = sigma_df2_yr$p75, y1 = sigma_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df2_yr$mean, sigma_df2$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)
points(sigma_df2_yr$mean, sigma_df2$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
text(sigma_df2_yr$p75, sigma_df2$p25, labels = sigma_df2_yr$sigma, pos = c(3,3), cex = 0.75)

# bspp
plot(bspp_df2_yr$mean, bspp_df2$mean,
     xlab = "with bsp on prvs year", ylab = "no bsp on prvs year", main = "bspp", type = "n", frame = FALSE,
     ylim = range(c(bspp_df2$p25, bspp_df2$p75)),
     xlim = range(c(bspp_df2_yr$p25, bspp_df2_yr$p75)))
arrows(x0 = bspp_df2_yr$mean, y0 = bspp_df2$p25,
       x1 = bspp_df2_yr$mean, y1 = bspp_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df2_yr$p25, y0 = bspp_df2$mean,
       x1 = bspp_df2_yr$p75, y1 = bspp_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df2_yr$mean, bspp_df2$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# aspp
plot(aspp_df2_yr$mean, aspp_df2$mean,
     xlab = "with bsp on prvs year", ylab = "no bsp on prvs year", main = "aspp", type = "n", frame = FALSE,
     ylim = range(c(aspp_df2$p25, aspp_df2$p75)),
     xlim = range(c(aspp_df2_yr$p25, aspp_df2_yr$p75)))
arrows(x0 = aspp_df2_yr$mean, y0 = aspp_df2$p25,
       x1 = aspp_df2_yr$mean, y1 = aspp_df2$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = aspp_df2_yr$p25, y0 = aspp_df2$mean,
       x1 = aspp_df2_yr$p75, y1 = aspp_df2$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(aspp_df2_yr$mean, aspp_df2$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

# # atreeid
treeid_key <- unique(empts[, c("id", "treeid_num", "latbi")])
treeid_key <- treeid_key[order(treeid_key$treeid_num), ]
nrow(treeid_key)

# whole dataset
empts$spp_num    <- match(empts$latbi,    unique(empts$latbi))
empts$treeid_num <- match(empts$treeid, unique(empts$treeid))
treeid_key_gdd <- unique(empts[, c("id", "treeid_num", "latbi")])
treeid_key_gdd <- treeid_key_gdd[order(treeid_key_gdd$treeid_num), ]
length(unique(empts$treeid))

common_trees <- intersect(treeid_key$treeid, treeid_key_gdd$treeid)

idx_gdd   <- treeid_key_gdd$treeid_num[match(common_trees, treeid_key_gdd$treeid)]
idx_yr <- treeid_key$treeid_num[match(common_trees, treeid_key$treeid)]

treeid_df2_matched       <- treeid_df2[match(idx_gdd, as.integer(treeid_df2$treeid)), ]
treeid_df2_yr_matched <- treeid_df2_yr[match(idx_yr, as.integer(treeid_df2_yr$treeid)), ]

plot(treeid_df2_yr_matched$mean, treeid_df2_matched$mean,
     xlab = "with bsp on prvs year", ylab = "no bsp on prvs year", main = "atreeid", type = "n", frame = FALSE,
     ylim = range(c(treeid_df2_matched$p25, treeid_df2_matched$p75)),
     xlim = range(c(treeid_df2_yr_matched$p25, treeid_df2_yr_matched$p75)))
arrows(x0 = treeid_df2_yr_matched$mean, y0 = treeid_df2_matched$p25,
       x1 = treeid_df2_yr_matched$mean, y1 = treeid_df2_matched$p75,
       angle = 90, code = 3, length = 0, lwd = 1, col = "darkgray")
arrows(x0 = treeid_df2_yr_matched$p25, y0 = treeid_df2_matched$mean,
       x1 = treeid_df2_yr_matched$p75, y1 = treeid_df2_matched$mean,
       angle = 90, code = 3, length = 0, lwd = 1, col = "darkgray")
points(treeid_df2_yr_matched$mean, treeid_df2_matched$mean, pch = 16, col = "#0a6a3c", cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)

dev.off()


