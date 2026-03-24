# comparison zscored vs non zscored

# CRD 24 March 2026
# comparison of results that are zscored vs non zscored

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 
options(mc.cores = parallel::detectCores())
options(digits = 3)

# Load library 
library(rstan)

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
source('rcode/utilExtractParam.R')

emp <- read.csv("output/empiricalDataMAIN.csv")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Most restricted amount of data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit model GDD
emp2 <- emp[!is.na(emp$pgsGDD5) & !is.na(emp$lengthMM),]

# transform my groups to numeric values
emp2$spp_num <- match(emp2$latbi, unique(emp2$latbi))
emp2$treeid_num <- match(emp2$id, unique(emp2$id))

# transform data in vectors for GDD
y <- emp2$lengthMM # ring width in mm
N <- nrow(emp2)
Nspp <- length(unique(emp2$spp_num))
species <- as.numeric(as.character(emp2$spp_num))
treeid <- as.numeric(emp2$treeid_num)
Ntreeid <- length(unique(treeid))

# different response variables
gdd <- emp2$pgsGDD5 / 200

# Fit model GDD
rstan_options(auto_write = TRUE)
gddmodel <- stan_model("stan/TSmodelGrowthGDD.stan")
fitgdd <- sampling(gddmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gdd"),
                   warmup = 1000, iter=2000, chains=4)
saveRDS(fitgdd, "output/stanOutput/fitGrowthGDD")

# Fit model GDD zscored
rstan_options(auto_write = TRUE)
gdd <- (emp2$pgsGDD5 - mean(emp2$pgsGDD5)) / sd(emp2$pgsGDD5)
gddmodel <- stan_model("stan/TSmodelGrowthGDD.stan")
fitgddz <- sampling(gddmodel, data = c("N","y",
                                      "Nspp","species",
                                      "Ntreeid", "treeid", 
                                      "gdd"),
                   warmup = 1000, iter=2000, chains=4)
saveRDS(fitgddz, "output/stanOutput/fitGrowthGDDZscored")

# GDD restricted (non-zscored)
df_fitgdd <- as.data.frame(fitgdd)

sigma_df_gdd <- extract_params(df_fitgdd, "sigma", "mean", "sigma")
bspp_df_gdd <- extract_params(df_fitgdd, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df_gdd     <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df_gdd <- subset(treeid_df_gdd, !grepl("z|sigma", treeid))
aspp_df_gdd <- extract_params(df_fitgdd, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df_gdd <- subset(treeid_df_gdd, !grepl("prior", treeid))

# GDD zscored
df_fitgdd_z <- as.data.frame(fitgddz)
sigma_df_gdd_z <- extract_params(df_fitgdd_z, "sigma", "mean", "sigma")
bspp_df_gdd_z  <- extract_params(df_fitgdd_z, "bsp", "fit_bspp", "spp", "bsp\\[(\\d+)\\]")
treeid_df_gdd_z   <- extract_params(df_fitgdd_z, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df_gdd_z <- subset(treeid_df_gdd_z, !grepl("z|sigma", treeid))
aspp_df_gdd_z <- extract_params(df_fitgdd_z, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")
treeid_df_gdd_z <- subset(treeid_df_gdd_z, !grepl("prior", treeid))

# Open device
jpeg("figures/empiricalData/GDDZscoredVSRaw.jpeg", width = 6, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2))

# sigma
plot(sigma_df_gdd$mean, sigma_df_gdd_z$mean,
     xlab = "raw GDD", ylab = "z-scored GDD", main = "sigma", type = "n", frame = FALSE,
     ylim = range(c(sigma_df_gdd_z$mean_per25, sigma_df_gdd_z$mean_per75)),
     xlim = range(c(sigma_df_gdd$mean_per25 -0.5 , sigma_df_gdd$mean_per75 + 0.5)))
arrows(x0 = sigma_df_gdd$mean, y0 = sigma_df_gdd_z$mean_per25,
       x1 = sigma_df_gdd$mean, y1 = sigma_df_gdd_z$mean_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = sigma_df_gdd$mean_per25, y0 = sigma_df_gdd_z$mean,
       x1 = sigma_df_gdd$mean_per75, y1 = sigma_df_gdd_z$mean,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(sigma_df_gdd$mean, sigma_df_gdd_z$mean,
       pch = 16, col = "#046C9A", cex = 1.5)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)
text(sigma_df_gdd$mean_per75, sigma_df_gdd_z$mean_per25,
     labels = sigma_df_gdd$sigma, pos = c(3, 3), cex = 0.75)

# bspp
plot(bspp_df_gdd$fit_bspp, bspp_df_gdd_z$fit_bspp,
     xlab = "raw GDD", ylab = "z-scored GDD", main = "bspp", type = "n", frame = FALSE,
     ylim = range(c(bspp_df_gdd_z$fit_bspp_per25, bspp_df_gdd_z$fit_bspp_per75)),
     xlim = range(c(bspp_df_gdd$fit_bspp_per25, bspp_df_gdd$fit_bspp_per75)))
arrows(x0 = bspp_df_gdd$fit_bspp, y0 = bspp_df_gdd_z$fit_bspp_per25,
       x1 = bspp_df_gdd$fit_bspp, y1 = bspp_df_gdd_z$fit_bspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = bspp_df_gdd$fit_bspp_per25, y0 = bspp_df_gdd_z$fit_bspp,
       x1 = bspp_df_gdd$fit_bspp_per75, y1 = bspp_df_gdd_z$fit_bspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(bspp_df_gdd$fit_bspp, bspp_df_gdd_z$fit_bspp,
       pch = 16, col = "#046C9A", cex = 1.5)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)

# aspp
plot(aspp_df_gdd$fit_aspp, aspp_df_gdd_z$fit_aspp,
     xlab = "raw GDD", ylab = "z-scored GDD", main = "aspp", type = "n", frame = FALSE,
     ylim = range(c(aspp_df_gdd_z$fit_aspp_per25, aspp_df_gdd_z$fit_aspp_per75)),
     xlim = range(c(aspp_df_gdd$fit_aspp_per25, aspp_df_gdd$fit_aspp_per75)))
arrows(x0 = aspp_df_gdd$fit_aspp, y0 = aspp_df_gdd_z$fit_aspp_per25,
       x1 = aspp_df_gdd$fit_aspp, y1 = aspp_df_gdd_z$fit_aspp_per75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = aspp_df_gdd$fit_aspp_per25, y0 = aspp_df_gdd_z$fit_aspp,
       x1 = aspp_df_gdd$fit_aspp_per75, y1 = aspp_df_gdd_z$fit_aspp,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(aspp_df_gdd$fit_aspp, aspp_df_gdd_z$fit_aspp,
       pch = 16, col = "#046C9A", cex = 1.5)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)

# atreeid
plot(treeid_df_gdd$fit_atreeid, treeid_df_gdd_z$fit_atreeid,
     xlab = "raw GDD", ylab = "z-scored GDD", main = "atreeid", type = "n", frame = FALSE,
     ylim = range(c(treeid_df_gdd_z$fit_atreeid_per25, treeid_df_gdd_z$fit_atreeid_per75)),
     xlim = range(c(treeid_df_gdd$fit_atreeid_per25, treeid_df_gdd$fit_atreeid_per75)))
arrows(x0 = treeid_df_gdd$fit_atreeid, y0 = treeid_df_gdd_z$fit_atreeid_per25,
       x1 = treeid_df_gdd$fit_atreeid, y1 = treeid_df_gdd_z$fit_atreeid_per75,
       angle = 90, code = 3, length = 0, lwd = 0.8, col = "darkgray")
arrows(x0 = treeid_df_gdd$fit_atreeid_per25, y0 = treeid_df_gdd_z$fit_atreeid,
       x1 = treeid_df_gdd$fit_atreeid_per75, y1 = treeid_df_gdd_z$fit_atreeid,
       angle = 90, code = 3, length = 0, lwd = 0.8, col = "darkgray")
points(treeid_df_gdd$fit_atreeid, treeid_df_gdd_z$fit_atreeid,
       pch = 16, col = "#046C9A", cex = 0.8)
abline(0, 1, lty = 2, col = "#B40F20", lwd = 2)

dev.off()


# check prior vs posterior
# full posterior
columns <- colnames(df_fitgdd_z)[!grepl("prior", colnames(df_fitgdd))]
sigma_df <- df_fitgdd_z[, columns[grepl("sigma", columns)]]
bspp_df <- df_fitgdd_z[, columns[grepl("bsp", columns)]]
treeid_df <- df_fitgdd_z[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df <- df_fitgdd_z[, columns[grepl("aspp", columns)]]

pdf(file = "figures/empiricalData/gddModelPriorVSPosteriorz.pdf", width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(3, 2))

# a
plot(density(df_fitgdd_z[, "a_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     xlab = "a", ylim = c(0,0.5))
lines(density(df_fitgdd_z[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(df_fitgdd_z[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,2))
lines(density(df_fitgdd_z[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(df_fitgdd_z[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0,2))
lines(density(df_fitgdd_z[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(df_fitgdd_z[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", xlim = c(-20, 20), ylim = c(0, 0.15))
for (col in colnames(aspp_df)) {
  lines(density(aspp_df[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(df_fitgdd_z[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 1.8))
for (col in colnames(bspp_df)) {
  lines(density(bspp_df[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
samples <- util$extract_expectand_vals(fitgddz)
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
