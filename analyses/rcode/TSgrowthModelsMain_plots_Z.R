# coringtreespotters model
# CRD 2 April 2025

# plotting z-scored models

# housekeeping
# rm(list=ls())  
# options(stringsAsFactors = FALSE)

if (length(grep("christophe_rouleau-desrochers", getwd())) > 0) {
  setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")
} else if (length(grep("lizzie", getwd())) > 0) {
  setwd("/Users/lizzie/Documents/git/projects/others/christophe/coringtreespotters/analyses")
} else  {
  setwd("/home/crouleau/coringtreespotters/analyses")
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Recover objects from models ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
source("rcode/TSgrowthModelsMain.R")

makeplots <- TRUE

# specify colors
renoir <- c("#17154f", "#2f357c", "#6c5d9e", "#9d9cd5", "#b0799a", "#e48171", 
            "#bf3729", "#e69b00", "#f5bb50", "#ada43b", "#355828")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# GDD posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fit <- readRDS("output/stanOutput/fitGrowthGDDZscored")
df_fit <- as.data.frame(fit)

# full posterior
columns <- colnames(df_fit)
sigma_df <- df_fit[, columns[grepl("sigma", columns)]]
bspp_df <- df_fit[, columns[grepl("bspp", columns)]]
treeid_df <- df_fit[, grepl("treeid", columns) & 
                      !grepl("z", columns) &
                      !grepl("sigma", columns)]
aspp_df <- df_fit[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

# posterior summaries
sigma_df2_z  <- extract_params(df_fit, "sigma", "mean", "sigma")
bspp_df2_z   <- extract_params(df_fit, "bsp", "fit_bspp", "spp", "bspp\\[(\\d+)\\]")
treeid_df2_z <- extract_params(df_fit, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_z <- subset(treeid_df2_z, !grepl("z", treeid) & !grepl("sigma", treeid))
aspp_df2_z   <- extract_params(df_fit, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")

treeid_df2_z$treeid_name <- empts$id[match(treeid_df2_z$treeid, empts$treeid_num)]
aspp_df2_z$spp_name <- empts$latbi[match(aspp_df2_z$spp, empts$spp_num)]
bspp_df2_z$spp_name <- empts$latbi[match(bspp_df2_z$spp, empts$spp_num)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# GSL posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fitgsl <- readRDS("output/stanOutput/fitGrowthGSLZscored")

df_fitgsl <- as.data.frame(fitgsl)

# full posterior
columns <- colnames(df_fitgsl)[!grepl("prior", colnames(df_fitgsl))]
sigma_df_gsl <- df_fitgsl[, columns[grepl("sigma", columns)]]
bspp_df_gsl <- df_fitgsl[, columns[grepl("bsp", columns)]]
treeid_df_gsl <- df_fitgsl[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df_gsl <- df_fitgsl[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)

# posterior summaries
sigma_df2_z_gsl  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2_z_gsl   <- extract_params(df_fitgsl, "bsp", "fit_bspp", 
                                 "spp", "bspp\\[(\\d+)\\]")
treeid_df2_z_gsl <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", 
                                 "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_z_gsl <- subset(treeid_df2_z, !grepl("z|sigma", treeid))
aspp_df2_z_gsl   <- extract_params(df_fitgsl, "aspp", "fit_aspp", 
                                 "spp", "aspp\\[(\\d+)\\]")

treeid_df2_z_gsl$treeid_name <- empts$id[match(treeid_df2_z_gsl$treeid, empts$treeid_num)]
bspp_df2_z_gsl$spp_name <- empts$latbi[match(bspp_df2_z_gsl$spp, empts$spp_num)]
aspp_df2_z_gsl$spp_name <- empts$latbi[match(aspp_df2_z_gsl$spp, empts$spp_num)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# SOS posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fitsos <- readRDS("output/stanOutput/fitGrowthSOSZscored")

df_fitsos <- as.data.frame(fitsos)

# full posterior
columns <- colnames(df_fitsos)[!grepl("prior", colnames(df_fitsos))]
sigma_df_sos <- df_fitsos[, columns[grepl("sigma", columns)]]
bspp_df_sos <- df_fitsos[, columns[grepl("bsp", columns)]]
treeid_df_sos <- df_fitsos[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df_sos <- df_fitsos[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df_sos) <- 1:ncol(bspp_df_sos)
colnames(treeid_df_sos) <- 1:ncol(treeid_df_sos)
colnames(aspp_df_sos) <- 1:ncol(aspp_df_sos)

# posterior summaries
sigma_df2_z_sos  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_z_sos   <- extract_params(df_fitsos, "bsp", "fit_bspp", 
                                 "spp", "bspp\\[(\\d+)\\]")
treeid_df2_z_sos <- extract_params(df_fitsos, "atreeid", "fit_atreeid", 
                                 "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_z_sos <- subset(treeid_df2_z_sos, !grepl("z|sigma", treeid))
aspp_df2_z_sos   <- extract_params(df_fitsos, "aspp", "fit_aspp", 
                                 "spp", "aspp\\[(\\d+)\\]")

treeid_df2_z_sos$treeid_name <- empts$id[match(treeid_df2_z_sos$treeid, empts$treeid_num)]
bspp_df2_z_sos$spp_name <- empts$latbi[match(bspp_df2_z_sos$spp, empts$spp_num)]
aspp_df2_z_sos$spp_name <- empts$latbi[match(aspp_df2_z_sos$spp, empts$spp_num)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# EOS posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fiteos <- readRDS("output/stanOutput/fitGrowthEOSZscored")

df_fiteos <- as.data.frame(fiteos)

# full posterior
columns <- colnames(df_fiteos)[!grepl("prior", colnames(df_fiteos))]
sigma_df_eos <- df_fiteos[, columns[grepl("sigma", columns)]]
bspp_df_eos <- df_fiteos[, columns[grepl("bsp", columns)]]
treeid_df_eos <- df_fiteos[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df_eos <- df_fiteos[, columns[grepl("aspp", columns)]]

# change colnames
colnames(bspp_df_eos) <- 1:ncol(bspp_df_eos)
colnames(treeid_df_eos) <- 1:ncol(treeid_df_eos)
colnames(aspp_df_eos) <- 1:ncol(aspp_df_eos)

# posterior summaries
sigma_df2_z_eos  <- extract_params(df_fiteos, "sigma", "mean", "sigma")
bspp_df2_z_eos   <- extract_params(df_fiteos, "bsp", "fit_bspp", 
                                 "spp", "bspp\\[(\\d+)\\]")
treeid_df2_z_eos <- extract_params(df_fiteos, "atreeid", "fit_atreeid", 
                                 "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_z_eos <- subset(treeid_df2_z_eos, !grepl("z|sigma", treeid))
aspp_df2_z_eos   <- extract_params(df_fiteos, "aspp", "fit_aspp", 
                                 "spp", "aspp\\[(\\d+)\\]")

treeid_df2_z_eos$treeid <- as.numeric(treeid_df2_z_eos$treeid)
treeid_df2_z_eos$treeid_name <- empts$id[match(treeid_df2_z_eos$treeid, empts$treeid_num)]
bspp_df2_z_eos$spp_name <- empts$latbi[match(bspp_df2_z_eos$spp, empts$spp_num)]
aspp_df2_z_eos$spp_name <- empts$latbi[match(aspp_df2_z_eos$spp, empts$spp_num)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Define objects used throughout the models ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
n_spp <- length(unique(empts$latbi))
y_pos <- rev(1:n_spp)

# get the spp and site identities for each tree id
treeid_spp <- unique(empts[, c("treeid_num", "spp_num", "id", "latbi")])

# get a vector for each treeid for each species
spp1vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 1]
spp2vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 2]
spp3vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 3]
spp4vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 4]
spp5vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 5]
spp6vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 6]
spp7vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 7]
spp8vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 8]
spp9vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 9]
spp10vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 10]
spp11vec <- treeid_spp$treeid_num[treeid_spp$spp_num == 11]

spp_list <- list(
  "1" = spp1vec,
  "2" = spp2vec,
  "3" = spp3vec,
  "4" = spp4vec,
  "5" = spp5vec,
  "6" = spp6vec,
  "7" = spp7vec,
  "8" = spp8vec,
  "9" = spp9vec,
  "10" = spp10vec,
  "11" = spp11vec
)

sppvecnum <- 1:11
sppvecname <- unique(treeid_spp$latbi)

# for mu plots
species_order <- rev(unique(empts$latbi))

colscommon <- c(
  "Red maple"           = renoir[1],
  "Sugar maple"         = renoir[2],
  "Yellow buckeye"      = renoir[3],
  "Yellow birch"        = renoir[4],
  "River birch"         = renoir[5],
  "Pignut hickory"      = renoir[6],
  "Shagbark hickory"    = renoir[7],
  "Eastern cottonwood"  = renoir[8],
  "White oak"           = renoir[9],
  "Northern red oak"    = renoir[10],
  "American basswood"   = renoir[11]
)

colslatbi <- c(
  "Acer rubrum"           = renoir[1],
  "Acer saccharum"        = renoir[2],
  "Aesculus flava"        = renoir[3],
  "Betula alleghaniensis" = renoir[4],
  "Betula nigra"          = renoir[5],
  "Carya glabra"          = renoir[6],
  "Carya ovata"           = renoir[7],
  "Populus deltoides"     = renoir[8],
  "Quercus alba"          = renoir[9],
  "Quercus rubra"         = renoir[10],
  "Tilia americana"       = renoir[11]
)

# vector of treeids
subyvec <- vector()
for (i in 1:length(unique(empts$treeid_num))) {
  subyvec[i] <- paste("atreeid", "[",i,"]", sep = "")  
}

if(makeplots) {

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Mu plots #####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### bsp ###### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
jpeg(file = "figures/growthModelsMain/zscored/muALLbspp.jpeg",
     width = 1800, height = 2500, res = 300)

layout(matrix(c(
  1, 5,
  2, 5,
  3, 5,
  4, 5
), nrow = 4, byrow = TRUE),
widths = c(0.7, 0.4))

# set margins throught
custommar <- c(4, 2, 2, 1)

# Row 1: GDD
par(mar = custommar)
plot(bspp_df2_z$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5), 
     xlab = "GDD slope standardized effect size", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_z$fit_bspp_per5,  y_pos, bspp_df2_z$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_z$fit_bspp_per25, y_pos, bspp_df2_z$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("(a) Growing degree days", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 2: GSL
par(mar = custommar)
plot(bspp_df2_z_gsl$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "GSL slope standardized effect size", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_z_gsl$fit_bspp_per5,  y_pos, bspp_df2_z_gsl$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_z_gsl$fit_bspp_per25, y_pos, bspp_df2_z_gsl$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("(b) Growing season length", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 3: SOS
par(mar = custommar)
plot(bspp_df2_z_sos$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "SOS slope standardized effect size", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_z_sos$fit_bspp_per5,  y_pos, bspp_df2_z_sos$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_z_sos$fit_bspp_per25, y_pos, bspp_df2_z_sos$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("(c) Start of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 4: EOS
par(mar = custommar)
plot(bspp_df2_z_eos$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "EOS slope standardized effect size", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_z_eos$fit_bspp_per5,  y_pos, bspp_df2_z_eos$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_z_eos$fit_bspp_per25, y_pos, bspp_df2_z_eos$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("(d) End of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Slot 5: species legend
par(mar = c(1, 1, 1, 1))
plot.new()
legend("center",
       legend = sapply(unique(bspp_df2_z$spp_name), 
                       function(x) parse(text = paste0("italic('", x, "')"))),
       col    = unique(colslatbi),
       pch    = 16, pt.cex = 1.5, bty = "n", cex = 1.2,
       title  = "Species", title.font = 2)

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
###### asp ######
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
jpeg(file = "figures/growthModelsMain/zscored/muALLaspp.jpeg",
     width = 1800, height = 2200, res = 300)

layout(matrix(c(
  1, 5,
  2, 5,
  3, 5,
  4, 5
), nrow = 4, byrow = TRUE),
widths = c(0.7, 0.4))

# Row 1: GDD
par(mar = custommar)
plot(aspp_df2_z$fit_aspp, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_z$fit_aspp_per5,  y_pos, aspp_df2_z$fit_aspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(aspp_df2_z$fit_aspp_per25, y_pos, aspp_df2_z$fit_aspp_per75, y_pos,
         col = colslatbi, lwd = 3)
abline(v = 0, lty = 2, col = "black")
mtext("Growing degree days", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 2: GSL
par(mar = custommar)
plot(aspp_df2_z_gsl$fit_aspp, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_z_gsl$fit_aspp_per5,  y_pos, aspp_df2_z_gsl$fit_aspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(aspp_df2_z_gsl$fit_aspp_per25, y_pos, aspp_df2_z_gsl$fit_aspp_per75, y_pos,
         col = colslatbi, lwd = 3)
abline(v = 0, lty = 2, col = "black")
mtext("Growing season length", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 3: SOS
par(mar = custommar)
plot(aspp_df2_z_sos$fit_aspp, y_pos,
     xlim = c(-10, 10),ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "", 
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_z_sos$fit_aspp_per5,  y_pos, aspp_df2_z_sos$fit_aspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(aspp_df2_z_sos$fit_aspp_per25, y_pos, aspp_df2_z_sos$fit_aspp_per75, y_pos,
         col = colslatbi, lwd = 3)
abline(v = 0, lty = 2, col = "black")
mtext("Start of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 4: EOS
par(mar = custommar)
plot(aspp_df2_z_eos$fit_aspp, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5), 
     xlab = "Log ring width intercept values (mm)", ylab = "", 
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_z_eos$fit_aspp_per5,  y_pos, aspp_df2_z_eos$fit_aspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(aspp_df2_z_eos$fit_aspp_per25, y_pos, aspp_df2_z_eos$fit_aspp_per75, y_pos,
         col = colslatbi, lwd = 3)
abline(v = 0, lty = 2, col = "black")
mtext("End of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Slot 5: species legend
par(mar = c(1, 1, 1, 1))
plot.new()
legend("center",
       legend = sapply(unique(aspp_df2_z$spp_name), 
                       function(x) parse(text = paste0("italic('", x, "')"))),
       col    = unique(colslatbi),
       pch    = 16, pt.cex = 1.5, bty = "n", cex = 1.2,
       title  = "Species", title.font = 2)

dev.off()


}