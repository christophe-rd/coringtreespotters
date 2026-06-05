# coringtreespotters model
# CRD 6 February 2025

# Goal: Plot model TS

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

makeplots <- T
runzscore <- F

# acronym latbi
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

# Load parameter summaries generated in growthModelsMain.R ####
sigma_df2_ts_gdd  <- read.csv("output/GM_GDDparam_sigma.csv")
bspp_df2_ts_gdd   <- read.csv("output/GM_GDDparam_bspp.csv")
treeid_df2_ts_gdd <- read.csv("output/GM_GDDparam_treeid.csv")
aspp_df2_ts_gdd   <- read.csv("output/GM_GDDparam_aspp.csv")
ayear_df2_ts_gdd   <- read.csv("output/GM_GDDparam_ayear.csv")

treeid_df2_ts_gdd$id <- as.numeric(treeid_df2_ts_gdd$id)  
treeid_df2_ts_gdd$treeid_name <- empts$id[match(treeid_df2_ts_gdd$id, empts$treeid_num)]
bspp_df2_ts_gdd$spp_name <- empts$latbi[match(bspp_df2_ts_gdd$spp, empts$spp_num)]
aspp_df2_ts_gdd$spp_name <- empts$latbi[match(aspp_df2_ts_gdd$spp, empts$spp_num)]
ayear_df2_ts_gdd$year_name <- empts$year[match(ayear_df2_ts_gdd$year, empts$year_num)]

# GSL
sigma_df2_ts_gsl  <- read.csv("output/GM_GSLparam_sigma.csv")
bspp_df2_ts_gsl   <- read.csv("output/GM_GSLparam_bspp.csv")
treeid_df2_ts_gsl <- read.csv("output/GM_GSLparam_treeid.csv")
aspp_df2_ts_gsl   <- read.csv("output/GM_GSLparam_aspp.csv")
ayear_df2_ts_gsl  <- read.csv("output/GM_GSLparam_ayear.csv")

treeid_df2_ts_gsl$id <- as.numeric(treeid_df2_ts_gsl$id)
treeid_df2_ts_gsl$treeid_name <- empts$id[match(treeid_df2_ts_gsl$id, empts$treeid_num)]
bspp_df2_ts_gsl$spp_name <- empts$latbi[match(bspp_df2_ts_gsl$spp, empts$spp_num)]
aspp_df2_ts_gsl$spp_name <- empts$latbi[match(aspp_df2_ts_gsl$spp, empts$spp_num)]
ayear_df2_ts_gsl$year_name <- empts$year[match(ayear_df2_ts_gsl$year, empts$year_num)]

# SOS 
sigma_df2_ts_sos  <- read.csv("output/GM_SOSparam_sigma.csv")
bspp_df2_ts_sos   <- read.csv("output/GM_SOSparam_bspp.csv")
treeid_df2_ts_sos <- read.csv("output/GM_SOSparam_treeid.csv")
aspp_df2_ts_sos   <- read.csv("output/GM_SOSparam_aspp.csv")
ayear_df2_ts_sos      <- read.csv("output/GM_SOSparam_ayear.csv")

treeid_df2_ts_sos$id <- as.numeric(treeid_df2_ts_sos$id)
treeid_df2_ts_sos$treeid_name <- empts$id[match(treeid_df2_ts_sos$id, empts$treeid_num)]
bspp_df2_ts_sos$spp_name <- empts$latbi[match(bspp_df2_ts_sos$spp, empts$spp_num)]
aspp_df2_ts_sos$spp_name <- empts$latbi[match(aspp_df2_ts_sos$spp, empts$spp_num)]
ayear_df2_ts_sos$year_name <- empts$year[match(ayear_df2_ts_sos$year, empts$year_num)]

# EOS
sigma_df2_ts_eos  <- read.csv("output/GM_EOSparam_sigma.csv")
bspp_df2_ts_eos   <- read.csv("output/GM_EOSparam_bspp.csv")
treeid_df2_ts_eos <- read.csv("output/GM_EOSparam_treeid.csv")
aspp_df2_ts_eos   <- read.csv("output/GM_EOSparam_aspp.csv")
ayear_df2_ts_eos  <- read.csv("output/GM_EOSparam_ayear.csv")

treeid_df2_ts_eos$id <- as.numeric(treeid_df2_ts_eos$id)
treeid_df2_ts_eos$treeid_name <- empts$id[match(treeid_df2_ts_eos$id, empts$treeid_num)]
bspp_df2_ts_eos$spp_name <- empts$latbi[match(bspp_df2_ts_eos$spp, empts$spp_num)]
aspp_df2_ts_eos$spp_name <- empts$latbi[match(aspp_df2_ts_eos$spp, empts$spp_num)]
ayear_df2_ts_eos$year_name <- empts$year[match(ayear_df2_ts_eos$year, empts$year_num)]

if(makeplots) {
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# GDD posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fitgdd <- readRDS("output/stanOutput/fitGrowthGDD")
df_fitgdd <- as.data.frame(fitgdd)

# full posterior
columns <- colnames(df_fitgdd)[!grepl("prior", colnames(df_fitgdd))]
sigma_df <- df_fitgdd[, columns[grepl("sigma", columns)]]
bspp_df <- df_fitgdd[, columns[grepl("bspp", columns)]]
treeid_df <- df_fitgdd[, grepl("treeid", columns) & 
                      !grepl("z", columns) &
                      !grepl("sigma", columns)]
aspp_df <- df_fitgdd[, columns[grepl("aspp", columns)]]
ayear_df <- df_fitgdd[, columns[grepl("ayear", columns) & 
                                  !grepl("mean", columns)]]

# change colnames
colnames(bspp_df) <- 1:ncol(bspp_df)
colnames(treeid_df) <- 1:ncol(treeid_df)
colnames(aspp_df) <- 1:ncol(aspp_df)
colnames(ayear_df) <- 1:ncol(ayear_df)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# GSL posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fitgsl <- readRDS("output/stanOutput/fitGrowthGSL")

df_fitgsl <- as.data.frame(fitgsl)

# full posterior
columns <- colnames(df_fitgsl)[!grepl("prior", colnames(df_fitgsl))]
sigma_df_gsl <- df_fitgsl[, columns[grepl("sigma", columns)]]
bspp_df_gsl <- df_fitgsl[, columns[grepl("bsp", columns)]]
treeid_df_gsl <- df_fitgsl[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df_gsl <- df_fitgsl[, columns[grepl("aspp", columns)]]
ayear_df_gsl <- df_fitgsl[, columns[grepl("ayear", columns) & 
                                  !grepl("mean", columns)]]

# change colnames
colnames(bspp_df_gsl) <- 1:ncol(bspp_df_gsl)
colnames(treeid_df_gsl) <- 1:ncol(treeid_df_gsl)
colnames(aspp_df_gsl) <- 1:ncol(aspp_df_gsl)
colnames(ayear_df_gsl) <- 1:ncol(ayear_df_gsl)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# SOS posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fitsos <- readRDS("output/stanOutput/fitGrowthSOS")

df_fitsos <- as.data.frame(fitsos)

# full posterior
columns <- colnames(df_fitsos)[!grepl("prior", colnames(df_fitsos))]
sigma_df_sos <- df_fitsos[, columns[grepl("sigma", columns)]]
bspp_df_sos <- df_fitsos[, columns[grepl("bsp", columns)]]
treeid_df_sos <- df_fitsos[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df_sos <- df_fitsos[, columns[grepl("aspp", columns)]]
ayear_df_sos <- df_fitsos[, columns[grepl("ayear", columns) & 
                                  !grepl("mean", columns)]]

# change colnames
colnames(bspp_df_sos) <- 1:ncol(bspp_df_sos)
colnames(treeid_df_sos) <- 1:ncol(treeid_df_sos)
colnames(aspp_df_sos) <- 1:ncol(aspp_df_sos)
colnames(ayear_df_sos) <- 1:ncol(ayear_df_sos)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# EOS posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fiteos <- readRDS("output/stanOutput/fitGrowthEOS")

df_fiteos <- as.data.frame(fiteos)

# full posterior
columns <- colnames(df_fiteos)[!grepl("prior", colnames(df_fiteos))]
sigma_df_eos <- df_fiteos[, columns[grepl("sigma", columns)]]
bspp_df_eos <- df_fiteos[, columns[grepl("bsp", columns)]]
treeid_df_eos <- df_fiteos[, grepl("treeid", columns) & !grepl("z|sigma", columns)]
aspp_df_eos <- df_fiteos[, columns[grepl("aspp", columns)]]
ayear_df_eos <- df_fiteos[, columns[grepl("ayear", columns) & 
                                  !grepl("mean", columns)]]

# change colnames
colnames(bspp_df_eos) <- 1:ncol(bspp_df_eos)
colnames(treeid_df_eos) <- 1:ncol(treeid_df_eos)
colnames(aspp_df_eos) <- 1:ncol(aspp_df_eos)
colnames(ayear_df_eos) <- 1:ncol(ayear_df_eos)

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

treeidvecnum <- unique(treeid_spp$treeid_num)
treeidvecname <- treeid_spp$id

# for mu plots
species_order <- unique(empts$latbi)

# vector of treeids
subyvec <- vector()
for (i in 1:length(unique(empts$treeid_num))) {
  subyvec[i] <- paste("atreeid", "[",i,"]", sep = "")  
}

# axis sizes
mysizeaxis <- 1.1
mysizelab <- 1.2

if(makeplots) {

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot lines with quantiles ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GDD: Prep posterior reconstruction #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# recover full intercepts for each tree id
fullintercept_cols <- grep("^fullintercept", colnames(df_fitgdd), value = TRUE)
fullintercept <- df_fitgdd[, fullintercept_cols]
colnames(fullintercept) <- 1:ncol(fullintercept)

# recover each slope
treeid_slope_cols <- grep("^treeid_slope", colnames(df_fitgdd), value = TRUE)
treeid_bspp <- df_fitgdd[, treeid_slope_cols] / tsgddscale
colnames(treeid_bspp) <- 1:ncol(treeid_bspp)

# recover sim ypred for each gdd X tree id for each iteration
# y_post_array is [n_draws, Ngddseq, Ntreeid]
y_post_array <- extract(fitgdd, "y_post")$y_post

# posterior array for each species [n_draws, Ngddseq, Nspp]
spp_post_array <- extract(fitgdd, "spp_post")$spp_post

gddseq <- dgdd$gddseq

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GDD: per treeid, facet #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# PDF output
pdf(file = "figures/growthModelsMain/TSgrowthModelSlopesperTreeid.pdf", width = 10, height = 8)
# Layout: 2 rows x 2 columns per page
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

for (i in seq_along(treeidvecnum)) { # i = 1
  tree_col <- as.character(treeidvecnum[i])
  tree_col_name <- as.character(treeidvecname[i])
  
  # y_post_array is [n_draws, Ngddseq, Ntreeid]
  # slice out this tree, transpose to [Ngddseq, n_draws]
  y_post <- t(y_post_array[, , i])
  
  # color line by spp
  tree_id_num <- as.integer(tree_col)
  
  # index the dots per treeid
  emp_treeid <- empts[empts$treeid_num == tree_id_num, ]
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # empty plot first
  plot(empts$pgsGDD5, dgdd$y, type = "n",
       ylim = range(c(emp_treeid$loglength, y_low, y_high), na.rm = TRUE),
       xlab = "Primary growing season GDD", ylab = "log(ring width)",
       frame = FALSE,
       main = tree_col_name)
  
  spp_id <- treeid_spp$latbi[match(tree_id_num, treeid_spp$treeid_num)]
  line_col <- tscolslatbi[spp_id]
  
  # shaded interval
  polygon(c(gddseq, rev(gddseq)),
          c(y_low, rev(y_high)),
          col = adjustcolor(line_col, alpha.f = 0.3),
          border = NA)
  
  # mean line
  lines(gddseq, y_mean, col = line_col, lwd = 2)
  
  points(emp_treeid$pgsGDD5, emp_treeid$loglength,
         pch = 16, cex = 2, col = line_col)
}
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GDD: per Spp, facet, shaped by treeid #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# jpeg output
jpeg(filename = "figures/growthModelsMain/TSgrowthModelSlopesperSppFacetGDDtree.jpeg",
     width = 2400, height = 2400, res = 300)
# Layout: 2 rows x 2 columns per page
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

treeidsymbol <- c(15, 16, 17, 8, 9)

for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_name <- as.character(sppvecname[i])
  
  # subset empirical data correctly
  emp_spp <- empts[empts$latbi == spp_name, ]
  
  # spp_post_array is [n_draws, Ngddseq, Nspp]
  # slice and transpose to [Ngddseq, n_draws]
  y_post <- t(spp_post_array[, , i])
  
  # summaries
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # species-specific ylim
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$pgsGDD5, emp_spp$loglength,
       type = "n",
       ylim = ylim_spp,
       xlab = "Growing season growing degree days (GDD)",
       ylab = "log(ring width)",
       frame = FALSE,
       main = bquote(italic(.(spp_name))))
  
  # add panel letter
  mtext(paste0("(", letters[i], ")"),
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1.2)
  
  # color
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(gddseq, rev(gddseq)),
          c(y_low, rev(y_high)),
          col = adjustcolor(line_col, alpha.f = 0.3),
          border = NA)
  
  lines(gddseq, y_mean, col = line_col, lwd = 2)
  
  # Add treeid symbols
  unique_trees <- unique(emp_spp$treeid_num)
  sym_per_row  <- treeidsymbol[match(emp_spp$treeid_num, unique_trees)]
  
  points(emp_spp$pgsGDD5, emp_spp$loglength,
         pch = sym_per_row,
         cex = 1, col = line_col)
  
  # legend("topleft",
  #        legend = names(yrshapes),
  #        pch    = yrshapes,
  #        col    = line_col,
  #        pt.cex = 1.5, bty = "n", cex = 1.2,
  #        title  = "Year", title.font = 2)
}
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GDD: per Spp, facet, shaped by year #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
unique_years <- sort(unique(emp_spp$year))
year_syms <- setNames(seq_along(unique_years), unique_years)
# jpeg output
jpeg(filename = "figures/growthModelsMain/TSgrowthModelSlopesperSppFacetGDDyr.jpeg",
     width = 2400, height = 2400, res = 300)
# Layout: 2 rows x 2 columns per page
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

treeidsymbol <- c(15, 16, 17, 8, 9)

for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_name <- as.character(sppvecname[i])
  
  # subset empirical data correctly
  emp_spp <- empts[empts$latbi == spp_name, ]
  
  # spp_post_array is [n_draws, Ngddseq, Nspp]
  # slice and transpose to [Ngddseq, n_draws]
  y_post <- t(spp_post_array[, , i])
  
  # summaries
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # species-specific ylim
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$pgsGDD5, emp_spp$loglength,
       type = "n",
       ylim = ylim_spp,
       xlab = "Growing season growing degree days (GDD)",
       ylab = "log(ring width)",
       frame = FALSE,
       main = bquote(italic(.(spp_name))))
  
  # add panel letter
  mtext(paste0("(", letters[i], ")"),
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1.2)
  
  # color
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(gddseq, rev(gddseq)),
          c(y_low, rev(y_high)),
          col = adjustcolor(line_col, alpha.f = 0.3),
          border = NA)
  
  lines(gddseq, y_mean, col = line_col, lwd = 2)

  points(emp_spp$pgsGDD5, emp_spp$loglength,
         pch = year_syms,
         cex = 1, col = line_col)
}
plot.new()
par(mar = c(1,1,1,1))
legend("topleft",
       legend = names(year_syms),
       pch    = year_syms,
       col    = line_col,
       pt.cex = 1.5, bty = "n", cex = 1.2,
       title  = "Year", title.font = 2)

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### GDD: Per spp, facetted with treeid slopes #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
pdf(file = "figures/growthModelsMain/TSgrowthModelSlopesperSppTreeidGDD.pdf", width = 10, height = 8)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_name <- as.character(sppvecname[i])
  spp_num  <- sppvecnum[i]
  
  emp_spp <- empts[empts$latbi == spp_name, ]
  
  # species-level posterior [Ngddseq, n_draws]
  y_post_spp <- t(spp_post_array[, , i])
  
  y_mean <- apply(y_post_spp, 1, mean)
  y_low  <- apply(y_post_spp, 1, quantile, 0.25)
  y_high <- apply(y_post_spp, 1, quantile, 0.75)
  
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  line_col <- tscolslatbi[spp_name]
  
  plot(emp_spp$pgsGDD5, emp_spp$loglength,
       type = "n",
       ylim = ylim_spp,
       xlab = "Growing season growing degree days (GDD)",
       ylab = "log(ring width)",
       frame = FALSE,
       main = bquote(italic(.(spp_name))))
  
  polygon(c(gddseq, rev(gddseq)),
          c(y_low, rev(y_high)),
          col = adjustcolor(line_col, alpha.f = 0.3),
          border = NA)
  
  lines(gddseq, y_mean, col = line_col, lwd = 3)
  
  # add individual treeid variation
  treeidspp <- treeid_spp$treeid_num[which(treeid_spp$spp_num == spp_num)]
  
  for (j in seq_along(treeidspp)) { # j = 1
    tree_id_num <- treeidspp[j]
    tree_idx    <- which(treeidvecnum == tree_id_num)
    
    # slice this tree from y_post_array [Ngddseq, n_draws]
    y_post_tree <- t(y_post_array[, , tree_idx])
    
    y_mean_tree <- apply(y_post_tree, 1, mean)
    y_low_tree  <- apply(y_post_tree, 1, quantile, 0.25)
    y_high_tree <- apply(y_post_tree, 1, quantile, 0.75)
    
    sym <- treeidsymbol[j]
    
    treeidtempsymbol <- emp_spp[emp_spp$treeid_num == tree_id_num, ]
    
    points(treeidtempsymbol$pgsGDD5, treeidtempsymbol$loglength,
           pch = sym, col = line_col)
    
    polygon(c(gddseq, rev(gddseq)),
            c(y_low_tree, rev(y_high_tree)),
            col = adjustcolor(line_col, alpha.f = 0.1),
            border = NA)
    
    lines(gddseq, y_mean_tree, col = line_col, lwd = 1)
  }
}
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### GDD: per spp, non facetted #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
spp_post_array <- extract(fitgdd, "spp_post")$spp_post

jpeg(filename = "figures/growthModelsMain/TSgrowthModelSlopesperSppGDD.jpeg",
     width = 2400, height = 2400, res = 300)

par(mar = c(4, 4, 2, 1))

plot(empts$pgsGDD5, dgdd$y, type = "n",
     ylim = range(min(empts$loglength), max(empts$loglength)),
     xlab = "Primary growing season GDD", ylab = "log(ring width)",
     main = "species growth responses")

for (i in seq_along(sppvecnum)) { # i = 1
  spp_name <- as.character(sppvecname[i])
  y_post   <- t(spp_post_array[, , i])
  
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(gddseq, rev(gddseq)),
          c(y_low, rev(y_high)),
          col = adjustcolor(line_col, alpha.f = 0.2),
          border = NA)
  
  lines(gddseq, y_mean, col = line_col, lwd = 2)
}
dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# GSL: Prep posterior reconstruction ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# recover full intercepts for each tree id
fullintercept_cols_gsl <- grep("^fullintercept", colnames(df_fitgsl), value = TRUE)
fullintercept_gsl <- df_fitgsl[, fullintercept_cols_gsl]
colnames(fullintercept_gsl) <- 1:ncol(fullintercept_gsl)

# recover each slope
treeid_slope_cols_gsl <- grep("^treeid_slope", colnames(df_fitgsl), value = TRUE)
treeid_bspp_gsl <- df_fitgsl[, treeid_slope_cols_gsl] / gslscale
colnames(treeid_bspp_gsl) <- 1:ncol(treeid_bspp_gsl)

# recover sim ypred for each gsl X tree id for each iteration
# y_post_array_gsl is [n_draws, Ngslseq, Ntreeid]
y_post_array_gsl <- extract(fitgsl, "y_post")$y_post

# posterior array for each species [n_draws, Ngslseq, Nspp]
spp_post_array_gsl <- extract(fitgsl, "spp_post")$spp_post

gslseq <- dgsl$gslseq

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GSL: per Spp, facet, shaped by treeid #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(filename = "figures/growthModelsMain/TSgrowthModelSlopesperSppFacetGSLtree.jpeg",
     width = 2400, height = 2400, res = 300)
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

treeidsymbol <- c(15, 16, 17, 8, 9)

for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_name <- as.character(sppvecname[i])
  line_col <- tscolslatbi[spp_name]
  
  emp_spp <- empts[empts$latbi == spp_name, ]
  
  y_post_gsl <- t(spp_post_array_gsl[, , i])
  
  y_mean_gsl <- apply(y_post_gsl, 1, mean)
  y_low_gsl  <- apply(y_post_gsl, 1, quantile, 0.25)
  y_high_gsl <- apply(y_post_gsl, 1, quantile, 0.75)
  
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$pgsGSL, emp_spp$loglength,
       type = "n",
       ylim = ylim_spp,
       xlab = "Growing season length (days)",
       ylab = "log(ring width)",
       frame = FALSE,
       main = bquote(italic(.(spp_name))))
  
  mtext(paste0("(", letters[i], ")"),
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1.2)
  
  polygon(c(gslseq, rev(gslseq)),
          c(y_low_gsl, rev(y_high_gsl)),
          col = adjustcolor(line_col, alpha.f = 0.3),
          border = NA)
  
  lines(gslseq, y_mean_gsl, col = line_col, lwd = 2)
  
  unique_trees <- unique(emp_spp$treeid_num)
  sym_per_row  <- treeidsymbol[match(emp_spp$treeid_num, unique_trees)]
  
  points(emp_spp$pgsGSL, emp_spp$loglength,
         pch = sym_per_row, cex = 1, col = line_col)
}
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GSL: per Spp, facet, shaped by year #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# jpeg output
jpeg(filename = "figures/growthModelsMain/TSgrowthModelSlopesperSppFacetGSLyr.jpeg",
     width = 2400, height = 2400, res = 300)
# Layout: 2 rows x 2 columns per page
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_name <- as.character(sppvecname[i])
  
  # subset empirical data correctly
  emp_spp <- empts[empts$latbi == spp_name, ]
  
  # spp_post_array is [n_draws, Ngddseq, Nspp]
  # slice and transpose to [Ngddseq, n_draws]
  y_post <- t(spp_post_array[, , i])
  
  # summaries
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # species-specific ylim
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$pgsGSL, emp_spp$loglength,
       type = "n",
       ylim = ylim_spp,
       xlab = "Growing season growing degree days (GDD)",
       ylab = "log(ring width)",
       frame = FALSE,
       main = bquote(italic(.(spp_name))))
  
  # add panel letter
  mtext(paste0("(", letters[i], ")"),
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1.2)
  
  # color
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(gslseq, rev(gslseq)),
          c(y_low, rev(y_high)),
          col = adjustcolor(line_col, alpha.f = 0.3),
          border = NA)
  
  lines(gslseq, y_mean, col = line_col, lwd = 2)
  
  points(emp_spp$pgsGSL, emp_spp$loglength,
         pch = year_syms,
         cex = 1, col = line_col)
}
plot.new()
par(mar = c(1,1,1,1))
legend("topleft",
       legend = names(year_syms),
       pch    = year_syms,
       col    = line_col,
       pt.cex = 1.5, bty = "n", cex = 1.2,
       title  = "Year", title.font = 2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# SOS: Prep posterior reconstruction ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# recover full intercepts for each tree id
fullintercept_cols_sos <- grep("^fullintercept", colnames(df_fitsos), value = TRUE)
fullintercept_sos <- df_fitsos[, fullintercept_cols_sos]
colnames(fullintercept_sos) <- 1:ncol(fullintercept_sos)

# recover each slope
treeid_slope_cols_sos <- grep("^treeid_slope", colnames(df_fitsos), value = TRUE)
treeid_bspp_sos <- df_fitsos[, treeid_slope_cols_sos] / sosscale
colnames(treeid_bspp_sos) <- 1:ncol(treeid_bspp_sos)

# recover sim ypred for each sos X tree id for each iteration
# y_post_array_sos is [n_draws, Nsosseq, Ntreeid]
y_post_array_sos <- extract(fitsos, "y_post")$y_post

# posterior array for each species [n_draws, Nsosseq, Nspp]
spp_post_array_sos <- extract(fitsos, "spp_post")$spp_post

sosseq <- dsos$sosseq

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### SOS: per Spp, facet #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(filename = "figures/growthModelsMain/TSgrowthModelSlopesperSppFacetSOS.jpeg",
     width = 2400, height = 2400, res = 300)
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

treeidsymbol <- c(15, 16, 17, 8, 9)

for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_name <- as.character(sppvecname[i])
  line_col <- tscolslatbi[spp_name]
  
  emp_spp <- empts[empts$latbi == spp_name, ]
  
  y_post_sos <- t(spp_post_array_sos[, , i])
  
  y_mean_sos <- apply(y_post_sos, 1, mean)
  y_low_sos  <- apply(y_post_sos, 1, quantile, 0.25)
  y_high_sos <- apply(y_post_sos, 1, quantile, 0.75)
  
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$leafout, emp_spp$loglength,
       type = "n",
       ylim = ylim_spp,
       xlab = "Leafout day of year",
       ylab = "log(ring width)",
       frame = FALSE,
       main = bquote(italic(.(spp_name))))
  
  mtext(paste0("(", letters[i], ")"),
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1.2)
  
  polygon(c(sosseq, rev(sosseq)),
          c(y_low_sos, rev(y_high_sos)),
          col = adjustcolor(line_col, alpha.f = 0.3),
          border = NA)
  
  lines(sosseq, y_mean_sos, col = line_col, lwd = 2)
  
  unique_trees <- unique(emp_spp$treeid_num)
  sym_per_row  <- treeidsymbol[match(emp_spp$treeid_num, unique_trees)]
  
  points(emp_spp$leafout, emp_spp$loglength,
         pch = sym_per_row, cex = 1, col = line_col)
}
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### SOS: per Spp, non-facetted #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(filename = "figures/growthModelsMain/TSgrowthModelSlopesperSppNoFacetSOS.jpeg",
     width = 2400, height = 2400, res = 300)

par(mar = c(4, 4, 2, 1))

plot(empts$leafout, dsos$y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)),
     xlab = "Leafout day of year", ylab = "log(ring width)",
     main = "")

for (i in seq_along(sppvecnum)) { # i = 1
  spp_name   <- as.character(sppvecname[i])
  y_post_sos <- t(spp_post_array_sos[, , i])
  
  y_mean <- apply(y_post_sos, 1, mean)
  y_low  <- apply(y_post_sos, 1, quantile, 0.25)
  y_high <- apply(y_post_sos, 1, quantile, 0.75)
  
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(sosseq, rev(sosseq)),
          c(y_low, rev(y_high)),
          col = adjustcolor(line_col, alpha.f = 0.1),
          border = NA)
  
  lines(sosseq, y_mean, col = line_col, lwd = 2)
}
dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# EOS: Prep posterior reconstruction ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# recover full intercepts for each tree id
fullintercept_cols_eos <- grep("^fullintercept", colnames(df_fiteos), value = TRUE)
fullintercept_eos <- df_fiteos[, fullintercept_cols_eos]
colnames(fullintercept_eos) <- 1:ncol(fullintercept_eos)

# recover each slope
treeid_slope_cols_eos <- grep("^treeid_slope", colnames(df_fiteos), value = TRUE)
treeid_bspp_eos <- df_fiteos[, treeid_slope_cols_eos] / eosscale
colnames(treeid_bspp_eos) <- 1:ncol(treeid_bspp_eos)

# recover sim ypred for each eos X tree id for each iteration
# y_post_array_eos is [n_draws, Neosseq, Ntreeid]
y_post_array_eos <- extract(fiteos, "y_post")$y_post

# posterior array for each species [n_draws, Neosseq, Nspp]
spp_post_array_eos <- extract(fiteos, "spp_post")$spp_post

eosseq <- deos$eosseq

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### EOS: per Spp, facet #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(filename = "figures/growthModelsMain/TSgrowthModelSlopesperSppFacetEOS.jpeg",
     width = 2400, height = 2400, res = 300)
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

treeidsymbol <- c(15, 16, 17, 8, 9)

for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_name <- as.character(sppvecname[i])
  line_col <- tscolslatbi[spp_name]
  
  emp_spp <- empts[empts$latbi == spp_name, ]
  
  y_post_eos <- t(spp_post_array_eos[, , i])
  
  y_mean_eos <- apply(y_post_eos, 1, mean)
  y_low_eos  <- apply(y_post_eos, 1, quantile, 0.25)
  y_high_eos <- apply(y_post_eos, 1, quantile, 0.75)
  
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$coloredLeaves, emp_spp$loglength,
       type = "n",
       ylim = ylim_spp,
       xlab = "Budset day of year",
       ylab = "log(ring width)",
       frame = FALSE,
       main = bquote(italic(.(spp_name))))
  
  mtext(paste0("(", letters[i], ")"),
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1.2)
  
  polygon(c(eosseq, rev(eosseq)),
          c(y_low_eos, rev(y_high_eos)),
          col = adjustcolor(line_col, alpha.f = 0.3),
          border = NA)
  
  lines(eosseq, y_mean_eos, col = line_col, lwd = 2)
  
  unique_trees <- unique(emp_spp$treeid_num)
  sym_per_row  <- treeidsymbol[match(emp_spp$treeid_num, unique_trees)]
  
  points(emp_spp$coloredLeaves, emp_spp$loglength,
         pch = sym_per_row, cex = 1, col = line_col)
}
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### EOS: per Spp, non-facetted #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(filename = "figures/growthModelsMain/TSgrowthModelSlopesperSppNoFacetEOS.jpeg",
     width = 2400, height = 2400, res = 300)

par(mar = c(4, 4, 2, 1))

plot(empts$coloredLeaves, deos$y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)),
     xlab = "Budset day of year", ylab = "log(ring width)",
     main = "")

for (i in seq_along(sppvecnum)) { # i = 1
  spp_name   <- as.character(sppvecname[i])
  y_post_eos <- t(spp_post_array_eos[, , i])
  
  y_mean <- apply(y_post_eos, 1, mean)
  y_low  <- apply(y_post_eos, 1, quantile, 0.25)
  y_high <- apply(y_post_eos, 1, quantile, 0.75)
  
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(eosseq, rev(eosseq)),
          c(y_low, rev(y_high)),
          col = adjustcolor(line_col, alpha.f = 0.1),
          border = NA)
  
  lines(eosseq, y_mean, col = line_col, lwd = 2)
}
dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Mu plots #####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
custommar <- c(4, 4, 2, 1.2)
library(rsvg)
img_thermom <- rsvg::rsvg("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/figures/pictogramsLeaves/thermometer.svg")
img_calenda <- rsvg::rsvg("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/figures/pictogramsLeaves/calendar.svg")
img_leafout <- rsvg::rsvg("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/figures/pictogramsLeaves/bepaPicLeafout.svg")
img_budset  <- rsvg::rsvg("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/figures/pictogramsLeaves/bepaPicBudset.svg")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### bsp ###### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
jpeg(file = "figures/growthModelsMain/TSmuALLbspp.jpeg",
     width = 2800, height = 2400, res = 300)

layout(matrix(c(
  1, 2, 5,
  3, 4, 5
), nrow = 2, byrow = TRUE), widths = c(2, 2, 1.2))

mumar <- c(4, 1, 4, 1)

# Panel 1: GDD
par(mar = mumar)
plot(bspp_df2_ts_gdd$mean, y_pos,
     xlim = c(-0.5, 1.2), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change per 7 spring days GDD", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_gdd$p5,  y_pos, bspp_df2_ts_gdd$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_gdd$p25, y_pos, bspp_df2_ts_gdd$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("(a) Growing degree days", adj = 0, side = 3, line = 2.5, font = 2, cex = 0.9)
# arrows(x0 = -0.05, y0 = n_spp + 0.85, x1 = -0.5, y1 = n_spp + 0.85, length = 0.1, xpd = TRUE)
# text(-0.18, n_spp + 0.85, "Smaller/Cooler", pos = 3, xpd = TRUE, cex = 0.9)
arrows(x0 = 0.05, y0 = n_spp + 0.85, x1 = 0.5, y1 = n_spp + 0.85, length = 0.1, xpd = TRUE)
text(0.18, n_spp + 0.85, "Larger/Warmer", pos = 3, xpd = TRUE, cex = 0.9)
usr <- par("usr")
rasterImage(img_thermom, usr[1], usr[4] - diff(usr[3:4]) * 0.25, usr[1] + diff(usr[1:2]) * 0.20, usr[4])


# Panel 3: SOS
par(mar = mumar)
plot(bspp_df2_ts_sos$mean, y_pos,
     xlim = c(-0.5, 0.6), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change per 7 days of leafout", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_sos$p5,  y_pos, bspp_df2_ts_sos$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_sos$p25, y_pos, bspp_df2_ts_sos$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("(b) Start of season", adj = 0, side = 3, line = 2.5, font = 2, cex = 0.9)
arrows(x0 = -0.05, y0 = n_spp + 0.85, x1 = -0.5, y1 = n_spp + 0.85, length = 0.1, xpd = TRUE)
text(-0.18, n_spp + 0.85, "Larger/Earlier", pos = 3, xpd = TRUE, cex = 0.9)
# arrows(x0 = 0.05, y0 = n_spp + 0.85, x1 = 0.5, y1 = n_spp + 0.85, length = 0.1, xpd = TRUE)
# text(0.18, n_spp + 0.85, "Larger/Later", pos = 3, xpd = TRUE, cex = 0.9)
usr <- par("usr")
rasterImage(img_leafout, usr[1], usr[4] - diff(usr[3:4]) * 0.35, usr[1] + diff(usr[1:2]) * 0.25, usr[4])

# Panel 2: GSL
par(mar = mumar)
plot(bspp_df2_ts_gsl$mean, y_pos,
     xlim = c(-0.5, 1.2), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change per 7 days of GSL", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_gsl$p5,  y_pos, bspp_df2_ts_gsl$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_gsl$p25, y_pos, bspp_df2_ts_gsl$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("(c) Growing season length", adj = 0, side = 3, line = 2.5, font = 2, cex = 0.9)
# arrows(x0 = -0.05, y0 = n_spp + 0.85, x1 = -0.5, y1 = n_spp + 0.85, length = 0.1, xpd = TRUE)
# text(-0.18, n_spp + 0.85, "Smaller/Shorter", pos = 3, xpd = TRUE, cex = 0.9)
arrows(x0 = 0.05, y0 = n_spp + 0.85, x1 = 0.5, y1 = n_spp + 0.85, length = 0.1, xpd = TRUE)
text(0.18, n_spp + 0.85, "Larger/Longer", pos = 3, xpd = TRUE, cex = 0.9)
usr <- par("usr")
rasterImage(img_calenda, usr[1], usr[4] - diff(usr[3:4]) * 0.25, usr[1] + diff(usr[1:2]) * 0.20, usr[4])

# Panel 4: EOS
par(mar = mumar)
plot(bspp_df2_ts_eos$mean, y_pos,
     xlim = c(-0.5, 0.6), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change per 7 days of budset", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_eos$p5,  y_pos, bspp_df2_ts_eos$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_eos$p25, y_pos, bspp_df2_ts_eos$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("(d) End of season", adj = 0, side = 3, line = 2.5, font = 2, cex = 0.9)
arrows(x0 = -0.05, y0 = n_spp + 0.85, x1 = -0.5, y1 = n_spp + 0.85, length = 0.1, xpd = TRUE)
text(-0.18, n_spp + 0.85, "Larger/Earlier", pos = 3, xpd = TRUE, cex = 0.9)
# arrows(x0 = 0.05, y0 = n_spp + 0.85, x1 = 0.5, y1 = n_spp + 0.85, length = 0.1, xpd = TRUE)
# text(0.18, n_spp + 0.85, "Smaller/Later", pos = 3, xpd = TRUE, cex = 0.9)
usr <- par("usr")
rasterImage(img_budset, usr[1], usr[4] - diff(usr[3:4]) * 0.35, usr[1] + diff(usr[1:2]) * 0.25, usr[4])

# Panel 5: species legend
par(mar = c(mumar))
plot.new()
legend("center",
       legend = sapply(unique(bspp_df2_ts_gdd$spp_name),
                       function(x) parse(text = paste0("italic('", x, "')"))),
       col    = unique(tscolslatbi),
       pch    = 16, pt.cex = 1.5, bty = "n", cex = 1.2,
       title  = "Species", title.font = 2)
dev.off()  

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### bspp with lines #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
custommar <- c(4, 4, 3, 1.2)

jpeg(file = "figures/growthModelsMain/TSmuALLbsppWlines.jpeg",
     width = 2800, height = 3000, res = 300)

layout(matrix(c(
  1, 5, 9,
  2, 6, 9,
  3, 7, 9,
  4, 8, 9
), nrow = 4, byrow = TRUE),
widths = c(1.1, 1.2, 0.6))


# Row 1, Col 1, Slot 5 : GDD
par(mar = custommar)
plot(bspp_df2_ts_gdd$mean, y_pos,
     xlim = c(-0.5, 0.4), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change in averaged GDD of 7 spring days", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_gdd$p5,  y_pos, bspp_df2_ts_gdd$p95, y_pos,
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_gdd$p25, y_pos, bspp_df2_ts_gdd$p75, y_pos,
         col = tscolslatbi, lwd = 3)
mtext("(a) Growing degree days", side = 3, adj = 0, font = 2, cex = 0.9)
usr <- par("usr")
rasterImage(img_thermom, usr[1], usr[4] - diff(usr[3:4]) * 0.40, usr[1] + diff(usr[1:2]) * 0.22, usr[4])

# Row 2, Col 1, Slot 6 : GSL
par(mar = custommar)
plot(bspp_df2_ts_gsl$mean, y_pos,
     xlim = c(-0.5, 0.4), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change per 7 days of GSL", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_gsl$p5,  y_pos, bspp_df2_ts_gsl$p95, y_pos,
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_gsl$p25, y_pos, bspp_df2_ts_gsl$p75, y_pos,
         col = tscolslatbi, lwd = 3)
mtext("(b) Growing season length", side = 3, adj = 0, font = 2, cex = 0.9)
usr <- par("usr")
rasterImage(img_calenda, usr[1], usr[4] - diff(usr[3:4]) * 0.45, usr[1] + diff(usr[1:2]) * 0.25, usr[4])

# Row 3, Col 1, Slot 7 : SOS
par(mar = custommar)
plot(bspp_df2_ts_sos$mean, y_pos,
     xlim = c(-0.4, 0.4), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change per 7 days of leafout", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_sos$p5,  y_pos, bspp_df2_ts_sos$p95, y_pos,
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_sos$p25, y_pos, bspp_df2_ts_sos$p75, y_pos,
         col = tscolslatbi, lwd = 3)
mtext("(c) Start of season", side = 3, adj = 0, font = 2, cex = 0.9)
usr <- par("usr")
rasterImage(img_leafout, usr[1], usr[4] - diff(usr[3:4]) * 0.45, usr[1] + diff(usr[1:2]) * 0.22, usr[4])

# Row 4, Col 1, Slot 8 : EOS
par(mar = custommar)
plot(bspp_df2_ts_eos$mean, y_pos,
     xlim = c(-0.4, 0.4), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change per 7 days of colored leaves", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_eos$p5,  y_pos, bspp_df2_ts_eos$p95, y_pos,
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_eos$p25, y_pos, bspp_df2_ts_eos$p75, y_pos,
         col = tscolslatbi, lwd = 3)
mtext("(d) End of season", side = 3, adj = 0, font = 2, cex = 0.9)
usr <- par("usr")
rasterImage(img_budset, usr[1], usr[4] - diff(usr[3:4]) * 0.45, usr[1] + diff(usr[1:2]) * 0.25, usr[4])

# Row 1, Col 2, Slot 5 : GDD
par(mar = custommar)
plot(empts$pgsGDD5, dgdd$y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)), 
     xlab = "Growing season growing degree days (GDD)", ylab = "log(ring width)",
     main = "")
mtext("(e)", side = 3, adj = 0, font = 2, cex = 0.9)

# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  spp_name <- as.character(sppvecname[i])
  y_post <- t(spp_post_array[, , i])
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(gddseq, rev(gddseq)),
          c(y_low, rev(y_high)), # low and high interval
          col = adjustcolor(line_col, alpha.f = 0.1), border = NA)
  
  lines(gddseq, y_mean,
        col = line_col,
        lwd = 2)
  
  empts_spp <- empts[empts$latbi == spp_name, ]
  
}

# Row 2, Col 2, Slot 6 : GSL
par(mar = custommar)
plot(empts$pgsGSL, dgsl$y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)), 
     xlab = "Growing season length (days)", ylab = "log(ring width)",
     main = "")
mtext("(f)", side = 3, adj = 0, font = 2, cex = 0.9)

# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  spp_name <- as.character(sppvecname[i])
  y_post <- t(spp_post_array_gsl[, , i])
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(gslseq, rev(gslseq)),
          c(y_low, rev(y_high)), # low and high interval
          col = adjustcolor(line_col, alpha.f = 0.1), border = NA)
  
  lines(gslseq, y_mean,
        col = line_col,
        lwd = 2)
  
  empts_spp <- empts[empts$latbi == spp_name, ]
}

# Row 3, Col 2, Slot 7 : SOS
par(mar = custommar)
plot(empts$leafout, dsos$y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)), 
     xlab = "Leafout day of year", ylab = "log(ring width)",
     main = "")
mtext("(g)", side = 3, adj = 0, font = 2, cex = 0.9)

# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  spp_name <- as.character(sppvecname[i])
  y_post <- t(spp_post_array_sos[, , i])
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(sosseq, rev(sosseq)),
          c(y_low, rev(y_high)), # low and high interval
          col = adjustcolor(line_col, alpha.f = 0.1), border = NA)
  
  lines(sosseq, y_mean,
        col = line_col,
        lwd = 2)
  
  emp_spp <- empts[empts$latbi == spp_name, ]
}

# Row 4, Col 2, Slot 8 : EOS
par(mar = custommar)
plot(empts$coloredLeaves, deos$y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)), 
     xlab = "Budset day of year", ylab = "log(ring width)",
     main = "")
mtext("(h)", side = 3, adj = 0, font = 2, cex = 0.9)

# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  spp_name <- as.character(sppvecname[i])
  y_post <- t(spp_post_array_eos[, , i])
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  line_col <- tscolslatbi[spp_name]
  
  polygon(c(eosseq, rev(eosseq)),
          c(y_low, rev(y_high)), # low and high interval
          col = adjustcolor(line_col, alpha.f = 0.1), border = NA)
  
  lines(eosseq, y_mean,
        col = line_col,
        lwd = 2)
  
  emp_spp <- empts[empts$latbi == spp_name, ]
}

# Slot 9: species legend
par(mar = c(1, 1, 1, 1))
plot.new()
legend("center",
       legend = sapply(unique(bspp_df2_ts_gdd$spp_name), 
                       function(x) parse(text = paste0("italic('", x, "')"))),
       col    = tscolslatbi,
       pch    = 16, pt.cex = 1.5, bty = "n", cex = 1.5,
       title  = "Species", title.font = 2)

dev.off() 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### aspp #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
jpeg(file = "figures/growthModelsMain/TSmuALLaspp.jpeg",
     width = 1800, height = 2200, res = 300)

layout(matrix(c(
  1, 5,
  2, 5,
  3, 5,
  4, 5
), nrow = 4, byrow = TRUE),
widths = c(0.7, 0.4))

# Row 1: GDD
par(mar = c(5, 8, 2, 2))
plot(aspp_df2_ts_gdd$mean, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "",
     yaxt = "n", pch = 15, cex = 2, col = tscolslatbi, frame.plot = FALSE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_ts_gdd$p5,  y_pos, aspp_df2_ts_gdd$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(aspp_df2_ts_gdd$p25, y_pos, aspp_df2_ts_gdd$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("(a) Growing degree days", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 2: GSL
par(mar = c(5, 8, 2, 2))
plot(aspp_df2_ts_gsl$mean, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "",
     yaxt = "n", pch = 15, cex = 2, col = tscolslatbi, frame.plot = FALSE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_ts_gsl$p5,  y_pos, aspp_df2_ts_gsl$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(aspp_df2_ts_gsl$p25, y_pos, aspp_df2_ts_gsl$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("(b) Growing season length", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 3: SOS
par(mar = c(5, 8, 2, 2))
plot(aspp_df2_ts_sos$mean, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "",
     yaxt = "n", pch = 15, cex = 2, col = tscolslatbi, frame.plot = FALSE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_ts_sos$p5,  y_pos, aspp_df2_ts_sos$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(aspp_df2_ts_sos$p25, y_pos, aspp_df2_ts_sos$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("(c) Start of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 4: EOS
par(mar = c(5, 8, 2, 2))
plot(aspp_df2_ts_eos$mean, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "",
     yaxt = "n", pch = 15, cex = 2, col = tscolslatbi, frame.plot = FALSE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_ts_eos$p5,  y_pos, aspp_df2_ts_eos$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(aspp_df2_ts_eos$p25, y_pos, aspp_df2_ts_eos$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("(d) End of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Slot 5: species legend
par(mar = c(1, 1, 1, 1))
plot.new()
legend("center",
       legend = sapply(unique(aspp_df2_ts_gdd$spp_name),
                       function(x) parse(text = paste0("italic('", x, "')"))),
       col    = unique(tscolslatbi),
       pch    = 16, pt.cex = 1.5, bty = "n", cex = 1.2,
       title  = "Species", title.font = 2)
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### full treeid mu plots intercept #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
f <- as.data.frame(fitgdd)
columns <- colnames(f)
a <- f[,"a"]
fullintercept2 <- fullintercept - a

# get posterior means and quantiles
treeid_df4 <- data.frame(
  treeid   = character(ncol(fullintercept2)),
  mean     = numeric(ncol(fullintercept2)),
  p5  = NA, p25 = NA, p75 = NA, p95 = NA
)

for (i in 1:ncol(fullintercept2)) { # i = 1
  treeid_df4$treeid[i]  <- colnames(fullintercept2)[i]
  treeid_df4$mean[i]    <- round(mean(fullintercept2[[i]]), 3)
  treeid_df4$p5[i]      <- round(quantile(fullintercept2[[i]], probs = 0.05), 3)
  treeid_df4$p25[i]     <- round(quantile(fullintercept2[[i]], probs = 0.25), 3)
  treeid_df4$p75[i]     <- round(quantile(fullintercept2[[i]], probs = 0.75), 3)
  treeid_df4$p95[i]     <- round(quantile(fullintercept2[[i]], probs = 0.95), 3)
}

# get the og treeid names and spp back
treeid_df4$treeid      <- as.numeric(treeid_df4$treeid)
treeid_df4$treeid_name <- empts$id[match(treeid_df4$treeid, empts$treeid_num)]
treeid_df4$spp_name    <- empts$latbi[match(treeid_df4$treeid, empts$treeid_num)]
treeid_df4$spp_num     <- empts$spp_num[match(treeid_df4$treeid, empts$treeid_num)]

# species mean from full intercepts pooled across treeids
aspp_df <- f[, columns[grepl("aspp", columns) & !grepl("prior", columns)]]
ayearmean <- f[, grepl("mean_ayear", columns)]
aspp_df <- aspp_df + ayearmean
colnames(aspp_df) <- 1:ncol(aspp_df)

aspp_df4 <- data.frame(
  species = names(aspp_df),
  mean = sapply(aspp_df, function(x) round(mean(unlist(x)), 3)),
  p5   = sapply(aspp_df, function(x) round(quantile(unlist(x), 0.05), 3)),
  p25  = sapply(aspp_df, function(x) round(quantile(unlist(x), 0.25), 3)),
  p75  = sapply(aspp_df, function(x) round(quantile(unlist(x), 0.75), 3)),
  p95  = sapply(aspp_df, function(x) round(quantile(unlist(x), 0.95), 3))
)
aspp_df4$spp_name <- empts$latbi[match(aspp_df4$species, empts$spp_num)]

# Prep for figure
gap <- 2

treeid_df4$spp <- factor(treeid_df4$spp_name, levels = species_order)

treeid_df4 <- treeid_df4[order(treeid_df4$spp, treeid_df4$treeid), ]
treeid_df4$y_pos <- seq_len(nrow(treeid_df4))

total_rows <- nrow(treeid_df4) + (length(species_order) - 1) * gap
current_y  <- total_rows

for (sp in species_order) {
  idx <- which(treeid_df4$spp == sp)
  n   <- length(idx)
  treeid_df4$y_pos[idx] <- current_y:(current_y - n + 1)
  current_y <- current_y - n - gap
}

pdf(file = "figures/growthModelsMain/TSmeanPlotGrowthGDD_treeidBYspp.pdf",
    width = 9, height = 10)
par(mar = c(4, 6, 6, 2))

plot(NA, NA,
     xlim = range(c(treeid_df4$p5 - 1, treeid_df4$p95 + 1.8)),
     ylim = c(0.5, max(treeid_df4$y_pos) + 0.5),
     xlab = "Tree id and species effects", ylab = "",
     bty = "l", yaxt = "n")

segments(x0 = treeid_df4$p5, x1 = treeid_df4$p95, y0 = treeid_df4$y_pos,
         col = adjustcolor(tscolslatbi[treeid_df4$spp_name], alpha.f = 0.7), lwd = 1)

segments(x0 = treeid_df4$p25, x1 = treeid_df4$p75, y0 = treeid_df4$y_pos,
         col = adjustcolor(tscolslatbi[treeid_df4$spp_name], alpha.f = 0.7), lwd = 1.5)

points(treeid_df4$mean, treeid_df4$y_pos,
       cex = 0.8, pch = 16,
       col = adjustcolor(tscolslatbi[treeid_df4$spp_name], alpha.f = 0.7))

spp_y_top <- tapply(treeid_df4$y_pos, treeid_df4$spp_name, max)
aspp_df4$y_pos  <- spp_y_top[aspp_df4$spp_name] + 1

segments(x0 = aspp_df4$p5, x1 = aspp_df4$p95, y0 = aspp_df4$y_pos,
         col = adjustcolor(tscolslatbi[aspp_df4$spp_name], alpha.f = 0.9), lwd = 2)

segments(x0 = aspp_df4$p25, x1 = aspp_df4$p75, y0 = aspp_df4$y_pos,
         col = tscolslatbi[aspp_df4$spp_name], lwd = 3)

points(aspp_df4$mean, aspp_df4$y_pos,
       pch = 16, col = tscolslatbi[aspp_df4$spp_name], cex = 1.5)

abline(v = 0, lty = 2)

axis(side = 2, at = treeid_df4$y_pos, labels = treeid_df4$treeid_name,
     cex.axis = 0.5, las = 1)

spp_y <- tapply(treeid_df4$y_pos, treeid_df4$spp_name, mean)
species_legend_order <- names(sort(spp_y, decreasing = TRUE))

legend(
  x = max(treeid_df4$p95)-0.6,
  y = max(treeid_df4$y_pos),
  legend = as.expression(lapply(species_legend_order, function(x)
    substitute(italic(s), list(s = x))
  )),
  col    = tscolslatbi[species_legend_order],
  pch = 16, pt.cex = 1.2, title = "Species", bty = "n")

dev.off()


}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### ayear #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "figures/growthModelsMain/TSmuayear.jpeg",
     width = 1600, height = 1600, res = 300)

ayear_df2_ts_gdd$year_name <- as.character(ayear_df2_ts_gdd$year_name)
years_ordered <- sort(unique(ayear_df2_ts_gdd$year_name))
ayear_df2_ts_gdd$y_pos <- match(ayear_df2_ts_gdd$year_name, years_ordered)

par(mar = c(4, 4, 4, 4))
plot(ayear_df2_ts_gdd$mean, ayear_df2_ts_gdd$y_pos,
     xlim = c(-1.5, 1.), ylim = c(0.5, length(years_ordered) + 0.5),
     xlab = "Ring width intercept values (mm)", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colsyr[ayear_df2_ts_gdd$year_name], 
     frame.plot = FALSE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(ayear_df2_ts_gdd$p5,  ayear_df2_ts_gdd$y_pos, 
         ayear_df2_ts_gdd$p95, ayear_df2_ts_gdd$y_pos, 
         lwd = 1.5, col = colsyr[ayear_df2_ts_gdd$year_name])
segments(ayear_df2_ts_gdd$p25, ayear_df2_ts_gdd$y_pos, 
         ayear_df2_ts_gdd$p75, ayear_df2_ts_gdd$y_pos, 
         lwd = 3, colsyr[ayear_df2_ts_gdd$year_name])
axis(side = 2, at = seq_along(years_ordered), labels = years_ordered, las = 1)

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### ayear with boxplot #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg("figures/growthModelsMain/TSayearBoxplot.jpeg", width = 3000, height = 2000, res = 300)
par(oma = c(0, 0, 2, 0))
layout(matrix(c(1,  2,  3,  4,  5,
                1,  6,  7,  8,  9,
                1, 10, 11, 12,  0), nrow = 3, ncol = 5, byrow = TRUE),
       widths = c(1.2, 1, 1, 1, 1))

# Left: mu plot
par(mar = c(4, 4, 5, 1))

ayear_df2_ts_gdd <- ayear_df2_ts_gdd[rev(ayear_df2_ts_gdd$year),]
y_pos_yr <- ayear_df2_ts_gdd$year

plot(ayear_df2_ts_gdd$mean, ayear_df2_ts_gdd$y_pos,
     xlim = c(-1, 1), ylim = c(0.5, length(years_ordered) + 0.5),
     xlab = "log(ring width) intercept values (mm)", ylab = "",
     yaxt = "n", pch = 16, cex = 2, 
     col = colsyr[as.character(ayear_df2_ts_gdd$year_name)],
     frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(ayear_df2_ts_gdd$p5,  ayear_df2_ts_gdd$y_pos, 
         ayear_df2_ts_gdd$p95, ayear_df2_ts_gdd$y_pos, 
         lwd = 1.5, 
         col = colsyr[as.character(ayear_df2_ts_gdd$year_name)])
segments(ayear_df2_ts_gdd$p25, ayear_df2_ts_gdd$y_pos, 
         ayear_df2_ts_gdd$p75, ayear_df2_ts_gdd$y_pos, 
         lwd = 3, 
         col = colsyr[as.character(ayear_df2_ts_gdd$year_name)])
axis(side = 2, at = seq_along(years_ordered), labels = years_ordered, las = 1)
mtext("(a) Year intercept", 
      side = 3, outer = TRUE, adj = 0.05, font = 2, cex = 1, line = 0.2)

# Right: boxplots
for(sp in sppvecname) { # sp = "A. rubrum"
  par(mar = c(2, 5, 2, 1))
  dat <- empts[empts$latbi == sp,]
  yr_levels <- sort(unique(dat$year))
  dat$year <- factor(dat$year, levels = sort(as.character(ayear_df2_ts_gdd$year_name)))
  boxplot(lengthMM ~ year, data = dat,
          # main = bquote(italic(.(sp))),
          xlab = "Year", ylab = "Ring width (mm)",
          col = adjustcolor(colsyr[levels(dat$year)], alpha.f = 0.5),
          border = adjustcolor(colsyr[levels(dat$year)], alpha.f = 0.5),
          medcol = "black",
          whisklty = 1, staplewex = 0, medlty = 1, medlwd = 0.8,
          outpch = 16, outcex = 0.7, outcol = "black")
  title(main = bquote(italic(.(sp))), line = 0.7, cex.main =1.3)
  stripchart(lengthMM ~ year, data = dat,
             method = "jitter", jitter = 0.08,
             pch = 16, cex = 0.7, col = "black",
             vertical = TRUE, add = TRUE)
}
mtext("(b) Ring width (mm) observations per year and species",
      side = 3, outer = TRUE, adj = 0.5, font = 2, cex = 1, line = 0.2)
dev.off()

##### Box plot alone #####
jpeg("figures/growthModelsMain/boxplotRingWidth.jpeg",
     width = 2400, height = 2000, res = 300)

par(mfrow = c(3, 4), mar = c(2, 5, 3, 1))
for(sp in sppvecname) { # sp = "A. incana" 
  dat <- empts[empts$latbi == sp,]
  dat$year <- factor(dat$year, levels = sort(as.character(ayear_df2_ts_gdd$year_name)))
  boxplot(lengthMM ~ year, data = dat,
          # main = bquote(italic(.(sp))),
          xlab = "", ylab = "Ring width (mm)",
          col = adjustcolor(colsyr[levels(dat$year)], alpha.f = 0.5),
          border = adjustcolor(colsyr[levels(dat$year)], alpha.f = 0.5),
          medcol = "black",
          whisklty = 1, staplewex = 0, medlty = 1, outpch = 16, outcex = 0.7, outcol = "black",
          cex.axis = mysizeaxis, cex.lab = mysizelab)
  mtext(bquote(italic(.(sp))), side = 3, line = 0.5, cex = 0.8)
  stripchart(lengthMM ~ year, data = dat,
             method = "jitter", jitter = 0.08,
             pch = 16, cex = 0.7, col = "black",
             vertical = TRUE, add = TRUE)
}

dev.off()

##### checks temporary for betall #####
t <- subset(empts, latbi %in% "B. alleghaniensis")
plot(t$loglength ~ t$pgsGDD5)
unique(empts$latbi)

n_spp <- length(unique(empts$latbi))
y_pos <- rev(1:n_spp)
par(mfrow=c(1,2))
plot(bspp_df2_ts_gdd$mean, y_pos,
     xlim = c(-0.5, 1.2), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change per 7 spring days GDD", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_gdd$p5,  y_pos, bspp_df2_ts_gdd$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_gdd$p25, y_pos, bspp_df2_ts_gdd$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("Growing degree days bspp", adj = 0, side = 3, line = 2.5, font = 2, cex = 0.9)

legend(
  x = 0.25,
  y = 10,
  legend = as.expression(lapply(species_legend_order, function(x)
    substitute(italic(s), list(s = x))
  )),
  col    = tscolslatbi[species_legend_order],
  pch = 16, pt.cex = 1.2, title = "Species", bty = "n")

plot(aspp_df2_ts_gdd$mean, y_pos,
     xlim = c(-5, 5), ylim = c(0.5, n_spp + 0.5),
     xlab = "log(ring width) change per 7 spring days GDD", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_ts_gdd$p5,  y_pos, aspp_df2_ts_gdd$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(aspp_df2_ts_gdd$p25, y_pos, aspp_df2_ts_gdd$p75, y_pos, col = tscolslatbi, lwd = 3)
mtext("Growing degree days aspp", adj = 0, side = 3, line = 2.5, font = 2, cex = 0.9)
# Slopes with treeid

jpeg(file = "figures/growthModelsMain/BallSlopesById.jpeg", 
     width = 2400, height = 2400, res = 300)
par(mfrow = c(1, 1))

spp_name <- "B. alleghaniensis"
spp_num  <- 4

emp_spp <- empts[empts$latbi == spp_name, ]

# species-level posterior [Ngddseq, n_draws]
y_post_spp <- t(spp_post_array[, , spp_num])

y_mean <- apply(y_post_spp, 1, mean)
y_low  <- apply(y_post_spp, 1, quantile, 0.25)
y_high <- apply(y_post_spp, 1, quantile, 0.75)

ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)

line_col <- tscolslatbi[spp_name]

plot(emp_spp$pgsGDD5, emp_spp$loglength,
     type = "n",
     ylim = ylim_spp,
     xlab = "Growing season growing degree days (GDD)",
     ylab = "log(ring width)",
     frame = FALSE,
     main = bquote(italic(.(spp_name))))

polygon(c(gddseq, rev(gddseq)),
        c(y_low, rev(y_high)),
        col = adjustcolor(line_col, alpha.f = 0.3),
        border = NA)

lines(gddseq, y_mean, col = line_col, lwd = 3)

# add individual treeid variation
treeidspp <- treeid_spp$treeid_num[which(treeid_spp$spp_num == spp_num)]
  
  for (j in seq_along(treeidspp)) { # j = 1
    tree_id_num <- treeidspp[j]
    tree_idx    <- which(treeidvecnum == tree_id_num)
    
    # slice this tree from y_post_array [Ngddseq, n_draws]
    y_post_tree <- t(y_post_array[, , tree_idx])
    
    y_mean_tree <- apply(y_post_tree, 1, mean)
    y_low_tree  <- apply(y_post_tree, 1, quantile, 0.25)
    y_high_tree <- apply(y_post_tree, 1, quantile, 0.75)
    
    sym <- treeidsymbol[j]
    
    treeidtempsymbol <- emp_spp[emp_spp$treeid_num == tree_id_num, ]
    
    points(treeidtempsymbol$pgsGDD5, treeidtempsymbol$loglength,
           pch = sym, col = "black")
    
    polygon(c(gddseq, rev(gddseq)),
            c(y_low_tree, rev(y_high_tree)),
            col = adjustcolor(line_col, alpha.f = 0.1),
            border = NA)
    
    lines(gddseq, y_mean_tree, col = line_col, lwd = 1)
  }
legend("topright",
       legend = treeid_spp$id[which(treeid_spp$spp_num == spp_num)],
       pch = treeidsymbol,
       col = line_col,
       bty = "n")
dev.off()

# By year
jpeg(file = "figures/growthModelsMain/BallSlopesByYr.jpeg", 
     width = 2400, height = 2400, res = 300)
par(mfrow = c(1, 1))

plot(emp_spp$pgsGDD5, emp_spp$loglength,
     type = "n",
     ylim = ylim_spp,
     xlab = "Growing season growing degree days (GDD)",
     ylab = "log(ring width)",
     frame = FALSE,
     main = bquote(italic(.(spp_name))))

polygon(c(gddseq, rev(gddseq)),
        c(y_low, rev(y_high)),
        col = adjustcolor(line_col, alpha.f = 0.3),
        border = NA)

lines(gddseq, y_mean, col = line_col, lwd = 3)


for (j in seq_along(treeidspp)) {
  tree_id_num <- treeidspp[j]
  tree_idx    <- which(treeidvecnum == tree_id_num)
  
  y_post_tree <- t(y_post_array[, , tree_idx])
  y_mean_tree <- apply(y_post_tree, 1, mean)
  y_low_tree  <- apply(y_post_tree, 1, quantile, 0.25)
  y_high_tree <- apply(y_post_tree, 1, quantile, 0.75)
  
  treeidtempsymbol <- emp_spp[emp_spp$treeid_num == tree_id_num, ]
  
  points(treeidtempsymbol$pgsGDD5, treeidtempsymbol$loglength,
         pch = year_syms[as.character(treeidtempsymbol$year)],
         col = "black")
  
  polygon(c(gddseq, rev(gddseq)),
          c(y_low_tree, rev(y_high_tree)),
          col = adjustcolor(line_col, alpha.f = 0.1),
          border = NA)
  
  lines(gddseq, y_mean_tree, col = line_col, lwd = 1)
}

legend("topright",
       legend = unique_years,
       pch = year_syms,
       col = "black",
       bty = "n")

dev.off() 



##### checks temporary for BETNIG #####
t <- subset(empts, latbi %in% "B. nigra")
# plot(t$loglength ~ t$pgsGDD5)
unique(empts$latbi)

# Slopes with treeid

jpeg(file = "figures/growthModelsMain/BnigSlopesById.jpeg", 
     width = 2400, height = 2400, res = 300)
par(mfrow = c(1, 1))

spp_name <- "B. nigra"
spp_num  <- 5

emp_spp <- empts[empts$latbi == spp_name, ]

# species-level posterior [Ngddseq, n_draws]
y_post_spp <- t(spp_post_array[, , spp_num])

y_mean <- apply(y_post_spp, 1, mean)
y_low  <- apply(y_post_spp, 1, quantile, 0.25)
y_high <- apply(y_post_spp, 1, quantile, 0.75)

ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)

line_col <- tscolslatbi[spp_name]

plot(emp_spp$pgsGDD5, emp_spp$loglength,
     type = "n",
     ylim = ylim_spp,
     xlab = "Growing season growing degree days (GDD)",
     ylab = "log(ring width)",
     frame = FALSE,
     main = bquote(italic(.(spp_name))))

polygon(c(gddseq, rev(gddseq)),
        c(y_low, rev(y_high)),
        col = adjustcolor(line_col, alpha.f = 0.3),
        border = NA)

lines(gddseq, y_mean, col = line_col, lwd = 3)

# add individual treeid variation
treeidspp <- treeid_spp$treeid_num[which(treeid_spp$spp_num == spp_num)]

for (j in seq_along(treeidspp)) { # j = 1
  tree_id_num <- treeidspp[j]
  tree_idx    <- which(treeidvecnum == tree_id_num)
  
  # slice this tree from y_post_array [Ngddseq, n_draws]
  y_post_tree <- t(y_post_array[, , tree_idx])
  
  y_mean_tree <- apply(y_post_tree, 1, mean)
  y_low_tree  <- apply(y_post_tree, 1, quantile, 0.25)
  y_high_tree <- apply(y_post_tree, 1, quantile, 0.75)
  
  sym <- treeidsymbol[j]
  
  treeidtempsymbol <- emp_spp[emp_spp$treeid_num == tree_id_num, ]
  
  points(treeidtempsymbol$pgsGDD5, treeidtempsymbol$loglength,
         pch = sym, col = "black")
  
  polygon(c(gddseq, rev(gddseq)),
          c(y_low_tree, rev(y_high_tree)),
          col = adjustcolor(line_col, alpha.f = 0.1),
          border = NA)
  
  lines(gddseq, y_mean_tree, col = line_col, lwd = 1)
}
legend("topright",
       legend = treeid_spp$id[which(treeid_spp$spp_num == spp_num)],
       pch = treeidsymbol,
       col = line_col,
       bty = "n")
dev.off()

# By year
jpeg(file = "figures/growthModelsMain/BnigSlopesByYr.jpeg", 
     width = 2400, height = 2400, res = 300)
par(mfrow = c(1, 1))

plot(emp_spp$pgsGDD5, emp_spp$loglength,
     type = "n",
     ylim = ylim_spp,
     xlab = "Growing season growing degree days (GDD)",
     ylab = "log(ring width)",
     frame = FALSE,
     main = bquote(italic(.(spp_name))))

polygon(c(gddseq, rev(gddseq)),
        c(y_low, rev(y_high)),
        col = adjustcolor(line_col, alpha.f = 0.3),
        border = NA)

lines(gddseq, y_mean, col = line_col, lwd = 3)


unique_years <- sort(unique(emp_spp$year))
year_syms <- setNames(seq_along(unique_years), unique_years)

for (j in seq_along(treeidspp)) {
  tree_id_num <- treeidspp[j]
  tree_idx    <- which(treeidvecnum == tree_id_num)
  
  y_post_tree <- t(y_post_array[, , tree_idx])
  y_mean_tree <- apply(y_post_tree, 1, mean)
  y_low_tree  <- apply(y_post_tree, 1, quantile, 0.25)
  y_high_tree <- apply(y_post_tree, 1, quantile, 0.75)
  
  treeidtempsymbol <- emp_spp[emp_spp$treeid_num == tree_id_num, ]
  
  points(treeidtempsymbol$pgsGDD5, treeidtempsymbol$loglength,
         pch = year_syms[as.character(treeidtempsymbol$year)],
         col = "black")
  
  polygon(c(gddseq, rev(gddseq)),
          c(y_low_tree, rev(y_high_tree)),
          col = adjustcolor(line_col, alpha.f = 0.1),
          border = NA)
  
  lines(gddseq, y_mean_tree, col = line_col, lwd = 1)
}

legend("topright",
       legend = unique_years,
       pch = year_syms,
       col = "black",
       bty = "n")

dev.off() 

}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Ring width summaries ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
rwmints <- aggregate(lengthMM ~ latbi, empts, FUN = min)
rwmeannts <- aggregate(lengthMM ~ latbi, empts, FUN = mean)
rwmaxts <- aggregate(lengthMM ~ latbi, empts, FUN = max)

rwsumts <- merge(rwmints, rwmaxts, by = "latbi")
colnames(rwsumts) <- c("latbi","min", "max")
rwsumts$mean <- rwmeannts$lengthMM[match(rwsumts$latbi, rwmeannts$latbi)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Z-scored output ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if(runzscore) {
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### GDD posterior recovery #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
fitgdd <- readRDS("output/stanOutput/fitGrowthGDDZscored")

df_fitgdd <- as.data.frame(fitgdd)

# posterior summaries
sigma_df2_z  <- extract_params(df_fitgdd, "sigma", "mean", "sigma")
bspp_df2_z   <- extract_params(df_fitgdd, "bsp", "fit_bspp", 
                               "spp", "bsp\\[(\\d+)\\]")
treeid_df2_z <- extract_params(df_fitgdd, "atreeid", "fit_atreeid", 
                               "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_z <- subset(treeid_df2_z, !grepl("z|sigma", treeid))
aspp_df2_z   <- extract_params(df_fitgdd, "aspp", "fit_aspp", 
                               "spp", "aspp\\[(\\d+)\\]")

treeid_df2_z$treeid_name <- empts$treeid[match(treeid_df2_z$treeid, empts$treeid_num)]
bspp_df2_z$spp_name <- empts$latbi[match(bspp_df2_z$spp, empts$spp_num)]
aspp_df2_z$spp_name <- empts$latbi[match(aspp_df2_z$spp, empts$spp_num)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### GSL posterior recovery #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
fitgsl <- readRDS("output/stanOutput/fitGrowthGSLZscored")

df_fitgsl <- as.data.frame(fitgsl)

# posterior summaries
sigma_df2_z_gsl  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2_z_gsl   <- extract_params(df_fitgsl, "bsp", "fit_bspp", 
                                   "spp", "bsp\\[(\\d+)\\]")
treeid_df2_z_gsl <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", 
                                   "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_z_gsl <- subset(treeid_df2_z, !grepl("z|sigma", treeid))
aspp_df2_z_gsl   <- extract_params(df_fitgsl, "aspp", "fit_aspp", 
                                   "spp", "aspp\\[(\\d+)\\]")

treeid_df2_z_gsl$treeid <- as.numeric(treeid_df2_z_gsl$treeid)
treeid_df2_z_gsl$treeid_name <- empts$treeid[match(treeid_df2_z_gsl$treeid, empts$treeid_num)]
bspp_df2_z_gsl$spp_name <- empts$latbi[match(bspp_df2_z_gsl$spp, empts$spp_num)]
aspp_df2_z_gsl$spp_name <- empts$latbi[match(aspp_df2_z_gsl$spp, empts$spp_num)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### SOS posterior recovery #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
fitsos <- readRDS("output/stanOutput/fitGrowthSOSZscored")

df_fitsos <- as.data.frame(fitsos)

# posterior summaries
sigma_df2_z_sos  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_z_sos   <- extract_params(df_fitsos, "bsp", "fit_bspp", 
                                   "spp", "bsp\\[(\\d+)\\]")
treeid_df2_z_sos <- extract_params(df_fitsos, "atreeid", "fit_atreeid", 
                                   "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_z_sos <- subset(treeid_df2_z_sos, !grepl("z|sigma", treeid))
aspp_df2_z_sos   <- extract_params(df_fitsos, "aspp", "fit_aspp", 
                                   "spp", "aspp\\[(\\d+)\\]")

treeid_df2_z_sos$treeid <- as.numeric(treeid_df2_z_sos$treeid)
treeid_df2_z_sos$treeid_name <- empts$treeid[match(treeid_df2_z_sos$treeid, empts$treeid_num)]
bspp_df2_z_sos$spp_name <- empts$latbi[match(bspp_df2_z_sos$spp, empts$spp_num)]
aspp_df2_z_sos$spp_name <- empts$latbi[match(aspp_df2_z_sos$spp, empts$spp_num)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### EOS posterior recovery #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
fiteos <- readRDS("output/stanOutput/fitGrowthEOSZscored")

df_fiteos <- as.data.frame(fiteos)

# posterior summaries
sigma_df2_z_eos  <- extract_params(df_fiteos, "sigma", "mean", "sigma")
bspp_df2_z_eos   <- extract_params(df_fiteos, "bsp", "fit_bspp", 
                                   "spp", "bsp\\[(\\d+)\\]")
treeid_df2_z_eos <- extract_params(df_fiteos, "atreeid", "fit_atreeid", 
                                   "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_z_eos <- subset(treeid_df2_z_eos, !grepl("z|sigma", treeid))
aspp_df2_z_eos   <- extract_params(df_fiteos, "aspp", "fit_aspp", 
                                   "spp", "aspp\\[(\\d+)\\]")
treeid_df2_z_eos$treeid <- as.numeric(treeid_df2_z_eos$treeid)
treeid_df2_z_eos$treeid_name <- empts$treeid[match(treeid_df2_z_eos$treeid, empts$treeid_num)]
bspp_df2_z_eos$spp_name <- empts$latbi[match(bspp_df2_z_eos$spp, empts$spp_num)]
aspp_df2_z_eos$spp_name <- empts$latbi[match(aspp_df2_z_eos$spp, empts$spp_num)]

# Add predictors and bind
bspp_df2_z$pred <- "GDD"
bspp_df2_z_gsl$pred <- "GSL"
bspp_df2_z_sos$pred <- "SOS"
bspp_df2_z_eos$pred <- "EOS"
bspp_z_binded_ts <- rbind(bspp_df2_z, bspp_df2_z_gsl, bspp_df2_z_sos, bspp_df2_z_eos)

arub_ts <- subset(bspp_z_binded_ts, spp_name %in% "A. rubrum")
asac_ts <- subset(bspp_z_binded_ts, spp_name %in% "A. saccharum")
afla_ts <- subset(bspp_z_binded_ts, spp_name %in% "A. flava")
ball_ts <- subset(bspp_z_binded_ts, spp_name %in% "B. alleghaniensis")
bnig_ts <- subset(bspp_z_binded_ts, spp_name %in% "B. nigra")
cgla_ts <- subset(bspp_z_binded_ts, spp_name %in% "C. glabra")
cova_ts <- subset(bspp_z_binded_ts, spp_name %in% "C. ovata")
pdel_ts <- subset(bspp_z_binded_ts, spp_name %in% "P. deltoide")
qalb_ts <- subset(bspp_z_binded_ts, spp_name %in% "Q. alba")
qrub_ts <- subset(bspp_z_binded_ts, spp_name %in% "Q. rubra")
tame_ts <- subset(bspp_z_binded_ts, spp_name %in% "T. americana")

bspp_z_binded_ts$fit_bspp_abs <- abs(bspp_z_binded_ts$mean)

agg_z_ts <- aggregate(fit_bspp_abs ~ spp_name, bspp_z_binded_ts, function(f) max(f))

# Aggregate to get only the max effect size for each species
max_ES_ts <- merge(agg_z_ts, bspp_z_binded_ts[, c("mean", "p5", "p95", "pred", "fit_bspp_abs", "spp_name")], 
                   by = c("spp_name", "fit_bspp_abs"))

max_ES_ts$fit_bspp_abs <- NULL

s <- max_ES_ts[order(max_ES_ts$pred),]
}