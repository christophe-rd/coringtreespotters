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

makeplots <- FALSE

# specify colors
renoir <- c("#17154f", "#2f357c", "#6c5d9e", "#9d9cd5", "#b0799a", "#e48171", 
            "#bf3729", "#e69b00", "#f5bb50", "#ada43b", "#355828")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# GDD posterior recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fit <- readRDS("output/stanOutput/fitGrowthGDD")
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
sigma_df2_ts  <- extract_params(df_fit, "sigma", "mean", "sigma")
bspp_df2_ts   <- extract_params(df_fit, "bsp", "fit_bspp", "spp", "bspp\\[(\\d+)\\]")
treeid_df2_ts <- extract_params(df_fit, "atreeid", "fit_atreeid", "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_ts <- subset(treeid_df2_ts, !grepl("z", treeid) & !grepl("sigma", treeid))
aspp_df2_ts   <- extract_params(df_fit, "aspp", "fit_aspp", "spp", "aspp\\[(\\d+)\\]")

treeid_df2_ts$treeid_name <- empts$id[match(treeid_df2_ts$treeid, empts$treeid_num)]
aspp_df2_ts$spp_name <- empts$latbi[match(aspp_df2_ts$spp, empts$spp_num)]
bspp_df2_ts$spp_name <- empts$latbi[match(bspp_df2_ts$spp, empts$spp_num)]

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

# change colnames
colnames(bspp_df_gsl) <- 1:ncol(bspp_df_gsl)
colnames(treeid_df_gsl) <- 1:ncol(treeid_df_gsl)
colnames(aspp_df_gsl) <- 1:ncol(aspp_df_gsl)

# posterior summaries
sigma_df2_ts_gsl  <- extract_params(df_fitgsl, "sigma", "mean", "sigma")
bspp_df2_ts_gsl   <- extract_params(df_fitgsl, "bsp", "fit_bspp", 
                                 "spp", "bspp\\[(\\d+)\\]")
treeid_df2_ts_gsl <- extract_params(df_fitgsl, "atreeid", "fit_atreeid", 
                                 "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_ts_gsl <- subset(treeid_df2_ts_gsl, !grepl("z|sigma", treeid))
aspp_df2_ts_gsl   <- extract_params(df_fitgsl, "aspp", "fit_aspp", 
                                 "spp", "aspp\\[(\\d+)\\]")

treeid_df2_ts_gsl$treeid_name <- empts$id[match(treeid_df2_ts_gsl$treeid, empts$treeid_num)]
bspp_df2_ts_gsl$spp_name <- empts$latbi[match(bspp_df2_ts_gsl$spp, empts$spp_num)]
aspp_df2_ts_gsl$spp_name <- empts$latbi[match(aspp_df2_ts_gsl$spp, empts$spp_num)]

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

# change colnames
colnames(bspp_df_sos) <- 1:ncol(bspp_df_sos)
colnames(treeid_df_sos) <- 1:ncol(treeid_df_sos)
colnames(aspp_df_sos) <- 1:ncol(aspp_df_sos)

# posterior summaries
sigma_df2_ts_sos  <- extract_params(df_fitsos, "sigma", "mean", "sigma")
bspp_df2_ts_sos   <- extract_params(df_fitsos, "bsp", "fit_bspp", 
                                 "spp", "bspp\\[(\\d+)\\]")
treeid_df2_ts_sos <- extract_params(df_fitsos, "atreeid", "fit_atreeid", 
                                 "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_ts_sos <- subset(treeid_df2_ts_sos, !grepl("z|sigma", treeid))
aspp_df2_ts_sos   <- extract_params(df_fitsos, "aspp", "fit_aspp", 
                                 "spp", "aspp\\[(\\d+)\\]")

treeid_df2_ts_sos$treeid_name <- empts$id[match(treeid_df2_ts_sos$treeid, empts$treeid_num)]
bspp_df2_ts_sos$spp_name <- empts$latbi[match(bspp_df2_ts_sos$spp, empts$spp_num)]
aspp_df2_ts_sos$spp_name <- empts$latbi[match(aspp_df2_ts_sos$spp, empts$spp_num)]

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

# change colnames
colnames(bspp_df_eos) <- 1:ncol(bspp_df_eos)
colnames(treeid_df_eos) <- 1:ncol(treeid_df_eos)
colnames(aspp_df_eos) <- 1:ncol(aspp_df_eos)

# posterior summaries
sigma_df2_ts_eos  <- extract_params(df_fiteos, "sigma", "mean", "sigma")
bspp_df2_ts_eos   <- extract_params(df_fiteos, "bsp", "fit_bspp", 
                                 "spp", "bspp\\[(\\d+)\\]")
treeid_df2_ts_eos <- extract_params(df_fiteos, "atreeid", "fit_atreeid", 
                                 "treeid", "atreeid\\[(\\d+)\\]")
treeid_df2_ts_eos <- subset(treeid_df2_ts_eos, !grepl("z|sigma", treeid))
aspp_df2_ts_eos   <- extract_params(df_fiteos, "aspp", "fit_aspp", 
                                 "spp", "aspp\\[(\\d+)\\]")

treeid_df2_ts_eos$treeid <- as.numeric(treeid_df2_ts_eos$treeid)
treeid_df2_ts_eos$treeid_name <- empts$id[match(treeid_df2_ts_eos$treeid, empts$treeid_num)]
bspp_df2_ts_eos$spp_name <- empts$latbi[match(bspp_df2_ts_eos$spp, empts$spp_num)]
aspp_df2_ts_eos$spp_name <- empts$latbi[match(aspp_df2_ts_eos$spp, empts$spp_num)]

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
# Plot lines with quantiles ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GDD: Prep posterior reconstruction #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# start by filling a df with treeid intercepts only
atreeid_gdd<- subset(df_fit, select = subyvec)
colnames(atreeid_gdd) <- 1:length(subyvec)

# the spp values for each tree id
treeid_aspp_gdd <- data.frame(matrix(ncol = ncol(atreeid_gdd), nrow = nrow(df_fit)))
colnames(treeid_aspp_gdd) <- colnames(atreeid_gdd)

for (i in seq_len(ncol(treeid_aspp_gdd))) {
  tree_id <- as.integer(colnames(treeid_aspp_gdd)[i])
  spp_id <- treeid_spp$spp_num[match(tree_id, treeid_spp$treeid_num)]
  treeid_aspp_gdd[, i] <- aspp_df[, spp_id]
}
treeid_aspp_gdd

# recover a
treeid_a_gdd <- data.frame(matrix(ncol = ncol(atreeid_gdd), nrow = nrow(df_fit)))
colnames(treeid_a_gdd) <- colnames(atreeid_gdd)

for (i in seq_len(ncol(treeid_a_gdd))) { # i = 1
  treeid_a_gdd[, i] <- df_fit[, "a"]
}

# sum all 3 dfs together to get the full intercept for each treeid
fullintercept <-
  treeid_a_gdd + 
  atreeid_gdd +
  treeid_aspp_gdd 
fullintercept

# now get the slope for each treeid
treeid_bspp <- data.frame(matrix(ncol = ncol(atreeid_gdd), nrow = nrow(df_fit)))
colnames(treeid_bspp) <- colnames(atreeid_gdd)

# back convert the slopes to their original scales
bspp_df4 <- bspp_df
for (i in 1:ncol(bspp_df4)){
  bspp_df4[[i]] <- bspp_df4[[i]] / gddscale
}

for (i in seq_len(ncol(treeid_bspp))) { # i = 30
  tree_id <- as.integer(colnames(treeid_bspp)[i])
  spp_id <- treeid_spp$spp_num[match(tree_id, treeid_spp$treeid_num)]
  treeid_bspp[, i] <- bspp_df4[, spp_id]
}
treeid_bspp

treeidvecnum <- 1:ncol(fullintercept)
treeidvecname <- treeid_spp$id
gddseq <- seq(min(empts$pgsGDD5), max(empts$pgsGDD5), length.out = 100)  
y_post_list <- list()  # store posterior predictions in a list where each tree id gets matrix

# below I create a list where each row is the posterior estimate for each value of gdd (so the first row correspond to the model estimate for the first gdd value stored in x) and each column is the iteration (from 1 to 8000)
for (i in seq_along(treeidvecnum)) { # i = 1
  tree_col <- as.character(treeidvecnum[i]) 
  y_post <- sapply(1:nrow(df_fit), function(f) {
    rnorm(length(gddseq), 
          fullintercept[f, tree_col] + treeid_bspp[f, tree_col] * gddseq, 
          sigma_df$sigma_y[f])
  })
  y_post_list[[tree_col]] <- y_post
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GDD: per treeid, facet #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# PDF output
pdf(file = "figures/growthModelsMain/growthModelSlopesperTreeid.pdf", width = 10, height = 8)
# Layout: 2 rows × 2 columns per page
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Loop over trees again to plot each tree individually
for (i in seq_along(treeidvecnum)) { # i = 1
  tree_col <- as.character(treeidvecnum[i])
  tree_col_name <- as.character(treeidvecname[i])
  y_post <- y_post_list[[tree_col]]
  
  # color line by spp
  tree_id_num <- as.integer(tree_col)
  
  # index the dots per treeid
  emp_treeid <- empts[empts$treeid_num == tree_id_num, ]
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # empty plot first
  plot(empts$pgsGDD5, y, type = "n", 
       ylim = range(c(emp_treeid$loglength, y_low, y_high), na.rm = TRUE),
       xlab = "Primary growing season GDD", ylab = "Log ring width (mm)",
       frame = FALSE,
       main = tree_col_name) # set the name for each plot
  
  spp_id <- treeid_spp$latbi[match(tree_id_num, treeid_spp$treeid_num)]
  
  line_col <- colslatbi[spp_id]
  
  # shaded interval
  polygon(c(gddseq, rev(gddseq)), 
          c(y_low, # lower interval
            rev(y_high)), # high interval
          col = adjustcolor(line_col, alpha.f = 0.3), 
          border = NA)
  
  # mean line
  lines(gddseq, y_mean,
        col = line_col,
        lwd = 2)
  
  points(
    emp_treeid$pgsGDD5,
    emp_treeid$loglength,
    pch = 16,
    cex = 2,
    col = line_col)
}
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GDD: per Spp, facet #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
mean_post_list <- list()
for (i in seq_along(treeidvecnum)) {
  tree_col <- as.character(treeidvecnum[i])
  mean_post_list[[tree_col]] <- sapply(1:nrow(df_fit), function(f) {
    fullintercept[f, tree_col] + treeid_bspp[f, tree_col] * gddseq
    # no sigma_y yet
  })
}

# average the mean predictions across trees within species
spp_mean_list <- lapply(spp_list, function(tree_vec) {
  Reduce("+", mean_post_list[as.character(tree_vec)]) / length(tree_vec)
})

# re-simulate sigma on the averaged mean
spp_post_list <- lapply(spp_mean_list, function(mean_mat) {
  sapply(1:nrow(df_fit), function(f) {
    rnorm(length(gddseq), mean_mat[, f], sigma_df$sigma_y[f])
  })
})

# jpeg output
jpeg(filename = "figures/growthModelsMain/growthModelSlopesperSppFacetGDD.jpeg",
  width = 2400, height = 2400, res = 300)
# Layout: 4 cols x 3 rows
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

treeidsymbol <- c(15, 16, 17, 8, 9)
# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  
  # define spp num
  spp_num <- as.integer(spp_column)
  
  # subset empirical data correctly
  emp_spp <- empts[empts$spp_num == spp_num, ]
  
  spp_column <- as.character(sppvecnum[i]) 
  y_post <- spp_post_list[[spp_column]]
  
  # summaries
  y_mean <- apply(y_post, 1, median)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # species-specific ylim
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$pgsGDD5, emp_spp$loglength,
       type = "n",
       # ylim = ylim_spp,
       ylim = c(-1, 3),
       xlab = "Growing season growing degree days (GDD)",
       ylab = "Log ring width (mm)",
       frame = FALSE,
       main = spp_column_name)
  
  # add panel letter 
  mtext(paste0("(", letters[i], ")"), 
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1)
  
  # color
  line_col <- renoir[spp_num]
  
  polygon(
    c(gddseq, rev(gddseq)),
    c(y_low, rev(y_high)),
    col = adjustcolor(line_col, alpha.f = 0.3),
    border = NA
  )
  
  lines(gddseq, y_mean, col = line_col, lwd = 2)
  
  # Add atreid symbols
  unique_trees <- unique(emp_spp$treeid_num)
  sym_per_row  <- treeidsymbol[match(emp_spp$treeid_num, unique_trees)]
  
  points(emp_spp$pgsGDD5, emp_spp$loglength,
         pch = sym_per_row,
         cex = 1,
         col = line_col)
  
  points(
    emp_spp$pgsGDD5,
    emp_spp$loglength,
    pch = 16,
    cex = 1,
    col = line_col
  )
}

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### GDD: Per spp, facetted with treeid slopes #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
pdf(file = "figures/growthModelsMain/growthModelSlopesperSppTreeidGDD.pdf", width = 10, height = 8)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Loop over trees again to plot each tree individually
for (i in seq(sppvecnum)) { # i = 11
  
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  
  # define spp num
  spp_num <- as.integer(spp_column)
  
  # subset empirical data correctly
  emp_spp <- empts[empts$spp_num == spp_num, ]
  
  spp_column <- as.character(sppvecnum[i]) 
  y_post <- spp_post_list[[spp_column]]
  
  # summaries
  y_mean <- apply(y_post, 1, median)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # species-specific ylim
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$pgsGDD5, emp_spp$loglength,
       type = "n",
       ylim = ylim_spp,
       xlab = "Growing season growing degree days (GDD)",
       ylab = "Log ring width (mm)",
       frame = FALSE,
       main = spp_column_name)
  
  # color
  line_col <- renoir[spp_num]
  
  polygon(
    c(gddseq, rev(gddseq)),
    c(y_low, rev(y_high)),
    col = adjustcolor(line_col, alpha.f = 0.3),
    border = NA
  )
  
  lines(gddseq, y_mean, col = line_col, lwd = 3)
  
  # add tree id variation
  treeidspp <- treeid_spp$treeid_num[which(treeid_spp$spp_num == spp_num)]
  
  for (j in treeidspp) { # j = 46
    tree_col <- as.character(treeidvecnum[j])
    tree_col_name <- as.character(treeidvecname[j])
    y_post <- y_post_list[[tree_col]]
    
    # color line by spp
    tree_id_num <- as.integer(tree_col)
    
    # calculate mean and 50% credible interval (25%-75%)
    y_mean <- apply(y_post, 1, mean)
    y_low  <- apply(y_post, 1, quantile, 0.25)
    y_high <- apply(y_post, 1, quantile, 0.75)
    
    # Add atreid symbols
    tree_pos <- which(treeidspp == j) 
    sym <- treeidsymbol[tree_pos]
    
    treeidtempsymbol <- emp_spp[emp_spp$treeid_num == j,]
    
    points(treeidtempsymbol$pgsGDD5, treeidtempsymbol$loglength,
           pch = sym,
           col = line_col)
    
    # shaded interval
    polygon(c(gddseq, rev(gddseq)),
            c(y_low, # lower interval
              rev(y_high)), # high interval
            col = adjustcolor(line_col, alpha.f = 0.1),
            border = NA)
    
    # mean line
    lines(gddseq, y_mean,
          col = line_col,
          lwd = 1)
  }
}
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### GDD: per spp, non facetted #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# PDF output
jpeg(
  filename = "figures/growthModelsMain/growthModelSlopesperSppGDD.jpeg",
  width = 2400,      
  height = 2400,
  res = 300          
)

plot(empts$pgsGDD5, y, type = "n", 
     ylim = range(min(empts$loglength), max(empts$loglength)), 
     xlab = "Primary growing season GDD", ylab = "Log ring width (mm)",
     main = "species growth responses")

# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  y_post <- spp_post_list[[spp_column]]
  
  # color line by spp
  spp_num <- as.integer(spp_column)
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  line_col <- renoir[spp_num]
  
  # # shaded interval
  # polygon(c(gddseq, rev(gddseq),
  #         c(y_low, # lower interval
  #           rev(y_high)), # high interval
  #         col = adjustcolor(line_col, alpha.f = 0.1),
  #         border = NA))

  # mean line
  lines(gddseq, y_mean,
        col = line_col,
        lwd = 2)
  
  emp_spp <- empts[empts$spp_num == spp_num, ]

  # points(
  #   emp_spp$pgsGDD5,
  #   emp_spp$loglength,
  #   pch = 16,
  #   cex = 1,
  #   col = line_col)
  
  # legend(
  #   "topleft",
  #   legend = sppvecname,
  #   col = renoir[as.integer(sppvecnum)],
  #   lwd = 2,
  #   pch = 16,
  #   bty = "n",
  #   cex = 1.5
  # )
  
}
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GSL: Prep posterior reconstruction #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# start by filling a df with treeid intercepts only
atreeidsub_gsl <- subset(df_fitgsl, select = subyvec)
colnames(atreeidsub_gsl) <- 1:length(subyvec)

# the spp values for each tree id
treeid_aspp_gsl <- data.frame(matrix(ncol = ncol(atreeidsub_gsl), nrow = nrow(df_fitgsl)))
colnames(treeid_aspp_gsl) <- colnames(atreeidsub_gsl)

for (i in seq_len(ncol(treeid_aspp_gsl))) {
  tree_id <- as.integer(colnames(treeid_aspp_gsl)[i])
  spp_id <- treeid_spp$spp_num[match(tree_id, treeid_spp$treeid_num)]
  treeid_aspp_gsl[, i] <- aspp_df_gsl[, spp_id]
}
treeid_aspp_gsl

# recover a
treeid_a_gsl <- data.frame(matrix(ncol = ncol(atreeidsub_gsl), nrow = nrow(df_fitgsl)))
colnames(treeid_a_gsl) <- colnames(atreeidsub_gsl)

for (i in seq_len(ncol(treeid_a_gsl))) { # i = 1
  treeid_a_gsl[, i] <- df_fitgsl[, "a"]
}

# sum all 3 dfs together to get the full intercept for each treeid
fullintercept_gsl <-
  treeid_a_gsl + 
  atreeidsub_gsl +
  treeid_aspp_gsl 
fullintercept_gsl

# now get the slope for each treeid
treeid_bspp_gsl <- data.frame(matrix(ncol = ncol(atreeidsub_gsl), nrow = nrow(df_fitgsl)))
colnames(treeid_bspp_gsl) <- colnames(atreeidsub_gsl)

# back convert the slopes to their original scales
bspp_df4_gsl <- bspp_df_gsl
for (i in 1:ncol(bspp_df4_gsl)){
  bspp_df4_gsl[[i]] <- bspp_df4_gsl[[i]] / 10
}

for (i in seq_len(ncol(treeid_bspp_gsl))) { # i = 30
  tree_id <- as.integer(colnames(treeid_bspp_gsl)[i])
  spp_id <- treeid_spp$spp_num[match(tree_id, treeid_spp$treeid_num)]
  treeid_bspp_gsl[, i] <- bspp_df4_gsl[, spp_id]
}
treeid_bspp_gsl
gslseq <- seq(min(empts$pgsGSL), max(empts$pgsGSL), length.out = 100)  
y_post_list_gsl <- list()  # store posterior predictions in a list where each tree id gets matrix

# below I create a list where each row is the posterior estimate for each value of gdd (so the first row correspond to the model estimate for the first gdd value stored in x) and each column is the iteration (from 1 to 8000)
for (i in seq_along(treeidvecnum)) { # i = 1
  tree_col <- as.character(treeidvecnum[i]) 
  y_post <- sapply(1:nrow(df_fitgsl), function(f) {
    rnorm(length(gslseq), 
          fullintercept_gsl[f, tree_col] + treeid_bspp_gsl[f, tree_col] * gslseq, 
          sigma_df_gsl$sigma_y[f])
  })
  y_post_list_gsl[[tree_col]] <- y_post
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### GSL: per Spp, facet #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
mean_post_list_gsl <- list()
for (i in seq_along(treeidvecnum)) {
  tree_col <- as.character(treeidvecnum[i])
  mean_post_list_gsl[[tree_col]] <- sapply(1:nrow(df_fitgsl), function(f) {
    fullintercept_gsl[f, tree_col] + treeid_bspp_gsl[f, tree_col] * gslseq
    # no sigma_y yet
  })
}

# average the mean predictions across trees within species
spp_mean_list_gsl <- lapply(spp_list, function(tree_vec) {
  Reduce("+", mean_post_list_gsl[as.character(tree_vec)]) / length(tree_vec)
})

# re-simulate sigma on the averaged mean
spp_post_list_gsl <- lapply(spp_mean_list_gsl, function(mean_mat) {
  sapply(1:nrow(df_fitgsl), function(f) {
    rnorm(length(gslseq), mean_mat[, f], sigma_df_gsl$sigma_y[f])
  })
})

# jpeg output
jpeg(filename = "figures/growthModelsMain/growthModelSlopesperSppFacetGSL.jpeg",
     width = 2400, height = 2400, res = 300)
# Layout: 4 cols x 3 rows
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

treeidsymbol <- c(15, 16, 17, 8, 9)
# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  
  # define spp num
  spp_num <- as.integer(spp_column)
  
  # subset empirical data correctly
  emp_spp <- empts[empts$spp_num == spp_num, ]
  
  spp_column <- as.character(sppvecnum[i]) 
  y_post <- spp_post_list_gsl[[spp_column]]
  
  # summaries
  y_mean <- apply(y_post, 1, median)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # species-specific ylim
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$pgsGSL, emp_spp$loglength,
       type = "n",
       # ylim = ylim_spp,
       ylim = c(-1, 3),
       xlab = "Growing season length (days)",
       ylab = "Log ring width (mm)",
       frame = FALSE,
       main = spp_column_name) 
  
  # add panel letter 
  mtext(paste0("(", letters[i], ")"), 
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1)
  
  # color
  line_col <- renoir[spp_num]
  
  polygon(
    c(gslseq, rev(gslseq)),
    c(y_low, rev(y_high)),
    col = adjustcolor(line_col, alpha.f = 0.3),
    border = NA
  )
  
  lines(gslseq, y_mean, col = line_col, lwd = 2)
  
  # Add atreid symbols
  unique_trees <- unique(emp_spp$treeid_num)
  sym_per_row  <- treeidsymbol[match(emp_spp$treeid_num, unique_trees)]
  
  points(emp_spp$pgsGSL, emp_spp$loglength,
         pch = sym_per_row,
         cex = 1,
         col = line_col)
  
  points(
    emp_spp$pgsGSL,
    emp_spp$loglength,
    pch = 16,
    cex = 1,
    col = line_col
  )
}

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### SOS: Prep posterior reconstruction #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# start by filling a df with treeid intercepts only
atreeidsub_sos <- subset(df_fitsos, select = subyvec)
colnames(atreeidsub_sos) <- 1:length(subyvec)

# the spp values for each tree id
treeid_aspp_sos <- data.frame(matrix(ncol = ncol(atreeidsub_sos), nrow = nrow(df_fitsos)))
colnames(treeid_aspp_sos) <- colnames(atreeidsub_sos)

for (i in seq_len(ncol(treeid_aspp_sos))) {
  tree_id <- as.integer(colnames(treeid_aspp_sos)[i])
  spp_id <- treeid_spp$spp_num[match(tree_id, treeid_spp$treeid_num)]
  treeid_aspp_sos[, i] <- aspp_df_sos[, spp_id]
}
treeid_aspp_sos

# recover a
treeid_a_sos <- data.frame(matrix(ncol = ncol(atreeidsub_sos), nrow = nrow(df_fitsos)))
colnames(treeid_a_sos) <- colnames(atreeidsub_sos)

for (i in seq_len(ncol(treeid_a_sos))) { # i = 1
  treeid_a_sos[, i] <- df_fitsos[, "a"]
}

# sum all 3 dfs together to get the full intercept for each treeid
fullintercept_sos <-
  treeid_a_sos + 
  atreeidsub_sos +
  treeid_aspp_sos 
fullintercept_sos

# now get the slope for each treeid
treeid_bspp_sos <- data.frame(matrix(ncol = ncol(atreeidsub_sos), nrow = nrow(df_fitsos)))
colnames(treeid_bspp_sos) <- colnames(atreeidsub_sos)

# back convert the slopes to their original scales
bspp_df4_sos <- bspp_df_sos
for (i in 1:ncol(bspp_df4_sos)){
  bspp_df4_sos[[i]] <- bspp_df4_sos[[i]] / 5
}

for (i in seq_len(ncol(treeid_bspp_sos))) { # i = 30
  tree_id <- as.integer(colnames(treeid_bspp_sos)[i])
  spp_id <- treeid_spp$spp_num[match(tree_id, treeid_spp$treeid_num)]
  treeid_bspp_sos[, i] <- bspp_df4_sos[, spp_id]
}
treeid_bspp_sos

sosseq <- seq(min(empts$leafout), max(empts$leafout), length.out = 100)  
y_post_list_sos <- list()  # store posterior predictions in a list where each tree id gets matrix

# below I create a list where each row is the posterior estimate for each value of gdd (so the first row correspond to the model estimate for the first gdd value stored in x) and each column is the iteration (from 1 to 8000)
for (i in seq_along(treeidvecnum)) { # i = 1
  tree_col <- as.character(treeidvecnum[i]) 
  y_post <- sapply(1:nrow(df_fitsos), function(f) {
    rnorm(length(sosseq), 
          fullintercept_sos[f, tree_col] + treeid_bspp_sos[f, tree_col] * sosseq, 
          sigma_df_sos$sigma_y[f])
  })
  y_post_list_sos[[tree_col]] <- y_post
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### SOS: per Spp, facet #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
mean_post_list_sos <- list()
for (i in seq_along(treeidvecnum)) {
  tree_col <- as.character(treeidvecnum[i])
  mean_post_list_sos[[tree_col]] <- sapply(1:nrow(df_fitsos), function(f) {
    fullintercept_sos[f, tree_col] + treeid_bspp_sos[f, tree_col] * sosseq
    # no sigma_y yet
  })
}

# average the mean predictions across trees within species
spp_mean_list_sos <- lapply(spp_list, function(tree_vec) {
  Reduce("+", mean_post_list_sos[as.character(tree_vec)]) / length(tree_vec)
})

# re-simulate sigma on the averaged mean
spp_post_list_sos <- lapply(spp_mean_list_sos, function(mean_mat) {
  sapply(1:nrow(df_fitsos), function(f) {
    rnorm(length(sosseq), mean_mat[, f], sigma_df_sos$sigma_y[f])
  })
})

# jpeg output
jpeg(filename = "figures/growthModelsMain/growthModelSlopesperSppFacetSOS.jpeg",
     width = 2400, height = 2400, res = 300)
# Layout: 4 cols x 3 rows
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

treeidsymbol <- c(15, 16, 17, 8, 9)
# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  
  # define spp num
  spp_num <- as.integer(spp_column)
  
  # subset empirical data correctly
  emp_spp <- empts[empts$spp_num == spp_num, ]
  
  spp_column <- as.character(sppvecnum[i]) 
  y_post <- spp_post_list_sos[[spp_column]]
  
  # summaries
  y_mean <- apply(y_post, 1, median)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # species-specific ylim
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$leafout, emp_spp$loglength,
       type = "n",
       # ylim = ylim_spp,
       ylim = c(-1, 3),
       xlab = "Leafout day of year",
       ylab = "Log ring width (mm)",
       frame = FALSE,
       main = spp_column_name)
  
  # add panel letter 
  mtext(paste0("(", letters[i], ")"), 
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1)
  
  # color
  line_col <- renoir[spp_num]
  
  polygon(
    c(sosseq, rev(sosseq)),
    c(y_low, rev(y_high)),
    col = adjustcolor(line_col, alpha.f = 0.3),
    border = NA
  )
  
  lines(sosseq, y_mean, col = line_col, lwd = 2)
  
  # Add atreid symbols
  unique_trees <- unique(emp_spp$treeid_num)
  sym_per_row  <- treeidsymbol[match(emp_spp$treeid_num, unique_trees)]
  
  points(emp_spp$leafout, emp_spp$loglength,
         pch = sym_per_row,
         cex = 1,
         col = line_col)
  
  points(
    emp_spp$leafout,
    emp_spp$loglength,
    pch = 16,
    cex = 1,
    col = line_col
  )
}

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### EOS: Prep posterior reconstruction #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# start by filling a df with treeid intercepts only
atreeidsub_eos <- subset(df_fiteos, select = subyvec)
colnames(atreeidsub_eos) <- 1:length(subyvec)

# the spp values for each tree id
treeid_aspp_eos <- data.frame(matrix(ncol = ncol(atreeidsub_eos), nrow = nrow(df_fiteos)))
colnames(treeid_aspp_eos) <- colnames(atreeidsub_eos)

for (i in seq_len(ncol(treeid_aspp_eos))) {
  tree_id <- as.integer(colnames(treeid_aspp_eos)[i])
  spp_id <- treeid_spp$spp_num[match(tree_id, treeid_spp$treeid_num)]
  treeid_aspp_eos[, i] <- aspp_df_eos[, spp_id]
}
treeid_aspp_eos

# recover a
treeid_a_eos <- data.frame(matrix(ncol = ncol(atreeidsub_eos), nrow = nrow(df_fiteos)))
colnames(treeid_a_eos) <- colnames(atreeidsub_eos)

for (i in seq_len(ncol(treeid_a_eos))) { # i = 1
  treeid_a_eos[, i] <- df_fiteos[, "a"]
}

# sum all 3 dfs together to get the full intercept for each treeid
fullintercept_eos <-
  treeid_a_eos + 
  atreeidsub_eos +
  treeid_aspp_eos 
fullintercept_eos

# now get the slope for each treeid
treeid_bspp_eos <- data.frame(matrix(ncol = ncol(atreeidsub_eos), nrow = nrow(df_fiteos)))
colnames(treeid_bspp_eos) <- colnames(atreeidsub_eos)

# back convert the slopes to their original scales
bspp_df4_eos <- bspp_df_eos
for (i in 1:ncol(bspp_df4_eos)){
  bspp_df4_eos[[i]] <- bspp_df4_eos[[i]] / 10
}

for (i in seq_len(ncol(treeid_bspp_eos))) { # i = 30
  tree_id <- as.integer(colnames(treeid_bspp_eos)[i])
  spp_id <- treeid_spp$spp_num[match(tree_id, treeid_spp$treeid_num)]
  treeid_bspp_eos[, i] <- bspp_df4_eos[, spp_id]
}
treeid_bspp_eos

eosseq <- seq(min(empts$coloredLeaves), max(empts$coloredLeaves), length.out = 100)  
y_post_list_eos <- list()  # store posterior predictions in a list where each tree id gets matrix

# below I create a list where each row is the posterior estimate for each value of gdd (so the first row correspond to the model estimate for the first gdd value stored in x) and each column is the iteration (from 1 to 8000)
for (i in seq_along(treeidvecnum)) { # i = 1
  tree_col <- as.character(treeidvecnum[i]) 
  y_post <- sapply(1:nrow(df_fiteos), function(f) {
    rnorm(length(eosseq), 
          fullintercept_eos[f, tree_col] + treeid_bspp_eos[f, tree_col] * eosseq, 
          sigma_df_eos$sigma_y[f])
  })
  y_post_list_eos[[tree_col]] <- y_post
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### EOS: per Spp, facet #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
mean_post_list_eos <- list()
for (i in seq_along(treeidvecnum)) {
  tree_col <- as.character(treeidvecnum[i])
  mean_post_list_eos[[tree_col]] <- sapply(1:nrow(df_fiteos), function(f) {
    fullintercept_eos[f, tree_col] + treeid_bspp_eos[f, tree_col] * eosseq
    # no sigma_y yet
  })
}

# average the mean predictions across trees within species
spp_mean_list_eos <- lapply(spp_list, function(tree_vec) {
  Reduce("+", mean_post_list_eos[as.character(tree_vec)]) / length(tree_vec)
})

# re-simulate sigma on the averaged mean
spp_post_list_eos <- lapply(spp_mean_list_eos, function(mean_mat) {
  sapply(1:nrow(df_fiteos), function(f) {
    rnorm(length(eosseq), mean_mat[, f], sigma_df_eos$sigma_y[f])
  })
})

# jpeg output
jpeg(filename = "figures/growthModelsMain/growthModelSlopesperSppFacetEOS.jpeg",
     width = 2400, height = 2400, res = 300)
# Layout: 4 cols x 3 rows
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))

treeidsymbol <- c(15, 16, 17, 8, 9)
# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  
  # define spp num
  spp_num <- as.integer(spp_column)
  
  # subset empirical data correctly
  emp_spp <- empts[empts$spp_num == spp_num, ]
  
  spp_column <- as.character(sppvecnum[i]) 
  y_post <- spp_post_list_eos[[spp_column]]
  
  # summaries
  y_mean <- apply(y_post, 1, median)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  # species-specific ylim
  ylim_spp <- range(c(emp_spp$loglength, y_low, y_high), na.rm = TRUE)
  
  plot(emp_spp$coloredLeaves, emp_spp$loglength,
       type = "n",
       # ylim = ylim_spp,
       ylim = c(-1, 3),
       xlab = "Growing season length (days)",
       ylab = "Log ring width (mm)",
       frame = FALSE,
       main = spp_column_name)
  # add panel letter 
  mtext(paste0("(", letters[i], ")"), 
        side = 3, adj = 0, line = 0.3, font = 2, cex = 1)
  
  # color
  line_col <- renoir[spp_num]
  
  polygon(
    c(eosseq, rev(eosseq)),
    c(y_low, rev(y_high)),
    col = adjustcolor(line_col, alpha.f = 0.3),
    border = NA
  )
  
  lines(eosseq, y_mean, col = line_col, lwd = 2)
  
  # Add atreid symbols
  unique_trees <- unique(emp_spp$treeid_num)
  sym_per_row  <- treeidsymbol[match(emp_spp$treeid_num, unique_trees)]
  
  points(emp_spp$coloredLeaves, emp_spp$loglength,
         pch = sym_per_row,
         cex = 1,
         col = line_col)
  
  points(
    emp_spp$coloredLeaves,
    emp_spp$loglength,
    pch = 16,
    cex = 1,
    col = line_col
  )
}

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Mu plots #####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### bsp ###### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
jpeg(file = "figures/growthModelsMain/muALLbspp.jpeg",
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
plot(bspp_df2_ts$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5), 
     xlab = "Log ring width (mm) change in averaged GDD of 10 spring days", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts$fit_bspp_per5,  y_pos, bspp_df2_ts$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_ts$fit_bspp_per25, y_pos, bspp_df2_ts$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("Growing degree days", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 2: GSL
par(mar = custommar)
plot(bspp_df2_ts_gsl$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width (mm) change per 10 days of growing season length", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_gsl$fit_bspp_per5,  y_pos, bspp_df2_ts_gsl$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_ts_gsl$fit_bspp_per25, y_pos, bspp_df2_ts_gsl$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("Growing season length", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 3: SOS
par(mar = custommar)
plot(bspp_df2_ts_sos$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width (mm) change per 5 days of leafout", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_sos$fit_bspp_per5,  y_pos, bspp_df2_ts_sos$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_ts_sos$fit_bspp_per25, y_pos, bspp_df2_ts_sos$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("Start of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 4: EOS
par(mar = custommar)
plot(bspp_df2_ts_eos$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width (mm) change per 10 days of budset", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_eos$fit_bspp_per5,  y_pos, bspp_df2_ts_eos$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_ts_eos$fit_bspp_per25, y_pos, bspp_df2_ts_eos$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("End of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Slot 5: species legend
par(mar = c(1, 1, 1, 1))
plot.new()
legend("center",
       legend = sapply(unique(bspp_df2_ts$spp_name), 
                       function(x) parse(text = paste0("italic('", x, "')"))),
       col    = unique(colslatbi),
       pch    = 16, pt.cex = 1.5, bty = "n", cex = 1.2,
       title  = "Species", title.font = 2)

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### bspp with lines #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
jpeg(file = "figures/growthModelsMain/muALLbsppWlines.jpeg",
     width = 2800, height = 2800, res = 300)

layout(matrix(c(
  1, 5, 9,
  2, 6, 9,
  3, 7, 9,
  4, 8, 9
), nrow = 4, byrow = TRUE),
widths = c(1.3, 1, 0.5)) 

# Row 1, Col 1, Slot 5 : GDD
par(mar = custommar)
plot(bspp_df2_ts$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width (mm) change in averaged GDD of 10 spring days", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts$fit_bspp_per5,  y_pos, bspp_df2_ts$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_ts$fit_bspp_per25, y_pos, bspp_df2_ts$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("(a) Growing degree days", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 2, Col 1, Slot 6 : GSL
par(mar = custommar)
plot(bspp_df2_ts_gsl$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width (mm) change per 10 days of GSL", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_gsl$fit_bspp_per5,  y_pos, bspp_df2_ts_gsl$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_ts_gsl$fit_bspp_per25, y_pos, bspp_df2_ts_gsl$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("(b) Growing season length", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 3, Col 1, Slot 7 : SOS
par(mar = custommar)
plot(bspp_df2_ts_sos$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width (mm) change per 5 days of leafout", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_sos$fit_bspp_per5,  y_pos, bspp_df2_ts_sos$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_ts_sos$fit_bspp_per25, y_pos, bspp_df2_ts_sos$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("(c) Start of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 4, Col 1, Slot 8 : EOS
par(mar = custommar)
plot(bspp_df2_ts_eos$fit_bspp, y_pos,
     xlim = c(-0.8, 0.8), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width (mm) change per 10 days of coloredLeaves", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_eos$fit_bspp_per5,  y_pos, bspp_df2_ts_eos$fit_bspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(bspp_df2_ts_eos$fit_bspp_per25, y_pos, bspp_df2_ts_eos$fit_bspp_per75, y_pos,
         col = colslatbi, lwd = 3)
mtext("(d) End of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 1, Col 2, Slot 5 : GDD
par(mar = custommar)
plot(empts$pgsGDD5, y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)), 
     xlab = "Growing season growing degree days (GDD)", ylab = "Log ring width (mm)",
     main = "")
mtext("(e)", side = 3, adj = 0, font = 2, cex = 0.9)

# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  y_post <- spp_post_list[[spp_column]]
  
  # color line by spp
  spp_num <- as.integer(spp_column)
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post, 1, mean)
  y_low  <- apply(y_post, 1, quantile, 0.25)
  y_high <- apply(y_post, 1, quantile, 0.75)
  
  line_col <- colslatbi[spp_num]
  
  polygon(c(gddseq, rev(gddseq)),
          c(y_low, rev(y_high)), # low and high interval
          col = adjustcolor(line_col, alpha.f = 0.1), border = NA)
  
  lines(gddseq, y_mean,
        col = line_col,
        lwd = 2)
  
  emp_spp <- empts[empts$spp_num == spp_num, ]
}

# Row 2, Col 2, Slot 6 : GSL
par(mar = custommar)
plot(empts$pgsGSL, y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)), 
     xlab = "Growing season length (days)", ylab = "Log ring width (mm)",
     main = "")
mtext("(f)", side = 3, adj = 0, font = 2, cex = 0.9)

# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  y_post_gsl <- spp_post_list_gsl[[spp_column]]
  
  # color line by spp
  spp_num <- as.integer(spp_column)
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post_gsl, 1, mean)
  y_low  <- apply(y_post_gsl, 1, quantile, 0.25)
  y_high <- apply(y_post_gsl, 1, quantile, 0.75)
  
  line_col <- colslatbi[spp_num]
  
  polygon(c(gslseq, rev(gslseq)),
          c(y_low, rev(y_high)), # low and high interval
          col = adjustcolor(line_col, alpha.f = 0.1), border = NA)
  
  lines(gslseq, y_mean,
        col = line_col,
        lwd = 2)
  
  emp_spp <- empts[empts$spp_num == spp_num, ]
}

# Row 3, Col 2, Slot 7 : SOS
par(mar = custommar)
plot(empts$leafout, y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)), 
     xlab = "Leafout day of year", ylab = "Log ring width (mm)",
     main = "")
mtext("(g)", side = 3, adj = 0, font = 2, cex = 0.9)

# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  y_post_sos <- spp_post_list_sos[[spp_column]]
  
  # color line by spp
  spp_num <- as.integer(spp_column)
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post_sos, 1, mean)
  y_low  <- apply(y_post_sos, 1, quantile, 0.25)
  y_high <- apply(y_post_sos, 1, quantile, 0.75)
  
  line_col <- colslatbi[spp_num]
  
  polygon(c(sosseq, rev(sosseq)),
          c(y_low, rev(y_high)), # low and high interval
          col = adjustcolor(line_col, alpha.f = 0.1), border = NA)
  
  lines(sosseq, y_mean,
        col = line_col,
        lwd = 2)
  
  emp_spp <- empts[empts$spp_num == spp_num, ]
}

# Row 4, Col 2, Slot 8 : EOS
par(mar = custommar)
plot(empts$coloredLeaves, y, type = "n", frame = FALSE,
     ylim = range(min(empts$loglength), max(empts$loglength)), 
     xlab = "coloredLeaves day of year", ylab = "Log ring width (mm)",
     main = "")
mtext("(h)", side = 3, adj = 0, font = 2, cex = 0.9)

# Loop over trees again to plot each tree individually
for (i in seq_along(sppvecnum)) { # i = 1
  spp_column <- as.character(sppvecnum[i])
  spp_column_name <- as.character(sppvecname[i])
  y_post_eos <- spp_post_list_eos[[spp_column]]
  
  # color line by spp
  spp_num <- as.integer(spp_column)
  
  # calculate mean and 50% credible interval (25%-75%)
  y_mean <- apply(y_post_eos, 1, mean)
  y_low  <- apply(y_post_eos, 1, quantile, 0.25)
  y_high <- apply(y_post_eos, 1, quantile, 0.75)
  
  line_col <- colslatbi[spp_num]
  
  polygon(c(eosseq, rev(eosseq)),
          c(y_low, rev(y_high)), # low and high interval
          col = adjustcolor(line_col, alpha.f = 0.1), border = NA)
  
  lines(eosseq, y_mean,
        col = line_col,
        lwd = 2)
  
  emp_spp <- empts[empts$spp_num == spp_num, ]
}

# Slot 9: species legend
par(mar = c(1, 1, 1, 1))
plot.new()
legend("center",
       legend = sapply(unique(bspp_df2_ts$spp_name), 
                       function(x) parse(text = paste0("italic('", x, "')"))),
       col    = unique(colslatbi),
       pch    = 16, pt.cex = 1.5, bty = "n", cex = 1.2,
       title  = "Species", title.font = 2)

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
###### asp ######
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
jpeg(file = "figures/growthModelsMain/muALLaspp.jpeg",
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
plot(aspp_df2_ts$fit_aspp, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_ts$fit_aspp_per5,  y_pos, aspp_df2_ts$fit_aspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(aspp_df2_ts$fit_aspp_per25, y_pos, aspp_df2_ts$fit_aspp_per75, y_pos,
         col = colslatbi, lwd = 3)
abline(v = 0, lty = 2, col = "black")
mtext("Growing degree days", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 2: GSL
par(mar = custommar)
plot(aspp_df2_ts_gsl$fit_aspp, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_ts_gsl$fit_aspp_per5,  y_pos, aspp_df2_ts_gsl$fit_aspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(aspp_df2_ts_gsl$fit_aspp_per25, y_pos, aspp_df2_ts_gsl$fit_aspp_per75, y_pos,
         col = colslatbi, lwd = 3)
abline(v = 0, lty = 2, col = "black")
mtext("Growing season length", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 3: SOS
par(mar = custommar)
plot(aspp_df2_ts_sos$fit_aspp, y_pos,
     xlim = c(-10, 10),ylim = c(0.5, n_spp + 0.5),
     xlab = "Log ring width intercept values (mm)", ylab = "", 
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_ts_sos$fit_aspp_per5,  y_pos, aspp_df2_ts_sos$fit_aspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(aspp_df2_ts_sos$fit_aspp_per25, y_pos, aspp_df2_ts_sos$fit_aspp_per75, y_pos,
         col = colslatbi, lwd = 3)
abline(v = 0, lty = 2, col = "black")
mtext("Start of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Row 4: EOS
par(mar = custommar)
plot(aspp_df2_ts_eos$fit_aspp, y_pos,
     xlim = c(-10, 10), ylim = c(0.5, n_spp + 0.5), 
     xlab = "Log ring width intercept values (mm)", ylab = "", 
     yaxt = "n", pch = 16, cex = 2, col = colslatbi, frame.plot = FALSE,      
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(aspp_df2_ts_eos$fit_aspp_per5,  y_pos, aspp_df2_ts_eos$fit_aspp_per95, y_pos,
         col = colslatbi, lwd = 1.5)
segments(aspp_df2_ts_eos$fit_aspp_per25, y_pos, aspp_df2_ts_eos$fit_aspp_per75, y_pos,
         col = colslatbi, lwd = 3)
abline(v = 0, lty = 2, col = "black")
mtext("End of season", side = 3, adj = 0, font = 2, cex = 0.9)

# Slot 5: species legend
par(mar = c(1, 1, 1, 1))
plot.new()
legend("center",
       legend = sapply(unique(aspp_df2_ts$spp_name), 
                       function(x) parse(text = paste0("italic('", x, "')"))),
       col    = unique(colslatbi),
       pch    = 16, pt.cex = 1.5, bty = "n", cex = 1.2,
       title  = "Species", title.font = 2)

dev.off()


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
###### full treeid mu plots intercept ###### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
treeid_df2_ts$treeid <- as.numeric(treeid_df2_ts$treeid)
treeid_df2_ts$treeid_name <- empts$id[match(treeid_df2_ts$treeid, empts$treeid_num)]

# now do the same, but for species
treeid_df2_ts$spp <- empts$latbi[match(treeid_df2_ts$treeid, empts$treeid_num)]

sub <- subset(empts, select = c("treeid_num", "spp_num"))
sub <- sub[!duplicated(sub$treeid_num),]
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# sum atreeid, asp, asite
fullintercept2 <-
  atreeid_gdd +
  treeid_aspp_gdd 
fullintercept2
fullatreeid2 <- as.data.frame(fullintercept2)

# empty treeid dataframe
treeid_df4 <- data.frame(
  treeid = character(ncol(fullintercept2)),
  fit_atreeid = numeric(ncol(fullintercept2)),  
  fit_atreeid_per5 = NA, 
  fit_atreeid_per25 = NA,
  fit_atreeid_per75 = NA,
  fit_atreeid_per95 = NA
)
for (i in 1:ncol(fullintercept2)) { # i = 1
  treeid_df4$treeid[i] <- colnames(fullintercept2)[i]         
  treeid_df4$fit_atreeid[i] <- round(mean(fullintercept2[[i]]),3)  
  treeid_df4$fit_atreeid_per5[i] <- round(quantile(fullintercept2[[i]], probs = 0.05), 3)
  treeid_df4$fit_atreeid_per25[i] <- round(quantile(fullintercept2[[i]], probs = 0.25), 3)
  treeid_df4$fit_atreeid_per75[i] <- round(quantile(fullintercept2[[i]], probs = 0.75), 3)
  treeid_df4$fit_atreeid_per95[i] <- round(quantile(fullintercept2[[i]], probs = 0.95), 3)
}
treeid_df4

# get the og treeid names, spp and site back:
treeid_df4$treeid <- as.numeric(treeid_df4$treeid)
treeid_df4$treeid_name <- empts$id[match(treeid_df4$treeid, empts$treeid_num)]
treeid_df4$spp_name <- empts$latbi[match(treeid_df4$treeid, empts$treeid_num)]

# Plot!
pdf(
  file = "figures/growthModelsMain/meanPlotGrowthGDD_treeidBYspp.pdf",
  width = 9,  
  height = 10
)
par(mar = c(
  4, 
  6, 
  6, 
  5))

# define a gap between species clusters
gap <- 2

# y positions
current_y <- 1

treeid_df4$spp  <- factor(treeid_df4$spp_name, levels = species_order)

treeid_df4 <- treeid_df4[
  order(treeid_df4$spp, treeid_df4$treeid),
]

treeid_df4$y_pos <- seq_len(nrow(treeid_df4))

total_rows <- nrow(treeid_df4) + (length(species_order) - 1) * gap
current_y <- total_rows  # start at top

for(sp in species_order){
  idx <- which(treeid_df4$spp == sp)
  n <- length(idx)
  
  # assign positions counting downward
  treeid_df4$y_pos[idx] <- current_y:(current_y - n + 1)
  
  # move cursor down with a gap
  current_y <- current_y - n - gap
}


# Set up empty plot
plot(
  NA, NA,
  xlim = range(c(treeid_df4$fit_atreeid_per5-0.5,
                 treeid_df4$fit_atreeid_per95+0.5)),
  ylim = c(0.5, max(treeid_df4$y_pos) + 0.5),
  xlab = "treeid intercept values",
  ylab = "",
  # frame.plot = FALSE,
  bty = "l",
  yaxt = "n"  
)

# Add horizontal error bars (5–95%)
segments(
  x0 = treeid_df4$fit_atreeid_per5,
  x1 = treeid_df4$fit_atreeid_per95,
  y0 = treeid_df4$y_pos,
  col = adjustcolor(colslatbi[treeid_df4$spp], alpha.f = 0.7),
  lwd = 1
)

# --- Add thicker horizontal error bars (25–75%) ---
segments(
  x0 = treeid_df4$fit_atreeid_per25,
  x1 = treeid_df4$fit_atreeid_per75,
  y0 = treeid_df4$y_pos,
  col = adjustcolor(colslatbi[treeid_df4$spp], alpha.f = 0.7),
  lwd = 1.5
)

# --- Add the points ---
points(
  treeid_df4$fit_atreeid,
  treeid_df4$y_pos,
  cex = 0.8,
  pch = 16,
  col = adjustcolor(colslatbi[treeid_df4$spp], alpha.f = 0.7)
)

spp_y_top <- tapply(treeid_df4$y_pos, treeid_df4$spp, max)
aspp_df2_ts$y_pos <- spp_y_top[aspp_df2_ts$spp] + 1

segments(
  x0 = aspp_df2_ts$fit_aspp_per5,
  x1 = aspp_df2_ts$fit_aspp_per95,
  y0 = aspp_df2_ts$y_pos,
  col = adjustcolor(colslatbi[aspp_df2_ts$spp_name], alpha.f = 0.9),
  lwd = 2
)

segments(
  x0 = aspp_df2_ts$fit_aspp_per25,
  x1 = aspp_df2_ts$fit_aspp_per75,
  y0 = aspp_df2_ts$y_pos,
  col = colslatbi[aspp_df2_ts$spp],
  lwd = 3
)
points(
  aspp_df2_ts$fit_aspp,
  aspp_df2_ts$y_pos,
  pch = 16,
  col  = colslatbi[aspp_df2_ts$spp],
  # col = "black",
  cex = 1.5
)

# Add vertical line at 0
abline(v = 0, lty = 2)

# Add custom y-axis labels (reverse order if needed)
axis(
  side = 2,
  at = treeid_df4$y_pos,
  labels = treeid_df4$treeid_name,
  cex.axis = 0.5,
  las = 1
)
# spp mean
spp_y <- tapply(treeid_df4$y_pos, treeid_df4$spp, mean)
species_legend_order <- names(sort(spp_y, decreasing = TRUE))

## species legend (colors matched by name)
legend(
  x = max(treeid_df4$fit_atreeid_per95) - 5,
  y = max(treeid_df4$y_pos) -16,
  legend = species_legend_order,
  col = colslatbi[species_legend_order],    # index so colors match
  pch = 16,
  pt.cex = 1.2,
  title = "Species",
  bty = "n"
)
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
site_df2_z   <- extract_params(df_fitgdd, "asite", "fit_a_site", 
                               "site", "asite\\[(\\d+)\\]")


treeid_df2_z$treeid_name <- emp$treeid[match(treeid_df2_z$treeid, emp$treeid_num)]
bspp_df2_z$spp_name <- emp$latbi[match(bspp_df2_z$spp, emp$spp_num)]
site_df2_z$site_name <- emp$site[match(site_df2_z$site, emp$site_num)]
aspp_df2_z$spp_name <- emp$latbi[match(aspp_df2_z$spp, emp$spp_num)]

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
site_df2_z_gsl   <- extract_params(df_fitgsl, "asite", "fit_a_site", 
                                   "site", "asite\\[(\\d+)\\]")

treeid_df2_z_gsl$treeid <- as.numeric(treeid_df2_z_gsl$treeid)
treeid_df2_z_gsl$treeid_name <- emp$treeid[match(treeid_df2_z_gsl$treeid, emp$treeid_num)]
bspp_df2_z_gsl$spp_name <- emp$latbi[match(bspp_df2_z_gsl$spp, emp$spp_num)]
site_df2_z_gsl$site_name <- emp$site[match(site_df2_z_gsl$site, emp$site_num)]
aspp_df2_z_gsl$spp_name <- emp$latbi[match(aspp_df2_z_gsl$spp, emp$spp_num)]

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
site_df2_z_sos   <- extract_params(df_fitsos, "asite", "fit_a_site", 
                                   "site", "asite\\[(\\d+)\\]")

treeid_df2_z_sos$treeid <- as.numeric(treeid_df2_z_sos$treeid)
treeid_df2_z_sos$treeid_name <- emp$treeid[match(treeid_df2_z_sos$treeid, emp$treeid_num)]
bspp_df2_z_sos$spp_name <- emp$latbi[match(bspp_df2_z_sos$spp, emp$spp_num)]
site_df2_z_sos$site_name <- emp$site[match(site_df2_z_sos$site, emp$site_num)]
aspp_df2_z_sos$spp_name <- emp$latbi[match(aspp_df2_z_sos$spp, emp$spp_num)]

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
site_df2_z_eos   <- extract_params(df_fiteos, "asite", "fit_a_site", 
                                   "site", "asite\\[(\\d+)\\]")

treeid_df2_z_eos$treeid <- as.numeric(treeid_df2_z_eos$treeid)
treeid_df2_z_eos$treeid_name <- emp$treeid[match(treeid_df2_z_eos$treeid, emp$treeid_num)]
bspp_df2_z_eos$spp_name <- emp$latbi[match(bspp_df2_z_eos$spp, emp$spp_num)]
site_df2_z_eos$site_name <- emp$site[match(site_df2_z_eos$site, emp$site_num)]
aspp_df2_z_eos$spp_name <- emp$latbi[match(aspp_df2_z_eos$spp, emp$spp_num)]

# Add predictors and bind
bspp_df2_z$pred <- "GDD"
bspp_df2_z_gsl$pred <- "GSL"
bspp_df2_z_sos$pred <- "SOS"
bspp_df2_z_eos$pred <- "EOS"
bspp_z_binded <- rbind(bspp_df2_z, bspp_df2_z_gsl, bspp_df2_z_sos, bspp_df2_z_eos)

arub <- subset(bspp_z_binded, spp_name %in% "Acer rubrum")
asac <- subset(bspp_z_binded, spp_name %in% "Acer saccharum")
afla <- subset(bspp_z_binded, spp_name %in% "Aesculus flava")
ball <- subset(bspp_z_binded, spp_name %in% "Betula alleghaniensis")
bnig <- subset(bspp_z_binded, spp_name %in% "Betula nigra")
cgla <- subset(bspp_z_binded, spp_name %in% "Carya glabra")
cova <- subset(bspp_z_binded, spp_name %in% "Carya ovata")
pdel <- subset(bspp_z_binded, spp_name %in% "Populus deltoide")
qalb <- subset(bspp_z_binded, spp_name %in% "Quercus alba")
qrub <- subset(bspp_z_binded, spp_name %in% "Quercus rubra")
tame <- subset(bspp_z_binded, spp_name %in% "Tilia americana")

"Acer rubrum"
"Acer saccharum"
"Aesculus flava"
"Betula alleghaniensis"
"Betula nigra"
"Carya glabra"
"Carya ovata"
"Populus deltoide"
"Quercus alba"
"Quercus rubra"
"Tilia americana"
