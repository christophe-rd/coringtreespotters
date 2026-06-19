# coringtreespotters empirical data plots!
# CRD 4 December 2025
# Start plotting empirical data for coringtreespotters

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 
options(digits = 5)
# quartz()

# Load library 
library(ggplot2)
library(rstan)
library(future)
library(wesanderson)
library(patchwork)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(zoo)

setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")
source('/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/rcode/tools.R')

# flags
makeggplots <- FALSE

empts <- read.csv("output/empiricalDataMAIN.csv")
gdd <- read.csv("output/gddByYear.csv")
allringwidths <- read.csv("output/ringWidthTS.csv")
longtermgdd <- read.csv("output/longTermGDDperYear.csv")
longtermgdd5yr <- read.csv("output/longTermGDD5YrAvg.csv")

allringwidths$lengthMM <- allringwidths$lengthCM * 10

# from arboretum website most of the info is in the above dfs, just in case I want to look at other stuff
coord <- read.csv("input/listTreesfromInteractiveMap.csv", header=TRUE)

empts$accessionYear <- as.numeric(substr(empts$accessionDate, 7,11))

# now do the same for ringwidth DO THIS
length <- aggregate(lengthMM ~ id + yearCor, allringwidths, FUN = mean)
length$idyear <- paste(length$id, length$yearCor)

allringwidths$idyear <- paste(allringwidths$id, allringwidths$yearCor)

allringwidths2 <- allringwidths[!duplicated(allringwidths$idyear),]

allringwidths2$lengthMM <- length$lengthMM[match(allringwidths2$idyear, length$idyear)]

allringwidths2$accessionYear <- empts$accessionYear[match(allringwidths2$id, empts$id)]

allringwidths2$age <- allringwidths2$yearCor - allringwidths2$accessionYear

empts$plantNickname <- paste(empts$latbi, empts$plantNickname, sep = " ")
# ringwidth X GDD in PGS
renoir <- c("#17154f", "#2f357c", "#6c5d9e", "#9d9cd5", "#b0799a", "#e48171", "#bf3729", "#e69b00", "#f5bb50", "#ada43b", "#355828")

if (makeggplots) {
subsetbass <- subset(empts, latbi == "Tilia americana")
subsetbass$DBH
ggplot(empts, aes(x = pgsGDD10, y = lengthMM, 
                color = latbi, 
                fill = latbi)) +
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = renoir) +
  scale_fill_manual(values = renoir) +
  facet_wrap(~latbi) +
  labs(y = "Ring width (mm)", x = "Growing degree days (GDD)", color = "Tree Species") +
  theme_bw() +
  theme(legend.key.height = unit(1.5, "lines"),
        strip.text = element_text(face = "bold.italic", size = 10)) +
  guides(fill = "none", color = "none") 
ggsave("figures/empiricalData/sppLinearRegressions_pgsGDD.jpeg", width = 8, height = 6, units = "in", dpi = 300)

# plot just basswood
subsetbass <- subset(empts, latbi == "Tilia americana")
ggplot(subsetbass, aes(x = pgsGDD10, y = lengthMM, 
                color = id, 
                fill = id)) +
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = renoir) +
  scale_fill_manual(values = renoir) +
  facet_wrap(~latbi) +
  labs(y = "Ring width (mm)", x = "Growing degree days (GDD)", color = "Tree Species") +
  theme_bw() +
  theme(legend.key.height = unit(1.5, "lines"),
        strip.text = element_text(face = "bold.italic", size = 10)) +
  guides(fill = "none", color = "none") 
ggsave("figures/empiricalData/sppLinearRegressions_pgsGDD_TIAM.jpeg", width = 8, height = 6, units = "in", dpi = 300)

empts$year2 <- as.factor(empts$year)


}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot ring width x age at ring, 1 page per species ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# FOR WHOLE CHRONOLOGY
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
emp2 <- empts
emp2$treeAge <- emp2$year - emp2$accessionYear
allringwidths2$treeAge <- allringwidths2$yearCor - allringwidths2$accessionYear

# remove the negative ages:
vec <- unique(allringwidths2$id[which(allringwidths2$treeAge < 0)])
negativesages <- subset(allringwidths2, (id %in% vec))
negativesages
coord$accessionyear <-  substr(coord$Accession.Date,7,10)
coord$Received.How[which(coord$accessionyear %in% unique(negativesages$accessionYear))]
coord$Received.As[which(coord$accessionyear %in% unique(negativesages$accessionYear))]
coord$Plant.ID[which(coord$accessionyear %in% unique(negativesages$accessionYear))]
allringwidths2 <- subset(allringwidths2, !(id %in% vec))

# mean age of tree
age <- aggregate(treeAge ~ id, allringwidths2, FUN = mean)
allringwidths2$meanAge <- age$treeAge[match(allringwidths2$id, age$id)]
allringwidths2 <- allringwidths2[order(allringwidths2$meanAge),]
allringwidths2 <- allringwidths2[order(allringwidths2$id),]
allringwidths2$latbi <- empts$latbi[match(allringwidths2$id, empts$id)]
allringwidths2$plantNickname <- empts$plantNickname[match(allringwidths2$id, empts$id)]

# add 5 year gdd average
allringwidths2$gdd <- longtermgdd5yr$GDD_moving_avg[match(allringwidths2$yearCor, longtermgdd5yr$year)]

pdf("figures/empiricalData/ringwidthXyear_bySpeciesAllYrs.pdf", width = 10, height = 6)
species_list <- unique(allringwidths2$latbi)
renoir_named <- setNames(renoir, species_list)

# Loop over each species
for (sp in species_list) { # sp = "Sugar maple"
  df_sp <- allringwidths2[allringwidths2$latbi == sp, ]
  ids   <- unique(df_sp$plantNickname)
  par(
    mfrow = n2mfrow(length(ids)),
    mar = c(4, 4, 2, 4), 
    oma = c(2, 2, 2, 1)
  )
  ylim <- range(df_sp$lengthMM, na.rm = TRUE)
  
  # Loop over each individual tree
  for (id_i in ids) { # id_i = "Sugar maple 187-2006*B"
    d <- df_sp[df_sp$plantNickname == id_i, ]
    fit <- lm(lengthMM ~ yearCor, d)
    xlim <- range(d$yearCor, na.rm = TRUE)
    ylim_gdd <- range(d$gdd, na.rm = TRUE)  
    
    d <- d[order(d$yearCor), ]
    
    plot(
      d$yearCor, d$gdd,
      type = "l",
      col = adjustcolor("#B40F20", alpha.f = 0.4),
      lwd = 1,
      lty = 1,
      xlim = xlim,
      ylim = ylim_gdd,
      axes = FALSE,
      xlab = "",
      ylab = ""
    )
    axis(4, col = "#B40F20", col.axis = "#B40F20")
    mtext("Growing degree days (GDD)", side = 4, line = 2.5, col = "#B40F20", cex = 0.7)
    
    legend("topleft", 
           legend = c("Ring width", "GDD"),
           col = c("black", "#B40F20"),
           lty = c(2, 2),
           pch = c(NA, NA),
           lwd = c(2, 2),
           cex = 0.7,
           bty = "n")
    
    mean_age <- mean(d$meanAge, na.rm = TRUE)
    text(
      x = xlim[2] - 0.05 * diff(xlim),
      y = ylim[2] - 0.05 * diff(ylim),
      labels = paste("Mean age =", round(mean_age, 1)),
      adj = c(1, 1),
      cex = 1
    )
    par(new = TRUE)
    plot(
      d$yearCor, d$lengthMM,
      type = "l",
      pch = 16,
      cex = 1.2,
      lwd = 1.5,
      col = "black",
      xlim = xlim,
      ylim = ylim,
      xlab = "Calendar year",
      ylab = "Ring width (mm)",
      main = paste(id_i)
    )
    abline(fit, col = "black", lwd = 1)
  }
  mtext(sp, side = 3, outer = TRUE, line = 0.5, cex = 1.4, font = 2)
} 
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# FOR CHRONOLOGY THE STUDY PERIOD (9 YEARS)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
allringwidths3 <- subset(allringwidths2, yearCor >2015)

# PLOT!.
pdf("figures/empiricalData/ringwidthXage_bySpecies9Yrs.pdf", width = 10, height = 6)

species_list <- unique(allringwidths3$latbi)
renoir_named <- setNames(renoir, species_list)

# here I loop over each page that fits 1 species at a time
for (sp in species_list) {
  df_sp <- allringwidths3[allringwidths3$latbi == sp, ]
  ids   <- unique(df_sp$id)
  par(
    mfrow = n2mfrow(length(ids)),
    mar = c(4, 4, 2, 1),
    oma = c(2, 2, 2, 1)
  )
  ylim <- range(df_sp$lengthMM, na.rm = TRUE)
  
  # here I loop over each individuals of each species
  for (id_i in ids) {
    d <- df_sp[df_sp$id == id_i, ]
    fit <- lm(lengthMM ~ treeAge, d)
    xlim <- range(d$treeAge, na.rm = TRUE)
    plot(
      d$treeAge, d$lengthMM,
      pch = 16,
      cex = 0.9,
      col = renoir_named[sp],
      xlim = xlim,
      ylim = ylim,
      xlab = "Tree age at ring",
      ylab = "Ring width (mm)",
      main = paste(id_i)
    )
    abline(fit, col = "black", lwd = 1)
    mean_age <- mean(d$meanAge, na.rm = TRUE)
    text(
      x = xlim[2] - 0.05 * diff(xlim),   # right side
      y = ylim[2] - 0.05 * diff(ylim),   # top
      labels = paste("Mean age =", round(mean_age, 1)),
      adj = c(1, 1),                     # right + top alignment
      cex = 1
    )
  }
  mtext(sp, side = 3, outer = TRUE, line = 0.5, cex = 1.4, font = 2)
}

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# FOR CHRONOLOGY THE STUDY PERIOD (9 YEARS) 1 PAGE
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
allringwidths3 <- subset(allringwidths2, yearCor >2015)
pdf("figures/empiricalData/RWXage_bySpp9Yrs1page.pdf", width = 10, height = 10)

ids   <- unique(allringwidths3$id)
par(mfrow = c(7,7),
    cex.main = 0.7,      
    mar = c(2, 2, 2, 0.5))
for (id_i in ids) {
  d <- allringwidths3[allringwidths3$id == id_i, ]
  fit <- lm(lengthMM ~ treeAge, d)
  xlim <- range(d$treeAge, na.rm = TRUE)
  ylim <- range(d$lengthMM, na.rm = TRUE)
  
  sp <- d$latbi[1]
  spp_col <- renoir_named[sp]
  
  plot(
    d$treeAge, d$lengthMM,
    pch = 16,
    cex = 0.9,
    col = spp_col,
    xlim = xlim,
    ylim = ylim,
    frame = FALSE,
    # xlab = "Tree age at ring",
    # ylab = "Ring width (mm)",
    xlab = "",
    ylab = "",
    main = paste(id_i)
  )
  abline(fit, col = "black", lwd = 1)
  mean_age <- mean(d$meanAge, na.rm = TRUE)
  text(
    x = xlim[2] - 0.05 * diff(xlim),   # right side
    y = ylim[2] - 0.05 * diff(ylim),   # top
    labels = paste("Mean age =", round(mean_age, 1)),
    adj = c(1, 1),                     # right + top alignment
    cex = 0.7
  )
}
dev.off()

# Fill in the regression coefficients
rwagecoef <- data.frame(id = unique(allringwidths3$id), 
                        latbi = NA, 
                        intercept = NA,
                        slope = NA)
rwagecoef$latbi <- allringwidths3$latbi[match(rwagecoef$id, allringwidths3$id)]

for (i in unique(rwagecoef$id)) { # i = "BEAL_12843_A"
  d <- allringwidths3[allringwidths3$id == i, ]
  fit <- lm(lengthMM ~ treeAge, d)
  rwagecoef$intercept[rwagecoef$id == i] <- fit$coefficients[1]
  rwagecoef$slope[rwagecoef$id == i] <- fit$coefficients[2]
}

# count how many have slopes lower than 0 and >0
nrow(rwagecoef[which(rwagecoef$slope < 0),])
nrow(rwagecoef[which(rwagecoef$slope > 0),])

# Fill in the regression coefficients FOR ALL YEARS
rwagecoefAll <- data.frame(id = unique(allringwidths2$id), 
                        latbi = NA, 
                        intercept = NA,
                        slope = NA)
rwagecoefAll$latbi <- allringwidths2$latbi[match(rwagecoefAll$id, allringwidths2$id)]

for (i in unique(rwagecoefAll$id)) { # i = "BEAL_12843_A"
  d <- allringwidths2[allringwidths2$id == i, ]
  fit <- lm(lengthMM ~ treeAge, d)
  rwagecoefAll$intercept[rwagecoefAll$id == i] <- fit$coefficients[1]
  rwagecoefAll$slope[rwagecoefAll$id == i] <- fit$coefficients[2]
}

# count how many have slopes lower than 0 and >0
nrow(rwagecoefAll[which(rwagecoefAll$slope < 0),])
nrow(rwagecoefAll[which(rwagecoefAll$slope > 0),])

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# FOR CHRONOLOGY WITH PHENODATA
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
emp2 <- empts[!is.na(empts$lengthMM),]

emp2$treeAge <- emp2$year - emp2$accessionYear
# remove the the negative ages referencing the previous vector with negative ages ids
emp2 <- subset(emp2, !(id %in% vec))

# mean age of tree
age <- aggregate(treeAge ~ id, emp2, FUN = mean)
emp2$meanAge <- age$treeAge[match(emp2$id, age$id)]

emp2 <- emp2[order(emp2$meanAge),]

emp2$latbi <- empts$latbi[match(emp2$id, empts$id)]

# PLOT!.
pdf("figures/empiricalData/ringwidthXage_bySpeciesOnlyLeafout.pdf", width = 10, height = 6)

species_list <- unique(emp2$latbi)
renoir_named <- setNames(renoir, species_list)

# here I loop over each page that fits 1 species at a time
for (sp in species_list) {
  df_sp <- emp2[emp2$latbi == sp, ]
  ids   <- unique(df_sp$id)
  par(
    mfrow = n2mfrow(length(ids)),
    mar = c(4, 4, 2, 1),
    oma = c(2, 2, 2, 1)
  )
  ylim <- range(df_sp$lengthMM, na.rm = TRUE)
  
  # here I loop over each individuals of each species
  for (id_i in ids) {
    d <- df_sp[df_sp$id == id_i, ]
    fit <- lm(lengthMM ~ treeAge, d)
    xlim <- range(d$treeAge, na.rm = TRUE)
    plot(
      d$treeAge, d$lengthMM,
      pch = 16,
      cex = 1.2,
      col = renoir_named[sp],
      xlim = xlim,
      ylim = ylim,
      xlab = "Tree age at ring",
      ylab = "Ring width (mm)",
      main = paste(id_i)
    )
    abline(fit, col = "black", lwd = 1)
    mean_age <- mean(d$meanAge, na.rm = TRUE)
    text(
      x = xlim[2] - 0.05 * diff(xlim),   # right side
      y = ylim[2] - 0.05 * diff(ylim),   # top
      labels = paste("Mean age =", round(mean_age, 1)),
      adj = c(1, 1),                     # right + top alignment
      cex = 1
    )
  }
  mtext(sp, side = 3, outer = TRUE, line = 0.5, cex = 1.4, font = 2)
}

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot all tree together in one page by year FOR PAPER ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
empts$newid <- paste0(empts$latbi, ": \n", substr(empts$id, 6, 15))
vec <- unique(empts$newid)
# one row per individual, with species attached
treeInfo <- unique(empts[, c("newid", "latbi")])

# 1 guaranteed individual per species
oneEach <- tapply(treeInfo$newid, treeInfo$latbi, function(x) sample(x, 1))

# fill remainder randomly from what's left
remaining <- setdiff(treeInfo$newid, oneEach)
fillN <- 20 - length(oneEach)
extra <- sample(remaining, fillN)

ids20 <- c(oneEach, extra)

suby <- subset(empts, newid %in% ids20 & year > 2015 & year < 2025)

ids <- unique(suby$newid)

pdf("figures/empiricalData/rwXyearRestricted.pdf", width = 8, height = 10)

empts$year <- as.integer(empts$year)

par(mfrow = c(5, 4),
    mar = c(3, 2, 2, 0.5),
    mgp = c(1.5, 0.5, 0))

for(i in unique(suby$newid)) { # i = "QUAL_21815_E"
  sub <- suby[suby$newid == i, ]
  if(nrow(sub) == 0) next
  col_vals <- renoir[as.numeric(factor(sub$latbi, levels = levels(factor(emp2$latbi))))]
  
  plot(sub$year, sub$lengthMM,
       col = col_vals, 
       pch = 16,
       cex = 1.5,
       main = i,
       xlab = "",
       xaxt = "n",
       ylab = "",
       tck = -0.02,
       bty = 'l')
  
  axis(1, at = seq(floor(min(sub$year)), floor(max(sub$year)), by = 1), tck = -0.02)
  
  # regression line per species
  # for(sp in unique(sub$latbi)) { # sp = "River birch"
  #   ssp <- sub[sub$latbi == sp, ]
  #   fit <- lm(lengthMM ~ year, data = ssp)
  #   abline(fit, col = renoir[as.numeric(factor(sp, levels = levels(factor(empts$latbi))))], lwd =1.8) 
  # }
}
dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot crossdating example ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
TIAM <- subset(allringwidths2, species == "TIAM")
markers <- c(1965, 
             1966, 
             1981, # gypsy moth defoliation
             2012, 
             2016)

ggplot(TIAM, aes(x = yearCor, y = lengthMM, color = plantNickname, 
                 group = idrep)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = markers, linetype = "dashed") +
  labs(title = "",
       x = "Year",
       y = "Ring width (Length)",
       color = "Core ID") +
  facet_wrap(~plantNickname, nrow = 5, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(min(TIAM$yearCor), 
                                  max(TIAM$yearCor), 
                                  by = 5)) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = wes_palette("FantasticFox1"))
ggsave("figures/tiamspaghetti_plot.jpeg", width = 10, height = 6, 
       units = "in", dpi = 300)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plotting phenology ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Leafout 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf("figures/empiricalData/leafoutXyear.pdf", width = 8, height = 6)
spp <- unique(empts$latbi)
empts$year <- as.integer(empts$year)
par(mfrow = c(4, 3), mar = c(2, 2, 1.5, 0.5), mgp = c(1.5, 0.5, 0))
for(i in spp) {
  sub <- empts[empts$latbi == i, ]
  sp_col <- renoir[as.numeric(factor(i, levels = levels(factor(empts$latbi))))]
  
  plot(sub$year, sub$leafout,
       col = sp_col,
       pch = 16,
       cex = 1,
       main = i,
       xlab = "",
       xaxt = "n",
       ylab = "",
       tck = -0.03,
       bty = 'l')
  
  axis(1, at = seq(floor(min(sub$year)), floor(max(sub$year)), by = 2), tck = -0.03)
  
  for(sp in unique(sub$plantNickname)) {
    sids <- sub[sub$plantNickname == sp, ]
    sids <- sids[order(sids$year), ]
    lines(sids$year, sids$leafout, col = sp_col, lwd = 0.8)
  }
}
dev.off()
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# colored leaves 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf("figures/empiricalData/coloredLeavesXyear.pdf", width = 8, height = 6)
par(mfrow = c(4, 3), mar = c(2, 2, 1.5, 0.5), mgp = c(1.5, 0.5, 0))
for(i in spp) {
  sub <- empts[empts$latbi == i, ]
  sp_col <- renoir[as.numeric(factor(i, levels = levels(factor(empts$latbi))))]
  
  plot(sub$year, sub$coloredLeaves,
       col = sp_col,
       pch = 16,
       cex = 1,
       main = i,
       xlab = "",
       xaxt = "n",
       ylab = "",
       tck = -0.03,
       bty = 'l')
  
  axis(1, at = seq(floor(min(sub$year)), floor(max(sub$year)), by = 2), tck = -0.03)
  
  for(sp in unique(sub$plantNickname)) {
    sids <- sub[sub$plantNickname == sp, ]
    sids <- sids[order(sids$year), ]
    lines(sids$year, sids$coloredLeaves, col = sp_col, lwd = 0.8)
  }
}
dev.off()


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Boxplot of years per species ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
species <- unique(empts$latbi)
years <- sort(unique(empts$year))
n_sp <- length(species)

# yr_levels <- sort(unique(dat$year))
# bpcols <- colsyr[as.character(yr_levels)]

jpeg("figures/empiricalData/boxplotRingWidth.jpeg", width = 2600, height = 2000, res = 300)
par(mfrow = c(4, 3), mar = c(2, 4, 3, 1))
for(sp in species) { # sp = "A. rubrum"
  dat <- empts[empts$latbi == sp,]
  yr_levels <- sort(unique(dat$year))
  bpcols <- colsyr[as.character(yr_levels)]
  
  boxplot(lengthMM ~ year, data = dat,
          main = bquote(italic(.(sp))),
          xlab = "Year", ylab = "Ring width (mm)",
          col = adjustcolor(bpcols, alpha.f = 0.5),
          border = adjustcolor(bpcols, alpha.f = 0.8),
          medcol = "black",
          whisklty = 1, staplewex = 0, medlty = 1, medlwd = 0.8,
          outpch = 16, outcex = 0.7, outcol = "black")
  
  stripchart(lengthMM ~ year, data = dat,
             method = "jitter", jitter = 0.08,
             pch = 16, cex = 0.7, col = "black",
             vertical = TRUE, add = TRUE)
}
dev.off()


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Time series phenological data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# I can't really do the same thing as I did for Wildchrokie because we can't assume that each tree was visited on a given date.
# comb <- read.csv("output/uncleanedTimeseriesPheno.csv")
# comb$yeardoy <- paste(comb$year, comb$doy, sep = "_")
# comb2 <- comb[!duplicated(comb$yeardoy),]
# nrow(comb)
# minbb <- aggregate(budburst ~ year, emp, FUN = min)
# meanbb <- aggregate(budburst ~ year, emp, FUN = mean)
# maxbb <- aggregate(budburst ~ year, emp, FUN = max)
# bb <- merge(minbb, meanbb, by = "year")
# bb <- merge(bb, maxbb, by = "year")
# colnames(bb) <- c("year", "min", "mean", "max")
# 
# jpeg(
#   filename = "figures/empiricalData/phenoTimeseries.jpeg",
#   width = 2000, height = 2800, res = 400)
# colsyr
# par(mfrow = c(length(unique(comb2$year)), 1))
# for (yr in sort(unique(comb2$year))) { # i =1
#   hist(comb2$doy[comb2$year == yr], breaks = seq(0, 366, by = 14),
#        main = yr, xlab = "Day of year", xlim = c(50, 366), ylim = c(0,4),
#        ylab = "Number of observations per 14 days", col = adjustcolor(colsyr[as.character(yr)], alpha.f = 0.2))
#   bbx <- bb[bb$year == yr,]
#   segments(x0 = bbx$min, y0 = 0, y1 = 5, lwd = 2, lty = 1, col = "#247d3f")
#   segments(x0 = bbx$mean, y0 = 0, y1 = 5, lwd = 2, lty = 1, col = "black")
#   segments(x0 = bbx$max, y0 = 0, y1 = 5, lwd = 2, lty = 1, col = "#da7901")
#   if(yr == min(unique(comb2$year))) {
#     legend("topright", legend = c("Min", "Mean", "Max"),
#            col = c("#247d3f", "black", "#da7901"),
#            lwd = 2, lty = 1, bty = "n", cex = 0.8)
#   }
# }
# dev.off()


