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

# flags
makeggplots <- FALSE

emp <- read.csv("output/empiricalDataMAIN.csv")
gdd <- read.csv("output/gddByYear.csv")
allringwidths <- read.csv("output/ringWidthTS.csv")
longtermgdd <- read.csv("output/longTermGDDperYear.csv")
longtermgdd5yr <- read.csv("output/longTermGDD5YrAvg.csv")

emp$accessionYear <- as.numeric(substr(emp$accessionDate, 7,11))

# now do the same for ringwidth DO THIS
length <- aggregate(lengthCM ~ id + yearCor, allringwidths, FUN = mean)
length$lengthMM <- length$lengthCM*10
length$idyear <- paste(length$id, length$yearCor)

allringwidths$idyear <- paste(allringwidths$id, allringwidths$yearCor)

allringwidths2 <- allringwidths[!duplicated(allringwidths$idyear),]

allringwidths2$lengthMM <- length$lengthMM[match(allringwidths2$idyear, length$idyear)]

allringwidths2$accessionYear <- emp$accessionYear[match(allringwidths2$id, emp$id)]

allringwidths2$age <- allringwidths2$yearCor - allringwidths2$accessionYear

emp$plantNickname <- paste(emp$commonName, emp$plantNickname, sep = " ")
# ringwidth X GDD in PGS
renoir <- c("#17154f", "#2f357c", "#6c5d9e", "#9d9cd5", "#b0799a", "#f6b3b0", "#e48171", "#bf3729", "#e69b00", "#f5bb50", "#ada43b", "#355828")

if (makeggplots) {
subsetbass <- subset(emp, symbol == "TIAM")
subsetbass$DBH
ggplot(emp, aes(x = pgsGDD10, y = lengthMM, 
                color = commonName, 
                fill = commonName)) +
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = renoir) +
  scale_fill_manual(values = renoir) +
  facet_wrap(~commonName) +
  labs(y = "Ring width (mm)", x = "Growing degree days (GDD)", color = "Tree Species") +
  theme_bw() +
  theme(legend.key.height = unit(1.5, "lines"),
        strip.text = element_text(face = "bold.italic", size = 10)) +
  guides(fill = "none", color = "none") 
ggsave("figures/empiricalData/sppLinearRegressions_pgsGDD.jpeg", width = 8, height = 6, units = "in", dpi = 300)

# plot just basswood
subsetbass <- subset(emp, symbol == "TIAM")
ggplot(subsetbass, aes(x = pgsGDD10, y = lengthMM, 
                color = id, 
                fill = id)) +
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = renoir) +
  scale_fill_manual(values = renoir) +
  facet_wrap(~commonName) +
  labs(y = "Ring width (mm)", x = "Growing degree days (GDD)", color = "Tree Species") +
  theme_bw() +
  theme(legend.key.height = unit(1.5, "lines"),
        strip.text = element_text(face = "bold.italic", size = 10)) +
  guides(fill = "none", color = "none") 
ggsave("figures/empiricalData/sppLinearRegressions_pgsGDD_TIAM.jpeg", width = 8, height = 6, units = "in", dpi = 300)

emp$year2 <- as.factor(emp$year)
# new symbols and stuff
ggplot(emp, aes(x = pgsGDD10, y = lengthMM)) +
  geom_point(size = 2, alpha = 0.9,
             aes(color = year2, 
                 fill = year2)) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "black") +
  # scale_color_manual(values = wes_palette("AsteroidCity1")) +
  # scale_fill_manual(values = wes_palette("AsteroidCity1")) +
  # scale_shape_manual(values = c(21, 22, 23, 24, 25)) +  
  facet_wrap(~commonName, nrow = 4, ncol = 3) +
  labs(y = "Ring width (mm)", 
       x = "Growing degree days (GDD)", 
       color = "Year",
       fill = "Year",
       shape = "Site") +  
  theme_bw() 
ggsave("figures/empiricalData/sppLinearRegressions_pgsGDD.jpeg", width = 6, height = 8, units = "in", dpi = 300)

ggplot(emp, aes(x = year, y = lengthMM, 
                color = commonName, 
                fill = commonName)) +
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0) +
  scale_color_manual(values = renoir) +
  scale_fill_manual(values = renoir) +
  facet_wrap(~id) +
  labs(y = "Ring width (mm)", x = "Growing degree days (GDD)", color = "Tree Species") +
  theme_minimal() +
  theme(legend.key.height = unit(1.5, "lines"),
        strip.text = element_text(face = "bold.italic", size = 10)) +
  guides(fill = "none", color = "none") 
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot ring width x age at ring, 1 page per species ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# FOR WHOLE CHRONOLOGY
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
emp2 <- emp
str(emp2)
emp2$treeAge <- emp2$year - emp2$accessionYear
allringwidths2$treeAge <- allringwidths2$yearCor - allringwidths2$accessionYear

# remove the negative ages:
vec <- unique(allringwidths2$id[which(allringwidths2$treeAge < 0)])
allringwidths2 <- subset(allringwidths2, !(id %in% vec))

# mean age of tree
age <- aggregate(treeAge ~ id, allringwidths2, FUN = mean)
allringwidths2$meanAge <- age$treeAge[match(allringwidths2$id, age$id)]
allringwidths2 <- allringwidths2[order(allringwidths2$meanAge),]
allringwidths2$commonName <- emp$commonName[match(allringwidths2$id, emp$id)]
allringwidths2$plantNickname <- emp$plantNickname[match(allringwidths2$id, emp$id)]

# add 5 year gdd average
allringwidths2$gdd <- longtermgdd5yr$GDD_moving_avg[match(allringwidths2$yearCor, longtermgdd5yr$year)]

pdf("figures/empiricalData/ringwidthXyear_bySpeciesAllYrs.pdf", width = 10, height = 6)
species_list <- unique(allringwidths2$commonName)
renoir_named <- setNames(renoir, species_list)

# Loop over each species
for (sp in species_list) { # sp = "Sugar maple"
  df_sp <- allringwidths2[allringwidths2$commonName == sp, ]
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

species_list <- unique(allringwidths3$commonName)
renoir_named <- setNames(renoir, species_list)

# here I loop over each page that fits 1 species at a time
for (sp in species_list) {
  df_sp <- allringwidths3[allringwidths3$commonName == sp, ]
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
# FOR CHRONOLOGY WITH PHENODATA
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
emp2 <- emp
emp2$treeAge <- emp2$year - emp2$accessionYear

# remove the the negative ages referencing the previous vector with negative ages ids
emp2 <- subset(emp2, !(id %in% vec))

# mean age of tree
age <- aggregate(treeAge ~ id, emp2, FUN = mean)
emp2$meanAge <- age$treeAge[match(emp2$id, age$id)]

emp2 <- emp2[order(emp2$meanAge),]

emp2$commonName <- emp$commonName[match(emp2$id, emp$id)]

# PLOT!.
pdf("figures/empiricalData/ringwidthXage_bySpeciesOnlyLeafout.pdf", width = 10, height = 6)

species_list <- unique(emp2$commonName)
renoir_named <- setNames(renoir, species_list)

# here I loop over each page that fits 1 species at a time
for (sp in species_list) {
  df_sp <- emp2[emp2$commonName == sp, ]
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
# Plot all tree together in one page ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
pdf("figures/empiricalData/ringwidthXyearPhenoData1page.pdf", 
    width = 11, height = 10)
ids <- unique(emp$plantNickname)
par(mfrow = c(ceiling(length(ids)/5), 5), 
    mar = c(2, 2, 1.5, 0.5),
    mgp = c(1.5, 0.5, 0))
emp$year <- as.integer(emp$year)
for(i in ids) { # i = 
  sub <- emp[emp$plantNickname == i, ]
  col_vals <- renoir[as.numeric(factor(sub$commonName, levels = levels(factor(emp$commonName))))]
  
  plot(sub$year, sub$lengthMM,
       col = col_vals,
       pch = 16,
       cex = 1,
       main = i,
       xlab = "",
       xaxt = "n",
       ylab = "",
       tck = -0.1,
       bty = 'l')
  
  axis(1, at = seq(floor(min(sub$year)), floor(max(sub$year)), by = 2), tck = -0.1)
  # regression line per species
  for(sp in unique(sub$commonName)) {
    ssp <- sub[sub$commonName == sp, ]
    fit <- lm(lengthMM ~ year, data = ssp)
    abline(fit, col = renoir[as.numeric(factor(sp, levels = levels(factor(emp$commonName))))], lwd =0.5) 
  }
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
spp <- unique(emp$commonName)
emp$year <- as.integer(emp$year)
par(mfrow = c(4, 3), mar = c(2, 2, 1.5, 0.5), mgp = c(1.5, 0.5, 0))
for(i in spp) {
  sub <- emp[emp$commonName == i, ]
  sp_col <- renoir[as.numeric(factor(i, levels = levels(factor(emp$commonName))))]
  
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
  sub <- emp[emp$commonName == i, ]
  sp_col <- renoir[as.numeric(factor(i, levels = levels(factor(emp$commonName))))]
  
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
# Map ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- Get the map data ---
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- Define bounding box for northeastern North America ---
# Adjust these coordinates as needed
lat_min <- 41
lat_max <- 48
lon_min <- -78
lon_max <- -63

# --- Create example points along a latitudinal gradient ---
# These are arbitrary example locations
locations <- data.frame(
  name = c("Harvard Forest (MA)", "White Mountains (NH)", "Dartmouth College (NH)", "St-Hyppolyte (Qc)"),
  lon = c(-72.2, -71, -70.66, -74.01),
  lat = c(42.55, 44.11, 44.92, 45.98)
)

locations2 <- locations[order(-locations$lat), ]

locations2$col <-  wes_palettes$Darjeeling1[1:4]

special_point <- data.frame(
  name = "Arnold Arboretum of 
  Harvard University (MA)",
  lon = -71.13358611669867,
  lat = 42.29601035316377
)
special_sf <- st_as_sf(special_point, coords = c("lon", "lat"), crs = 4326)

# Convert to sf object
points_sf <- st_as_sf(locations2, coords = c("lon", "lat"), crs = 4326)

# --- Plot the map ---
ggplot(data = world) +
  geom_sf(fill = "white", color = "gray60") +
  geom_sf(data = points_sf, color = locations2$col, size = 4) +
  geom_sf(data = special_sf, color = "#E54E21", shape = 8, size = 6, stroke = 1.2) +
  geom_text(data = locations2, aes(x = lon, y = lat, label = name),
            nudge_y = 0.35, nudge_x = 0, size = 4.5, fontface = "bold") +
  geom_text(data = special_point,
            aes(x = lon, y = lat, label = name),
            nudge_y = 0.2, nudge_x = 2.5, color = "#E54E21", size = 5, fontface = "bold") +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  theme_minimal() +   
  theme(strip.text = element_blank(),                    
        legend.key.height = unit(1.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  labs(
    title = "",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted")
  )
ggsave("figures/mapSourcePop.jpeg", width = 9, height = 6, units = "in", dpi = 300)


