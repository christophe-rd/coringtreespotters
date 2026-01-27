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
library(rstanarm)
library(future)
library(shinystan)
library(wesanderson)
library(patchwork)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")

emp <- read.csv("output/empiricalDataMAIN.csv")
gdd <- read.csv("output/gddByYear.csv")
allringwidths <- read.csv("output/ringWidthTS.csv")
# # load fit
# fit <- readRDS("output/stanOutput/fitEmpirical_stanlmer")
# fit_pgsNgrowingdays <- readRDS("output/stanOutput/fit_pgsNgrowingdays_Empirical_stanlmer")

# faceted
# order by spp
emp <- emp[order(emp$spp), ]
emp$lengthMM <- emp$lengthCM*10

# emp$year <- as.factor(emp$year)


# ringwidth X GDD in PGS
renoir <- c("#17154f", "#2f357c", "#6c5d9e", "#9d9cd5", "#b0799a", "#f6b3b0", "#e48171", "#bf3729", "#e69b00", "#f5bb50", "#ada43b", "#355828")

subsetbass <- subset(emp, symbol == "TIAM")
subsetbass$DBH
ggplot(emp, aes(x = pgsGDD, y = lengthMM, 
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
ggplot(subsetbass, aes(x = pgsGDD, y = lengthMM, 
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

emp$year <- as.factor(emp$year)
# new symbols and stuff
ggplot(emp, aes(x = pgsGDD, y = lengthMM)) +
  geom_point(size = 2, alpha = 0.9,
             aes(color = year, 
                 fill = year)) + 
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

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot ring width x age at ring, 1 page per species ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
emp$accessionYear <- as.numeric(substr(emp$accessionDate, 7,11))
allringwidths2 <- allringwidths
allringwidths2$accessionYear <- emp$accessionYear[match(allringwidths2$id, emp$id)]
allringwidths2$commonName <- emp$commonName[match(allringwidths2$id, emp$id)]
allringwidths2$lengthMM <- allringwidths2$lengthCM*10
allringwidths2$treeAge <- allringwidths2$yearCor - allringwidths2$accessionYear

# remove the the negative ages:
vec <- unique(allringwidths2$id[which(allringwidths2$treeAge < 0)])
allringwidths2 <- subset(allringwidths2, !(id %in% vec))

# mean age of tree
age <- aggregate(treeAge ~ id, allringwidths2, FUN = mean)
allringwidths2$meanAge <- age$treeAge[match(allringwidths2$id, age$id)]

allringwidths2 <- allringwidths2[order(allringwidths2$meanAge),]


# PLOT!.
pdf("figures/empiricalData/ringwidthXage_bySpecies.pdf", width = 10, height = 6)

species_list <- unique(allringwidths2$commonName)
renoir_named <- setNames(renoir, species_list)

# here I loop over each page that fits 1 species at a time
for (sp in species_list) {
  df_sp <- allringwidths2[allringwidths2$commonName == sp, ]
  ids   <- unique(df_sp$id)
  par(
    mfrow = n2mfrow(length(ids)),
    mar = c(4, 4, 2, 1),
    oma = c(2, 2, 2, 1)
  )
  
  # shared limits across the individuals of a given species
  xlim <- range(df_sp$treeAge, na.rm = TRUE)
  ylim <- range(df_sp$lengthMM, na.rm = TRUE)
  
  # here I loop over each individuals of each species
  for (id_i in ids) {
    d <- df_sp[df_sp$id == id_i, ]
    fit <- lm(lengthMM ~ treeAge, d)
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
    abline(fit, col = "black", lwd = 2)
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






emp2 <- emp[!duplicated(emp$idrep),]
emp2 <- aggregate(lengthMM ~ idrep, emp, FUN = mean)

emp2$accessionYear <- emp$accessionYear[match(emp2$idrep, emp$idrep)]
emp2$commonName <- emp$commonName[match(emp2$idrep, emp$idrep)]


# color coded by number of frost free days
frostfree <- subset(gdd, minTempC > 0)

# count the nb of frost free days per year
countfrost <- frostfree %>% count(year)
colnames(countfrost) <- c("year", "countFrostFree")

emp <- merge(emp, countfrost, by = "year")

# associate cols
emp$colfrost <- NA
unique(emp$countFrostFree)
emp$colfrost[which(emp$year %in% c(2018, 2019))] <- "#81A88D"
emp$colfrost[which(emp$year %in% c(2020))] <- "#02401B"

emp$countFrostFree <- as.factor(emp$countFrostFree)
ggplot(emp, aes(x = pgsGDD, y = lengthMM)) +
  geom_point(size = 2, alpha = 1,
             aes(shape = site,
                 color = countFrostFree, 
                 fill = countFrostFree)) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "black") +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +  
  facet_wrap(~commonName) +
  labs(y = "Ring width (mm)", 
       x = "Growing degree days (GDD)", 
       color = "Number of frost free days",
       fill = "Number of frost free days",
       shape = "Site") +  
  theme_bw() +
  guides(color = guide_legend(override.aes = list(shape = 21)),
         fill = guide_legend(override.aes = list(shape = 21)))
ggsave("figures/empiricalData/sppLinearRegressions_pgsGDD_frostFreeDays.jpeg", width = 8, height = 6, units = "in", dpi = 300)

# color coded by tree id
### start with bepa
# BELOW IS FOR WILDCHROKIE ADAPT TO TS
bepa <- subset(emp, spp == "BETPAP")
bepaplot <- ggplot(bepa, aes(x = pgsGDD, y = lengthMM)) +
geom_point(size = 2, alpha = 1) + 
  # geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "black") +
  labs(y = "", 
       x = "", 
       color = "treeid",
       fill = "treeid") +  
  facet_wrap(~treeid)+
  theme_bw() +
  guides(color = guide_legend(override.aes = list(shape = 21)),
         fill = guide_legend(override.aes = list(shape = 21)))
ggsave("figures/empiricalData/sppLinearRegressions_pgsGDD_betpap.jpeg", bepaplot, width = 8, height = 6, units = "in", dpi = 300)

# path them!
combinedtreeid <- (bepaplot)/
  (betpopplot) /
  (betallplot) /
  (alnincplot) 
combinedtreeid
ggsave("figures/empiricalData/sppLinearRegressions_pgsGDD_combined.jpeg", combinedtreeid, width = 8, height = 16, units = "in", dpi = 300)

  # full growing season
ggplot(emp, aes(x = fgsGDD, y = lengthCM, 
                color = symbol, 
                fill = symbol)) +
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = wes_palette("AsteroidCity1")) +
  scale_fill_manual(values = wes_palette("AsteroidCity1")) +
  facet_wrap(~symbol) +
  labs(y = "Ring width (cm)", x = "Growing degree days (GDD)", color = "Tree Species") +
  theme_minimal() +
    theme(strip.text = element_blank(),         
          legend.key.height = unit(1.5, "lines")) +
  guides(fill = "none") 
ggsave("figures/empiricalData/sppLinearRegressions.jpeg", 
       width = 9, height = 6, units = "in", dpi = 300)

# just the number of days, without gdd
emp$pgsNgrowingdays <- emp$budset - emp$leafout
emp$fgsNgrowingdays <- emp$leafcolor - emp$budburst

# number of days  in pgs
ggplot(emp) +
  # geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(aes(x = pgsNgrowingdays, y = lengthCM, 
                  color = symbol, 
                  fill = symbol),
              method = "lm", se = TRUE, alpha = 0.2) +

  scale_color_manual(values = wes_palette("AsteroidCity1")) +
  scale_fill_manual(values = wes_palette("AsteroidCity1")) +
  facet_wrap(~symbol) +
  labs(y = "Ring width (cm)", x = "", color = "Tree Species") +
  theme_minimal() +
  theme(strip.text = element_blank(),         
          legend.key.height = unit(1.5, "lines")) +
  guides(fill = "none") 
ggsave("figures/empiricalData/sppLinearRegressions_pgsNdays.jpeg", width = 9, height = 6, units = "in", dpi = 300)
# number of days  in fgs
ggplot(emp, aes(x = fgsNgrowingdays, y = lengthCM, 
                color = symbol, 
                fill = symbol)) +
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = wes_palette("AsteroidCity1")) +
  scale_fill_manual(values = wes_palette("AsteroidCity1")) +
  facet_wrap(~sppfull) +
  labs(y = "Ring width (cm)", x = "", color = "Tree Species") +
  theme_minimal() +   theme(strip.text = element_blank(),                    legend.key.height = unit(1.5, "lines")) +
  theme(strip.text = element_blank(),
        legend.key.height = unit(1.5, "lines")) +
  guides(fill = "none") 
# # plot gdd
# gddstats <- aggregate(GDD_10 ~ doy, FUN = mean, data = gdd)
# colnames(gddstats)[2] <-  "mean"
# gddstats2 <- aggregate(GDD_10 ~ doy, FUN = min, data = gdd)
# colnames(gddstats2)[2] <-  "min"
# gddstats3 <- aggregate(GDD_10 ~ doy, FUN = max, data = gdd)
# colnames(gddstats3)[2] <-  "max"
# 
# gddstats <- merge(gddstats, gddstats2, by = "doy")
# gddstats <- merge(gddstats, gddstats3, by = "doy")
# 
# ggplot(gddstats, aes(x = doy, y = mean)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2, color = NA) +
#   geom_vline(xintercept = 5) +
#   labs(title = "GDD accumulation")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Map ####

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


[1] "#FF0000" "#00A08A" "#F2AD00" "#F98400"

 # Fitting empirical data with stan_lmer ####

