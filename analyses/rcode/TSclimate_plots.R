# Coringtreespotters climate data with phenology
# CRD 27 May 2026

# housekeeping
# rm(list=ls())
# options(stringsAsFactors = FALSE)
# options(max.print = 150)
# options(digits = 3)

# Load library 
library(ggplot2)
library(rstan)
library(future)
library(wesanderson)
library(patchwork) 

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
source('/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/rcode/tools.R')

# flags
makeplots <- FALSE

tsempir <- read.csv("output/empiricalDataMAIN.csv")
climatesum <- read.csv("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/output/climateSummariesYear.csv")
climatesummonth <- read.csv("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/output/climateSummariesByMonth.csv")
gddyr <- read.csv("output/gddByYear.csv")
weldhillclim <- read.csv("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/output/weldhillClimateCleaned.csv")

# transform my groups to numeric values
tsempir$spp_num <- match(tsempir$latbi, unique(tsempir$latbi))
tsempir$year_num <- match(tsempir$year, unique(tsempir$year))

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Climate data #### 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
tsempir$leafout <- as.integer(tsempir$leafout)
tsempir$coloredLeaves <- as.integer(tsempir$coloredLeaves)

gddyr$yeardoy <- paste(gddyr$year, gddyr$doy, sep = "_")
tsempir$yeardoybudburst <- paste(tsempir$year, tsempir$budburst, sep = "_")
tsempir$yeardoyleafout <- paste(tsempir$year, tsempir$leafout, sep = "_")
tsempir$yeardoycoloredLeaves <- paste(tsempir$year, tsempir$coloredLeaves, sep = "_")

yearcolors <- c("#931e18", "#da7901", "#247d3f")
par(mfrow = c(1,1))
years <- sort(unique(tsempir$year))

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

tsempir <- subset(tsempir, year != 2015)
firststeps <- colorRampPalette(c("#9cc184", "#192813"))(length(years))
years      <- sort(unique(tsempir$year))

# precipitation at leafout
if (makeplots) {
tsempir$winterPptLeafout <- mapply(function(leafout_doy, obs_year) {
  # takes the previous year accumulation of ppt in december
  sub <- weldhillclim[(weldhillclim$year == obs_year - 1 & weldhillclim$doy >= 335) |
                        # then going into the current year condition
                        (weldhillclim$year == obs_year & weldhillclim$doy <= leafout_doy), ]
  sum(sub$pptMM, na.rm = TRUE) # sum the ppt over our period of interest
  }, tsempir$leafout, tsempir$year) # apply the function to each of those 2 arguments

plot(tsempir$winterPptLeafout, tsempir$leafout,
     xlab = "precipitation accumulation (mm) at leafout", ylab = "leafout",
     pch = 16, frame = FALSE, xlim = c(200, 700),
     col = colsyr[match(tsempir$year, years)],
     main = "leafout X precipitation accumulation (mm) at leafout")

for (i in seq_along(years)) { # i = 1
  year_dat <- tsempir[tsempir$year == years[i], ]
  
  lm_fit <- lm(leafout ~ winterPptLeafout, data = year_dat)
  x_seq  <- seq(min(year_dat$winterPptLeafout, na.rm = TRUE), 
                max(year_dat$winterPptLeafout, na.rm = TRUE), length.out = 200)
  pred   <- predict(lm_fit, newdata = data.frame(winterPptLeafout = x_seq))
  
  lines(x_seq, pred, 
        col = colsyr[i],
        lwd = 2)
  legend("bottomright",
         legend = years, 
         col= colsyr, pch = 16, lty = 1, lwd = 2,
         title  = "Year")
}
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Climate summaries ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# common objects across coloredLeaves and leafout
clim_vars  <- c("TempMeanMax", "TempMeanMean","TempMeanMin")

emp_clim <- merge(tsempir, climatesum, by = "year", all.x = TRUE)

colnames(emp_clim)[which(colnames(emp_clim) %in% "tmeanmax")] <- "TempMeanMax"
colnames(emp_clim)[which(colnames(emp_clim) %in% "tmeanmean")] <- "TempMeanMean"
colnames(emp_clim)[which(colnames(emp_clim) %in% "tmeanmin")] <- "TempMeanMin"

emp_climlo <- emp_clim[!is.na(emp_clim$leafout),]
emp_climbs <- emp_clim[!is.na(emp_clim$coloredLeaves),]

emp_climlo$anomleafout <- emp_climlo$leafout - mean(emp_climlo$leafout)
emp_climbs$anomcoloredLeaves <- emp_climbs$coloredLeaves - mean(emp_climbs$coloredLeaves)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# CoringTreespotters ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
emp_climts <- merge(tsempir, climatesum, by = "year", all.y = TRUE)

colnames(emp_climts)[which(colnames(emp_climts) %in% "pdsi")] <- "PDSI"
colnames(emp_climts)[which(colnames(emp_climts) %in% "tmeanmax")] <- "TempMeanMax"
colnames(emp_climts)[which(colnames(emp_climts) %in% "tmeanmean")] <- "TempMeanMean"
colnames(emp_climts)[which(colnames(emp_climts) %in% "tmeanmin")] <- "TempMeanMin"
colnames(emp_climts)[which(colnames(emp_climts) %in% "ppt")] <- "Precip"

emp_climtslo <- emp_climts[!is.na(emp_climts$leafout),]
emp_climtscl <- emp_climts[!is.na(emp_climts$coloredLeaves),]

emp_climtslo$anomleafout <- emp_climtslo$leafout - mean(emp_climtslo$leafout)
emp_climtscl$anomleafcolor <- emp_climtscl$coloredLeaves - mean(emp_climtscl$coloredLeaves)

clim_vars <- c("TempMeanMin", "TempMeanMean", "TempMeanMax")
periods <- c("DJF", "MAM")

emp_climtslo$spp_num <- match(emp_climtslo$latbi, unique(emp_climtslo$latbi))
emp_climtslo$year_num <- match(emp_climtslo$id, unique(emp_climtslo$id))
emp_climtslo$year_num <- match(emp_climtslo$year, unique(emp_climtslo$year))


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Phenology ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
leafoutbyyr <- aggregate(leafout ~ year + latbi, tsempir, FUN = mean)
coloredLeavesbyyr <- aggregate(coloredLeaves ~ year + latbi, tsempir, FUN = mean)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Climate summaries ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
yrts <- c(2016:2024)
climatesummonth$monthname <- month.name[climatesummonth$month]

moderatedrought <- subset(climatesummonth, pdsi < -2 & pdsi > -3)
severedrought <- subset(climatesummonth, pdsi < -3)

# Checks for temperatures and PDSI ####
tsgsl <- aggregate(pgsGSL ~ year, tsempir, FUN = mean, na.rm =TRUE)
tsgsl[order(tsgsl$pgsGSL),]

tssos <- aggregate(leafout ~ year, tsempir, FUN = mean, na.rm =TRUE)
tssos[order(tssos$leafout),]

tseos <- aggregate(coloredLeaves ~ year, tsempir, FUN = mean, na.rm =TRUE)
tseos[order(tseos$coloredLeaves),]

tsclim <- subset(climatesum, year %in% 2016:2024)
tsdjf <- subset(tsclim, period %in% "DJF")
tsdjf[order(tsdjf$tmeanmean),]
tsmam <- subset(tsclim, period %in% "MAM")
tsmam[order(tsmam$tmeanmean),]
tsjja <- subset(tsclim, period %in% "JJA")
tsjja[order(tsjja$tmeanmean),]
tsson <- subset(tsclim, period %in% "SON")
tsson[order(tsson$tmeanmean),]
tsmam[order(tsmam$pdsi),]
tsjja[order(tsjja$pdsi),]
tsson[order(tsson$pdsi),]

tsrw <- aggregate(lengthMM ~ year, tsempir, FUN = mean, na.rm =TRUE)
tsrw[order(tsrw$lengthMM),]

agggdd <- aggregate(pgsGDD5 ~ year, tsempir, FUN = mean)
agggdd[order(agggdd$pgsGDD5),]

# Late spring frosts ####
combined <- merge(tsempir[, c("id", "year", "budburst")], 
                  weldhillclim[, c("year", "doy", "minTempC")], 
                  by = "year")
# Define frost threshold (usually 0 or -2.2 for hardy buds)
frost_threshold <- 0

# Subset to days after budburst where it froze
late_frosts <- combined[combined$doy > combined$budburst & 
                          combined$minTempC <= frost_threshold, ]
unique(combined$year[which(combined$budburst %in% min(combined$budburst, na.rm = TRUE))])
# Remove rows with NA budburst (if any)
late_frosts <- late_frosts[!is.na(late_frosts$budburst), ]

# Create a summary data frame
frost_summary <- aggregate(minTempC ~ id + year, 
                           data = late_frosts, 
                           FUN = function(x) c(Count = length(x), MinTemp = min(x)))

# Flatten the aggregate result into a clean data frame
frost_summary <- do.call(data.frame, frost_summary)
colnames(frost_summary) <- c("id", "year", "frost_days_after_burst", "minimum_frost_temp")

