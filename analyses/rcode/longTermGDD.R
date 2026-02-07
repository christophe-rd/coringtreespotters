# Climate comparison
# CRD 5 February 2026
# Get long-term climate data records from boston airport to plot on ring width chronologies for the treespotters

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 
options(digits = 3)
# quartz()

# Load library 
library(ggplot2)
library(wesanderson)
library(patchwork)
library(pollen)

if (length(grep("christophe_rouleau-desrochers", getwd())) > 0) {
  setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")
} else if (length(grep("lizzie", getwd())) > 0) {
  setwd("/Users/lizzie/Documents/git/projects/others/christophe/coringtreespotters/analyses")
} else  {
  setwd("/home/crouleau/coringtreespotters/analyses")
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Start with years 2012 to 2020
#from https://labs.arboretum.harvard.edu/weather/
logan <- read.csv("input/loganAirportClimate.csv") 

colnames(logan) <- c(
  "stationID", 
  "name",
  "date",
  "frgt",
  "precipitation",
  "snowe",
  "snwd",
  "meanTempC",
  "maxTempC",
  "minTempC",
  "wesd"
)

str(logan)
logan$year <- as.numeric(format(as.Date(logan$date), "%Y"))
logan$doy <- as.numeric(format(as.Date(logan$date), "%j"))

###Calculate gdd
logan <- logan[order(logan$year, logan$date), ]

logan$GDD_5 <- gdd(tmax = logan$maxT, tmin = logan$minT, tbase = 5, type = "B")

# Initialize the GDD_5 column
logan$GDD_5 <- NA

# Get unique years
years <- unique(logan$year)

# Loop through each year
for (y in years) {
  # Find rows for this year
  year_rows <- which(logan$year == y)
  
  # Calculate GDD for this year only
  logan$GDD_5[year_rows] <- gdd(tmax = logan$maxTempC[year_rows], 
                                tmin = logan$minTempC[year_rows], 
                                tbase = 5, 
                                type = "B")
}
logan2 <- subset(logan, doy > 121 & doy < 243)

str(logan2)
gddperyear <- aggregate(GDD_5 ~ year, logan2, FUN = max)
library(dplyr)

gdd_5yr_intervals <- gddperyear %>%
  mutate(interval = floor((year - min(year)) / 5)) %>%
  group_by(interval) %>%
  summarise(
    year_start = min(year),
    year_end = max(year),
    GDD_avg = mean(GDD_5, na.rm = TRUE),
    n_years = n()
  ) %>%
  select(-interval)

# write csv
write.csv(gddperyear, "output/longTermGDDperYear.csv", row.names = FALSE)
write.csv(gdd_5yr_intervals, "output/longTermGDD5YrAvg.csv", row.names = FALSE)


