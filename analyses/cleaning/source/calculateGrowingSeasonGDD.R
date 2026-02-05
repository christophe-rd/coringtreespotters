## From main wildchrokie repo
# CRD on 4 December 2025

# Goal is calculate primary and full growing season for treespotters trees

# Note: for this to work, the code "getAll.R" needs to be run upstream

# Clear workspace 
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")

# Load libraries
library(pollen)

# copy of ts
ts <- ts_cleaned

###calculate primary growth season and full growing season in days
ts$pgs <- ts$coloredLeaves-ts$leafout
ts$fgs <- ts$coloredLeaves-ts$budburst

nrow(ts[!is.na(ts$pgs), ])
nrow(ts[!is.na(ts$fgs), ])

# calculate GDD
###give each year a seperate data frame
y15 <- subset(weldhillcomp, year=="2015")
y16 <- subset(weldhillcomp, year=="2016")
y17 <- subset(weldhillcomp, year=="2017")
y18 <- subset(weldhillcomp, year=="2018")
y19 <- subset(weldhillcomp, year=="2019")
y20 <- subset(weldhillcomp, year=="2020")
y21 <- subset(weldhillcomp, year=="2021")
y22 <- subset(weldhillcomp, year=="2022")
y23 <- subset(weldhillcomp, year=="2023")
y24 <- subset(weldhillcomp, year=="2024")

###Calculate gdd
y15$GDD_10 <- gdd(tmax = y15$maxT, tmin = y15$minT, tbase = 10, type = "B")
y16$GDD_10 <- gdd(tmax = y16$maxT, tmin = y16$minT, tbase = 10, type = "B")
y17$GDD_10 <- gdd(tmax = y17$maxT, tmin = y17$minT, tbase = 10, type = "B")
y18$GDD_10 <- gdd(tmax = y18$maxT, tmin = y18$minT, tbase = 10, type = "B")
y19$GDD_10 <- gdd(tmax = y19$maxT, tmin = y19$minT, tbase = 10, type = "B")
y20$GDD_10 <- gdd(tmax = y20$maxT, tmin = y20$minT, tbase = 10, type = "B")
y21$GDD_10 <- gdd(tmax = y21$maxT, tmin = y21$minT, tbase = 10, type = "B")
y22$GDD_10 <- gdd(tmax = y22$maxT, tmin = y22$minT, tbase = 10, type = "B")
y23$GDD_10 <- gdd(tmax = y23$maxT, tmin = y23$minT, tbase = 10, type = "B")
y24$GDD_10 <- gdd(tmax = y24$maxT, tmin = y24$minT, tbase = 10, type = "B")

###Calculate gdd
y15$GDD_5 <- gdd(tmax = y15$maxT, tmin = y15$minT, tbase = 5, type = "B")
y16$GDD_5 <- gdd(tmax = y16$maxT, tmin = y16$minT, tbase = 5, type = "B")
y17$GDD_5 <- gdd(tmax = y17$maxT, tmin = y17$minT, tbase = 5, type = "B")
y18$GDD_5 <- gdd(tmax = y18$maxT, tmin = y18$minT, tbase = 5, type = "B")
y19$GDD_5 <- gdd(tmax = y19$maxT, tmin = y19$minT, tbase = 5, type = "B")
y20$GDD_5 <- gdd(tmax = y20$maxT, tmin = y20$minT, tbase = 5, type = "B")
y21$GDD_5 <- gdd(tmax = y21$maxT, tmin = y21$minT, tbase = 5, type = "B")
y22$GDD_5 <- gdd(tmax = y22$maxT, tmin = y22$minT, tbase = 5, type = "B")
y23$GDD_5 <- gdd(tmax = y23$maxT, tmin = y23$minT, tbase = 5, type = "B")
y24$GDD_5 <- gdd(tmax = y24$maxT, tmin = y24$minT, tbase = 5, type = "B")

gdd <- rbind(y15, y16, y17, y18, y19, y20, y21, y22, y23, y24)
str(gdd)

write.csv(gdd, "output/gddByYear.csv")

nrow(ts[!is.na(ts$leafout),]) # missing 9 rows of NaN

unique(ts$budburst)
unique(ts$leafout)
unique(ts$coloredLeaves)

# create new GDD empty columns
ts$budburstGDD5 <- NA
ts$leafoutGDD5 <- NA
ts$leafcolorGDD5 <- NA

ts$budburstGDD10 <- NA
ts$leafoutGDD10 <- NA
ts$leafcolorGDD10 <- NA

gdd$year <- as.numeric(gdd$year)

# Loop over rows FOR GDD 5
for(i in 1:nrow(ts)) {
  yr <- ts$year[i]
  
  # Budburst
  if(!is.na(ts$budburst[i])) {
    idx <- which(gdd$year == yr & gdd$doy == ts$budburst[i])
    if(length(idx) == 1) ts$budburstGDD5[i] <- gdd$GDD_5[idx]
  }
  
  # Leafout
  if(!is.na(ts$leafout[i])) { # 49
    idx <- which(gdd$year == yr & gdd$doy == ts$leafout[i])
    if(length(idx) == 1) ts$leafoutGDD5[i] <- gdd$GDD_5[idx]
  }
  
  # Leafcolor
  if(!is.na(ts$coloredLeaves[i])) {
    idx <- which(gdd$year == yr & gdd$doy == ts$coloredLeaves[i])
    if(length(idx) == 1) ts$leafcolorGDD5[i] <- gdd$GDD_5[idx]
  }
  
}

# Loop over rows FOR GDD 10
for(i in 1:nrow(ts)) {
  yr <- ts$year[i]
  
  # Budburst
  if(!is.na(ts$budburst[i])) {
    idx <- which(gdd$year == yr & gdd$doy == ts$budburst[i])
    if(length(idx) == 1) ts$budburstGDD10[i] <- gdd$GDD_10[idx]
  }
  
  # Leafout
  if(!is.na(ts$leafout[i])) { # 49
    idx <- which(gdd$year == yr & gdd$doy == ts$leafout[i])
    if(length(idx) == 1) ts$leafoutGDD10[i] <- gdd$GDD_10[idx]
  }
  
  # Leafcolor
  if(!is.na(ts$coloredLeaves[i])) {
    idx <- which(gdd$year == yr & gdd$doy == ts$coloredLeaves[i])
    if(length(idx) == 1) ts$leafcolorGDD10[i] <- gdd$GDD_10[idx]
  }
  
}
str(ts)

# check that the loop didn't miss any rows
nrow(ts[!is.na(ts$budburst),]) 
nrow(ts[!is.na(ts$budburstGDD10),])

nrow(ts[!is.na(ts$leafout),]) 
nrow(ts[!is.na(ts$leafoutGDD10),])

nrow(ts[!is.na(ts$coloredLeaves),]) 
nrow(ts[!is.na(ts$leafcolorGDD10),])

# add primary GS and full GS cols GDD 5
ts$pgsGDD5 <- ts$leafcolorGDD5 - ts$leafoutGDD5
nrow(ts[!is.na(ts$pgsGDD),]) 
ts$fgsGDD5 <- ts$leafcolorGDD5 - ts$budburstGDD5

# add primary GS and full GS cols GDD 10
ts$pgsGDD10 <- ts$leafcolorGDD10 - ts$leafoutGDD10
nrow(ts[!is.na(ts$pgsGDD),]) 
ts$fgsGDD10 <- ts$leafcolorGDD10 - ts$budburstGDD10

# NEW WAY TO CALCULATE GDD
# calculate leaf colouring avg per species
ts$sppyear <- paste(ts$commonName, ts$year, sep="")

colournona <- aggregate(coloredLeaves ~ sppyear, ts, FUN = mean)

ts$coloredLeavesAVG <- colournona$coloredLeaves[match(ts$sppyear, colournona$sppyear)]

# check number of extra rows this gives me
nrow(ts[!is.na(ts$coloredLeaves),])
nrow(ts[!is.na(ts$coloredLeavesAVG),]) # gives me 19 extra rows...

# add max gdd per year
# ts$fullGDD <- NA
# y18maxGDD <- max(y18$GDD_10)
# y19maxGDD <- max(y19$GDD_10)
# y20maxGDD <- max(y20$GDD_10)
# ts$fullGDD[which(ts$year == "2018")] <- y18maxGDD
# ts$fullGDD[which(ts$year == "2019")] <- y19maxGDD
# ts$fullGDD[which(ts$year == "2020")] <- y20maxGDD

obsdataWithGDD <- ts

