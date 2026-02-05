# Started 4 December 2025
# CRD

# get the different datasets together

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")

### === === === === === === === === === === ###
# Get cleaned data from this repo #
### === === === === === === === === === === ###
# 1. Get the data from the cleaning ring width file: coressub
source("cleaning/source/cleaningCores.R") 

# 2. Get observation data from main repo: ts_cleaned
source("cleaning/source/cleanObsData.R")

# 3. Read climate csv created from wildchrokie repo
weldhillcomp <- read.csv("input/weldhillClimateCleaned.csv")

# 4. Calculate primary and full growing season GDD: obsdataWithGDD
source("cleaning/source/calculateGrowingSeasonGDD.R")

# change some stuff so they match
names(coressub)[which(names(coressub) == "yearCor")] <- "year"
names(coressub)
names(obsdataWithGDD)

str(coressub)
str(obsdataWithGDD)
temp <- merge(coressub, obsdataWithGDD, by = c("id", "year"))

# rename species and remove duplicated symbol col
temp <- temp[ , !(names(temp) == "symbol")]
names(temp)[which(names(temp) == "species.x")] <- "symbol"
names(temp)[which(names(temp) == "species.y")] <- "species"

# # make some checks
# unique(temp3$sampleType)
# # remove 2 samples for a given individuals
# temp4 <- subset(temp3, sampleType != "coresWithCookies") 
# temp5 <- temp4[!is.na(temp4$pgsGDD),]
# temp5$idyear <- paste(temp5$treeid, temp5$year, sep = "_")
# temp6 <- temp5[!duplicated(temp5$idyear),] 
# 
temp$lengthMM <- temp$lengthCM*10

# average each cores per id
meancore <- aggregate(lengthMM ~ id + year, temp, FUN = mean)
meancore$idyear <- paste(meancore$id, meancore$year)

# reorganize and average the 2 cores per id
temp$idyear <- paste(temp$id, temp$year)
temp <- temp[!duplicated(temp$idyear), c("id", 
                                      "year",
                                      "symbol",
                                      "genus",
                                      "species",
                                      "latbi",
                                      "commonName",
                                      "budburst",
                                      "increasingLeafSize",
                                      "leafout",
                                      "flowers",
                                      "openFlowers",
                                      "pollenRelease",
                                      "fruits",
                                      "ripeFruits",
                                      "recentFruitorSeedDrop",
                                      "coloredLeaves",
                                      "DBH",
                                      "accessionDate",
                                      "pgs",
                                      "fgs",
                                      "budburstGDD5",
                                      "leafoutGDD5",
                                      "leafcolorGDD5",
                                      "pgsGDD5",
                                      "fgsGDD5",
                                      "budburstGDD10",
                                      "leafoutGDD10",
                                      "leafcolorGDD10",
                                      "pgsGDD10",
                                      "fgsGDD10",
                                      "idyear"
)]

temp$lengthMM <- meancore$lengthMM[match(temp$idyear, meancore$idyear)]
temp <- temp[order(temp$idyear), ]
temp <- subset(temp, select = !(names(temp) %in% "idyear"))

# write csv
write.csv(temp, "output/empiricalDataMAIN.csv")
# 
# # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# # Now without tree rings!
# obsdataWithGDD$siteplot <- sub("^[^_]*_(.*?)_.*$", "\\1", obsdataWithGDD$treeid)
# obsdataWithGDD$site <- substr(obsdataWithGDD$siteplot, 0,2)
# obsdataWithGDD$plot <- substr(obsdataWithGDD$siteplot, 3,4)
# obsdataWithGDD$replicate <- sub(".*_", "", obsdataWithGDD$treeid)
# 
# obsdataWithGDD <- obsdataWithGDD[order(
#   obsdataWithGDD$spp,
#   obsdataWithGDD$site,
#   obsdataWithGDD$replicate,
#   obsdataWithGDD$year
# ), ]
# 
# write_csv(obsdataWithGDD, "output/empiricalDataNORing.csv")
