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

# average 1 measurement per year
coressub$lengthMM <- coressub$lengthCM*10

# average each cores per id
meancore <- aggregate(lengthMM ~ id + year, coressub, FUN = mean)

# prep to add rw to obs data
meancore$idyear <- paste(meancore$id, meancore$year)
obsdataWithGDD$idyear <- paste(obsdataWithGDD$id, obsdataWithGDD$year)

# add rw to obs data
obsdataWithGDD$lengthMM <- meancore$lengthMM[match(obsdataWithGDD$idyear, 
                                                   meancore$idyear)]
# # make some checks
# unique(temp3$sampleType)
# # remove 2 samples for a given individuals
# temp4 <- subset(temp3, sampleType != "coresWithCookies") 
# temp5 <- temp4[!is.na(temp4$pgsGDD),]
# temp5$idyear <- paste(temp5$treeid, temp5$year, sep = "_")
# temp6 <- temp5[!duplicated(temp5$idyear),] 
# 
nrow(obsdataWithGDD[!is.na(obsdataWithGDD$leafout), ])
# reorganize 
obsdataWithGDD <- obsdataWithGDD[!duplicated(obsdataWithGDD$idyear), c("id", 
                                      "year",
                                      "genus",
                                      "species",
                                      "latbi",
                                      "commonName",
                                      "plantNickname",
                                      "budburst",
                                      "leafout",
                                      "coloredLeaves",
                                      "DBH",
                                      "lengthMM",
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
                                      "pgsGDD10AVG",
                                      "idyear"
)]

obsdataWithGDD <- obsdataWithGDD[order(obsdataWithGDD$idyear), ]
obsdataWithGDD$pgsGSL <- obsdataWithGDD$coloredLeaves - obsdataWithGDD$leafout
nrow(obsdataWithGDD[!is.na(obsdataWithGDD$leafout), ])
obsdataWithGDD$idyear <- NULL

# change the species names to abreviated genus
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Acer rubrum"]           <- "A. rubrum"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Acer saccharum"]        <- "A. saccharum"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Aesculus flava"]        <- "Ae. flava"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Betula alleghaniensis"] <- "B. alleghaniensis"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Betula nigra"]          <- "B. nigra"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Carya glabra"]          <- "C. glabra"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Carya ovata"]           <- "C. ovata"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Populus deltoides"]     <- "P. deltoides"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Quercus alba"]          <- "Q. alba"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Quercus rubra"]         <- "Q. rubra"
obsdataWithGDD$latbi[obsdataWithGDD$latbi == "Tilia americana"]       <- "T. americana"

# write csv
write.csv(obsdataWithGDD, "output/empiricalDataMAIN.csv")
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
