## Started 17 April 2024 ##
## By Lizzie, but pulling from clean_TS.R (and clean_TS_multgardens.R for first lines) ##
# updated by christophe on 4 march 2025 to adapt adapt the observations for coring the trees in april
# re-updated by CRD on 29 April 2025 in the plane from Denver -- > Vancouver. Removed a bunch of lines I considered redundant

## How to download data (Apr 2024):
# 1) Go to the NPN Data Downloader Tool: https://data.usanpn.org/observations
# And go to the Phenology Observation Portal
# 2) Select 'Individual Phenometrics' and press NEXT 
# 3) Set Date range applicable to your question and press 'Set Date' and NEXT
# 4) Select 'Partner Groups' tab on left: press the + next to 
# 'Botanic Gardens and Arboretums and select 'Arnold Arboretum - Tree Spotters'
# Press 'Set Groups' and NEXT
# 5) Select 'Output fields' tab on left: and select 'ObservedBy Person ID' and 'Multiple Observers' and ...
# in 2024 I added these (as I think they are in the code): 'NumYs in Series' and 'Multiple FirstY'


## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
# library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
# library(lubridate)


# Set Working Directory
# Lizzie's directory
# setwd("~/Documents/git/projects/treegarden/treespotters")
# Christophe's directory
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")
indPheno <- read.csv("input/individual_phenometrics_data2025.csv", header=TRUE) # from NPN
trees2core <- read.csv("input/2025ApprovedPlantListforcoring.csv", header=TRUE) # from list of trees we can core
coord <- read.csv("input/listTreesfromInteractiveMap.csv", header=TRUE) # from arboretum explorer map 

# Remove tree number from the plant nickname column
indPheno$plantNickname <- sub(", Tree \\d+", "", indPheno$Plant_Nickname)
unique(indPheno$plantNickname) 

### === === === === === === === ###
# Clean approved list for coring #
### === === === === === === === ###
# Select in the main df only the trees we can core
d <- indPheno[indPheno$plantNickname %in% trees2core$ACC_NUM.QUAL,]
### First let's do the obligatory cleaning checks with citizen scienece data
d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),] ## This selects data where multiple people observed the same phenophase.
d <- d[(d$NumYs_in_Series>=3),] ## This selects data again where the same phenophase was seen 3 times in a row
d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] ## And this limits to data where a no is followed by a yes, so that it is a new observation/new phenophase but has been detected within a reasonable timeframe
d$latbi <- paste(d$Genus, d$Species, sep=" ")

### === === === === === === === === ===###
# Clean Arboretum tree coordinates file #
### === === === === === === === === ===###
# select only coordinates column
coordcut <- coord[, c("Plant.ID", 
                      "Garden.Latitude", 
                      "Garden.Longitude", 
                      "Accession.Date", 
                      # "Plant.Source", 
                      "Habitat", 
                      "Garden.Location",
                      "DBH")]
# rename columns
coordcut <- coordcut %>%
  rename(
    PlantID = Plant.ID,
    gardenLocation = Garden.Location,
    lat = Garden.Latitude,
    long = Garden.Longitude,
    AccessionDate = Accession.Date,
    habitat = Habitat,
    DBH = DBH
  )

### === === === === === === === ===###
# Clean individual phenometric data #
### === === === === === === === ===###
# change from NPN output to more digestible column names
bb <- d %>%
  rename(
    lat = Latitude,
    long = Longitude,
    elev = Elevation_in_Meters,
    year = First_Yes_Year,
    month = First_Yes_Month,
    day = First_Yes_Day,
    doy = First_Yes_DOY,
    numYs = Multiple_Observers,
    phase = Phenophase_Description,
    id = Individual_ID,
    genus = Genus,
    species = Species,
    symbol = USDA_PLANTS_Symbol,
    plantNickname = plantNickname
  )
# add latin binomial column
bb$latbi <- paste(bb$genus, bb$species, sep=" ")

#clean common name column
bb$Common_Name[which(bb$Common_Name == "yellow birch")] <- "Yellow birch"
bb$Common_Name[which(bb$Common_Name == "river birch")] <- "River birch"
bb$Common_Name[which(bb$Common_Name == "sugar maple")] <- "Sugar maple"
bb$Common_Name[which(bb$Common_Name == "white oak")] <- "White oak"
bb$Common_Name[which(bb$Common_Name == "eastern cottonwood")] <- "Eastern cottonwood"
bb$Common_Name[which(bb$Common_Name == "yellow buckeye")] <- "Yellow buckeye"
bb$Common_Name[which(bb$Common_Name == "American beech")] <- "American beech"
bb$Common_Name[which(bb$Common_Name == "pignut hickory")] <- "Pignut hickory"
bb$Common_Name[which(bb$Common_Name == "shagbark hickory")] <- "Shagbark hickory"
bb$Common_Name[which(bb$Common_Name == "northern red oak")] <- "Northern red oak"
bb$Common_Name[which(bb$Common_Name == "red maple")] <- "Red maple"

bb.pheno<-dplyr::select(bb, genus, species, latbi, Common_Name, phase, year, doy, numYs, plantNickname)

# clean phenophase names
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leafDrop", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Colored leaves", "coloredLeaves", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Fruits", "fruits", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Ripe fruits", "ripeFruits", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Recent fruit or seed drop", "RecentFruitorSeedDrop", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Increasing leaf size", "IncreasingLeafSize", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Open flowers", "OpenFlowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Pollen release (flowers)", "PollenRelease", bb.pheno$phase)

### Now work on finding day of budburst, etc.
bb.pheno <-filter(bb.pheno, numYs>0)
# Below, I group each individual by phenophase and year to find the first observation (using the slice function), 
## so first day of budburst for that individual for that year
doy_pheno<-bb.pheno%>% 
  group_by(plantNickname, phase, year) %>% 
  slice(which.min(doy))

# remove duplicates
doy_pheno<-doy_pheno[!duplicated(doy_pheno),]
# spread this table so every phase gets its column!
phenos<-doy_pheno%>%tidyr::spread(phase, doy)

# remove outliers e.g. when budburst is way to late in the summer
phenos1 <- subset(phenos , budburst<160)
phenos2 <- subset(phenos1 , leafout<250) # redefine this to make sure its ok

# Add tree coordinates
phenosCoord <- merge(phenos, coordcut, by.x="plantNickname", by.y="PlantID", all.x=TRUE)
head(phenosCoord)
# write it up!
write.csv(phenosCoord, file="output/cleanTS.csv", row.names=FALSE)


