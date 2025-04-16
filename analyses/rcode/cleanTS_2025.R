## Started 17 April 2024 ##
## By Lizzie, but pulling from clean_TS.R (and clean_TS_multgardens.R for first lines) ##

# updated by christophe on 4 march 2025 to adapt adapt the observations for coring the trees in april

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
indPheno <- read.csv("input/individual_phenometrics_data2025.csv", header=TRUE)
trees2core <- read.csv("input/2025ApprovedPlantListforcoring.csv", header=TRUE)
coord <- read.csv("input/listTreesfromInteractiveMap.csv", header=TRUE)

# subset only for tree species
trees <- c("highbush blueberry",  "American witchhazel", "possumhaw")
treesdf <- subset(indPheno, !(Common_Name %in% trees))

# Remove tree number from the plant nickname column
treesdf$plantNickname <- sub(", Tree \\d+", "", treesdf$Plant_Nickname)

### === === === === === === === ###
# Clean approved list for coring #
### === === === === === === === ###
# Select in the main df only the trees we can core
d <- treesdf[treesdf$plantNickname %in% trees2core$ACC_NUM.QUAL,]
length(unique(d$plantNickname)) # right now it's 50 trees, not 52 because they gave permission to core 52 trees, but two of them are q. coccina for which we don't have data on. 
# setdiff(unique(trees2core$ACC_NUM.QUAL), unique(e$plantNickname))
### First let's do the obligatory cleaning checks with citizen scienece data
d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),] ## This selects data where multiple people observed the same phenophase
d <- d[(d$NumYs_in_Series>=3),] ## This selects data again where the same phenophase was seen 3 times in a row
d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] ## And this limits to data where a no is followed by a yes, so that it is a new observation/new phenophase but has been detected within a reasonable timeframe
str(d)

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
<<<<<<< HEAD
    symbol = USDA_PLANTS_Symbol,
    plantNickname = plantNickname
  )
bb$latbi <- paste(bb$genus, bb$species, sep="_")
=======
    plantNickname = plantNickname
  )

>>>>>>> fd4fc856699d80f367dc327b8341a8254f463e48
#clean common name column
bb$Common_Name[which(bb$Common_Name == "yellow birch")] <- "Yellow Birch"
bb$Common_Name[which(bb$Common_Name == "river birch")] <- "River Birch"
bb$Common_Name[which(bb$Common_Name == "sugar maple")] <- "Sugar Maple"
bb$Common_Name[which(bb$Common_Name == "American basswood")] <- "American Basswood"
bb$Common_Name[which(bb$Common_Name == "white oak")] <- "White Oak"
bb$Common_Name[which(bb$Common_Name == "eastern cottonwood")] <- "Eastern Cottonwood"
bb$Common_Name[which(bb$Common_Name == "yellow buckeye")] <- "Yellow Buckeye"
bb$Common_Name[which(bb$Common_Name == "American beech")] <- "American Beech"
bb$Common_Name[which(bb$Common_Name == "pignut hickory")] <- "Pignut Hickory"
bb$Common_Name[which(bb$Common_Name == "shagbark hickory")] <- "Shagbark Hickory"
bb$Common_Name[which(bb$Common_Name == "northern red oak")] <- "Northern Red Oak"
bb$Common_Name[which(bb$Common_Name == "red maple")] <- "Red Maple"

<<<<<<< HEAD
bb.pheno<-dplyr::select(bb, genus, species, latbi, symbol, Common_Name, phase, elev, year, doy, numYs, id, plantNickname)
=======
bb.pheno<-dplyr::select(bb, genus, species, Common_Name, phase, elev, year, doy, numYs, id, plantNickname)
>>>>>>> fd4fc856699d80f367dc327b8341a8254f463e48
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
bb.pheno<-filter(bb.pheno, numYs>0)
# Below, I group each individual by phenophase and year to find the first observation (using the slice function), 
## so first day of budburst for that individual for that year
doy_pheno<-bb.pheno%>% 
  group_by(plantNickname, phase, year) %>% 
  slice(which.min(doy))
doy_pheno<-doy_pheno[!duplicated(doy_pheno),]

### Clean observation error!
if(FALSE){ #Detected with new cleaning checks!! 3 July 2019 by Cat
  # QbyLizzie: How did you find this? - I found this by making initial raw data plots. There was a outlier and I went back 
  ## to check the data. It was a new volunteer who made a couple of mistakes.
  doy_pheno$doy<-ifelse(doy_pheno$species=="alleghaniensis" & doy_pheno$year==2016 & doy_pheno$doy==59, NA, doy_pheno$doy)
  doy_pheno<-doy_pheno[!is.na(doy_pheno$doy),]
}

#### Now start building a small data frame with phenophase info then add in climandpheno, chilling and photo
<<<<<<< HEAD
colstokeep<-c("genus", "species", "latbi", "symbol", "Common_Name", "id", "plantNickname", "year", "phase", "elev", "doy")
=======
colstokeep<-c("genus", "species", "Common_Name", "id", "plantNickname", "year", "phase", "elev", "doy")
>>>>>>> fd4fc856699d80f367dc327b8341a8254f463e48
phenos<-subset(doy_pheno, select=colstokeep)

phenos<-phenos[!duplicated(phenos),]

phenos<-phenos%>%tidyr::spread(phase, doy)

<<<<<<< HEAD
phenos <- subset(phenos, select=c("genus", "species", "latbi", "symbol", "Common_Name", "id", "plantNickname", "year", "elev", "budburst", "flowers", "fruits", "leafout", "coloredLeaves", "leafDrop"))
=======
phenos <- subset(phenos, select=c("genus", "species", "Common_Name", "id", "plantNickname", "year", "elev", "budburst", "flowers", "fruits", "leafout", "coloredLeaves", "leafDrop"))
>>>>>>> fd4fc856699d80f367dc327b8341a8254f463e48

### And now last observation for when to start calculating chilling
phenos$last.obs<-ave(phenos$leafDrop, phenos$plantNickname, phenos$year, FUN=last)
phenos$last.obs<-ifelse(is.na(phenos$last.obs), ave(phenos$coloredLeaves, phenos$plantNickname, phenos$year, FUN=last), phenos$last.obs)

## For gdd start and chilling end
phenos$gdd.start<-46 # 15 February for each year - arbitrary, can change

# Add tree coordinates
phenosCoord <- merge(phenos, coordcut, by.x="plantNickname", by.y="PlantID", all.x=TRUE)
head(phenosCoord)

# Add columns that micheal provided
write.csv(phenosCoord, file="output/cleanTS.csv", row.names=FALSE)


