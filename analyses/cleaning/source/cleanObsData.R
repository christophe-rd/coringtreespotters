## Started 17 April 2024 ##
## By Lizzie, but pulling from clean_TS.R (and clean_TS_multgardens.R for first lines) ##
# updated by christophe on 4 march 2025 to adapt adapt the observations for coring the trees in april
# re-updated by CRD on 29 April 2025 in the plane from Denver -- > Vancouver. Removed a bunch of lines I considered redundant

## How to download data (Apr 2024):
# 1) Go to the NPN Data Downloader Tool: https://data.usanpn.org/observations
# And go to the Phenology Observation Portal
# 2) Select 'Individual Phenometrics' and press NEXT 
# 3) Set Date range applicable to your question and press 'Set Date' and NEXT
# 4) Select 'Partner Groups' tab on left: press the + next to 'Botanic Gardens and Arboretums and select 'Arnold Arboretum - Tree Spotters'
# Press 'Set Groups' and NEXT
# 5) Select 'Output fields' tab on left: and select 'ObservedBy Person ID' and 'Multiple Observers' and ... in 2024 I added these (as I think they are in the code): 'NumYs in Series' and 'Multiple FirstY'

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
### First let's do the obligatory cleaning checks with citizen science data
d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),] ## This selects data where multiple people observed the same phenophase.
d <- d[(d$NumYs_in_Series>=3),] ## This selects data again where the same phenophase was seen 3 times in a row
d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] ## And this limits to data where a no is followed by a yes, so that it is a new observation/new phenophase but has been detected within a reasonable timeframe

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
bb$commonName <- bb$Common_Name
bb$commonName[which(bb$Common_Name == "yellow birch")] <- "Yellow birch"
bb$commonName[which(bb$Common_Name == "river birch")] <- "River birch"
bb$commonName[which(bb$Common_Name == "sugar maple")] <- "Sugar maple"
bb$commonName[which(bb$Common_Name == "white oak")] <- "White oak"
bb$commonName[which(bb$Common_Name == "eastern cottonwood")] <- "Eastern cottonwood"
bb$commonName[which(bb$Common_Name == "yellow buckeye")] <- "Yellow buckeye"
bb$commonName[which(bb$Common_Name == "American beech")] <- "American beech"
bb$commonName[which(bb$Common_Name == "pignut hickory")] <- "Pignut hickory"
bb$commonName[which(bb$Common_Name == "shagbark hickory")] <- "Shagbark hickory"
bb$commonName[which(bb$Common_Name == "northern red oak")] <- "Northern red oak"
bb$commonName[which(bb$Common_Name == "red maple")] <- "Red maple"

bb.pheno <- bb[, c("genus", "species", "latbi", "commonName", "symbol", "phase", "year", "doy", "numYs", "plantNickname")]

# clean phenophase names
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leafDrop", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Colored leaves", "coloredLeaves", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Fruits", "fruits", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Ripe fruits", "ripeFruits", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Recent fruit or seed drop", "recentFruitOrSeedDrop", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Increasing leaf size", "increasingLeafSize", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Open flowers", "openFlowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Pollen release (flowers)", "pollenRelease", bb.pheno$phase)

### Now work on finding day of budburst, etc.
bb.pheno <- filter(bb.pheno, numYs>0)

# Below, I group each individual by phenophase and year to find the first observation (using the slice function), 
## so first day of budburst for that individual for that year
doy_pheno <- bb.pheno%>% 
  group_by(plantNickname, phase, year) %>% 
  slice(which.min(doy))

# spread this table so every phase gets its column!
phenos <- doy_pheno %>% tidyr::spread(phase, doy)

# remove outliers e.g. when budburst is way to late in the summer
phenos1 <- subset(phenos, budburst < 160)
phenos2 <- subset(phenos1, leafout < 250) # redefine this to make sure its ok

# Add tree coordinates
phenosCoord <- merge(phenos2, coordcut, by.x="plantNickname", by.y="PlantID", all.x=TRUE)

head(phenosCoord)

# clean treeid names to fit my format

phenosCoord$plantNicknamelatbi <- paste(phenosCoord$plantNickname, phenosCoord$latbi, sep = "_")

phenosCoord$idnew <- NA
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "11253*A_Quercus rubra")] <- "QURU_11253_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "11253*A_Quercus rubra")] <- "QURU_11253_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "1196-84*A_Acer rubrum")] <- "ACRU_1196-84_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "1199*D_Betula nigra")] <- "BENI_1199_D"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "1199*J_Betula nigra")] <- "BENI_1199_J"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "1251-79*A_Betula nigra")] <- "BENI_1251-79_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "1251-79*B_Betula nigra")] <- "BENI_1251-79_B"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "1251-79*E_Betula nigra")] <- "BENI_1251-79_E"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12565*C_Acer saccharum")] <- "ACSA_12565_C"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12651*C_Aesculus flava")] <- "AEFL_12651_C"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12651*D_Aesculus flava")] <- "AEFL_12651_D"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12651*H_Aesculus flava")] <- "AEFL_12651_H"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12651*I_Aesculus flava")] <- "AEFL_12651_I"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12843*A_Betula alleghaniensis")] <- "BEAL_12843_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12843*D_Betula alleghaniensis")] <- "BEAL_12843_D"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12843*E_Betula alleghaniensis")] <- "BEAL_12843_E"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12843*H_Betula alleghaniensis")] <- "BEAL_12843_H"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12907*B_Carya ovata")] <- "CAOV_12907_B"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12907*D_Carya ovata")] <- "CAOV_12907_D"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12907*G_Carya ovata")] <- "CAOV_12907_G"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12907*I_Carya ovata")] <- "CAOV_12907_I"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "12907*N_Carya ovata")] <- "CAOV_12907_N"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "1323-82*A_Tilia americana")] <- "TIAM_1323-82_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "15350*A_Acer rubrum")] <- "ACRU_15350_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "16611*F_Populus deltoides")] <- "PODE_16611_F"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "16611*J_Populus deltoides")] <- "PODE_16611_J"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "16611*K_Populus deltoides")] <- "PODE_16611_K"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "17527*D_Tilia americana")] <- "TIAM_17527_D"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "17538*A_Tilia americana")] <- "TIAM_17538_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "187-2006*B_Acer saccharum")] <- "ACSA_187-2006_B"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "19804*A_Tilia americana")] <- "TIAM_19804_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "20098*A_Carya glabra")] <- "CAGL_20098_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "2019*R_Carya glabra")] <- "CAGL_2019_R"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "2019*S_Carya glabra")] <- "CAGL_2019_S"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "20645*A_Acer saccharum")] <- "ACSA_20645_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "21815*E_Quercus alba")] <- "QUAL_21815_E"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "22099*A_Populus deltoides")] <- "PODE_22099_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "227-2011*A_Quercus alba")] <- "QUAL_227-2011_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "22834*B_Acer saccharum")] <- "ACSA_22834_B"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "22886*D_Quercus alba")] <- "QUAL_22886_D"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "358-82*A_Quercus alba")]  <- "QUAL_358-82_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "524-2009*A_Acer rubrum")] <- "ACRU_524-2009_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "525-2009*A_Acer rubrum")] <- "ACRU_525-2009_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "5859*A_Quercus rubra")] <- "QURU_5859_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "5859*B_Quercus rubra")] <- "QURU_5859_B"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "611-2010*A_Quercus alba")] <- "QUAL_611-2010_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "689-2010*A_Acer saccharum")] <- "ACSA_689-2010_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "6990*A_Carya glabra")] <- "CAGL_6990_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "7141*A_Tilia americana")] <- "TIAM_7141_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "8197*A_Quercus rubra")] <- "QURU_8197_A"
phenosCoord$idnew[which(phenosCoord$plantNicknamelatbi == "925-79*B_Aesculus flava")] <- "AEFL_925-79_B"

# setdiff(unique(phenosCoord$idnew), unique(og$id2))
# setdiff(unique(og$id2), unique(phenosCoord$idnew))

ts_cleaned <- phenosCoord[c(
  "idnew",
  "genus",
  "species", 
  "latbi",
  "commonName",
  "symbol",
  "year",
  "budburst",
  "increasingLeafSize",
  "leafout",
  "flowers",
  "openFlowers",
  "pollenRelease",
  "fruits",
  "ripeFruits",
  "recentFruitOrSeedDrop",
  "coloredLeaves",
  "DBH",
  "AccessionDate",
  "lat",
  "long"
)]

colnames(ts_cleaned) <- c(
  "id",
  "genus",
  "species", 
  "latbi",
  "commonName",
  "symbol",
  "year",
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
  "lat",
  "long"
)

ts_cleaned <- ts_cleaned[order(
  ts_cleaned$latbi,
  ts_cleaned$id,
  ts_cleaned$year
), ]

# write it up!
write.csv(ts_cleaned, file="output/cleanTS.csv", row.names=FALSE)

# remove duplicates
temp <- phenosCoord[!duplicated(phenosCoord$plantNickname),]
# count per latbi
temp %>%
  count(latbi)
