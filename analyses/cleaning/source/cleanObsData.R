## Started 17 April 2024 ##
## By Lizzie, but pulling from clean_TS.R (and clean_TS_multgardens.R for first lines) ##
# updated by christophe on 4 march 2025 to adapt adapt the observations for coring the trees in april
# re-updated by CRD on 29 April 2025 in the plane from Denver -- > Vancouver. 

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

leafcolorchecks <- subset(indPheno, plantNickname %in% c("15350*A", "22834*B", "12651*D"))
coloredleaves <- subset(leafcolorchecks, Phenophase_Description == "Colored leaves")
coloredleaves2 <- subset(indPheno, Phenophase_Description == "Colored leaves")
hist(coloredleaves2$First_Yes_DOY)
leafcolorchecks$Phenophase_Description

coloredleaves$Individual_ID <- as.numeric(coloredleaves$First_Yes_DOY)

coloredleaves$sppname <- paste(coloredleaves$Genus, coloredleaves$Species, coloredleaves$plantNickname, sep = "_")

ggplot(coloredleaves, aes(x = First_Yes_DOY, fill = sppname)) + 
  geom_histogram() + 
  facet_wrap(First_Yes_Year~sppname) + 
  labs(x="observed leaf colouring date") +
  geom_vline(xintercept=150) +
  theme_minimal() 


unique(leafcolorchecks$plantNickname)
                          
### === === === === === === === ###
# Clean approved list for coring #
### === === === === === === === ###
# Select in the main df only the trees we can core
d <- indPheno[indPheno$plantNickname %in% trees2core$ACC_NUM.QUAL,]
coloredleaves3 <- subset(d, Phenophase_Description == "Colored leaves")
hist(coloredleaves3$First_Yes_DOY)

# save d before cleaning to get raw observations data
d2 <- d

### First let's do the obligatory cleaning checks with citizen science data
d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),] ## This selects data where multiple people observed the same phenophase.
coloredleaves3 <- subset(d, Phenophase_Description == "Colored leaves")
hist(coloredleaves3$First_Yes_DOY)

d <- d[(d$NumYs_in_Series>=3),] ## This selects data again where the same phenophase was seen 3 times in a row
coloredleaves3 <- subset(d, Phenophase_Description == "Colored leaves")
hist(coloredleaves3$First_Yes_DOY)

d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] ## And this limits to data where a no is followed by a yes, so that it is a new observation/new phenophase but has been detected within a reasonable timeframe


coloredleaves3 <- subset(d, Phenophase_Description == "Colored leaves")
hist(coloredleaves3$First_Yes_DOY)

coloredleaves3$sppname <- paste(coloredleaves3$Genus, coloredleaves3$Species, coloredleaves3$plantNickname, sep = "_")

coloredleaves3 <- subset(coloredleaves3, plantNickname %in% c("15350*A", "22834*B", "12651*D"))

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
## so first day of phenophase for that individual for that year and max for leaf color as discussed here: https://github.com/christophe-rd/coringtreespotters/issues/14
for(p in unique(bb.pheno$phase)) {
  assign(p, bb.pheno[bb.pheno$phase == p, ])
}
# only 3 phenophases of interest for now, can add more later.
budburstagg <- aggregate(doy ~ plantNickname + year, budburst, FUN = min)
colnames(budburstagg) <- c("plantNickname", "year", "budburst")
leafoutagg <-  aggregate(doy ~ plantNickname + year, leafout, FUN = min)
colnames(leafoutagg) <- c("plantNickname", "year", "leafout")
leafcolagg <-  aggregate(doy ~ plantNickname + year, coloredLeaves, FUN = max)
colnames(leafcolagg) <- c("plantNickname", "year", "coloredLeaves")

# merge all together while saving the max number of observations
doy_pheno <- merge(budburstagg, leafoutagg, by = c("plantNickname", "year"), all = TRUE)
doy_pheno <- merge(doy_pheno, leafcolagg, by = c("plantNickname", "year"), all = TRUE)

# add spp name back
doy_pheno$latbi <- bb.pheno$latbi[match(doy_pheno$plantNickname, bb.pheno$plantNickname)]
doy_pheno$genus <- bb.pheno$genus[match(doy_pheno$plantNickname, bb.pheno$plantNickname)]
doy_pheno$species <- bb.pheno$species[match(doy_pheno$plantNickname, bb.pheno$plantNickname)]
doy_pheno$commonName <- bb.pheno$commonName[match(doy_pheno$plantNickname, bb.pheno$plantNickname)]

# remove outliers e.g. when budburst and leafout are way to late in the summer/fall
doy_pheno$budburst[which(doy_pheno$budburst > 180)] <- NA
doy_pheno$leafout[which(doy_pheno$leafout > 180)] <- NA
doy_pheno$coloredLeaves[which(doy_pheno$coloredLeaves < 190)] <- NA

# Add tree coordinates
phenosCoord <- merge(doy_pheno, coordcut, by.x="plantNickname", by.y="PlantID", all.x=TRUE)

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
  "plantNickname",
  "year",
  "budburst",
  "leafout",
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
  "plantNickname",
  "year",
  "budburst",
  "leafout",
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

# uncleaned pheno observations
if (FALSE){
bb2 <- d2 %>%
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
bb2$latbi <- paste(bb2$genus, bb2$species, sep=" ")

#clean common name column
bb2$commonName <- bb2$Common_Name
bb2$commonName[which(bb2$Common_Name == "yellow birch")] <- "Yellow birch"
bb2$commonName[which(bb2$Common_Name == "river birch")] <- "River birch"
bb2$commonName[which(bb2$Common_Name == "sugar maple")] <- "Sugar maple"
bb2$commonName[which(bb2$Common_Name == "white oak")] <- "White oak"
bb2$commonName[which(bb2$Common_Name == "eastern cottonwood")] <- "Eastern cottonwood"
bb2$commonName[which(bb2$Common_Name == "yellow buckeye")] <- "Yellow buckeye"
bb2$commonName[which(bb2$Common_Name == "American beech")] <- "American beech"
bb2$commonName[which(bb2$Common_Name == "pignut hickory")] <- "Pignut hickory"
bb2$commonName[which(bb2$Common_Name == "shagbark hickory")] <- "Shagbark hickory"
bb2$commonName[which(bb2$Common_Name == "northern red oak")] <- "Northern red oak"
bb2$commonName[which(bb2$Common_Name == "red maple")] <- "Red maple"

bb2.pheno <- bb2[, c("genus", "species", "latbi", "commonName", "symbol", "phase", "year", "doy", "numYs", "plantNickname")]

# clean phenophase names
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Breaking leaf buds", "budburst", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Leaves", "leafout", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Flowers or flower buds", "flowers", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Falling leaves", "leafDrop", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Colored leaves", "coloredLeaves", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Fruits", "fruits", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Ripe fruits", "ripeFruits", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Recent fruit or seed drop", "recentFruitOrSeedDrop", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Increasing leaf size", "increasingLeafSize", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Open flowers", "openFlowers", bb2.pheno$phase)
bb2.pheno$phase<-ifelse(bb2.pheno$phase=="Pollen release (flowers)", "pollenRelease", bb2.pheno$phase)

bb2.pheno <- bb2.pheno[(bb2.pheno$numYs>=1),]

write.csv(bb2.pheno,"output/uncleanedTimeseriesPheno.csv")
}