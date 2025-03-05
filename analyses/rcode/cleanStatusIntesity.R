### Clean main tree spotters data
# The goal of this code is to clean the data and select only the observations for the trees we will be coring in April 2025 (approved by the curator) 
# 25 Feb 2025 by CRD
  
# libraries
  
# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# set wd
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")

# read the whole treespotters data frame
d <- read.csv2("input/status_intensity_observation_data.csv", header = TRUE, sep=",")
unique(d$Plant_Nickname)
# read downloaded data from the arboretum's website
coord <- read.csv("input/listTreesfromInteractiveMap.csv", header=TRUE)
# select only coordinates column
coordcut <- coord[, c("Plant.ID", 
                      "Garden.Latitude", 
                      "Garden.Longitude", 
                      "Accession.Date", 
                      # "Plant.Source", 
                      "Habitat", 
                      "DBH")]

# add binomal latin names
d$GenusSpecies <- paste(d$Genus, sep="_", d$Species)
#clean common name column
d$Common_Name[which(d$Common_Name == "yellow birch")] <- "Yellow Birch"
d$Common_Name[which(d$Common_Name == "river birch")] <- "River Birch"
d$Common_Name[which(d$Common_Name == "sugar maple")] <- "Sugar Maple"
d$Common_Name[which(d$Common_Name == "American basswood")] <- "American Basswood"
d$Common_Name[which(d$Common_Name == "white oak")] <- "White Oak"
d$Common_Name[which(d$Common_Name == "eastern cottonwood")] <- "Eastern Cottonwood"
d$Common_Name[which(d$Common_Name == "yellow buckeye")] <- "Yellow Buckeye"
d$Common_Name[which(d$Common_Name == "American beech")] <- "American Beech"
d$Common_Name[which(d$Common_Name == "pignut hickory")] <- "Pignut Hickory"
d$Common_Name[which(d$Common_Name == "shagbark hickory")] <- "Shagbark Hickory"
d$Common_Name[which(d$Common_Name == "northern red oak")] <- "Northern Red Oak"
d$Common_Name[which(d$Common_Name == "possumhaw")] <- "Possumhaw"
d$Common_Name[which(d$Common_Name == "red maple")] <- "Red Maple"
d$Common_Name[which(d$Common_Name == "American witchhazel")] <- "American Witchhazel"
d$Common_Name[which(d$Common_Name == "highbush blueberry")] <- "Highbush Blueberry"

# read approved list of trees to core and remove non necessary columns
dcore <- read.csv("input/2025ApprovedPlantListforcoring.csv")[1:5]

# Clean colnames
colnames(dcore) <- c("availableToCore2025", "previouslyCored", "yearCored", "Plant_Nickname", "species")
# subset for the trees we are allowed to core
dcoresub <- subset(dcore, availableToCore2025 == "Y")
length(unique(dcoresub$Plant_Nickname))
# create vector for all the individuals we can core
id2corevec<-as.character(dcoresub$Plant_Nickname)
# remove the tree 6, and whatever follows the nickname in d:
d$plantNickname <- sub(", Tree \\d+", "", d$Plant_Nickname)
# replace manually the American beech
d$plantNickname[which(d$Plant_Nickname =="14585*J American beech")] <- "14585*J"
d$plantNickname[which(d$Plant_Nickname =="22798*A American beech")] <- "22798*A"
# then, in the main df, select only the trees we have permission to core
id2core <- subset(d, plantNickname %in% id2corevec) # missing 20517*A and 20517*B for which we can core, but i dont understand why they are not in b
# add year column and change format Observation Time
id2core$observationDate <- format(as.Date(id2core$Observation_Date, format = "%Y-%m-%d"), "%d-%m-%Y")
id2core$year <- format(as.Date(id2core$observationDate, format = "%d-%m-%Y"), "%Y")
# select the columns I want to keep
id2core2 <- id2core[, c("Observation_ID", 
                        "plantNickname",  
                        "ObservedBy_Person_ID", 
                        "Site_ID", 
                        "Site_Name", 
                        "Species_ID", 
                        # "Genus", 
                        # "Species", 
                        "Common_Name", 
                        "GenusSpecies", 
                        "USDA_PLANTS_Symbol", 
                        "ITIS_Number", 
                        "Individual_ID", 
                        "Protocol_ID", 
                        "Phenophase_ID", 
                        "Phenophase_Category", 
                        "Phenophase_Description", 
                        "Phenophase_Name", 
                        # "Phenophase_Definition_ID", 
                        "Species.Specific_Info_ID", 
                        "observationDate", 
                        "year",
                        # "Observation_Time", 
                        "Day_of_Year", 
                        "Phenophase_Status", 
                        "Intensity_Category_ID", 
                        "Site_Visit_ID" 
                        # "Observation_Comments" 
                        )]
str(id2core2)
# merge coord and id2core2
namemerge <- merge(id2core2, coordcut, by.x="plantNickname", by.y="Plant.ID", all.x=TRUE)
# write the cleaned data
write.csv(namemerge, "output/cleanedStatusIntensity.csv", row.names = FALSE) # now the file is is 62mb heavy which is better than 140mb. 

