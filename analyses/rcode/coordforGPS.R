### Create a small csv to import coordinates into the gps
# 8 Aril 2025 by CRD

# mapping trees to core!

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# set wd
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")
# read the list of trees I downloaded from the arboretum's website
d <- read.csv("output/cleanTS.csv", header=TRUE)
# count number of observations we have for each tree
count <- d %>% count(plantNickname)
# only one row per tree
ddup <- d[!duplicated(d$plantNickname),]
# merge ddup and count 
dmer <- merge(ddup, count, by="plantNickname")
# add categories of number of observations based on n column
# Add a new column with Low, Medium, High based on value
dmer$level <- ifelse(dmer$n <= 3, "low",
                   ifelse(dmer$n <= 7, "med", "high"))
# paste species and nickname
dmer$nickname_sp <- paste(dmer$plantNickname, "| ", dmer$genus, dmer$species)
# paste descritives that we want to have
dmer$locDBHPheno <- paste(
  "GarLoc:", dmer$gardenLocation,
  "| DBH:", dmer$DBH, "cm",
  "| PhenoLev:", dmer$level
)

# Select only these 4 columns
dtowrite <- dmer[,c("nickname_sp","lat","long", "locDBHPheno")]
colnames(dtowrite) <- c("name","lat","long", "description")
# Write to csv
write.csv(dtowrite, "output/treesToCore.csv", row.names=FALSE)
