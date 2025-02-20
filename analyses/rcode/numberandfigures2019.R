### Time to visualize the numbers
# 29 July 2019 - Cat

#change by crd to see if it works in November 2024

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)


# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

library(viridis)

# Set Working Directory
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/input/")
list.files()

b<-read.csv("individual_phenometrics_data.csv", header=TRUE)
head(b)
# read approved list of trees to core and remove non necessary columns
dcore <- read.csv("2025ApprovedPlantListforcoring.csv")[1:5]
head(dcore)
# read csv that contains the two tree names and nicknames
namemerge <- read.csv2("treeswithPhenodata.csv", header=TRUE, sep=",")

# rename columns so they are more consistent
colnames(dcore) <- c("availableToCore2025", "previouslyCored", "yearCored", "Plant_Nickname", "species")
dcore <- subset(dcore, availableToCore2025 == "Y")
nrow(dcore)
# Split individual Id and QUAL
dcore$plantNickname <- sub("\\*.*", "", dcore$Plant_Nickname)
namemerge$plantNickname <- sub(",.*", "", namemerge$Plant_Nickname)
# replace manually the American beech
namemerge$plantNickname[which(namemerge$Plant_Nickname =="14585*J American beech")] <- "14585*J"
namemerge$plantNickname[which(namemerge$Plant_Nickname =="22798*A American beech")] <- "22798*A"
# add nickname column since it's the one the arboretum uses
b$plantNickname <- namemerge$plantNickname[match(b$Individual_ID, namemerge$Individual_ID)]

### check for how many years of red maple we have
notasked2core <- unique(b$Individual_ID)
asked2core <- unique(namemerge$Individual_ID)
m <- c(notasked2core, asked2core)
m[!duplicated(m)]
unique_values <- m[!duplicated(m) & !duplicated(m, fromLast = TRUE)]
subset_df <- b[b$Individual_ID %in% unique_values, ]
redmaple <- subset(b, Common_Name == "red maple")
nrow(redmaple)
unique(redmaple$First_Yes_Year)


# b$plantNickname <- sub("\\*.*", "", b$Plant_Nickname)
# create vector for all the individuals we can core
id2core<-as.character(dcore$Plant_Nickname)
###### Breakdown of observations per species
b$spp <- paste(b$Genus, b$Species)
numobsperspp <- b %>% count(spp)
head(numobsperspp)
#colz <- colorRampPalette(brewer.pal(15, "Dark2"))(15)
colz <- viridis_pal(option="D")(15)
colourCount = length(unique(numobsperspp$spp))
getPalette = colorRampPalette(brewer.pal(8, "Dark2"))

sppnum <- ggplot(numobsperspp, aes(x=spp, y=n, fill=spp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=getPalette(colourCount), 
                    labels=numobsperspp$spp) + 
  labs(title="Total number of observed phenophases \n(e.g., leaves or flowers) for each species (2015-2019).") +
  scale_x_discrete(labels=c("Acer rubrum" = "Red Maple",
                            "Acer saccharum" = "Sugar Maple",
                            "Aesculus flava" = "Yellow Buckeye",
                            "Betula alleghaniensis" = "Yellow Birch",
                            "Betula nigra" = "River Birch",
                            "Carya glabra" = "Pignut Hickory",
                            "Carya ovata" = "Shagbarck Hickory",
                            "Fagus grandifolia" = "American Beech",
                            "Hamamelis virginiana" = "Witch hazel",
                            "Populus deltoides" = "Eastern Cottonwood",
                            "Quercus alba" = "White Oak",
                            "Quercus rubra" = "Red Oak",
                            "Tilia americana" = "American Linden",
                            "Vaccinium corymbosum" = "Highbush Blueberry",
                            "Viburnum nudum" = "Possumhaw")) +
  theme_classic() + 
  theme(legend.position="none", 
                          plot.margin=unit(c(1,3,1,1), "lines"),
                          plot.title = element_text(face="bold")) +
  ylab("Number of observed phenophases") + xlab("") +
  geom_text(aes(label=n), hjust=0.4, vjust=-1, col=getPalette(colourCount))

sppnum



# nb of observations per individuals for treespotters data
obsperind <- b[!duplicated(b$Individual_ID),]
str(obsperind)
nbobsperID <- b %>% count(Common_Name, Individual_ID, Last_Yes_Year)
nrow(nbobsperID)
head(nbobsperID)
nbobsperID <- nbobsperID[!is.na(nbobsperID$Individual_ID),]
# set color palet
colourCount <- length(unique(nbobsperID$Common_Name))
getPalette <- colorRampPalette(brewer.pal(9, "Set1")) 
nbobsperID$Individual_ID <- as.character(nbobsperID$Individual_ID)
# PLOT ok for now i guess
ggplot(nbobsperID, aes(x = n, y = factor(Individual_ID), fill = factor(Last_Yes_Year))) +
  geom_bar(stat = "identity", position = "stack") +  # Stack years within each bar
  labs(x = "Count", y = "Individual ID", fill = "Year") +
  theme_classic() + 
  theme(legend.position = "right", 
        plot.margin = unit(c(1, 3, 1, 1), "lines"),
        plot.title = element_text(face = "bold")) +
  scale_fill_manual(values = getPalette(length(unique(nbobsperID$Last_Yes_Year))))  # Adjust palette for years


vec <- c("sugar maple", "red maple")
redandsugar <- nbobsperID[nbobsperID$Common_Name %in% vec, ]
test <- redandsugar[!is.na(redandsugar$Last_Yes_Year),]

# Define the color palette
getPalette2 <- colorRampPalette(brewer.pal(3, "Dark2"))

# Ensure Last_Yes_Year is an ordered factor with 2015 at the bottom and 2020 at the top
test$Last_Yes_Year <- factor(test$Last_Yes_Year, levels = 2020:2015)  # Reverse order for stacking

# Plot with years ordered correctly in the stacked bars
# Ensure Last_Yes_Year is an ordered factor with 2015 at the bottom and 2020 at the top
redandsugar$Last_Yes_Year <- factor(redandsugar$Last_Yes_Year, levels = 2020:2015)  # Reverse order for stacking

# Plot for sugar and red maples
ggplot(redandsugar, aes(x = factor(Individual_ID), y = n, fill = Last_Yes_Year)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Common_Name, ncol = 3, scales = "free_x") +  # Use free_x to allow different x-axis scales
  labs(x = "Individual ID", y = "Count", fill = "Years") +
  theme_classic() +
  theme(legend.position = "right", 
        plot.margin = unit(c(1, 3, 1, 1), "lines"),
        plot.title = element_text(face = "bold")) +
  scale_fill_manual(values = getPalette2(length(unique(redandsugar$Last_Yes_Year))))

# nb of observations per individuals for treespotters data AND allowed trees to core
### Subset for trees we can core

filtered_dcore <- b[b$plantNickname %in% id2core, ]
nbobsperID <- filtered_dcore %>% count(Common_Name, plantNickname, Last_Yes_Year)
nbobsperID <- nbobsperID[!is.na(nbobsperID$plantNickname),]

ggplot(nbobsperID, aes(x = n, y = reorder(plantNickname, n), fill = Common_Name)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Individual ID") +
  theme_classic() + 
  theme(legend.position = "right", 
        plot.margin = unit(c(1, 3, 1, 1), "lines"),
        plot.title = element_text(face = "bold")) +
  scale_fill_manual(values = getPalette(colourCount))

### Now let's make a plot with time-series data
# Observations per year
numobsperyr <- b %>% count(Last_Yes_Year)
numobsperyr
cumulative <- numobsperyr %>%
  mutate(cumulative_sum = cumsum(n))
cols<-colorRampPalette(brewer.pal(8, "Dark2"))(5)
colpal <- colorRampPalette(c("blue3", "red3"))
colvir <- viridis_pal(option="C")(6)
ggplot(cumulative, aes(x=Last_Yes_Year, y=cumulative_sum)) + 
  geom_line() + 
  geom_point(aes(col=as.factor(Last_Yes_Year))) + 
  theme_classic() + 
  scale_color_manual(values=colpal(6), labels=cumulative$Last_Yes_Year) + 
  theme(legend.position = "none",
        plot.margin = unit(c(1,4.5,1,1), "lines"),
        plot.title=element_text(face="bold"),
        axis.text.x=element_text(color=colpal(6))) 

### Pie Chart
routes <- data.frame(route=c("Linden and North Woods", "Maple", "Shrub", "Birch", "Oak",
                             "Hickory", "Beech", "Peters Hill"), 
                     n=c(23027, 22861, 11135, 57517, 22293, 36661, 12838, 53795))
piechart <- ggplot(routes, aes(x="", y=n, fill=route))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + theme_classic() +
  scale_fill_brewer(palette="Dark2", name="Route") + ggtitle("Percentage of total observations by route") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title=element_text(face="bold")) +
  geom_text(aes(label = paste0(round((n/239710)*100), "%")), position = position_stack(vjust = 0.5))
  
quartz()
piechart

