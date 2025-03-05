### Time to visualize the numbers
# 29 July 2019 - Cat
# November 2023 updated by CRD 

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
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")
list.files()

b<-read.csv("output/cleanedStatusIntensity.csv", header=TRUE)
# === === === === === === === === === === === === === === ===
# subset for red maples
red <- subset(fulldf, Species == "rubrum")
# nicknames for redmaples
vec <- unique(red$Plant_Nickname)
# subset back to all data for red maples
redmaple <- subset(fulldf, Plant_Nickname %in% vec)
# remove duplicates
redmaple3 <- redmaple[!duplicated(redmaple$Plant_Nickname),]
subby <- redmaple3[, c("Individual_ID", "Plant_Nickname", "GenusSpecies")]
# write small csv for redmaples
write.csv2(subby, "output/listRedMaples.csv")
# === === === === === === === === === === === === === === ===
# Breakdown of observations per species
numobsperspp <- b %>% count(Common_Name)
head(numobsperspp)
#colz <- colorRampPalette(brewer.pal(15, "Dark2"))(15)
colz <- viridis_pal(option="D")(15)
colourCount = length(unique(b$Common_Name))
getPalette <- colorRampPalette(brewer.pal(8, "Dark2"))


sppnum <- ggplot(numobsperspp, aes(x=Common_Name, y=n, fill=Common_Name)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=getPalette(colourCount), 
                    labels=numobsperspp$Common_Name) + 
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
nbobsperID <- b %>% count(Common_Name, Observation_ID, year)
# reorder years so 2015 is on the left
nbobsperID$year <- factor(nbobsperID$year, levels = 2020:2015) 
fall_palette <- c("#B22222", "#D2691E", "#FF8C00", "#FFA500", "#FFD700", "#8B4513")
# Plot by species and years
countperspp <- ggplot(nbobsperID, aes(x = n, y = factor(Common_Name), fill = factor(year))) +
  geom_bar(stat = "identity", position = "stack") +  # Stack years within each bar
  labs(x = "Number of phenological observations", y = "Species", fill = "Year") +
  theme_classic() + 
  theme(legend.position = "right", 
        plot.margin = unit(c(1, 3, 1, 1), "lines"),
        plot.title = element_text(face = "bold")) +
  scale_fill_manual(values=fall_palette)
countperspp
ggsave("figures/countPerSpecies.pdf", countperspp, bg = "white", width =  10, height = 10,)
# vec <- c("sugar maple", "red maple")
# redandsugar <- nbobsperID[nbobsperID$Common_Name %in% vec, ]
# vec <- c("red maple")
# redandsugar <- nbobsperID[b$plantNickname %in% vec, ]
# test <- redandsugar[!is.na(redandsugar$Last_Yes_Year),]
# # Define the color palette
# getPalette2 <- colorRampPalette(brewer.pal(3, "Dark2"))
# 
# # Ensure Last_Yes_Year is an ordered factor with 2015 at the bottom and 2020 at the top
# test$Last_Yes_Year <- factor(test$Last_Yes_Year, levels = 2020:2015)  # Reverse order for stacking
# Plot with years ordered correctly in the stacked bars
# Ensure Last_Yes_Year is an ordered factor with 2015 at the bottom and 2020 at the top
# redandsugar$Last_Yes_Year <- factor(redandsugar$Last_Yes_Year, levels = 2020:2015)  # Reverse order for stacking

# # Plot for sugar and red maples
# graph <- ggplot(redandsugar, aes(x = factor(Individual_ID), y = n, fill = Last_Yes_Year)) +
#   geom_bar(stat = "identity", position = "stack") +
#   facet_wrap(~ Common_Name, ncol = 3, scales = "free_x") +  # Use free_x to allow different x-axis scales
#   labs(x = "Individual ID", y = "Count", fill = "Years") +
#   theme_classic() +
#   theme(legend.position = "right", 
#         plot.margin = unit(c(1, 3, 1, 1), "lines"),
#         plot.title = element_text(face = "bold")) +
#   scale_fill_manual(values = getPalette2(length(unique(redandsugar$Last_Yes_Year))))
# list.files()
# ggsave("output/redsugar.pdf", graph)

# number of observations per tree
nbobsperID <- b %>% count(Common_Name, plantNickname, year)
nbobsperID <- nbobsperID[!is.na(nbobsperID$plantNickname),]
obspertree <- ggplot(nbobsperID, aes(x = n, y = reorder(plantNickname, n), fill = Common_Name)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Individual ID") +
  theme_classic() + 
  theme(legend.position = "right", 
        plot.margin = unit(c(1, 3, 1, 1), "lines"),
        plot.title = element_text(face = "bold")) +
  scale_fill_manual(values = getPalette(colourCount))
ggsave("figures/observationPerTree.pdf", obspertree)

### Now let's make a plot with time-series data
# Observations per year
numobsperyr <- b %>% count(year)

cumulative <- numobsperyr %>%
  mutate(cumulative_sum = cumsum(n))
cols<-colorRampPalette(brewer.pal(8, "Dark2"))(5)
colpal <- colorRampPalette(c("blue3", "red3"))
colvir <- viridis_pal(option="C")(6)
ggplot(cumulative, aes(x=year, y=cumulative_sum)) + 
  geom_line() + 
  geom_point(aes(col=as.factor(year))) + 
  theme_classic() + 
  scale_color_manual(values=colpal(6), labels=cumulative$year) + 
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

