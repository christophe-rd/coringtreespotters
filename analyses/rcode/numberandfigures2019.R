### Time to visualize the numbers
# 29 July 2019 - Cat

#change by crd to see if it works
# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(egg)
library(viridis)

# Set Working Directory
# Set Working Directory
if(length(grep("christophe", getwd()) > 0)) {
} else if(length(grep("christophe", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")
} 
b<-read.csv("input/individual_phenometrics_data.csv", header=TRUE)


###### Breakdown of observations per species
b$spp <- paste(b$Genus, b$Species)

numobsperspp <- b %>% count(spp)
#colz <- colorRampPalette(brewer.pal(15, "Dark2"))(15)
colz <- viridis_pal(option="D")(15)
colourCount = length(unique(numobsperspp$spp))
getPalette = colorRampPalette(brewer.pal(8, "Spectral"))

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
  theme_classic() + theme(legend.position="none", 
                          plot.margin=unit(c(1,3,1,1), "lines"),
                          plot.title = element_text(face="bold")) +
  ylab("Number of observed phenophases") + xlab("") +
  coord_flip(expand=c(0,0), clip="off") + 
  geom_text(aes(label=n), hjust=-0.2, col=getPalette(colourCount))

quartz()
sppnum

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

# nb of observations per individuals
nbobsperID <- b %>% count(Common_Name, Individual_ID)
nbobsperID
str(b)
ggplot(nbobsperID, aes(x = n, y = factor(Individual_ID), fill = Common_Name)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Individual ID") +
  theme_minimal()
dim(nbobsperID)
dput(nbobsperID)
str(b)

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

