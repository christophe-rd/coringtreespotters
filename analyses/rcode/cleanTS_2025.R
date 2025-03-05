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
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(lubridate)


# Set Working Directory
# Lizzie's directory
# setwd("~/Documents/git/projects/treegarden/treespotters")
# Christophe's directory
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")
d <- read.csv("input/individual_phenometrics_data2025.csv", header=TRUE)

### First let's do the obligatory cleaning checks with citizen scienece data
d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),] ## This selects data where multiple people observed the same phenophase
d <- d[(d$NumYs_in_Series>=3),] ## This selects data again where the same phenophase was seen 3 times in a row
d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] ## And this limits to data where a no is followed by a yes, so that it is a new observation/new phenophase but has been detected within a reasonable timeframe

# subset only for tree species
trees <- c("highbush blueberry",  "American witchhazel", "possumhaw")
dd <- subset(d, !(Common_Name %in% trees))

# change from NPN output to more digestible column names
bb<-dd%>%
  rename(lat=Latitude)%>%
  rename(long=Longitude)%>%
  rename(elev=Elevation_in_Meters)%>%
  rename(year=First_Yes_Year)%>%
  rename(month=First_Yes_Month)%>%
  rename(day=First_Yes_Day)%>%
  rename(doy=First_Yes_DOY)%>%
  rename(numYs=Multiple_Observers)%>%
  rename(phase=Phenophase_Description)%>%
  rename(id=Individual_ID)%>%
  rename(genus=Genus)%>%
  rename(species=Species)
bb.pheno<-dplyr::select(bb, genus, species, Common_Name, phase, lat, long, elev, year, doy, numYs, id)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)


### Now work on finding day of budburst, etc.
bb.pheno<-filter(bb.pheno, numYs>0)
# Below, I group each individual by phenophase and year to find the first observation (using the slice function), 
## so first day of budburst for that individual for that year
doy_pheno<-bb.pheno%>% 
  group_by(id, phase, year) %>% 
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
colstokeep<-c("genus", "species", "Common_Name", "id","year", "phase","lat", "long", "elev", "doy")
phenos<-subset(doy_pheno, select=colstokeep)

phenos<-phenos[!duplicated(phenos),]

phenos<-phenos%>%tidyr::spread(phase, doy)

phenos$fruits <- phenos$Fruits
phenos$col.leaves<-phenos$`Colored leaves`
phenos$leafdrop<-phenos$`leaf drop`

phenos <- subset(phenos, select=c("genus", "species", "Common_Name", "id", "year", "lat", "long", "elev", "budburst", 
                                  "flowers", "fruits", "leafout", "col.leaves", "leafdrop"))

### And now last observation for when to start calculating chilling
phenos$last.obs<-ave(phenos$leafdrop, phenos$id, phenos$year, FUN=last)
phenos$last.obs<-ifelse(is.na(phenos$last.obs), ave(phenos$col.leaves, phenos$id, phenos$year, FUN=last), phenos$last.obs)

## For gdd start and chilling end
phenos$gdd.start<-46 # 15 February for each year - arbitrary, can change

write.csv(phenos, file="output/clean_treespotters_allphenodataApr2024.csv", row.names=FALSE)

if(FALSE){ # Not running
  
  ### Now clean it up a bit
  phenos<-phenos[!is.na(phenos$budburst),]
  phenos<-phenos[!is.na(phenos$leafout),]
  phenos<-phenos[!is.na(phenos$last.obs),]
  
  phenos$yr.end <- ifelse(phenos$year==2016, 366, 365)
  
  ## And now add in individual information...
  phenos$type <- "Treespotters"
  
  prov <- read.csv("output/provenanceinfo.csv", header=TRUE)
  prov <- subset(prov, select= c("Individual_ID", "provenance.lat", "provenance.long"))
  prov <- prov %>% rename(id=Individual_ID)
  
  ts <- full_join(prov, phenos)
  
  colstokeep <- c("id", "provenance.lat", "provenance.long", "genus", "species", "year", "budburst", "leafout", "yr.end", "gdd.start", "type")
  d <- subset(ts, select=colstokeep)
  d <- d[!is.na(d$genus),]
  
  d$id <- as.character(d$id)
}


#### Plotting! 
quartz()
ggplot(phenos, aes(x=year, y=budburst, color=id, group=id)) +
  geom_point()+
  geom_line()+
  facet_wrap( .~genus)

ggplot(phenos, aes(x=year, y=leafout, group=id)) +
  geom_point()+
  geom_line()+
  facet_wrap( .~genus)

ggplot(phenos, aes(x=year, y=leafdrop, group=id)) +
  geom_point()+
  geom_line()+
  facet_wrap( .~genus)

# cleaning ...
c <- subset(phenos, budburst<160)
phenoscleaner <- subset(c, leafout<250)
phenoscleaner$latbi <- paste(phenoscleaner$genus, phenoscleaner$species)
phenossmwide <- subset(phenoscleaner, select=c("genus", "species", "latbi", "id", "year", "budburst", "leafout", "leafdrop"))


bbplot <- ggplot(phenoscleaner, aes(x=year, y=budburst, color=as.factor(id), group=as.factor(id))) +
  geom_point() +
  geom_line() +
  facet_wrap(.~latbi) + 
  theme_bw() + 
  theme(legend.position = "none")

loplot <- ggplot(phenoscleaner, aes(x=year, y=leafout, color=as.factor(id), group=as.factor(id))) +
  geom_point() +
  geom_line() +
  facet_wrap(.~latbi) + 
  theme_bw() + 
  theme(legend.position = "none")


ldplot <- ggplot(phenoscleaner, aes(x=year, y=leafdrop, color=as.factor(id), group=as.factor(id))) +
  geom_point() +
  geom_line() +
  facet_wrap(.~latbi) + 
  theme_bw() + 
  theme(legend.position = "none")

ggsave("figures/update2024_budburst_byspp.pdf",bbplot, width=9, height=6)
ggsave("figures/update2024_leafout_byspp.pdf",loplot, width=9, height=6)
ggsave("figures/update2024_leafdrop_byspp.pdf",ldplot, width=9, height=6)


library(reshape2)
phenossm <- melt(phenossmwide, id = c("genus", "species", "latbi", "id", "year"), 
                 measure = c("budburst", "leafout", "leafdrop"))


ggplot(subset(phenossm, variable!="leafdrop"), aes(x=year, y=value, color=variable, group=id)) +
  geom_point()+
  # geom_line()+
  facet_wrap( .~latbi)
ggplot(phenossm, aes(x=year, y=value, color=variable, group=id)) +
  geom_point()+
  # geom_line()+
  facet_wrap( .~latbi)

# go for 3 phenophases and facet wrap by species using a nice color pannel
ggplot(phenoscleaner) +
  geom_point(aes(x = year, y = budburst, color = "Budburst")) + 
  geom_point(aes(x = year, y = leafout, color = "Leafout")) + 
  geom_point(aes(x = year, y = col.leaves, color = "Leaf Colouring")) + 
  geom_point(aes(x = year, y = leafdrop, color = "Leafdrop")) + 
  scale_color_manual(
    values = c(
      "Budburst" = "#66c2a5",
      "Leafout" = "#1b7837", 
      "Leaf Colouring" = "#e69f00", # Orange
      "Leafdrop" = "#d73027"   # Reddish-Brown
    ),
    limits = c("Budburst", "Leafout", "Leaf Colouring", "Leafdrop") # Defines legend order
  ) +
  labs(color = "Phenophase", x = "Year", y = "Day of Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels
  facet_wrap(~Common_Name)

# subset for acer rubrum as it's currently absent of the list of trees to core
acru <- subset(phenoscleaner, species == "rubrum")
# Create the plot
ggplot(acru) +
  geom_point(aes(x = year, y = budburst, color = "Budburst")) + 
  geom_point(aes(x = year, y = leafout, color = "Leafout")) + 
  geom_point(aes(x = year, y = col.leaves, color = "Leaf Colouring")) + 
  geom_point(aes(x = year, y = leafdrop, color = "Leafdrop")) + 
  scale_color_manual(
    values = c(
      "Budburst" = "#66c2a5",      # Light Green
      "Leafout" = "#1b7837",       # Dark Green
      "Leaf Colouring" = "#e69f00", # Orange
      "Leafdrop" = "#d73027"       # Reddish-Brown
    )
  ) +
  labs(color = "Phenophase", x = "Year", y = "Day of Year") +
  theme_minimal() +
  facet_wrap(~id)


View(acru)
cut <- acru[, c("id", "year","budburst", "leafout", "leafdrop", "col.leaves" )]
df_summary <- cut %>%
  group_by(id) %>%
  summarise(
    budburst = sum(!is.na(budburst)), 
    leafout = sum(!is.na(leafout)), 
    leafdrop = sum(!is.na(leafdrop)), 
    col.leaves = sum(!is.na(col.leaves))
  )

m <- read.csv("input/status_intensity_observation_data.csv")
mcut <- m[!duplicated(m$Individual_ID), c("Individual_ID", "Plant_Nickname")]
df_summary <- merge(df_summary, mcut, by.x = "id", by.y = "Individual_ID", all.x = TRUE)
df_summary <- df_summary[, c("Plant_Nickname", "budburst", "leafout", "leafdrop", "col.leaves")]
write.csv(df_summary, "output/nyearsAceRub.csv")
library(dplyr)
df_long <- cut %>%
  pivot_longer(cols = -c(id, year), names_to = "phenophase", values_to = "value") %>%
  filter(!is.na(value)) %>%
  distinct(year, phenophase) %>%
  arrange(phenophase, year)
df_summary <- merge(df_long, mcut, by.x = "id", by.y = "Individual_ID", all.x = TRUE)
df_summary <- df_summary[, c("Plant_Nickname", "year", "phenophase")]
write.csv(df_summary, "output/phenobyyearAceRub.csv")
