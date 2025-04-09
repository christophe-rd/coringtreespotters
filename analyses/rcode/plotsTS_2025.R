## Started 8 April 2025 ##
# Christophe and Lizzie

# goal of this script is to transfer the figure code of Tree spotters. It was originally in cleanTS_2025.R, but to make it more readable, I am transferring it here. 

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
d <- read.csv("output/clean_treespotters_allphenodataApr2025.csv", header=TRUE)

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

ggplot(phenos, aes(x=year, y=leafDrop, group=id)) +
  geom_point()+
  geom_line()+
  facet_wrap( .~genus)

# cleaning ...
c <- subset(phenos, budburst<160)
phenoscleaner <- subset(c, leafout<250)
phenoscleaner$latbi <- paste(phenoscleaner$genus, phenoscleaner$species)
phenossmwide <- subset(phenoscleaner, select=c("genus", "species", "latbi", "id", "year", "budburst", "leafout", "leafDrop"))


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

ldplot <- ggplot(phenoscleaner, aes(x=year, y=leafDrop, color=as.factor(id), group=as.factor(id))) +
  geom_point() +
  geom_line() +
  facet_wrap(.~latbi) + 
  theme_bw() + 
  theme(legend.position = "none")

ggsave("figures/update2024_budburst_byspp.pdf",bbplot, width=9, height=6)
ggsave("figures/update2024_leafout_byspp.pdf",loplot, width=9, height=6)
ggsave("figures/update2024_leafdrop_byspp.pdf",ldplot, width=9, height=6)

phenossm <- melt(phenossmwide, id = c("genus", "species", "latbi", "id", "year"), 
                 measure = c("budburst", "leafout", "leafDrop"))


ggplot(subset(phenossm, variable!="leafDrop"), aes(x=year, y=value, color=variable, group=id)) +
  geom_point()+
  # geom_line()+
  facet_wrap( .~latbi)
ggplot(phenossm, aes(x=year, y=value, color=variable, group=id)) +
  geom_point()+
  # geom_line()+
  facet_wrap( .~latbi)

# go for 3 phenophases and facet wrap by species using a nice color pannel
unique(phenoscleaner$species)

phenoscleaner <- subset(phenoscleaner, species != "grandifolia")
phenospp <- ggplot(phenoscleaner) +
  geom_point(aes(x = year, y = budburst, color = "Budburst")) + 
  geom_point(aes(x = year, y = leafout, color = "Leafout")) + 
  geom_point(aes(x = year, y = coloredLeaves, color = "Leaf Colouring")) + 
  geom_point(aes(x = year, y = leafDrop, color = "leafDrop")) + 
  scale_color_manual(
    values = c(
      "Budburst" = "#66c2a5",
      "Leafout" = "#1b7837", 
      "Leaf Colouring" = "#e69f00", # Orange
      "leafDrop" = "#d73027"   # Reddish-Brown
    ),
    limits = c("Budburst", "Leafout", "Leaf Colouring", "leafDrop") # Defines legend order
  ) +
  labs(color = "Phenophase", x = "Year", y = "Day of Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels
  facet_wrap(~Common_Name)
ggsave("figures/phenospp.pdf", phenospp)

# subset for acer rubrum as it's currently absent of the list of trees to core
acru <- subset(phenoscleaner, species == "rubrum")
# Create the plot
ggplot(acru) +
  geom_point(aes(x = year, y = budburst, color = "Budburst")) + 
  geom_point(aes(x = year, y = leafout, color = "Leafout")) + 
  geom_point(aes(x = year, y = coloredLeaves, color = "Leaf Colouring")) + 
  geom_point(aes(x = year, y = leafdrop, color = "Leafdrop")) + 
  scale_color_manual(
    values = c(
      "Budburst" = "#66c2a5",      # Light Green
      "Leafout" = "#1b7837",       # Dark Green
      "Leaf Colouring" = "#e69f00", # Orange
      "leafDrop" = "#d73027"       # Reddish-Brown
    )
  ) +
  labs(color = "Phenophase", x = "Year", y = "Day of Year") +
  theme_minimal() +
  facet_wrap(~id)


View(acru)
cut <- acru[, c("id", "year","budburst", "leafout", "leafDrop", "coloredLeaves" )]
df_summary <- cut %>%
  group_by(id) %>%
  summarise(
    budburst = sum(!is.na(budburst)), 
    leafout = sum(!is.na(leafout)), 
    leafDrop = sum(!is.na(leafDrop)), 
    coloredLeaves = sum(!is.na(coloredLeaves))
  )

m <- read.csv("input/status_intensity_observation_data.csv")
mcut <- m[!duplicated(m$Individual_ID), c("Individual_ID", "Plant_Nickname")]
df_summary <- merge(df_summary, mcut, by.x = "id", by.y = "Individual_ID", all.x = TRUE)
df_summary <- df_summary[, c("Plant_Nickname", "budburst", "leafout", "leafDrop", "coloredLeaves")]
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

