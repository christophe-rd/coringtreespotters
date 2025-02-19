# Checking tree ID to core trees at Arboretum
# CRD on 4 Nov 2024

setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/input/")
list.files()
allcues<- read.csv2("cleaned_tree_allcues.csv", header = TRUE, sep = ",")
head(allcues)

# how many unique IDs



# get full species name
allcues$Genus_Species <- paste(allcues$Genus, allcues$Species, sep="_")
subby <- allcues[!duplicated(allcues$ID),]
unique(subby$Genus_Species)
# Table for ID and species
suby <- allcues[, c("ID", "Genus_Species")]
suby2 <- suby[!duplicated(suby$ID), ]

#write CSV

write.csv(suby2, file = "treeswithPhenodata.csv")
