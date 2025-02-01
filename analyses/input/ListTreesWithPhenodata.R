# Checking tree ID to core trees at Arboretum
# CRD on 4 Nov 2024

setwd("/Users/christophe_rouleau-desrochers/Documents/github/treespotters/analysis/output")

allcues<- read.csv2("cleaned_tree_allcues.csv", header = TRUE, sep = ",")
head(allcues)

# how many unique IDs
uniqueid <- unique(allcues$ID)

# get full species name
allcues$Genus_Species <- paste(allcues$Genus, allcues$Species, sep="_")
head(allcues)

# Table for ID and species
suby <- allcues[, c("ID", "Genus_Species")]
suby2 <- suby[!duplicated(suby$ID), ]

#write CSV

write.csv(suby2, file = "treeswithPhenodata.csv")
