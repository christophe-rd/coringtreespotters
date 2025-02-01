### Trying to plot Tree Spotters data


rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(brms)

# Set Working Directory
setwd("~/Documents/git/treespotters/analysis")

## Data!
d<-read.csv("output/clean_tree_wclimate.csv", header=TRUE)