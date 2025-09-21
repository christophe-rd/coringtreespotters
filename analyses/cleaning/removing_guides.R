## REMOVE guides 

setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/data/coreImagesROI")

# rename in place: remove "_guides" from every file name
file.rename(
  from = list.files(pattern = "_guides"),
  to   = gsub("_guides", "", list.files(pattern = "_guides"))
)
