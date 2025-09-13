## REMOVE guides 

setwd("/Volumes/PortableSSD/CoringTreespotters/FromScan/cookies_with_guides")

# rename in place: remove "_guides" from every file name
file.rename(
  from = list.files(pattern = "_guides"),
  to   = gsub("_guides", "", list.files(pattern = "_guides"))
)
