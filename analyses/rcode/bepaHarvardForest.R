# B. papyrifera data in Harvard Forest
# CRD 21 April 2026
# look at the paper birch in harvard forest, thanks Neil!

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 
options(digits = 3)

# Load libraries
library(dplR)
library(tidyverse)

setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")

rwl1 <- read.rwl("input/SlabCity7_BEPArw.txt", format = "tucson")
rwl2 <- read.rwl("input/SlabCity8_BEPArw.txt", format = "tucson")
rwl <- combine.rwl(rwl1, rwl2)

spag.plot(rwl, zfac = 1)
rwl.detrended <- detrend(rwl, method = "Spline")
spag.plot(rwl.detrended, zfac = 1)

# averages 1 observation per year
crn <- chron(rwl.detrended)
yrs <- as.numeric(rownames(crn))
rwi <- crn[, 1]

plot(x=yrs, y = rwi, pch = 19, cex = 0.5,
     xlab = "Year", ylab = "RWI")
lines(x = yrs, y = rwi)
abline(lm(rwi ~ yrs), col = "red", lwd = 2)


# a whole lot more dots!
yrs <- as.numeric(rownames(rwl.detrended))
summary(rwl.detrended)
yrs <- yrs[yrs>1959]
rwl.sub <- rwl.detrended[rownames(rwl.detrended) > 1959, ]
plot(NULL, xlim = range(yrs), ylim = range(rwl.sub, na.rm = TRUE),
     xlab = "Year", ylab = "RWI")
for (i in 1:ncol(rwl.sub)) {
  points(yrs, rwl.sub[, i], pch = 19, cex = 0.3, col = rgb(0,0,0,0.2))
}
abline(lm(rowMeans(rwl.sub, na.rm = TRUE) ~ yrs), col = "red", lwd = 2)
