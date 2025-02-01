### 19 April 2019 - by Cat ###
## Let's play around with some Tree Spotters data!
## Find Provenance Lat info and Age!
## Updated 9 May 2018 with new TS data & Climate data
### Weather data downloaded from... http://labs.arboretum.harvard.edu/weather/

# ## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(randomForest)
library(geosphere)
library(anytime)
library(weathermetrics)
library(measurements)
library(lubridate)
library(egg)


# Set Working Directory
setwd("~/Documents/git/treespotters/analysis")

## Get all that data!
## Start with phenology data and clean
bb<-read.csv("input/individual_phenometrics_data.csv", header=TRUE)
bb<-bb%>%
  rename(lat=Latitude)%>%
  rename(long=Longitude)%>%
  rename(elev=Elevation_in_Meters)%>%
  rename(year=First_Yes_Year)%>%
  rename(month=First_Yes_Month)%>%
  rename(day=First_Yes_Day)%>%
  rename(doy=First_Yes_DOY)%>%
  rename(numYs=Multiple_Observers)%>%
  rename(photo=Daylength) %>%
  rename(phase=Phenophase_Description)%>%
  rename(ID=Individual_ID)
bb.pheno<-dplyr::select(bb, Genus, Species, Common_Name, phase, lat, long, elev, year, doy, numYs, photo, ID)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)

### Now work on finding day of budburst, etc.
bb.pheno<-filter(bb.pheno, numYs>0)
stan.bb<-bb.pheno%>% 
  group_by(ID, phase, year) %>% 
  slice(which.min(doy))
stan.bb$m.doy<-ave(stan.bb$doy, stan.bb$phase, stan.bb$Genus, stan.bb$year)
stan.bb<-stan.bb[!duplicated(stan.bb),]
stan.bb$photo<-stan.bb$photo/3600
x<-paste(stan.bb$year, stan.bb$doy)
stan.bb$date<-as.Date(strptime(x, format="%Y %j"))
stan.bb$photo<-ifelse(stan.bb$year==2018, daylength(stan.bb$lat, stan.bb$date), stan.bb$photo)

### Clean observation error!
stan.bb$doy<-ifelse(stan.bb$Species=="alleghaniensis"&stan.bb$year==2016&stan.bb$doy==59, NA, stan.bb$doy)
stan.bb<-stan.bb[!is.na(stan.bb$doy),]

## Now bring in climate data...
cc<-read.csv("input/weldhill.csv", header=TRUE)
cc<-cc%>%
  rename(date.time=Eastern.daylight.time)
cc$date<-gsub("\\s* .*$", '', cc$date.time)
cc$date<- as.Date(cc$date, "%m/%d/%Y")
cc$year<-substr(cc$date, 0, 4)
cc$doy<-yday(cc$date)
cc$hour<-gsub("^.* \\s*|\\s*:.*$", '', cc$date.time)

cc<-dplyr::select(cc, Solar.Rad.W.m.2, Wind.Speed.mph, Wind.Dir, Temp..F, Dewpt..F, Temp.Soil..F,
                  Rain.in, Pressure.inHg, date, year, doy, hour)
cc$tmin<-ave(cc$Temp..F, cc$date, FUN=min)
cc$tmin<-fahrenheit.to.celsius(cc$tmin)
cc$tmax<-ave(cc$Temp..F, cc$date, FUN=max)
cc$tmax<-fahrenheit.to.celsius(cc$tmax)
cc$tmean<-ave(cc$Temp..F, cc$date)
cc$tmean<-fahrenheit.to.celsius(cc$tmean)
cc$tchill<-ave(cc$Temp..F, cc$date, cc$hour)
cc$tchill<-fahrenheit.to.celsius(cc$tchill)

cc$tmin.soil<-ave(cc$Temp.Soil..F, cc$date, FUN=min)
cc$tmin.soil<-fahrenheit.to.celsius(cc$tmin.soil)
cc$tmax.soil<-ave(cc$Temp.Soil..F, cc$date, FUN=max)
cc$tmax.soil<-fahrenheit.to.celsius(cc$tmax.soil)
cc$tmean.soil<-ave(cc$Temp.Soil..F, cc$date)
cc$tmean.soil<-fahrenheit.to.celsius(cc$tmean.soil)

cc$dewpt<-fahrenheit.to.celsius(ave(cc$Dewpt..F, cc$date))
cc$Rain.in<-ave(cc$Rain.in, cc$date, FUN=sum)
cc$precip<-conv_unit(cc$Rain.in, "inch", "mm")
cc$solr<-ave(cc$Solar.Rad.W.m.2, cc$date)
cc$windsp<-ave(cc$Wind.Speed.mph, cc$date)
sm.cc<-dplyr::select(cc, -Temp..F, -Temp.Soil..F, -Dewpt..F, -Rain.in, -Wind.Dir, -Solar.Rad.W.m.2, -Wind.Speed.mph, -Pressure.inHg)
sm.cc<-sm.cc[!duplicated(sm.cc),]
cc<-arrange(sm.cc, doy, year)

ccx<-dplyr::select(cc, -hour)
ccx<-ccx[!duplicated(ccx),]

ccx$tchill<-ifelse(ccx$tchill>=0&ccx$tchill<=5, 1, 0)
ccx$chill<-ave(
  ccx$tchill, ccx$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ccx$chill.day<-ave(ccx$tchill, ccx$date, FUN = sum)

ccwarm<-ccx%>%dplyr::select(doy, year, tmean)
ccwarm<-ccwarm[!duplicated(ccwarm),]
ccwarm$twarm<-ccwarm$tmean
ccwarm$twarm<-ifelse(ccwarm$twarm>=5, ccwarm$twarm, 0)
ccwarm$gdd<-ave(
  ccwarm$twarm, ccwarm$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ccx<-full_join(ccx, ccwarm)

ccpre<-ccx%>%dplyr::select(doy, year, precip)
ccpre<-ccpre[!duplicated(ccpre),]
ccpre$aprecip <- ave(
  ccpre$precip, ccpre$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

ccx<-full_join(ccx, ccpre)
ccx<-dplyr::select(ccx, -precip)

ccfrz<-ccx%>%dplyr::select(doy, year, tmin)
ccfrz<-ccfrz[!duplicated(ccfrz),]
ccfrz$frost<-ifelse(ccfrz$tmin<=-2.2, 1, 0)
ccfrz$frost <- ave(
  ccfrz$frost, ccfrz$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ccx<-full_join(ccx, ccfrz)

test<-dplyr::select(ccx, doy, year, tmin, frost)
test<-subset(test, test$year==2017)
test<-filter(test, doy<220)
test<-test[!duplicated(test),]

stan.cc<-ccx[!duplicated(ccx),]


### Let's join em up!
stan.cc$year<-as.numeric(stan.cc$year)
stan.cc$doy<-as.numeric(stan.cc$doy)
stan.cc<-stan.cc%>%filter(year>=2015)
stan.cc$frz<-ifelse(stan.cc$tmin<=-2.2, 1, 0)

test<-dplyr::select(stan.cc, doy, year, tmin, frost, frz)
test<-subset(test, test$year==2017)
test<-filter(test, doy<220)
test<-test[!duplicated(test),]

stan.cc$chill<-ave(stan.cc$chill, stan.cc$date, FUN=max)
stan.cc$chill.day<-ave(stan.cc$chill.day, stan.cc$date, FUN=max)

stan.cc<-stan.cc[!duplicated(stan.cc),]

stan.bb$doy<-as.numeric(stan.bb$doy)

flava.bb<-stan.bb%>%filter(Species=="flava")%>%arrange(ID, year)
flava.bb$ID.year<-paste(flava.bb$ID, flava.bb$year)
flava.bb$first<-ave(flava.bb$doy, flava.bb$ID, flava.bb$year, FUN=min)
flava.bb$last<-ave(flava.bb$doy, flava.bb$ID, flava.bb$year, FUN=max)
days.btw <- Map(seq, flava.bb$first, flava.bb$last, by = 1)

flava <- data.frame(ID.year = rep.int(flava.bb$ID.year, vapply(days.btw, length, 1L)), 
                    doy = do.call(c, days.btw))
flava$Genus<-"Aesculus"
flava$Species<-"flava"
flava$ID<-substr(flava$ID.year, 1, 5)
flava$year<-as.numeric(substr(flava$ID.year, 7,10))
flava$doy<-as.numeric(flava$doy)
flava$ID<-as.numeric(flava$ID)
fl.frz<-dplyr::select(stan.cc, year, doy, frz)
fl.frz<-inner_join(flava, fl.frz, by=c("year", "doy"))
fl.frz<-arrange(fl.frz, doy, year, ID)
fl.frz$frz <- ave(
  fl.frz$frz, fl.frz$year, fl.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
fl.frz<-fl.frz[!duplicated(fl.frz),]

alleghaniensis.bb<-stan.bb%>%filter(Species=="alleghaniensis")%>%arrange(ID, year)
alleghaniensis.bb$ID.year<-paste(alleghaniensis.bb$ID, alleghaniensis.bb$year)
alleghaniensis.bb$first<-ave(alleghaniensis.bb$doy, alleghaniensis.bb$ID, alleghaniensis.bb$year, FUN=min)
alleghaniensis.bb$last<-ave(alleghaniensis.bb$doy, alleghaniensis.bb$ID, alleghaniensis.bb$year, FUN=max)
days.btw <- Map(seq, alleghaniensis.bb$first, alleghaniensis.bb$last, by = 1)

alleghaniensis <- data.frame(ID.year = rep.int(alleghaniensis.bb$ID.year, vapply(days.btw, length, 1L)), 
                             doy = do.call(c, days.btw))
alleghaniensis$Genus<-"Betula"
alleghaniensis$Species<-"alleghaniensis"
alleghaniensis$ID<-substr(alleghaniensis$ID.year, 1, 5)
alleghaniensis$year<-substr(alleghaniensis$ID.year, 7,10)
alleghaniensis$year<-as.numeric(substr(alleghaniensis$ID.year, 7,10))
alleghaniensis$doy<-as.numeric(alleghaniensis$doy)
alleghaniensis$ID<-as.numeric(alleghaniensis$ID)
al.frz<-dplyr::select(stan.cc, year, doy, frz)
al.frz<-inner_join(alleghaniensis, al.frz, by=c("year", "doy"))
al.frz<-arrange(al.frz, doy, year, ID)
al.frz$frz <- ave(
  al.frz$frz, al.frz$year, al.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
al.frz<-al.frz[!duplicated(al.frz),]

nigra.bb<-stan.bb%>%filter(Species=="nigra")%>%arrange(ID, year)
nigra.bb$ID.year<-paste(nigra.bb$ID, nigra.bb$year)
nigra.bb$first<-ave(nigra.bb$doy, nigra.bb$ID, nigra.bb$year, FUN=min)
nigra.bb$last<-ave(nigra.bb$doy, nigra.bb$ID, nigra.bb$year, FUN=max)
days.btw <- Map(seq, nigra.bb$first, nigra.bb$last, by = 1)

nigra <- data.frame(ID.year = rep.int(nigra.bb$ID.year, vapply(days.btw, length, 1L)), 
                    doy = do.call(c, days.btw))
nigra$Genus<-"Betula"
nigra$Species<-"nigra"
nigra$ID<-substr(nigra$ID.year, 1, 5)
nigra$year<-substr(nigra$ID.year, 7,10)
nigra$year<-as.numeric(substr(nigra$ID.year, 7,10))
nigra$doy<-as.numeric(nigra$doy)
nigra$ID<-as.numeric(nigra$ID)
ni.frz<-dplyr::select(stan.cc, year, doy, frz)
ni.frz<-inner_join(nigra, ni.frz, by=c("year", "doy"))
ni.frz<-arrange(ni.frz, doy, year, ID)
ni.frz$frz <- ave(
  ni.frz$frz, ni.frz$year, ni.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ni.frz<-ni.frz[!duplicated(ni.frz),]

glabra.bb<-stan.bb%>%filter(Species=="glabra")%>%arrange(ID, year)
glabra.bb$ID.year<-paste(glabra.bb$ID, glabra.bb$year)
glabra.bb$first<-ave(glabra.bb$doy, glabra.bb$ID, glabra.bb$year, FUN=min)
glabra.bb$last<-ave(glabra.bb$doy, glabra.bb$ID, glabra.bb$year, FUN=max)
days.btw <- Map(seq, glabra.bb$first, glabra.bb$last, by = 1)

glabra <- data.frame(ID.year = rep.int(glabra.bb$ID.year, vapply(days.btw, length, 1L)), 
                     doy = do.call(c, days.btw))
glabra$Genus<-"Carya"
glabra$Species<-"glabra"
glabra$ID<-substr(glabra$ID.year, 1, 5)
glabra$year<-substr(glabra$ID.year, 7,10)
glabra$year<-as.numeric(substr(glabra$ID.year, 7,10))
glabra$doy<-as.numeric(glabra$doy)
glabra$ID<-as.numeric(glabra$ID)
gl.frz<-dplyr::select(stan.cc, year, doy, frz)
gl.frz<-inner_join(glabra, gl.frz, by=c("year", "doy"))
gl.frz<-arrange(gl.frz, doy, year, ID)
gl.frz$frz <- ave(
  gl.frz$frz, gl.frz$year, gl.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
gl.frz<-gl.frz[!duplicated(gl.frz),]

ovata.bb<-stan.bb%>%filter(Species=="ovata")%>%arrange(ID, year)
ovata.bb$ID.year<-paste(ovata.bb$ID, ovata.bb$year)
ovata.bb$first<-ave(ovata.bb$doy, ovata.bb$ID, ovata.bb$year, FUN=min)
ovata.bb$last<-ave(ovata.bb$doy, ovata.bb$ID, ovata.bb$year, FUN=max)
days.btw <- Map(seq, ovata.bb$first, ovata.bb$last, by = 1)

ovata <- data.frame(ID.year = rep.int(ovata.bb$ID.year, vapply(days.btw, length, 1L)), 
                    doy = do.call(c, days.btw))
ovata$Genus<-"Carya"
ovata$Species<-"ovata"
ovata$ID<-substr(ovata$ID.year, 1, 5)
ovata$year<-substr(ovata$ID.year, 7,10)
ovata$year<-as.numeric(substr(ovata$ID.year, 7,10))
ovata$doy<-as.numeric(ovata$doy)
ovata$ID<-as.numeric(ovata$ID)
ov.frz<-dplyr::select(stan.cc, year, doy, frz)
ov.frz<-inner_join(ovata, ov.frz, by=c("year", "doy"))
ov.frz<-arrange(ov.frz, doy, year, ID)
ov.frz$frz <- ave(
  ov.frz$frz, ov.frz$year, ov.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ov.frz<-ov.frz[!duplicated(ov.frz),]

grandifolia.bb<-stan.bb%>%filter(Species=="grandifolia")%>%arrange(ID, year)
grandifolia.bb$ID.year<-paste(grandifolia.bb$ID, grandifolia.bb$year)
grandifolia.bb$first<-ave(grandifolia.bb$doy, grandifolia.bb$ID, grandifolia.bb$year, FUN=min)
grandifolia.bb$last<-ave(grandifolia.bb$doy, grandifolia.bb$ID, grandifolia.bb$year, FUN=max)
days.btw <- Map(seq, grandifolia.bb$first, grandifolia.bb$last, by = 1)

grandifolia <- data.frame(ID.year = rep.int(grandifolia.bb$ID.year, vapply(days.btw, length, 1L)), 
                          doy = do.call(c, days.btw))
grandifolia$Genus<-"Fagus"
grandifolia$Species<-"grandifolia"
grandifolia$ID<-substr(grandifolia$ID.year, 1, 5)
grandifolia$year<-substr(grandifolia$ID.year, 7,10)
grandifolia$year<-as.numeric(substr(grandifolia$ID.year, 7,10))
grandifolia$doy<-as.numeric(grandifolia$doy)
grandifolia$ID<-as.numeric(grandifolia$ID)
gr.frz<-dplyr::select(stan.cc, year, doy, frz)
gr.frz<-inner_join(grandifolia, gr.frz, by=c("year", "doy"))
gr.frz<-arrange(gr.frz, doy, year, ID)
gr.frz$frz <- ave(
  gr.frz$frz, gr.frz$year, gr.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
gr.frz<-gr.frz[!duplicated(gr.frz),]

deltoides.bb<-stan.bb%>%filter(Species=="deltoides")%>%arrange(ID, year)
deltoides.bb$ID.year<-paste(deltoides.bb$ID, deltoides.bb$year)
deltoides.bb$first<-ave(deltoides.bb$doy, deltoides.bb$ID, deltoides.bb$year, FUN=min)
deltoides.bb$last<-ave(deltoides.bb$doy, deltoides.bb$ID, deltoides.bb$year, FUN=max)
days.btw <- Map(seq, deltoides.bb$first, deltoides.bb$last, by = 1)

deltoides <- data.frame(ID.year = rep.int(deltoides.bb$ID.year, vapply(days.btw, length, 1L)), 
                        doy = do.call(c, days.btw))
deltoides$Genus<-"Populus"
deltoides$Species<-"deltoides"
deltoides$ID<-substr(deltoides$ID.year, 1, 5)
deltoides$year<-substr(deltoides$ID.year, 7,10)
deltoides$year<-as.numeric(substr(deltoides$ID.year, 7,10))
deltoides$doy<-as.numeric(deltoides$doy)
deltoides$ID<-as.numeric(deltoides$ID)
de.frz<-dplyr::select(stan.cc, year, doy, frz)
de.frz<-inner_join(deltoides, de.frz, by=c("year", "doy"))
de.frz<-arrange(de.frz, doy, year, ID)
de.frz$frz <- ave(
  de.frz$frz, de.frz$year, de.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
de.frz<-de.frz[!duplicated(de.frz),]

rubra.bb<-stan.bb%>%filter(Species=="rubra")%>%arrange(ID, year)
rubra.bb$ID.year<-paste(rubra.bb$ID, rubra.bb$year)
rubra.bb$first<-ave(rubra.bb$doy, rubra.bb$ID, rubra.bb$year, FUN=min)
rubra.bb$last<-ave(rubra.bb$doy, rubra.bb$ID, rubra.bb$year, FUN=max)
days.btw <- Map(seq, rubra.bb$first, rubra.bb$last, by = 1)

rubra <- data.frame(ID.year = rep.int(rubra.bb$ID.year, vapply(days.btw, length, 1L)), 
                    doy = do.call(c, days.btw))
rubra$Genus<-"Quercus"
rubra$Species<-"rubra"
rubra$ID<-substr(rubra$ID.year, 1, 5)
rubra$year<-substr(rubra$ID.year, 7,10)
rubra$year<-as.numeric(substr(rubra$ID.year, 7,10))
rubra$doy<-as.numeric(rubra$doy)
rubra$ID<-as.numeric(rubra$ID)
ru.frz<-dplyr::select(stan.cc, year, doy, frz)
ru.frz<-inner_join(rubra, ru.frz, by=c("year", "doy"))
ru.frz<-arrange(ru.frz, doy, year, ID)
ru.frz$frz <- ave(
  ru.frz$frz, ru.frz$year, ru.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ru.frz<-ru.frz[!duplicated(ru.frz),]

americana.bb<-stan.bb%>%filter(Species=="americana")%>%arrange(ID, year)
americana.bb$ID.year<-paste(americana.bb$ID, americana.bb$year)
americana.bb$first<-ave(americana.bb$doy, americana.bb$ID, americana.bb$year, FUN=min)
americana.bb$last<-ave(americana.bb$doy, americana.bb$ID, americana.bb$year, FUN=max)
days.btw <- Map(seq, americana.bb$first, americana.bb$last, by = 1)

americana <- data.frame(ID.year = rep.int(americana.bb$ID.year, vapply(days.btw, length, 1L)), 
                        doy = do.call(c, days.btw))
americana$Genus<-"Tilia"
americana$Species<-"americana"
americana$ID<-substr(americana$ID.year, 1, 5)
americana$year<-substr(americana$ID.year, 7,10)
americana$year<-as.numeric(substr(americana$ID.year, 7,10))
americana$doy<-as.numeric(americana$doy)
americana$ID<-as.numeric(americana$ID)
am.frz<-dplyr::select(stan.cc, year, doy, frz)
am.frz<-inner_join(americana, am.frz, by=c("year", "doy"))
am.frz<-arrange(am.frz, doy, year, ID)
am.frz$frz <- ave(
  am.frz$frz, am.frz$year, am.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am.frz<-am.frz[!duplicated(am.frz),]

alba.bb<-stan.bb%>%filter(Species=="alba")%>%arrange(ID, year)
alba.bb$ID.year<-paste(alba.bb$ID, alba.bb$year)
alba.bb$first<-ave(alba.bb$doy, alba.bb$ID, alba.bb$year, FUN=min)
alba.bb$last<-ave(alba.bb$doy, alba.bb$ID, alba.bb$year, FUN=max)
days.btw <- Map(seq, alba.bb$first, alba.bb$last, by = 1)

alba <- data.frame(ID.year = rep.int(alba.bb$ID.year, vapply(days.btw, length, 1L)), 
                   doy = do.call(c, days.btw))
alba$Genus<-"Quercus"
alba$Species<-"alba"
alba$ID<-substr(alba$ID.year, 1, 5)
alba$year<-substr(alba$ID.year, 7,10)
alba$year<-as.numeric(substr(alba$ID.year, 7,10))
alba$doy<-as.numeric(alba$doy)
alba$ID<-as.numeric(alba$ID)
alb.frz<-dplyr::select(stan.cc, year, doy, frz)
alb.frz<-inner_join(alba, alb.frz, by=c("year", "doy"))
alb.frz<-arrange(alb.frz, doy, year, ID)
alb.frz$frz <- ave(
  alb.frz$frz, alb.frz$year, alb.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
alb.frz<-alb.frz[!duplicated(alb.frz),]

saccharum.bb<-stan.bb%>%filter(Species=="saccharum")%>%arrange(ID, year)
saccharum.bb$ID.year<-paste(saccharum.bb$ID, saccharum.bb$year)
saccharum.bb$first<-ave(saccharum.bb$doy, saccharum.bb$ID, saccharum.bb$year, FUN=min)
saccharum.bb$last<-ave(saccharum.bb$doy, saccharum.bb$ID, saccharum.bb$year, FUN=max)
days.btw <- Map(seq, saccharum.bb$first, saccharum.bb$last, by = 1)

saccharum <- data.frame(ID.year = rep.int(saccharum.bb$ID.year, vapply(days.btw, length, 1L)), 
                        doy = do.call(c, days.btw))
saccharum$Genus<-"Acer"
saccharum$Species<-"saccharum"
saccharum$ID<-substr(saccharum$ID.year, 1, 5)
saccharum$year<-substr(saccharum$ID.year, 7,10)
saccharum$year<-as.numeric(substr(saccharum$ID.year, 7,10))
saccharum$doy<-as.numeric(saccharum$doy)
saccharum$ID<-as.numeric(saccharum$ID)
sa.frz<-dplyr::select(stan.cc, year, doy, frz)
sa.frz<-inner_join(saccharum, sa.frz, by=c("year", "doy"))
sa.frz<-arrange(sa.frz, doy, year, ID)
sa.frz$frz <- ave(
  sa.frz$frz, sa.frz$year, sa.frz$ID,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
sa.frz<-sa.frz[!duplicated(sa.frz),]

stan.bb<-ungroup(stan.bb)
bb.frz<-stan.bb%>%dplyr::select(Genus, Species, phase, year, doy, photo, lat, elev)

spp<-full_join(fl.frz, al.frz)
spp<-full_join(spp, ni.frz)
spp<-full_join(spp, gl.frz)
spp<-full_join(spp, ov.frz)
spp<-full_join(spp, gr.frz)
spp<-full_join(spp, am.frz)
spp<-full_join(spp, ru.frz)
spp<-full_join(spp, alb.frz)
spp<-full_join(spp, sa.frz)
spp<-full_join(spp, de.frz)

bb.frz<-inner_join(bb.frz, spp)
bb.frz<-bb.frz[!duplicated(bb.frz),]

rm("al.frz", "alb.frz", "alba", "alba.bb", "alleghaniensis", "alleghaniensis.bb", "am.frz", "americana", "americana.bb",
   "de.frz", "deltoides", "deltoides.bb", "fl.frz", "flava", "flava.bb", "gl.frz", "glabra", "glabra.bb", "gr.frz", 
   "grandifolia", "grandifolia.bb", "ni.frz", "nigra", "nigra.bb", "ov.frz", "ovata", "ovata.bb", "ru.frz", "rubra", "rubra.bb",
   "sa.frz", "saccharum", "saccharum.bb")

cc.frz<-dplyr::select(stan.cc, solr, windsp, aprecip, tmean, tmin, tmax, year, doy, dewpt, gdd, chill, chill.day, frost)
#chill.tree<-full_join(bb.frz, cc.frz)



tree<-inner_join(bb.frz, cc.frz)
tree<-tree%>%dplyr::select(-ID.year)

tree$spp<-paste(tree$Genus, tree$Species)
tree$spp<-as.numeric(as.factor(tree$spp))

tree<-tree[!duplicated(tree),]
tree$lat<-as.numeric(tree$lat)
tree$elev<-as.numeric(tree$elev)
tree$photo<-as.numeric(tree$photo)
tree$solr<-as.numeric(tree$solr)
tree$windsp<-as.numeric(tree$windsp)
tree$tmin<-as.numeric(tree$tmin)
tree$tmax<-as.numeric(tree$tmax)
tree$tmean<-as.numeric(tree$tmean)
tree$dewpt<-as.numeric(tree$dewpt)
tree$aprecip<-as.numeric(tree$aprecip)
tree$frz<-as.numeric(tree$frz)
tree<-tree%>%rename(false.spring=frz)
tree$gdd<-as.numeric(tree$gdd)
tree$chill<-as.numeric(tree$chill)
tree$frost<-as.numeric(tree$frost)

tree$doy<-ave(tree$doy, tree$ID, tree$year, tree$phase, FUN=first)

tree$solr<-ave(tree$solr, tree$doy, tree$year, tree$phase)
#tree$aprecip<-ave(tree$aprecip, tree$ID, tree$year, tree$phase, FUN=last)
#tree$gdd<-ave(tree$gdd, tree$ID, tree$year, tree$phase, FUN=last)
#tree$photo<-ave(tree$photo, tree$ID, tree$year, tree$phase, FUN=last)
#tree$tmin<-ave(tree$tmin, tree$doy, tree$year, tree$phase)
#tree$tmean<-ave(tree$tmean, tree$doy, tree$year, tree$phase)
#tree$tmax<-ave(tree$tmax, tree$doy, tree$year, tree$phase)
tree$tdiff<-tree$tmax-tree$tmin
tree$windsp<-ave(tree$windsp, tree$ID, tree$year, tree$phase, FUN=last)

#tree$dewpt<-ave(tree$dewpt, tree$ID, tree$year, tree$phase)
tree$lat<-ave(tree$lat, tree$ID)
tree$elev<-ave(tree$elev, tree$ID)
tree$false.spring<-ave(tree$false.spring, tree$ID, tree$year, tree$phase, FUN=last)

tree<-tree[!duplicated(tree),]

write.csv(tree, file="~/Documents/git/treespotters/analysis/output/tree_rf_data.csv", row.names = FALSE)

##### BUDBURST! #######
tree.bb<-subset(tree, phase=="budburst")
tree.bb<-dplyr::select(tree.bb, doy, spp, year, photo, windsp, tmin,
                       tmax, tmean,  aprecip, false.spring, gdd, chill, tdiff, frost)
tree.bb<-tree.bb[!duplicated(tree.bb),]


bb<-randomForest(doy ~ spp + year + photo + windsp + tmin +
                   tmax + tmean + aprecip + false.spring + gdd + chill + tdiff + frost, data=tree.bb)
bb.rf<-varImpPlot(bb)


###### LEAFOUT! #########
tree.lo<-subset(tree, phase=="leafout")
tree.lo<-dplyr::select(tree.lo, doy, spp, year, photo, windsp, tmin,
                           tmax, tmean,  aprecip, false.spring, gdd, chill, tdiff, frost)
#tree.lo<-dplyr::select(tree.lo, doy, spp, lat, elev, year, photo, solr, windsp, tmin,
 #                      tmax, tmean, dewpt, aprecip, false.spring, gdd, chill, tdiff, frost)
tree.lo<-tree.lo[!duplicated(tree.lo),]

lo<-randomForest(doy ~ spp + year + photo + windsp + tmin +
                  tmax + tmean + aprecip + false.spring + gdd + chill + tdiff + frost, data=tree.lo)

#lo<-randomForest(doy ~ spp + lat + elev + year + photo + solr + windsp + tmin +
 #                  tmax + tmean + dewpt + aprecip + false.spring + gdd + chill + tdiff + frost, data=tree.lo)
lo.rf<-varImpPlot(lo)

###### FLOWERS! #########
tree.flo<-subset(tree, phase=="flowers")
tree.flo<-dplyr::select(tree.flo, doy, spp, year, photo, windsp, tmin,
                        tmax, tmean, aprecip, false.spring, gdd, chill, tdiff, frost)
tree.flo<-tree.flo[!duplicated(tree.flo),]


flowers<-randomForest(doy ~ spp + year + photo + windsp + tmin +
                        tmax + tmean + aprecip + false.spring + gdd + chill + tdiff + frost, data=tree.flo)
flo.rf<-varImpPlot(flowers)

###### Fruits! #########
tree.fr<-subset(tree, phase=="Fruits")
tree.fr<-dplyr::select(tree.fr, doy, spp, year, photo, windsp, tmin,
                       tmax, tmean, aprecip, false.spring, gdd, chill, tdiff, frost)
tree.fr<-tree.fr[!duplicated(tree.fr),]


fruits<-randomForest(doy ~ spp + year + photo + windsp + tmin +
                       tmax + tmean + aprecip + false.spring + gdd + chill + tdiff + frost, data=tree.fr)
fruits.rf<-varImpPlot(fruits)

###### Colored Leaves! #########
tree.cl<-subset(tree, phase=="Colored leaves")
tree.cl<-dplyr::select(tree.cl, doy, spp, year, photo, windsp, tmin,
                       tmax, tmean, aprecip, false.spring, gdd, chill, tdiff, frost)
tree.cl<-tree.cl[!duplicated(tree.cl),]


cl.leaves<-randomForest(doy ~ spp + year + photo + windsp + tmin +
                          tmax + tmean + aprecip + false.spring + gdd + chill + tdiff + frost, data=tree.cl)
clleaves.rf<-varImpPlot(cl.leaves)

###### Leaf Drop! #########
tree.dr<-subset(tree, phase=="leaf drop")
tree.dr<-dplyr::select(tree.dr, doy, spp, year, photo, windsp, tmin,
                       tmax, tmean, aprecip, false.spring, gdd, chill, tdiff, frost)
tree.dr<-tree.dr[!duplicated(tree.dr),]


lf.drop<-randomForest(doy ~ spp + year + photo + windsp + tmin +
                        tmax + tmean + aprecip + false.spring + gdd + chill + tdiff + frost, data=tree.dr)
lfdrop.rf<-varImpPlot(lf.drop)

###### Ripe Fruit! #########
tree.ri<-subset(tree, phase=="Ripe fruits")
tree.ri<-dplyr::select(tree.ri, doy, spp, year, photo, windsp, tmin,
                       tmax, tmean, aprecip, false.spring, gdd, chill, tdiff, frost)
tree.ri<-tree.ri[!duplicated(tree.ri),]


ripe<-randomForest(doy ~ spp + year + photo + windsp + tmin +
                     tmax + tmean + aprecip + false.spring + gdd + chill + tdiff + frost, data=tree.ri)
ripe.rf<-varImpPlot(ripe)

###### Pollen! #########
tree.po<-subset(tree, phase=="Pollen release (flowers)")
tree.po<-dplyr::select(tree.po, doy, spp, year, photo, windsp, tmin,
                       tmax, tmean, aprecip, false.spring, gdd, chill, tdiff, frost)
tree.po<-tree.po[!duplicated(tree.po),]


pollen<-randomForest(doy ~ spp + year + photo + windsp + tmin +
                       tmax + tmean + aprecip + false.spring + gdd + chill + tdiff + frost, data=tree.po)
pollen.rf<-varImpPlot(pollen)

quartz()
par(mfrow=c(2,4))
varImpPlot(bb)
varImpPlot(lo)
varImpPlot(flowers)
varImpPlot(pollen)
varImpPlot(fruits)
varImpPlot(ripe)
varImpPlot(cl.leaves)
varImpPlot(lf.drop)


