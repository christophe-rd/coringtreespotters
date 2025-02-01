## Cat - 24 April 2018 ##
## Now some stan models looking at Tree Spotters data

# ## housekeeping
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
d<-read.csv("output/tree_rf_data.csv", header=TRUE)
id<-read.csv("input/IDinfo.csv", header=TRUE)

## ID info to find provenance
id<-subset(id, select=c("Individual_ID", "Plant_Nickname"))
id<-id[!duplicated(id),]
write.csv(id, file="output/provenanceinfo.csv", row.names = FALSE)


## Budburst
bb<-subset(d, phase=="budburst")
bb<-dplyr::select(bb, doy, spp, photo, gdd, ID, year, aprecip, solr, chill, false.spring, tdiff, tmean, tmax, tmin, frost)

bb$photo.z <- (bb$photo-mean(bb$photo,na.rm=TRUE))/(2*sd(bb$photo,na.rm=TRUE))
bb$force.z <- (bb$gdd-mean(bb$gdd,na.rm=TRUE))/(2*sd(bb$gdd,na.rm=TRUE))
bb$chill.z <- (bb$chill-mean(bb$chill,na.rm=TRUE))/(2*sd(bb$chill,na.rm=TRUE))

bb.stan<-stan_glmer(doy~photo.z+force.z+chill.z + photo.z:force.z + photo.z:chill.z + force.z:chill.z +
                    (photo.z+force.z+chill.z|spp), data=bb)


df<-d[(d$phase=="budburst"|d$phase=="leafout"),]
df$bb<-NA
df$bb<-ifelse(df$phase=="budburst", df$bb=df$doy, df$bb)
df$lo<-NA
df$lo<-ifelse(df$phase=="leafout", df$lo=df$doy, df$lo)
df<-dplyr::select(df, bb, year, PEP_ID, lat, long, lo)
df$pep.year<-paste(df$year, df$PEP_ID)

dxx<-data_frame()
days.btw<-array()
for(i in length(df$pep.year)){
  days.btw[i] <- Map(seq, df$bb[i], df$lo[i], by = 1)
  dxx <- data.frame(PEP_ID=df$PEP_ID, year=df$year, lat=df$lat, long=df$long,
                    pep.year = rep.int(df$pep.year, vapply(days.btw[i], length, 1L)), 
                    doy = do.call(c, days.btw[i]))
}

dxx<-dxx[!duplicated(dxx),]
dxx<-dplyr::select(dxx, -pep.year)
x<-paste(dxx$year, dxx$doy)
dxx$date<-as.Date(strptime(x, format="%Y %j"))
dxx$Date<- as.character(dxx$date)










#bb.stan<-stan_glmer(doy~solr+tmean+tmin+tmax+year+(1|spp), data=bb)

#bb.combo<-stan_glmer(doy~photo+cgdd+cprecip+csolr+tmean+tdiff+(1|spp), data=bb)

bb.inter<-stan_glmer(doy~photo+cgdd+cchill+photo:cgdd+photo:cchill+cgdd:cchill+cprecip+
                       (1|spp), data=bb)

bb.brm<-brm(doy~cgdd+photo+cprecip+(1|spp) + (cgdd-1|spp) +
              (photo-1|spp) + (cprecip-1|spp), data=bb)


m<-bb.brm
m.int<-posterior_interval(m)
sum.m<-summary(m)
cri.f<-as.data.frame(sum.m$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.f<-cri.f[-1,] #removing the intercept 
fdf1<-as.data.frame(rbind(as.vector(cri.f[,1]), as.vector(cri.f[,2]), as.vector(cri.f[,3])))
fdf2<-cbind(fdf1, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2)<-c(rownames(cri.f), "spp", "perc")

cri.r<-(ranef(m, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$spp
cri.r2<-cri.r[, ,-1]
cri.r2<-cri.r2[,-2,]
dims<-dim(cri.r2)
twoDimMat <- matrix(cri.r2, prod(dims[1:2]), dims[3])
mat2<-cbind(twoDimMat, c(rep(1:11, length.out=33)), rep(c("Estimate", "2.5%", "95%"), each=11))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "spp", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, cgdd:`cprecip`, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=36)) {
  for (j in seq(from=3, to=25, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$spp>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$spp<-as.factor(dfwide$spp)
## plotting
library(ggstance)
pd <- position_dodgev(height = -0.5)

estimates<-c("GDDs", "Photoperiod","Precipitation")
dfwide$legend<-factor(dfwide$spp,
                      labels=c("Overall Effects","Aesculus hippocastanum","Alnus glutinosa",
                               "Betula pendula","Fagus sylvatica","Fraxinus excelsior",
                               "Quercus robur"))
estimates<-rev(estimates)
#write.csv(dfwide, file="~/Documents/git/springfreeze/output/df_modforplot.csv", row.names=FALSE)
fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=spp, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=c("blue", "firebrick3", "firebrick1", "orangered1",
                              "orange3", "orange1","sienna4", "sienna2", "green4", "purple2", "magenta3", "magenta1"))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5)) +
  guides(size=FALSE, alpha=FALSE) +  
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key=element_blank(),legend.key.size = unit(0.15, "cm"),
                              legend.text=element_text(size=8), legend.position= c(0.78,0.88), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black")) #+
#xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
quartz()
fig1



## Leafout
lo<-subset(d, phase=="leafout")
lo<-dplyr::select(lo, doy, spp, photo, gdd, ID, year, aprecip, solr, chill, false.spring, tdiff, tmean, tmax, tmin, frost)

lo<-lo[!duplicated(lo),]

lo$cgdd<-lo$gdd/20
lo$cprecip<-lo$aprecip/20
lo$cchill<-lo$chill/50
lo$csolr<-lo$solr/20


lo.stan<-stan_glmer(doy~cgdd+frost+cchill+cgdd:cchill+
                      (1|spp), data=lo)

#lo.stan<-stan_glmer(doy~cgdd+frost+cchill+cgdd:cchill+(1|spp), data=lo)

lo.brm<-brm(doy~cgdd+frost+cchill+cgdd:cchill+
              (cgdd-1|spp) + (frost-1|spp) +
              (cgdd-1|spp) + (1|spp), data=lo)

