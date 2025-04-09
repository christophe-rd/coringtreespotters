### Analyse the data and make some figures
# Cat 28 Nov 2018

rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(brms)
library(rstan)
library(RColorBrewer)

# Set Working Directory
setwd("~/Documents/git/treespotters/analysis")

## Get all that data!
## Start with phenology data and clean
bb<-read.csv("output/cleaned_tree_allcues.csv", header=TRUE)
bb$budburst<-ifelse(bb$Species=="deltoides"&bb$year==2016&bb$budburst==84, NA, bb$budburst)
bb<-bb[!is.na(bb$budburst),]
ok16<-read.csv("output/okeefe2016.csv", header=TRUE)
ok17<-read.csv("output/okeefe2017.csv", header=TRUE)


#Clean up John's data first
ok16.bb<-ok16%>%
  dplyr::select(JULIAN, TREEID, BBRK) %>%
  rename(doy=JULIAN)%>%
  rename(ind=TREEID)%>%
  rename(budburst=BBRK)
ok16.bb$budburst<-ifelse(ok16.bb$budburst>=75, ok16.bb$budburst, NA)
ok16.bb<-ok16.bb[!is.na(ok16.bb$budburst),]

ok16.bb<-ok16.bb %>% group_by(ind) %>% filter(row_number(budburst) == 1)                   
ok16.bb$bb<-ok16.bb$doy
                  
bb16<-ok16.bb%>%dplyr::select(ind, bb)
bb16<-bb16[!is.na(bb16$bb),]

ok16.lo<-ok16%>%
  dplyr::select(JULIAN, TREEID, L95) %>%
  rename(doy=JULIAN)%>%
  rename(ind=TREEID)%>%
  rename(leafout=L95)
ok16.lo$leafout<-ifelse(ok16.lo$leafout>=75, ok16.lo$leafout, NA)
ok16.lo<-ok16.lo[!is.na(ok16.lo$leafout),]

ok16.lo<-ok16.lo %>% group_by(ind) %>% filter(row_number(leafout) == 1)                   
ok16.lo$lo<-ok16.lo$doy

lo16<-ok16.lo%>%dplyr::select(ind, lo)
lo16<-lo16[!is.na(lo16$lo),]

ok16<-full_join(bb16, lo16)
spps<-c("ACSA", "BEAL", "FAGR", "QUAL", "QURU")
ok16$spp<-substr(ok16$ind, 0, 4)
ok16<-ok16%>%filter(spp%in%spps)

ok16$spp<-ifelse(ok16$spp=="ACSA", "Acer saccharum", ok16$spp)
ok16$spp<-ifelse(ok16$spp=="BEAL", "Betula alleghaniensis", ok16$spp)
ok16$spp<-ifelse(ok16$spp=="FAGR", "Fagus grandifolia", ok16$spp)
ok16$spp<-ifelse(ok16$spp=="QUAL", "Quercus alba", ok16$spp)
ok16$spp<-ifelse(ok16$spp=="QURU", "Quercus rubra", ok16$spp)

ok16$type<-"Harvard Forest"

ok17.bb<-ok17%>%
  dplyr::select(JULIAN, TREEID, BBRK) %>%
  rename(doy=JULIAN)%>%
  rename(ind=TREEID)%>%
  rename(budburst=BBRK)
ok17.bb$budburst<-ifelse(ok17.bb$budburst>=75, ok17.bb$budburst, NA)
ok17.bb<-ok17.bb[!is.na(ok17.bb$budburst),]

ok17.bb<-ok17.bb %>% group_by(ind) %>% filter(row_number(budburst) == 1)                   
ok17.bb$bb<-ok17.bb$doy

bb17<-ok17.bb%>%dplyr::select(ind, bb)
bb17<-bb17[!is.na(bb17$bb),]

ok17.lo<-ok17%>%
  dplyr::select(JULIAN, TREEID, L95) %>%
  rename(doy=JULIAN)%>%
  rename(ind=TREEID)%>%
  rename(leafout=L95)
ok17.lo$leafout<-ifelse(ok17.lo$leafout>=75, ok17.lo$leafout, NA)
ok17.lo<-ok17.lo[!is.na(ok17.lo$leafout),]

ok17.lo<-ok17.lo %>% group_by(ind) %>% filter(row_number(leafout) == 1)                   
ok17.lo$lo<-ok17.lo$doy

lo17<-ok17.lo%>%dplyr::select(ind, lo)
lo17<-lo17[!is.na(lo17$lo),]

ok17<-full_join(bb17, lo17)
ok17$spp<-substr(ok17$ind, 0, 4)
ok17<-ok17%>%filter(spp%in%spps)

ok17$spp<-ifelse(ok17$spp=="ACSA", "Acer saccharum", ok17$spp)
ok17$spp<-ifelse(ok17$spp=="BEAL", "Betula alleghaniensis", ok17$spp)
ok17$spp<-ifelse(ok17$spp=="FAGR", "Fagus grandifolia", ok17$spp)
ok17$spp<-ifelse(ok17$spp=="QUAL", "Quercus alba", ok17$spp)
ok17$spp<-ifelse(ok17$spp=="QURU", "Quercus rubra", ok17$spp)
ok17$type<-"Harvard Forest"



bb$spp<-paste(bb$Genus, bb$Species, sep=" ")
bb$m.bb<-ave(bb$budburst, bb$spp, bb$year)

bb.16<-subset(bb, bb$year==2016)
bb.16<-dplyr::select(bb.16, spp, m.bb)
bb.16<-bb.16[!duplicated(bb.16),]

bb.16$type<-"TreeSpotters"
bbok16<-dplyr::select(ok16, spp, bb, type, ind)
bbok16$m.bb<-ave(bbok16$bb, bbok16$spp)
bbok16<-bbok16%>%ungroup%>%dplyr::select(-ind, -bb)
bbok16<-bbok16[!duplicated(bbok16),]
bb.16<-full_join(bb.16, bbok16)

cols <- colorRampPalette(brewer.pal(11,"Spectral"))(11)
quartz()
bars<- ggplot(bb.16, aes(x=spp, y=m.bb, fill=spp, alpha=type)) + geom_bar(col=cols, stat="identity", position="dodge") + theme_classic() + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim=c(0,125)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
  plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), legend.position = "none", axis.title.x = element_blank(), 
  axis.ticks.x = element_blank()) + ylab("Day of Budburst")


bb.16<-subset(bb, bb$year==2016)
bb.16$type<-"TreeSpotters"
bb.16<-dplyr::select(bb.16, spp, budburst, type)
bbok16<-ok16%>%ungroup%>%dplyr::select(spp, bb, type)%>%rename(budburst=bb)
bbok16<-bbok16[!duplicated(bbok16),]
bbok16$type<-"Z Harvard Forest"
bb.16<-bb.16[!duplicated(bb.16),]
bb.16<-full_join(bb.16, bbok16)

quartz()
box<-ggplot(bb.16, aes(x=spp, y=budburst, fill=spp, alpha=type)) + geom_boxplot(aes(fill=as.factor(spp), col=as.factor(spp), alpha=as.factor(type))) +
  theme_classic() + scale_alpha_manual(name="Dataset", values=c(0.2, 1), labels=c("TreeSpotters", "Harvard Forest")) +
  scale_y_continuous(expand = c(0, 0)) + #coord_cartesian(ylim=c(80,135)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()) + ylab("Day of Budburst") + guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

bb.18<-subset(bb, bb$year==2018)
bb.18<-dplyr::select(bb.18, spp, m.bb)
bb.18<-bb.18[!duplicated(bb.18),]

cols <- colorRampPalette(brewer.pal(12,"Set3"))(11)
quartz()
bars18<- ggplot(bb.18, aes(x=spp, y=m.bb, fill=spp)) + geom_bar(col=cols, stat="identity", position="dodge") + theme_classic() + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim=c(0,125)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), legend.position = "none", axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()) + ylab("Day of Budburst")


bb.18<-subset(bb, bb$year==2018)
bb.18<-dplyr::select(bb.18, spp, budburst)
bb.18<-bb.18[!duplicated(bb.18),]

quartz()
box18<-ggplot(bb.18, aes(x=spp, y=budburst, fill=spp)) + geom_boxplot(aes(fill=as.factor(spp), col=as.factor(spp))) +
  theme_classic() + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim=c(80,135)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), legend.position = "none", axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()) + ylab("Day of Budburst")


bb.17<-subset(bb, bb$year==2017)
bb.17<-dplyr::select(bb.17, spp, m.bb)
bb.17<-bb.17[!duplicated(bb.17),]

cols <- colorRampPalette(brewer.pal(12,"Set3"))(11)
quartz()
bars17<- ggplot(bb.17, aes(x=spp, y=m.bb, fill=spp)) + geom_bar(col=cols, stat="identity", position="dodge") + theme_classic() + 
  scale_y_continuous(expand = c(0, 0)) + coord_cartesian(ylim=c(0,125)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), legend.position = "none", axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()) + ylab("Day of Budburst")


bb.17<-subset(bb, bb$year==2017)
bb.17$type<-"TreeSpotters"
bb.17<-dplyr::select(bb.17, spp, budburst, type)
bbok17<-ok17%>%ungroup%>%dplyr::select(spp, bb, type)%>%rename(budburst=bb)
bbok17<-bbok17[!duplicated(bbok17),]
bbok17$type<-"Z Harvard Forest"
bb.17<-bb.17[!duplicated(bb.17),]
bb.17<-full_join(bb.17, bbok17)

quartz()
box<-ggplot(bb.17, aes(x=spp, y=budburst, fill=spp, alpha=type)) + geom_boxplot(aes(fill=as.factor(spp), col=as.factor(spp), alpha=as.factor(type))) +
  theme_classic() + scale_alpha_manual(name="Dataset", values=c(0.2, 1), labels=c("TreeSpotters", "Harvard Forest")) +
  scale_y_continuous(expand = c(0, 0)) + #coord_cartesian(ylim=c(80,135)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()) + ylab("Day of Budburst") + guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)


dts16<-subset(bb, bb$year==2016)
dts16$type<-"TreeSpotters"
dts16<-dplyr::select(dts16, spp, budburst, leafout, type)
dts16$dvr<-dts16$leafout-dts16$budburst
dts16<-dplyr::select(dts16, -budburst, -leafout)

dok16<-ok16%>%ungroup%>%dplyr::select(spp, bb, type, lo)
dok16$dvr<-dok16$lo-dok16$bb
dok16<-dplyr::select(dok16, -bb, -lo)
dok16<-dok16[!duplicated(dok16),]
dok16$type<-"Z Harvard Forest"
dts16<-dts16[!duplicated(dts16),]
d.16<-full_join(dok16, dts16)

quartz()
box<-ggplot(d.16, aes(x=spp, y=dvr, fill=spp, alpha=type)) + geom_boxplot(aes(fill=as.factor(spp), col=as.factor(spp), alpha=as.factor(type))) +
  theme_classic() + scale_alpha_manual(name="Dataset", values=c(0.2, 1), labels=c("TreeSpotters", "Harvard Forest")) +
  scale_y_continuous(expand = c(0, 0)) + #coord_cartesian(ylim=c(80,135)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()) + ylab("Duration of Vegetative Risk") + guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

dts17<-subset(bb, bb$year==2017)
dts17$type<-"TreeSpotters"
dts17<-dplyr::select(dts17, spp, budburst, leafout, type)
dts17$dvr<-dts17$leafout-dts17$budburst
dts17<-dplyr::select(dts17, -budburst, -leafout)

dok17<-ok17%>%ungroup%>%dplyr::select(spp, bb, type, lo)
dok17$dvr<-dok17$lo-dok17$bb
dok17<-dplyr::select(dok17, -bb, -lo)
dok17<-dok17[!duplicated(dok17),]
dok17$type<-"Z Harvard Forest"
dts17<-dts17[!duplicated(dts17),]
d.17<-full_join(dok17, dts17)

quartz()
box<-ggplot(d.17, aes(x=spp, y=dvr, fill=spp, alpha=type)) + geom_boxplot(aes(fill=as.factor(spp), col=as.factor(spp), alpha=as.factor(type))) +
  theme_classic() + scale_alpha_manual(name="Dataset", values=c(0.2, 1), labels=c("TreeSpotters", "Harvard Forest")) +
  scale_y_continuous(expand = c(0, 0)) + #coord_cartesian(ylim=c(80,135)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()) + ylab("Duration of Vegetative Risk") + guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)



##### Fall stuffs! 
bb18<-read.csv("output/clean_alldata.csv", header=TRUE)
bb18<-subset(bb18, bb18$year==2018)

bb18<-subset(bb18, select=c("Genus", "Species", "col.leaves", "leafdrop"))
bb18$spp<-paste(bb18$Genus, bb18$Species, sep=" ")

dts18<-subset(bb, bb$year==2018)
dts18$type<-"TreeSpotters"
dts18<-dplyr::select(dts18, spp, budburst, leafout, type)
dts18$dvr<-dts18$leafout-dts18$budburst
dts18<-dplyr::select(dts18, -budburst, -leafout)


ok18<-read.csv("~/Documents/git/microclimates/analyses/output/okeefe2018.csv", header=TRUE)

ok18.bb<-ok18%>%
  dplyr::select(JULIAN, TREEID, BBRK) %>%
  rename(doy=JULIAN)%>%
  rename(ind=TREEID)%>%
  rename(budburst=BBRK)
ok18.bb$budburst<-ifelse(ok18.bb$budburst>=75, ok18.bb$budburst, NA)
ok18.bb<-ok18.bb[!is.na(ok18.bb$budburst),]

ok18.bb<-ok18.bb %>% group_by(ind) %>% filter(row_number(budburst) == 1)                   
ok18.bb$bb<-ok18.bb$doy


ok18.lo<-ok18%>%
  dplyr::select(JULIAN, TREEID, L95) %>%
  rename(doy=JULIAN)%>%
  rename(ind=TREEID)%>%
  rename(leafout=L95)
ok18.lo$leafout<-ifelse(ok18.lo$leafout>=75, ok18.lo$leafout, NA)
ok18.lo<-ok18.lo[!is.na(ok18.lo$leafout),]

ok18.lo<-ok18.lo %>% group_by(ind) %>% filter(row_number(leafout) == 1)                   
ok18.lo$lo<-ok18.lo$doy

lo18<-ok18.lo%>%dplyr::select(ind, lo)
lo18<-lo18[!is.na(lo18$lo),]

bb18<-ok18.bb%>%dplyr::select(ind, bb)
bb18<-bb18[!is.na(bb18$bb),]

ok18<-full_join(bb18, lo18)
ok18$spp<-substr(ok18$ind, 0, 4)
ok18<-ok18%>%filter(spp%in%spps)

ok18$spp<-ifelse(ok18$spp=="ACSA", "Acer saccharum", ok18$spp)
ok18$spp<-ifelse(ok18$spp=="BEAL", "Betula alleghaniensis", ok18$spp)
ok18$spp<-ifelse(ok18$spp=="FAGR", "Fagus grandifolia", ok18$spp)
ok18$spp<-ifelse(ok18$spp=="QUAL", "Quercus alba", ok18$spp)
ok18$spp<-ifelse(ok18$spp=="QURU", "Quercus rubra", ok18$spp)
ok18$type<-"Harvard Forest"




dok18<-ok18%>%ungroup%>%dplyr::select(spp, bb, type, lo)
dok18$dvr<-dok18$lo-dok18$bb
dok18<-dplyr::select(dok18, -bb, -lo)
dok18<-dok18[!duplicated(dok18),]
dok18$type<-"Z Harvard Forest"
dts18<-dts18[!duplicated(dts18),]
d.18<-full_join(dok18, dts18)








quartz()
box<-ggplot(bb18, aes(x=spp, y=leafdrop, fill=spp)) + geom_boxplot(aes(fill=as.factor(spp), col=as.factor(spp))) +
  theme_classic() + scale_alpha_manual(name="Dataset", values=c(0.2, 1), labels=c("TreeSpotters", "Harvard Forest")) +
  scale_y_continuous(expand = c(0, 0)) + #coord_cartesian(ylim=c(80,135)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), axis.title.x = element_blank(), legend.position = "none",
        axis.ticks.x = element_blank()) + ylab("Duration of Vegetative Risk") 


box<-ggplot(d.18, aes(x=spp, y=dvr, fill=spp, alpha=type)) + geom_boxplot(aes(fill=as.factor(spp), col=as.factor(spp), alpha=as.factor(type))) +
  theme_classic() + scale_alpha_manual(name="Dataset", values=c(0.2, 1), labels=c("TreeSpotters", "Harvard Forest")) +
  scale_y_continuous(expand = c(0, 0)) + #coord_cartesian(ylim=c(80,135)) +
  theme(text=element_text(family="Helvetica"),legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()) + ylab("Duration of Vegetative Risk") + guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)


cg <- read.csv("~/Documents/git/microclimates/analyses/output/dvr_cg_2018.csv", header= TRUE)
cg$dvr <- cg$leafout - cg$budburst
spp <- c("ACEPEN", "AMECAN", "BETALL", "BETPAP", "QUERUB", "VACMYR", "VIBCAS")

cg <- 