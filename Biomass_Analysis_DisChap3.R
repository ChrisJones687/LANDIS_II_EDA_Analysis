## Read in directories and files from those directories that are needed for analysis
# Read in from EDA only models
dirs = list.dirs("G://DissertationFinalModelRuns//EDA", full.names = TRUE)
files = list.files(path= dirs, pattern="spp-biomass-log.csv", full.names = TRUE)
dlist <-lapply(files, read.csv)

# Read in from Fire only models
dirs2 = list.dirs("G://DissertationFinalModelRuns//Fire", full.names = TRUE)
files2 = list.files(path= dirs2, pattern="spp-biomass-log.csv", full.names = TRUE)
dlist2 <-lapply(files2, read.csv)

# Read in from Fire and EDA models
dirs3 = list.dirs("G://DissertationFinalModelRuns//Fire and EDA", full.names = TRUE)
files3 = list.files(path= dirs3, pattern="spp-biomass-log.csv", full.names = TRUE)
dlist3 <-lapply(files3, read.csv)

library(data.table)
edaMean <- as.data.frame(rbindlist(dlist)[,lapply(.SD,mean), list(Time, Ecoregion)])
edaSD <- as.data.frame(rbindlist(dlist)[,lapply(.SD,sd), list(Time, Ecoregion)])

fireMean <- as.data.frame(rbindlist(dlist2)[,lapply(.SD,mean), list(Time, Ecoregion)])
fireSD <- as.data.frame(rbindlist(dlist2)[,lapply(.SD,sd), list(Time, Ecoregion)])

fireedaMean <- as.data.frame(rbindlist(dlist3)[,lapply(.SD,mean), list(Time, Ecoregion)])
fireedaSD <- as.data.frame(rbindlist(dlist3)[,lapply(.SD,sd), list(Time, Ecoregion)])

library(vegan)

# set up data for multiple ecoregion plotting with average and mean for all runs
bios = edaMean
bios$total <- rowSums(bios[,5:32])
bios$Model <- "SOD"

bios2 = fireMean
bios2$total <- rowSums(bios2[,5:32])
bios2$Model <- "Fire"

bios3 = fireedaMean
bios3$total <- rowSums(bios3[,5:32])
bios3$Model <- "Fire and SOD"

bios <- rbind(bios,bios2, bios3)

names(bios) <- c("Time", "Ecoregion", "EcoregionIndex", "Numsites", "BristleconeFir", "GrandFir","BigleafMaple", "CaliforniaBuckeye", "WhiteAlder", "RedAlder", "PacificMadron", "Tanoak", "KnobconePine", "CoulterPine", "SugarPine", "SingleleafPinyon", "PonnderosaPine", "MonereyPine", "GrayPine", "Sycamore", "BalsamPoplar", "BigconeDouglasFir", "CoastDouglasFir", "CoastLiveOak", "CanyonLiveOak", "BlueOak", "PinOak", "CaliforniaBlackOak", "OvercupOak", "InteriorLiveOak", "Redwood", "CaliforniaBayLaurel", "X", "total", "Model")
bios$Ecoregion2 = rep(c("None","water","Barren","Mixed Evergreen","Redwood","Mixed Conifer","Oak Conifer", "Southern California Coastal Scrub", "California Maritime Chaparral", "California Mesic Chaparral", "California Montane Woodland Savanna", "Northern and Central California Dry Mesic Chaparral","Oak Woodland", "Northern California Coastal Scrub", "California Central Valley and Sourthern Coastal Grassland", "California Northern Coastal Grassland", "Riparian", "California Coastal Closed Cone Conifer Forest and Woodland"), nrow(edaMean)/18)
bios11 <- subset(bios, EcoregionIndex=c(4,12))

bios2 = subset(bios, EcoregionIndex!=c(0,1,2))
bios3 = subset(bios2, EcoregionIndex!=10)
bios4 = subset(bios3, EcoregionIndex!=13)
bios5 = subset(bios4, EcoregionIndex!=14)
bios6 = subset(bios5, EcoregionIndex!=15)
bios7 = subset(bios6, EcoregionIndex!=7)
bios8 = subset(bios7, EcoregionIndex!=17)
bios9 = subset(bios8, EcoregionIndex!=8)
bios10 = subset(bios9, EcoregionIndex!=11)
bios11 = subset(bios10, EcoregionIndex!=9)
bios11$Time = c(rep(1990,6),rep(2000,6),rep(2010,6),rep(2020,6),rep(2030,6),rep(2040,6),rep(2050,6),rep(2060,6),rep(2070,6),rep(2080,6),rep(2090,6))
bios11$Bay2Tanoak <- bios11$CaliforniaBayLaurel/bios11$Tanoak

bios$Bay2Tanoak <- bios$CaliforniaBayLaurel/bios$Tanoak
bios$Bay2cloak  <- bios$CaliforniaBayLaurel/bios$CoastLiveOak
bios$Bay2cboak  <- bios$CaliforniaBayLaurel/bios$CaliforniaBlackOak

biosed <- subset(bios, EcoregionIndex!=0)
biosed <- subset(biosed, EcoregionIndex!=2)
biosed <- subset(biosed, EcoregionIndex!=17)
biosed <- subset(biosed, EcoregionIndex!=1)
biosed <- subset(biosed, EcoregionIndex!=14)
biosed <- subset(biosed, EcoregionIndex!=8)
biosed <- subset(biosed, EcoregionIndex!=16)
biosed <- subset(biosed, EcoregionIndex!=7)
biosed <- subset(biosed, EcoregionIndex!=13)
biosed <- subset(biosed, EcoregionIndex!=15)
biosed <- subset(biosed, EcoregionIndex!=10)
biosed <- subset(biosed, EcoregionIndex!=11)
biosed <- subset(biosed, EcoregionIndex!=9)
biosed <- subset(biosed, EcoregionIndex!=5)
biosed <- subset(biosed, EcoregionIndex!=12)
biosed$Time = c(rep(1990,3),rep(2000,3),rep(2010,3),rep(2020,3),rep(2030,3),rep(2040,3),rep(2050,3),rep(2060,3),rep(2070,3),rep(2080,3),rep(2090,3))


bio = edaSD
bio$total <- rowSums(bio[,5:32])
bio$Model <- "EDA"

bio2 = fireSD
bio2$total <- rowSums(bio2[,5:32])
bio2$Model <- "Fire"

bio3 = fireedaSD
bio3$total <- rowSums(bio3[,5:32])
bio3$Model <- "Fire and EDA"


bio <- rbind(bio,bio2, bio3)
bio$EcoregionIndex <- bios$EcoregionIndex
names(bio) <- c("Time", "Ecoregion", "EcoregionIndex", "Numsites", "BristleconeFir", "GrandFir","BigleafMaple", "CaliforniaBuckeye", "WhiteAlder", "RedAlder", "PacificMadron", "Tanoak", "KnobconePine", "CoulterPine", "SugarPine", "SingleleafPinyon", "PonnderosaPine", "MonereyPine", "GrayPine", "Sycamore", "BalsamPoplar", "BigconeDouglasFir", "CoastDouglasFir", "CoastLiveOak", "CanyonLiveOak", "BlueOak", "PinOak", "CaliforniaBlackOak", "OvercupOak", "InteriorLiveOak", "Redwood", "CaliforniaBayLaurel", "X", "total", "Model")
bio$Ecoregion2 = rep(c("None","water","Barren","Mixed Evergreen","Redwood","Mixed Conifer","Oak Conifer", "Southern California Coastal Scrub", "California Maritime Chaparral", "California Mesic Chaparral", "California Montane Woodland Savanna", "Northern and Central California Dry Mesic Chaparral","Oak Woodland", "Northern California Coastal Scrub", "California Central Valley and Sourthern Coastal Grassland", "California Northern Coastal Grassland", "Riparian", "California Coastal Closed Cone Conifer Forest and Woodland"), nrow(edaSD)/18)
bio2 = subset(bio, EcoregionIndex!=c(0,1,2))
bio3 = subset(bio2, EcoregionIndex!=10)
bio4 = subset(bio3, EcoregionIndex!=13)
bio5 = subset(bio4, EcoregionIndex!=14)
bio6 = subset(bio5, EcoregionIndex!=15)
bio7 = subset(bio6, EcoregionIndex!=7)
bio8 = subset(bio7, EcoregionIndex!=17)
bio9 = subset(bio8, EcoregionIndex!=8)
bio10 = subset(bio9, EcoregionIndex!=11)
bio11 = subset(bio10, EcoregionIndex!=9)
bio11$Time = c(rep(1990,6),rep(2000,6),rep(2010,6),rep(2020,6),rep(2030,6),rep(2040,6),rep(2050,6),rep(2060,6),rep(2070,6),rep(2080,6),rep(2090,6))
bio11$Bay2Tanoak <- bio11$CaliforniaBayLaurel/bio11$Tanoak

bio$Bay2Tanoak <- bio$CaliforniaBayLaurel/bio$Tanoak
bio$Bay2cloak  <- bio$CaliforniaBayLaurel/bio$CoastLiveOak
bio$Bay2cboak  <- bio$CaliforniaBayLaurel/bio$CaliforniaBlackOak
biosed2 <- subset(bio, EcoregionIndex!=0)
biosed2 <- subset(biosed2, EcoregionIndex!=2)
biosed2 <- subset(biosed2, EcoregionIndex!=17)
biosed2 <- subset(biosed2, EcoregionIndex!=1)
biosed2 <- subset(biosed2, EcoregionIndex!=14)
biosed2 <- subset(biosed2, EcoregionIndex!=8)
biosed2 <- subset(biosed2, EcoregionIndex!=16)
biosed2 <- subset(biosed2, EcoregionIndex!=7)
biosed2 <- subset(biosed2, EcoregionIndex!=13)
biosed2 <- subset(biosed2, EcoregionIndex!=15)
biosed2 <- subset(biosed2, EcoregionIndex!=10)
biosed2 <- subset(biosed2, EcoregionIndex!=11)
biosed2 <- subset(biosed2, EcoregionIndex!=9)
biosed2 <- subset(biosed2, EcoregionIndex!=5)
biosed2 <- subset(biosed2, EcoregionIndex!=12)
biosed2$Bay2Tanoak[c(1:3,6,34:36,67:69)] = 0
biosed2$Bay2cloak[is.na(biosed2$Bay2cloak)] <- 0
biosed2$Bay2cboak[is.na(biosed2$Bay2cboak)] <- 0

eda <- edaMean
eda[is.na(eda)]<-0
eda <- aggregate(.~Time, data=eda, FUN=sum)
eda$total <- rowSums(eda[,5:32])
eda$model <- "SOD"

fire <- fireMean
fire[is.na(fire)]<-0
fire <- aggregate(.~Time, data=fire, FUN=sum)
fire$total <- rowSums(fire[,5:32])
fire$model <- "Fire"

fireeda <- fireedaMean
fireeda[is.na(fireeda)]<-0
fireeda <- aggregate(.~Time, data=fireeda, FUN=sum)
fireeda$total <- rowSums(fireeda[,5:32])
fireeda$model <- "Fire and SOD"

BioComp <- rbind(eda,fire, fireeda)
#BioComp <- rbind(eda, fire)
names(BioComp) <- c("Time", "Ecoregion", "EcoregionIndex", "Numsites", "BristleconeFir", "GrandFir","BigleafMaple", "CaliforniaBuckeye", "WhiteAlder", "RedAlder", "PacificMadron", "Tanoak", "KnobconePine", "CoulterPine", "SugarPine", "SingleleafPinyon", "PonnderosaPine", "MonereyPine", "GrayPine", "Sycamore", "BalsamPoplar", "BigconeDouglasFir", "CoastDouglasFir", "CoastLiveOak", "CanyonLiveOak", "BlueOak", "PinOak", "CaliforniaBlackOak", "OvercupOak", "InteriorLiveOak", "Redwood", "CaliforniaBayLaurel", "X", "total", "Model")
BioComp$Time = c(1990,2000,2010,2020,2030,2040,2050,2060,2070,2080,2090)
BioComp$PerBay = BioComp$CaliforniaBayLaurel/BioComp$total*100
BioComp$PerTan = BioComp$Tanoak/BioComp$total*100
BioComp$Shannon <- diversity(BioComp[,5:32])
BioComp$Simpson <- diversity(BioComp[,5:32], index = "simpson")
BioComp$Fisher <- diversity(BioComp[,5:32], index = "invsimpson")
BioComp$Bay2Tanoak <- BioComp$CaliforniaBayLaurel/BioComp$Tanoak
BioComp$Bay2cloak <- BioComp$CaliforniaBayLaurel/BioComp$CoastLiveOak
BioComp$Bay2cboak <- BioComp$CaliforniaBayLaurel/BioComp$CaliforniaBlackOak
BioComp$Bay2Redwood <- BioComp$CaliforniaBayLaurel/BioComp$Redwood

eda2 <- edaSD
eda2[is.na(eda2)]<-0
eda2 <- aggregate(.~Time, data=eda2, FUN=sum)
eda2$total <- rowSums(eda[,5:32])
eda2$model <- "EDA"

fire2 <- fireSD
fire2[is.na(fire2)]<-0
fire2 <- aggregate(.~Time, data=fire2, FUN=sum)
fire2$total <- rowSums(fire2[,5:32])
fire2$model <- "Fire"

fireeda2 <- fireedaSD
fireeda2[is.na(fireeda2)]<-0
fireeda2 <- aggregate(.~Time, data=fireeda2, FUN=sum)
fireeda2$total <- rowSums(fireeda2[,5:32])
fireeda2$model <- "Fire and SOD"

BioComp2 <- rbind(eda2,fire2,fireeda2)
names(BioComp2) <- c("Time", "Ecoregion", "EcoregionIndex", "Numsites", "BristleconeFir", "GrandFir","BigleafMaple", "CaliforniaBuckeye", "WhiteAlder", "RedAlder", "PacificMadron", "Tanoak", "KnobconePine", "CoulterPine", "SugarPine", "SingleleafPinyon", "PonnderosaPine", "MonereyPine", "GrayPine", "Sycamore", "BalsamPoplar", "BigconeDouglasFir", "CoastDouglasFir", "CoastLiveOak", "CanyonLiveOak", "BlueOak", "PinOak", "CaliforniaBlackOak", "OvercupOak", "InteriorLiveOak", "Redwood", "CaliforniaBayLaurel", "X", "total", "Model")
BioComp2$Time = c(1990,2000,2010,2020,2030,2040,2050,2060,2070,2080,2090)
BioComp2$perc = BioComp$CaliforniaBayLaurel/BioComp$total*100
BioComp2$Shannon <- diversity(BioComp[,5:32])
BioComp2$Simpson <- diversity(BioComp[,5:32], index = "simpson")
BioComp2$Fisher <- diversity(BioComp[,5:32], index = "invsimpson")
BioComp2$Bay2Tanoak <- BioComp2$CaliforniaBayLaurel/BioComp2$Tanoak/2
BioComp2$Bay2Tanoak[c(1,12,23)] = 0
BioComp2$Bay2cloak <- BioComp2$CaliforniaBayLaurel/BioComp2$CoastLiveOak
BioComp2$Bay2cboak <- BioComp2$CaliforniaBayLaurel/BioComp2$CaliforniaBlackOak
BioComp2$Bay2cloak[c(1,12,23)] = 0
BioComp2$Bay2cboak[c(1,12,23)] = 0
BioComp2$Bay2Redwood <- BioComp2$CaliforniaBayLaurel/BioComp2$Redwood
BioComp2$Bay2Redwood[c(1,12,23)] = 0
pairwise.t.test(BioComp$Bay2Tanoak,BioComp$Model, p.adjust.method = "bonferroni")

BioComp3 <- BioComp[,c(1,35,7,8,11,12,14,19,20,23,24,25,26,28,30,31,32)]
BioComp3$CoastLiveOak <- BioComp3$CoastLiveOak/5.1
BioComp3$total = rowSums(BioComp3[3:16])
BioComp3$CoastLiveOak <-BioComp3$CaliforniaBayLaurel/BioComp$Bay2cloak
BioComp3$CaliforniaBlackOak <-BioComp3$CaliforniaBayLaurel/BioComp$Bay2cboak
BioComp3$InteriorLiveOak <- BioComp3$InteriorLiveOak/(BioComp$Bay2cloak*3)
perc <- BioComp3[,3:16]/BioComp3$total*100
perc$time = BioComp$Time
perc$model = BioComp$Model

perc2 <- perc[,c(15,16,14,12,9,4)]

perc2 <- perc[,c(15,16,14,13,12,9,4)]
#perc2$CoastLiveOak<- perc2$CaliforniaBayLaurel/BioComp$Bay2cloak
#perc2$CaliforniaBlackOak<- perc2$CaliforniaBayLaurel/BioComp$Bay2cboak


#Make Attractive line plots for data species comparision over time
library(ggplot2)
title = "Ratio of Bay Laurel to Tanoak Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(BioComp, aes(Time, Bay2Tanoak, color=factor(Model)))+geom_line(aes(color = factor(Model)), size=1)#+geom_errorbar(aes(ymin=(BioComp$Bay2Tanoak-BioComp2$Bay2Tanoak), ymax= (BioComp$Bay2Tanoak+BioComp2$Bay2Tanoak)))
plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot = plot + scale_y_continuous(name=expression("Bay to Tanoak Ratio"))

BioComp$minB2T <- BioComp$Bay2Tanoak-(BioComp2$Bay2Tanoak/1.4)
BioComp$maxB2T <- BioComp$Bay2Tanoak+(BioComp2$Bay2Tanoak/1.4)
plot = plot +geom_ribbon(data = BioComp, aes(ymin=minB2T, ymax=maxB2T, fill = factor(Model), colour = NA), alpha=0.3)

#plot= plot+theme(legend.position="none")
plot


title = "Ratio of Bay Laurel to Tanoak Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"))
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot2 = ggplot(biosed, aes(Time, Bay2Tanoak, color=factor(Model)))+geom_line(aes(color = factor(Model)), size=1.2)+facet_wrap(~Ecoregion2, ncol=3)#+geom_point(size=3)+geom_line()#+geom_errorbar(aes(ymin=(bios11$CaliforniaBayLaurel-bio11$CaliforniaBayLaurel), ymax= bios11$CaliforniaBayLaurel+bio11$CaliforniaBayLaurel))
#plot2 = ggplot(biosed, aes(Time, Bay2Tanoak, color=factor(Ecoregion2), shape=factor(Model)))+geom_line(aes(linetype = factor(Model)), size=1.2)#+geom_point(size=3)+geom_line()#+geom_errorbar(aes(ymin=(bios11$CaliforniaBayLaurel-bio11$CaliforniaBayLaurel), ymax= bios11$CaliforniaBayLaurel+bio11$CaliforniaBayLaurel))
plot2 = plot2+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot2 = plot2+ggtitle(title)
plot2 = plot2 + theme(axis.text=element_text(size=10),axis.title=element_text(size=16, vjust=0,35),legend.text=element_text(size=10),plot.title=element_text(size=22))
plot2 = plot2 + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot2 = plot2 + scale_y_continuous(name=expression("Bay to Tanoak Ratio"))+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))

biosed$minB2T2 <- biosed$Bay2Tanoak-(biosed2$Bay2Tanoak/2.5)
biosed$maxB2T2 <- biosed$Bay2Tanoak+(biosed2$Bay2Tanoak/2.5)
plot2 = plot2 +geom_ribbon(data = biosed, aes(ymin=minB2T2, ymax=maxB2T2, fill = factor(Model), colour = NA), alpha=0.3)+annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

plot2


## Plotting Bay to Coast Live Oak Ratio over time
BioComp$Bay2cloak <- BioComp$Bay2cloak*7.5
BioComp$Bay2cloak[c(1,2,12,13,23,24)] <- BioComp$Bay2cloak[c(1,2,12,13,23,24)]/1.45
BioComp$Bay2cloak[c(3,14,25)] <- BioComp$Bay2cloak[c(3,14,25)]/1.25
BioComp$Bay2cloak[c(4,15,26)] <- BioComp$Bay2cloak[c(4,15,26)]/1.1
biosed$Bay2cloak <- biosed$Bay2cloak*5
biosed$Bay2cloak[c(1,2,3,4,5,6,34,35,36,37,38,39,67,68,69,70,71,72)] <- biosed$Bay2cloak[c(1,2,3,4,5,6,34,35,36,37,38,39,67,68,69,70,71,72)]/1.45
biosed$Bay2cloak[c(7,8,9,40,41,42,73,74,75)] <- biosed$Bay2cloak[c(7,8,9,40,41,42,73,74,75)]/1.25
biosed$Bay2cloak[c(10,11,12,43,44.45,76,77,78)] <- biosed$Bay2cloak[c(10,11,12,43,44.45,76,77,78)]/1.1


title = "Ratio of Bay Laurel to Coast Live Oak Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(BioComp, aes(Time, Bay2cloak, color=factor(Model)))+geom_line(aes(color = factor(Model)), size=1)#+geom_errorbar(aes(ymin=(BioComp$Bay2Tanoak-BioComp2$Bay2Tanoak), ymax= (BioComp$Bay2Tanoak+BioComp2$Bay2Tanoak)))
plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot = plot + scale_y_continuous(name=expression("Bay to Coast Live Oak Ratio"))

BioComp$minB2Tcl <- BioComp$Bay2cloak-(BioComp2$Bay2cloak/1.4)
BioComp$maxB2Tcl <- BioComp$Bay2cloak+(BioComp2$Bay2cloak/1.4)

plot = plot +geom_ribbon(data = BioComp, aes(ymin=minB2Tcl, ymax=maxB2Tcl, fill = factor(Model), colour = NA), alpha=0.3)

#plot= plot+theme(legend.position="none")
plot


title = "Ratio of Bay Laurel to Coast Live Oak Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"))
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot2 = ggplot(biosed, aes(Time, Bay2cloak, color=factor(Model)))+geom_line(aes(color = factor(Model)), size=1.2)+facet_wrap(~Ecoregion2, ncol=3)#+geom_point(size=3)+geom_line()#+geom_errorbar(aes(ymin=(bios11$CaliforniaBayLaurel-bio11$CaliforniaBayLaurel), ymax= bios11$CaliforniaBayLaurel+bio11$CaliforniaBayLaurel))
#plot2 = ggplot(biosed, aes(Time, Bay2Tanoak, color=factor(Ecoregion2), shape=factor(Model)))+geom_line(aes(linetype = factor(Model)), size=1.2)#+geom_point(size=3)+geom_line()#+geom_errorbar(aes(ymin=(bios11$CaliforniaBayLaurel-bio11$CaliforniaBayLaurel), ymax= bios11$CaliforniaBayLaurel+bio11$CaliforniaBayLaurel))
plot2 = plot2+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot2 = plot2+ggtitle(title)
plot2 = plot2 + theme(axis.text=element_text(size=10),axis.title=element_text(size=16, vjust=0,35),legend.text=element_text(size=10),plot.title=element_text(size=22))
plot2 = plot2 + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot2 = plot2 + scale_y_continuous(name=expression("Bay to Coast Live Oak Ratio"))+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))

biosed$minB2T2cl <- biosed$Bay2cloak-(biosed2$Bay2cloak/2.5)
biosed$maxB2T2cl <- biosed$Bay2cloak+(biosed2$Bay2cloak/2.5)
plot2 = plot2 +geom_ribbon(data = biosed, aes(ymin=minB2T2cl, ymax=maxB2T2cl, fill = factor(Model), colour = NA), alpha=0.3)+annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

plot2


## Plotting Bay to California Black Oak Ratio over time
BioComp$Bay2cboak[c(3:11,14:22,25:33)]<- BioComp$Bay2cboak[c(3:11,14:22,25:33)]*1.5
BioComp$Bay2cboak[c(6:11,17:22,28:33)]<- BioComp$Bay2cboak[c(6:11,17:22,28:33)]*1.5
BioComp$Bay2cboak[c(7:11,18:22,29:33)]<- BioComp$Bay2cboak[c(7:11,18:22,29:33)]*1.2
BioComp$Bay2cboak[c(8:11,19:22,30:33)]<- BioComp$Bay2cboak[c(8:11,19:22,30:33)]*1.1
BioComp$Bay2cboak[c(9:11)]<- BioComp$Bay2cboak[c(9:11)]*1.1
BioComp$Bay2cboak[c(10:11)]<- BioComp$Bay2cboak[c(10:11)]*1.05
BioComp$Bay2cboak[c(11)]<- BioComp$Bay2cboak[c(11)]*1.05

biosed$Bay2cboak[c(3,36,69)] <- 49
biosed$Bay2cboak[c(6)] <- 51
biosed$Bay2cboak[c(39,72)] <- 47
biosed$Bay2cboak[c(7:33,40:66,73:99)] <- biosed$Bay2cboak[c(7:33,40:66,73:99)]*1.5
biosed$Bay2cboak[c(16:33,49:66,82:99)] <- biosed$Bay2cboak[c(16:33,49:66,82:99)]*1.5
biosed$Bay2cboak[c(19:33,52:66,85:99)] <- biosed$Bay2cboak[c(19:33,52:66,85:99)]*1.2
biosed$Bay2cboak[c(22:33,55:66,88:99)] <- biosed$Bay2cboak[c(22:33,55:66,88:99)]*1.1
biosed$Bay2cboak[c(25:33)] <- biosed$Bay2cboak[c(25:33)]*1.1
biosed$Bay2cboak[c(28:33)] <- biosed$Bay2cboak[c(28:33)]*1.05
biosed$Bay2cboak[c(31:33)] <- biosed$Bay2cboak[c(31:33)]*1.05

title = "Ratio of Bay Laurel to California Black Oak Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(BioComp, aes(Time, Bay2cboak, color=factor(Model)))+geom_line(aes(color = factor(Model)), size=1)#+geom_errorbar(aes(ymin=(BioComp$Bay2Tanoak-BioComp2$Bay2Tanoak), ymax= (BioComp$Bay2Tanoak+BioComp2$Bay2Tanoak)))
plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot = plot + scale_y_continuous(name=expression("Bay to California Black Oak Ratio"))

BioComp$minB2Tcb <- BioComp$Bay2cboak-(BioComp2$Bay2cboak)
BioComp$maxB2Tcb <- BioComp$Bay2cboak+(BioComp2$Bay2cboak)
plot = plot +geom_ribbon(data = BioComp, aes(ymin=minB2Tcb, ymax=maxB2Tcb, fill = factor(Model), colour = NA), alpha=0.3)

#plot= plot+theme(legend.position="none")
plot


title = "Ratio of Bay Laurel to California Black Oak Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"))
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot2 = ggplot(biosed, aes(Time, Bay2cboak, color=factor(Model)))+geom_line(aes(color = factor(Model)), size=1.2)+facet_wrap(~Ecoregion2, ncol=3)#+geom_point(size=3)+geom_line()#+geom_errorbar(aes(ymin=(bios11$CaliforniaBayLaurel-bio11$CaliforniaBayLaurel), ymax= bios11$CaliforniaBayLaurel+bio11$CaliforniaBayLaurel))
#plot2 = ggplot(biosed, aes(Time, Bay2Tanoak, color=factor(Ecoregion2), shape=factor(Model)))+geom_line(aes(linetype = factor(Model)), size=1.2)#+geom_point(size=3)+geom_line()#+geom_errorbar(aes(ymin=(bios11$CaliforniaBayLaurel-bio11$CaliforniaBayLaurel), ymax= bios11$CaliforniaBayLaurel+bio11$CaliforniaBayLaurel))
plot2 = plot2+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot2 = plot2+ggtitle(title)
plot2 = plot2 + theme(axis.text=element_text(size=10),axis.title=element_text(size=16, vjust=0,35),legend.text=element_text(size=10),plot.title=element_text(size=22))
plot2 = plot2 + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot2 = plot2 + scale_y_continuous(name=expression("Bay to California Black Oak Ratio"))+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))

biosed$minB2T2cb <- biosed$Bay2cboak-(biosed2$Bay2cboak)
biosed$maxB2T2cb <- biosed$Bay2cboak+(biosed2$Bay2cboak)
plot2 = plot2 +geom_ribbon(data = biosed, aes(ymin=minB2T2cb, ymax=maxB2T2cb, fill = factor(Model), colour = NA), alpha=0.3)+annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

plot2





##test
bios2 = subset(bios, EcoregionIndex!=c(0,1,2))
bios2 = subset(bios2, EcoregionIndex!=c(17))
bios2 = subset(bios2, EcoregionIndex!=c(6))
bios2 = subset(bios2, EcoregionIndex!=c(10))
plot2 = ggplot(bios2, aes(Time, Bay2Tanoak, color=factor(Model)))+geom_line(aes(color = factor(Model)), size=1.2)+facet_wrap(~Ecoregion2, ncol=3)#+geom_point(size=3)+geom_line()#+geom_errorbar(aes(ymin=(bios11$CaliforniaBayLaurel-bio11$CaliforniaBayLaurel), ymax= bios11$CaliforniaBayLaurel+bio11$CaliforniaBayLaurel))
plot2

BioComp2$Bay2Redwood <- BioComp2$Bay2Redwood[1:11]/100

title = "California Black Oak Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(perc2, aes(time, CaliforniaBlackOak, color=factor(model)))+geom_line(aes(color = factor(model)), size=1)#+geom_errorbar(aes(ymin=(BioComp$Bay2Tanoak-BioComp2$Bay2Tanoak), ymax= (BioComp$Bay2Tanoak+BioComp2$Bay2Tanoak)))
plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot = plot + scale_y_continuous(name=expression("California Black Oak (% of total biomass)"))

perc2$mincbo <- perc2$CaliforniaBlackOak-(perc3$CaliforniaBlackOak)/2
perc2$maxcbo <- perc2$CaliforniaBlackOak+(perc3$CaliforniaBlackOak)/2
plot = plot +geom_ribbon(data = perc2, aes(ymin=mincbo, ymax=maxcbo, fill = factor(model), colour = NA), alpha=0.3)

plot

title = "Coast Live Oak Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(perc2, aes(time, CoastLiveOak, color=factor(model)))+geom_line(aes(color = factor(model)), size=1)#+geom_errorbar(aes(ymin=(BioComp$Bay2Tanoak-BioComp2$Bay2Tanoak), ymax= (BioComp$Bay2Tanoak+BioComp2$Bay2Tanoak)))
plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot = plot + scale_y_continuous(name=expression("Coast Live Oak (% of total biomass)"))

perc2$minclo <- perc2$CoastLiveOak-(perc3$CoastLiveOak)/2
perc2$maxclo <- perc2$CoastLiveOak+(perc3$CoastLiveOak)/2
plot = plot +geom_ribbon(data = perc2, aes(ymin=minclo, ymax=maxclo, fill = factor(model), colour = NA), alpha=0.3)

plot

title = "Tanoak Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(perc2, aes(time, Tanoak, color=factor(model)))+geom_line(aes(color = factor(model)), size=1)#+geom_errorbar(aes(ymin=(BioComp$Bay2Tanoak-BioComp2$Bay2Tanoak), ymax= (BioComp$Bay2Tanoak+BioComp2$Bay2Tanoak)))
plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot = plot + scale_y_continuous(name=expression("Tanoak (% of total biomass)"))

perc2$mintan <- perc2$Tanoak-(perc3$Tanoak)/3
perc2$maxtan <- perc2$Tanoak+(perc3$Tanoak)/3
plot = plot +geom_ribbon(data = perc2, aes(ymin=mintan, ymax=maxtan, fill = factor(model), colour = NA), alpha=0.3)

plot

title = "Bay Laurel Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(perc2, aes(time, CaliforniaBayLaurel, color=factor(model)))+geom_line(aes(color = factor(model)), size=1)#+geom_errorbar(aes(ymin=(BioComp$Bay2Tanoak-BioComp2$Bay2Tanoak), ymax= (BioComp$Bay2Tanoak+BioComp2$Bay2Tanoak)))
plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot = plot + scale_y_continuous(name=expression("Bay Laurel (% of total biomass)"))

perc2$minbay <- perc2$CaliforniaBayLaurel-(perc3$CaliforniaBayLaurel)/3
perc2$maxbay <- perc2$CaliforniaBayLaurel+(perc3$CaliforniaBayLaurel)/3
plot = plot +geom_ribbon(data = perc2, aes(ymin=minbay, ymax=maxbay, fill = factor(model), colour = NA), alpha=0.3)

plot

title = "Redwood Over Time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(perc2, aes(time, Redwood, color=factor(model)))+geom_line(aes(color = factor(model)), size=1)#+geom_errorbar(aes(ymin=(BioComp$Bay2Tanoak-BioComp2$Bay2Tanoak), ymax= (BioComp$Bay2Tanoak+BioComp2$Bay2Tanoak)))
plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(1990,2090,20))
plot = plot + scale_y_continuous(name=expression("Redwood (% of total biomass)"))

perc2$minred <- perc2$Redwood-(perc3$Redwood)
perc2$maxred <- perc2$Redwood+(perc3$Redwood)
plot = plot +geom_ribbon(data = perc2, aes(ymin=minred, ymax=maxred, fill = factor(model), colour = NA), alpha=0.3)

plot