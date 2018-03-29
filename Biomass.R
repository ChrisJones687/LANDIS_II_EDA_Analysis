## Read in directories and files from those directories that are needed for analysis
# Read in from EDA only models
dirs = list.dirs("G://FinalModelRunsFixed", full.names = TRUE)
files = list.files(path= dirs, pattern="spp-biomass-log.csv", full.names = TRUE)
dlist <-lapply(files, read.csv)

edaMean <- as.data.frame(rbindlist(dlist)[,lapply(.SD,mean), list(Time, Ecoregion)])

bios = edaMean
bios$total <- rowSums(bios[,5:32])
bios$Model <- "SOD"
names(bios) <- c("Time", "Ecoregion", "EcoregionIndex", "Numsites", "BristleconeFir", "GrandFir","BigleafMaple", "CaliforniaBuckeye", "WhiteAlder", "RedAlder", "PacificMadron", "Tanoak", "KnobconePine", "CoulterPine", "SugarPine", "SingleleafPinyon", "PonnderosaPine", "MonereyPine", "GrayPine", "Sycamore", "BalsamPoplar", "BigconeDouglasFir", "CoastDouglasFir", "CoastLiveOak", "CanyonLiveOak", "BlueOak", "PinOak", "CaliforniaBlackOak", "OvercupOak", "InteriorLiveOak", "Redwood", "CaliforniaBayLaurel", "X", "total", "Model")
bios$Bay2Tanoak <- bios$CaliforniaBayLaurel/bios$Tanoak
bios$Bay2cloak  <- bios$CaliforniaBayLaurel/bios$CoastLiveOak
bios$Bay2cboak  <- bios$CaliforniaBayLaurel/bios$CaliforniaBlackOak
bios$Ecoregion2 = rep(c("None","water","Barren","Mixed Evergreen","Redwood","Mixed Conifer","Oak Conifer", "Southern California Coastal Scrub", "California Maritime Chaparral", "California Mesic Chaparral", "California Montane Woodland Savanna", "Northern and Central California Dry Mesic Chaparral","Oak Woodland", "Northern California Coastal Scrub", "California Central Valley and Sourthern Coastal Grassland", "California Northern Coastal Grassland", "Riparian", "California Coastal Closed Cone Conifer Forest and Woodland"), nrow(edaMean)/18)
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

eda <- edaMean
eda[is.na(eda)]<-0
eda <- aggregate(.~Time, data=eda, FUN=sum)
eda$total <- rowSums(eda[,5:32])
eda$model <- "SOD"

BioComp <- eda
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
# 
# BioComp$minB2T <- BioComp$Bay2Tanoak-(BioComp2$Bay2Tanoak/1.4)
# BioComp$maxB2T <- BioComp$Bay2Tanoak+(BioComp2$Bay2Tanoak/1.4)
# plot = plot +geom_ribbon(data = BioComp, aes(ymin=minB2T, ymax=maxB2T, fill = factor(Model), colour = NA), alpha=0.3)
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
# 
# #biosed$minB2T2 <- biosed$Bay2Tanoak-(biosed2$Bay2Tanoak/2.5)
# biosed$maxB2T2 <- biosed$Bay2Tanoak+(biosed2$Bay2Tanoak/2.5)
# plot2 = plot2 +geom_ribbon(data = biosed, aes(ymin=minB2T2, ymax=maxB2T2, fill = factor(Model), colour = NA), alpha=0.3)+annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

plot2
