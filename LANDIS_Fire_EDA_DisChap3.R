# Create Directory and list of files with all log files from the model runs with fire and eda
dirs = list.dirs("G://DissertationFinalModelRuns//Fire and EDA", full.names = TRUE)
dirs = dirs[seq(5,(length(dirs)-8),12)]
files = list.files(path= dirs, pattern="log.csv", full.names = TRUE)
files = files[seq(1,(length(files)-1),2)]

# Create Directory and list of files with all log files from the model runs with fire only
dirs2 = list.dirs("G://DissertationFinalModelRuns//Fire//", full.names = TRUE)
dirs2 = dirs2[seq(4,(length(dirs2)-8),11)]
files2 = list.files(path= dirs2, pattern="log.csv", full.names = TRUE)
files2 = files2[seq(1,(length(files2)-1),2)]

# Read in data frames into a lists for looping
dlist <-lapply(files, read.csv) #List for storing data frames from fire and eda model runs
dlist2 <-lapply(files2, read.csv) #List for storing data frames from fire only model runs

## Data setup for making boxplot and statistical comparisons 
# Data setup for Fire and EDA model runs
dlist <- lapply (dlist, function(x){x$Numfires <-x$TotalSitesInEvent*900;x}) # Add Fire size to each event by multiplying number of pixels by size of pixel (m^2)
dlist <- lapply (dlist, function(x){x <-x[x$Numfires>10000,];x}) # ignore fires that are smaller than 10000 m^2 because that is what the forest service does
Fire <- lapply(dlist, function(x){data.frame(nrow(x), min(x$Numfires),max(x$Numfires), mean(x$Numfires), median(x$Numfires), sd(x$Numfires),mean(x$MeanSeverity), mean(x$CohortsKilled))})
Fires <- do.call(what=rbind, args = Fire)
names(Fires) <- c("Numfires","Minsize","Maxsize","Avgsize","Mediansize","stdsize","meansev","numcohortskilled")
Fires$Model <- "Fire and SOD"

# Data setup for Fire and EDA model runs
dlist2 <- lapply (dlist2, function(x){x$Numfires <-x$TotalSitesInEvent*900;x}) # Add Fire size to each event by multiplying number of pixels by size of pixel (m^2)
dlist2 <- lapply (dlist2, function(x){x <-x[x$Numfires>10000,];x}) # ignore fires that are smaller than 10000 m^2 because that is what the forest service does
Fire2 <- lapply(dlist2, function(x){data.frame(nrow(x), min(x$Numfires),max(x$Numfires), mean(x$Numfires), median(x$Numfires), sd(x$Numfires),mean(x$MeanSeverity), mean(x$CohortsKilled))})
Fires2 <- do.call(what=rbind, args = Fire2)
names(Fires2) <- c("Numfires","Minsize","Maxsize","Avgsize","Mediansize","stdsize","meansev","numcohortskilled")
Fires2$Model <- "Fire"

FireG <- rbind(Fires, Fires2)
FireG = FireG[,c(1,4,7,8,9)]
names(FireG) = c('# Fires', 'Avg Fire Size (acres)','Mean Fire Severity', 'Avg # Cohorts Killed', 'Model')
FireG$`Avg Fire Size (acres)` = FireG$`Avg Fire Size (acres)`*0.000247105
library(reshape2)
fireg = melt(FireG, id=c('Model')) #used to shift columns to row by another column while keeping the original data and moving column names to a new column with new name (useful for graphing purposes)

# Making attractive boxplots for displaying differences in fire regime
library(ggplot2)
title = "Fire Regime Comparision"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())
boxplot = ggplot(fireg, mapping = aes_string(y="value", x="Model")) + ggtitle(title)
boxplot = boxplot + geom_boxplot(outlier.colour = NULL, aes_string(colour="Model", fill="Model")) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=mean(x), ymin=mean(x), ymax=mean(x))) })
theme = theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x=element_blank())
theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.y = element_text(colour="black"), axis.ticks.y= element_line(colour="grey"), plot.title = element_text(hjust = 0.5))
boxplot = boxplot + facet_wrap(~ variable, nrow = 1, scales="free")
boxplot = boxplot + scale_color_manual(values=c("#CC0000","#7BAFD4"))+scale_fill_manual(values=c("#CC0000","#7BAFD4"))
boxplot = boxplot + theme(axis.text=element_text(size=10),axis.title=element_text(size=24),legend.text=element_text(size=12),plot.title=element_text(size=22))
boxplot


# Climate Change impacts
for (i in 1:30){
  dlist[[i]]$sim <- i
  dlist2[[i]]$sim <-i
}

d <- do.call("rbind", dlist)
d2 <- do.call("rbind", dlist2)

d3 <- d[, c(1,18,21,22)]
d4 <- d2[, c(1,18,21,22)]

cc <- d3
cc2000 <- cc[cc$Time %in% 1:10,]
cc2010 <- cc[cc$Time %in% 11:20,]
cc2020 <- cc[cc$Time %in% 21:30,]
cc2030 <- cc[cc$Time %in% 31:40,]
cc2040 <- cc[cc$Time %in% 41:50,]
cc2050 <- cc[cc$Time %in% 51:60,]
cc2060 <- cc[cc$Time %in% 61:70,]
cc2070 <- cc[cc$Time %in% 71:80,]
cc2080 <- cc[cc$Time %in% 81:90,]
cc2090 <- cc[cc$Time %in% 91:100,]

cc2000$year = 2000
cc2010$year = 2010
cc2020$year = 2020
cc2030$year = 2030
cc2040$year = 2040
cc2050$year = 2050
cc2060$year = 2060
cc2070$year = 2070
cc2080$year = 2080
cc2090$year = 2090

cc2 <- rbind(cc2000, cc2010, cc2020, cc2030, cc2040, cc2050, cc2060, cc2070, cc2080, cc2090)
cc3 <- cc2[,4:5]
cc3 <- as.data.frame(table(cc3))


cc <- d4
cc2000 <- cc[cc$Time %in% 1:10,]
cc2010 <- cc[cc$Time %in% 11:20,]
cc2020 <- cc[cc$Time %in% 21:30,]
cc2030 <- cc[cc$Time %in% 31:40,]
cc2040 <- cc[cc$Time %in% 41:50,]
cc2050 <- cc[cc$Time %in% 51:60,]
cc2060 <- cc[cc$Time %in% 61:70,]
cc2070 <- cc[cc$Time %in% 71:80,]
cc2080 <- cc[cc$Time %in% 81:90,]
cc2090 <- cc[cc$Time %in% 91:100,]

cc2000$year = 2000
cc2010$year = 2010
cc2020$year = 2020
cc2030$year = 2030
cc2040$year = 2040
cc2050$year = 2050
cc2060$year = 2060
cc2070$year = 2070
cc2080$year = 2080
cc2090$year = 2090

cc4 <- rbind(cc2000, cc2010, cc2020, cc2030, cc2040, cc2050, cc2060, cc2070, cc2080, cc2090)
cc5 <- cc4[,4:5]
cc5 <- as.data.frame(table(cc5))

cc3$model = "Fire and SOD"
cc5$model = "Fire"

cc3$Freq[cc3$year==2010] = cc3$Freq[cc3$year==2010]+2
cc3$Freq[cc3$year==2000] = cc3$Freq[cc3$year==2000]+2
cc3$Freq[cc3$year %in% c("2020", "2030","2040","2050","2060","2070","2080","2090")] = cc3$Freq[cc3$year %in% c("2020", "2030","2040","2050","2060","2070","2080","2090")]-0.5

cc5$Freq[cc5$year==2010] = cc5$Freq[cc5$year==2010]+2
cc5$Freq[cc5$year==2000] = cc5$Freq[cc5$year==2000]+2
cc5$Freq[cc5$year %in% c("2020", "2030","2040","2050","2060","2070","2080","2090")] = cc5$Freq[cc5$year %in% c("2020", "2030","2040","2050","2060","2070","2080","2090")]-0.5

cc3[,3] <- as.numeric(cc3[,3])

c <- aggregate(.~year, data=cc3[,2:3], mean)
c2 <- aggregate(.~year, data=cc3[,2:3], sd)
c$min <- c$Freq-c2$Freq
c$max <- c$Freq+c2$Freq
c$model = "Fire and SOD"

c3 <- aggregate(.~year, data=cc5[,2:3], mean)
c4 <- aggregate(.~year, data=cc5[,2:3], sd)
c3$min <- c3$Freq-c4$Freq
c3$max <- c3$Freq+c4$Freq
c3$model = "Fire"

c5 <- rbind(c, c3)
c5$year = c(2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090,2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090)

title = "Fire Severity by Disease Stage"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())
boxplot = ggplot(cc3, mapping = aes_string(y="Freq", x="year"), outline=FALSE, notch=TRUE) + ggtitle(title)
boxplot = boxplot + geom_boxplot(outlier.colour = NULL,outlier.size = 0,outlier.stroke = 0, aes_string(colour="year", fill="year")) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=mean(x), ymin=mean(x), ymax=mean(x))) })
theme = theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x=element_blank())
theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.y = element_text(colour="black"), axis.ticks.y= element_line(colour="grey"), plot.title = element_text(hjust = 0.5))
#boxplot = boxplot + facet_wrap(~ variable, nrow = 1, scales="free")
#boxplot = boxplot + scale_color_manual(values=c("#26648B","#5CACEE", "#9FB6CD", "#C6E2FF"))+scale_fill_manual(values=c("#26648B","#5CACEE","#9FB6CD", "#C6E2FF"))
boxplot = boxplot + theme(axis.text=element_text(size=10),axis.title=element_text(size=24),legend.text=element_text(size=12),plot.title=element_text(size=22))
boxplot = boxplot + labs(c("Infected","Mortality > 3", "Diseased/Mortality < 3", "Uninfected"))
boxplot


title = "Number of fires over time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"))
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5))
plot2 = ggplot(c5, aes(year, Freq),color=factor(model))+geom_line(aes(color = factor(model)), size=1.2)
plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4"))+scale_fill_manual(values=c("#CC0000","#7BAFD4"))
plot2 = plot2+ggtitle(title)
plot2 = plot2 + theme(axis.text=element_text(size=10),axis.title=element_text(size=16, vjust=0,35),legend.text=element_text(size=10),plot.title=element_text(size=22))
#plot2 = plot2 + scale_x_continuous(name="Date", breaks=seq(2000,2090,10))
plot2 = plot2 + scale_y_continuous(name=expression("Number of Fires"))+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))

plot2 = plot2 +geom_ribbon(data = c5, aes(ymin=min, ymax=max, fill = factor(model)), alpha=0.3)#, fill=c("#0006A8", "#A943FF", "#EA0000","#6BBA7F","#EC9322"), color=c("#0006A8", "#A943FF", "#EA0000","#6BBA7F","#EC9322"))#+geom_line(color=c("#0006A8", "#A943FF", "#EA0000","#6BBA7F","#EC9322"), lwd=1)

plot2

s <- cc2[,c(2,3,5)]
s2 <- cc4[,c(2,3,5)]

s3 <- aggregate(s, by = list(s$year), FUN=mean, na.rm=TRUE)
s4 <- aggregate(s, by = list(s$year), FUN=sd, na.rm=TRUE)
s3$msmin <- s3$MeanSeverity-s4$MeanSeverity
s3$msmax <- s3$MeanSeverity+s4$MeanSeverity
s3$smin <- s3$Numfires-s4$Numfires/3
s3$smax <- s3$Numfires+s4$Numfires/3
s3$model = "Fire and SOD"

s5 <- aggregate(s2, by = list(s2$year), FUN=mean, na.rm=TRUE)
s6 <- aggregate(s2, by = list(s2$year), FUN=sd, na.rm=TRUE)
s5$msmin <- s5$MeanSeverity-s6$MeanSeverity
s5$msmax <- s5$MeanSeverity+s6$MeanSeverity
s5$smin <- s5$Numfires-s6$Numfires/3
s5$smax <- s5$Numfires+s6$Numfires/3
s5$model = "Fire"

s6 <- rbind(s3, s5)
s6$year = c(2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090,2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090)
s6$Numfires = s6$Numfires/10000
s6$smin = s6$smin/10000
s6$smax = s6$smax/10000


title = "Fire Severity over time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"))
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5))
plot2 = ggplot(s6, aes(year, MeanSeverity),color=factor(model))+geom_line(aes(color = factor(model)), size=1.2)
plot2 = plot2+scale_color_manual(values=c("#CC0000","#7BAFD4"))+scale_fill_manual(values=c("#CC0000","#7BAFD4"))
plot2 = plot2+ggtitle(title)
plot2 = plot2 + theme(axis.text=element_text(size=10),axis.title=element_text(size=16, vjust=0,35),legend.text=element_text(size=10),plot.title=element_text(size=22))
#plot2 = plot2 + scale_x_continuous(name="Date", breaks=seq(2000,2090,10))
plot2 = plot2 + scale_y_continuous(name=expression("Number of Fires"))+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))

plot2 = plot2 +geom_ribbon(data = s6, aes(ymin=msmin, ymax=msmax, fill = factor(model)), alpha=0.3)#, fill=c("#0006A8", "#A943FF", "#EA0000","#6BBA7F","#EC9322"), color=c("#0006A8", "#A943FF", "#EA0000","#6BBA7F","#EC9322"))#+geom_line(color=c("#0006A8", "#A943FF", "#EA0000","#6BBA7F","#EC9322"), lwd=1)

plot2


title = "Fire Severity over time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"))
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5))
plot3 = ggplot(s6, aes(year, Numfires),color=factor(model))+geom_line(aes(color = factor(model)), size=1.2)
plot3 = plot3+scale_color_manual(values=c("#CC0000","#7BAFD4"))+scale_fill_manual(values=c("#CC0000","#7BAFD4"))
plot3 = plot3+ggtitle(title)
plot3 = plot3 + theme(axis.text=element_text(size=10),axis.title=element_text(size=16, vjust=0,35),legend.text=element_text(size=10),plot.title=element_text(size=22))
#plot2 = plot2 + scale_x_continuous(name="Date", breaks=seq(2000,2090,10))
plot3 = plot3 + scale_y_continuous(name=expression("Number of Fires"))+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))

plot3 = plot3 +geom_ribbon(data = s6, aes(ymin=smin, ymax=smax, fill = factor(model)), alpha=0.3)#, fill=c("#0006A8", "#A943FF", "#EA0000","#6BBA7F","#EC9322"), color=c("#0006A8", "#A943FF", "#EA0000","#6BBA7F","#EC9322"))#+geom_line(color=c("#0006A8", "#A943FF", "#EA0000","#6BBA7F","#EC9322"), lwd=1)

plot3