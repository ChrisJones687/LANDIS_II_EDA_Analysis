#library(arcgisbinding)
library(sp)
library(raster)
library(rgdal)
library(dismo)
#arc.check_product()

# Make a list of directories that can be looped over that can be looped over
dirs = list.dirs("G://EDA_fixed_Calibration", full.names = TRUE)
dirs = dirs[seq(4,(length(dirs)-7),10)]
file = list.files(path= dirs, pattern="eda-log.csv", full.names = TRUE)
files = lapply(file,read.csv)
file2 = substr(file, 27, 61)
file3 = (substr(file2, 1, 35))
TR <- as.numeric(substr(file3,3, 7))
AT <- as.numeric(substr(file3,11, 15))
Alpha <- as.numeric(substr(file3,32, 35))
Alpha2 <- as.numeric(substr(file3,32, 34))
for (i in 1:length(Alpha)) {
  if (is.na(Alpha[i])){
    Alpha[i] <- Alpha2[i]
    
  }
}
Sens <- data.frame (TR, AT, Alpha)

files2 <- lapply(files, function(x){x[c(1,3:7)]})
files3 <- lapply(files2, function(x){as.data.frame(lapply(x, as.numeric))})


filesSum <- sapply(files3, colSums)
filesSum <- as.data.frame(filesSum)
filesSum2 <- transpose(filesSum)
names(filesSum2) <- c('Time', 'InfectedSites', 'DiseasedSites', 'DamagedSites', 'TotalCohortsKilled', 'CohortsMortSppKilled')

files4 <- lapply(files3, function(x){x[c(1:2)]})

filesPerc <- files4
for (i in 1:21){
  filesPerc[[i]] <- files4[[i]][2:23,]
  for (j in 2:23){
    filesPerc[[i]][j,]<- files4[[i]][j,]/(files4[[i]][j,]+files4[[i]][j-1,])*100
  }
}


sens2 <- cbind(Sens, filesSum2$InfectedSites,oddR[,c(4,5,11:13)])
sens2$oddsratio <- (sens2$pospos*sens2$negneg2)/(sens2$posneg*sens2$negpos2)
sens2$AT[5:7] <- 0.40
names(sens2)[4] <- 'TotalInfectedSites'


Sens3 <- sens2[sens2$AT==0.40,]
Sens3 <- Sens3[order(Sens3$Alpha),]

write.csv(Sens3, "C:\\Users\\Chris\\Desktop\\sens.csv")

dlist <- lapply (dlist, function(x){x$Numfires <-x$TotalSitesInEvent*900;x})

