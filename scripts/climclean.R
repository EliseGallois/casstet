###############   S E T   U P   W O R K S P A C E   #################


rm(list=ls(all=TRUE))

#set working directory
setwd("/Volumes/Seagate Backup Plus Drive/")


library(dplR)
library(dplyr)
library(graphics)
library(stats)
library(utils)
library(tidyverse)
library(purrr)


############### U P L O A D     S I T E ###################
abi<-read.csv("abi.csv", header=T)
saa<-read.csv("saanat.csv", header=T)
dsk<-read.csv("dskt.csv", header=T)
arc<-read.csv("arct.csv", header=T)
lfs<-read.csv("lfst.csv", header=T)
tkt<-read.csv("tkt.csv", header=T)
ska<-read.csv("skat.csv", header=T)
atk<-read.csv("att.csv", header=T)
#get july averages only
abi <- filter(abi, m == "7")
saa <- filter(saa, m == "7")
dsk <- filter(dsk, m == "7")
arc <- filter(arc, m == "7")
lfs <- filter(lfs, m == "7")
tkt <- filter(tkt, m == "7")
ska <- filter(ska, m == "7")
atk <- filter(atk, m == "7")
#cut to 1962-2018
abi <- abi %>% filter(year>=1962&year<=2018)
saa <- saa %>% filter(year>=1962&year<=2018)
dsk <- dsk %>% filter(year>=1962&year<=2018)
arc <- arc %>% filter(year>=1962&year<=2018)
lfs <- lfs %>% filter(year>=1962&year<=2018)
tkt <- tkt %>% filter(year>=1962&year<=2018)
ska <- ska %>% filter(year>=1962&year<=2018)
atk <- atk %>% filter(year>=1962&year<=2018)
#remove month column
abi <- abi[,-2]
saa <- saa[,-2]
dsk <- dsk[,-2]
arc <- arc[,-2]
lfs <- lfs[,-2]
tkt <- tkt[,-2]
ska <- ska[,-2]
atk <- atk[,-2]

master <- Reduce(function(x,y) merge(x,y,by="year",all=TRUE) ,list(abi,saa,dsk,arc,lfs,tkt,ska,atk))





###############   U P L O A D    M E A D O W  #################

#upload alex fiord daily temp, precip and snow depth
clim<-read.csv("dailymeadow.csv", header=T)
moist<-read.csv("moisture.csv", header=T)
snow<-read.csv("snow.csv", header=T)

clim$monthyr <- format(as.Date(clim$date), "%m-%Y")


names(clim)  # print out the column (i.e., variable) names
dim(clim)    # number of rows and columns
head(clim)

#  Aggregate mxair on months and year and get mean
mxair <- aggregate(mxairT ~ monthyr , clim , mean )
#  Aggregate avair on months and year and get mean
avair <- aggregate(avairT ~ monthyr , clim , mean )
#  Aggregate avsurf on months and year and get mean
avsurf <- aggregate(avsurfT ~ monthyr , clim , mean )

#  Aggregate mxsoil on months and year and get mean
mxsoil <- aggregate(mxsoilT ~ monthyr , clim , mean )
#  Aggregate avsoil on months and year and get mean
avsoilT <- aggregate(avairT ~ monthyr , clim , mean )
#  Aggregate avsurf on months and year and get mean
avsurf <- aggregate(avsurfT ~ monthyr , clim , mean )
#  Aggregate adj snow on months and year and get mean
adj.snow <- aggregate(adj.snow ~ monthyr , clim , mean )



#upload cassiope site hobo averages
cass<-read.csv("casshobo.csv", header=T)
names(cass)  # print out the column (i.e., variable) names
dim(cass)    # number of rows and columns
head(cass)


cass$monthyr <- format(as.Date(cass$date), "%m-%Y")

#  Aggregate t-air on months and year and get mean
Tair <- aggregate(Tav ~ monthyr , cass , mean)
#  Aggregate c-air on months and year and get mean
Cair <- aggregate(Cav ~ monthyr , cass , mean)

plot(Tair$Tav, type = "l", col = 82)
points(Cair$Cav, type = "l", col = 12)


cjoin  <- full_join(Tair,Cair,avsurf,by = "monthyr")

#export masterchronology

write.table(cjoin, "/Volumes/Seagate Backup Plus Drive/monthvar.txt", sep="\t")


###############   M E R G E     D A T A F R A M E S   #################

cjoin  <- full_join(avsoilT,mxsoil,avsurf,by = "monthyr")

#export masterchronology

write.table(cjoin, "/Volumes/Seagate Backup Plus Drive/monthvar.txt", sep="\t")

############  U P L O A D     M O N T H L I E S ##############

#upload monthly alex fiord air temp averages and maximums
monthly <-read.csv("monthly.csv", header=T)

##### EXAMINE MONTHLIES ########
###############   U P L O A D    M E A D O W  #################


#upload alex fiord monthly temp
avair <-read.csv("avair.cs")

######### U P L O A D     S N O W & MOIST ##########
snow <-read.csv("snow.csv")
moist <-read.csv("moisture.csv")
#sep by sites & treatment
Fsnow <- snow %>% filter(Site %in% c("Fert"))
Fmoist <- moist %>% filter(Site %in% c("Fert"))
Dsnow <- snow %>% filter(Site %in% c("Dryas"))
Dmoist <- moist %>% filter(Site %in% c("Dryas"))
Csnow <- snow %>% filter(Site %in% c("Cassiope"))
Cmoist <- moist %>% filter(Site %in% c("Cassiope"))
FsnowC <- filter(Fsnow, Treatment == "C")
FsnowT <- filter(Fsnow, Treatment == "T")
FmoistC <- filter(Fmoist, T == "C")
FmoistT <- filter(Fmoist, T == "T")
DsnowC <- filter(Dsnow, Treatment == "C")
DsnowT <- filter(Dsnow, Treatment == "T")
DmoistC <- filter(Dmoist, T == "C")
DmoistT <- filter(Dmoist, T == "T")
CsnowC <- filter(Csnow, Treatment == "C")
CsnowT <- filter(Csnow, Treatment == "T")
CmoistC <- filter(Cmoist, T == "C")
CmoistT <- filter(Cmoist, T == "T")
#mean by year
FsnowC <- aggregate(FsnowC[, 6:7], list(FsnowC$Year), mean)
FsnowT <- aggregate(FsnowT[, 6:7], list(FsnowT$Year), mean)
DsnowC <- aggregate(DsnowC[, 6:7], list(DsnowC$Year), mean)
DsnowT <- aggregate(DsnowT[, 6:7], list(DsnowT$Year), mean)
CsnowC <- aggregate(CsnowC[, 6:7], list(CsnowC$Year), mean)
CsnowT <- aggregate(CsnowT[, 6:7], list(CsnowT$Year), mean)
ajoin  <- full_join(FsnowC,FsnowT,by = "Group.1")
bjoin  <- full_join(DsnowC,DsnowT,by = "Group.1")
cjoin  <- full_join(CsnowC,CsnowT,by = "Group.1")
djoin  <- full_join(ajoin,bjoin,by = "Group.1")
snowjoin  <- full_join(cjoin,djoin,by = "Group.1")


FmoistC <- aggregate(FmoistT[, 1:4], list(FmoistT$Year), mean)
FmoistT <- aggregate(FmoistT[, 1:4], list(FmoistT$Year), mean)
DmoistC <- aggregate(DmoistC[, 1:4], list(DmoistC$Year), mean)
DmoistT <- aggregate(DmoistT[, 1:4], list(DmoistT$Year), mean)
CmoistC <- aggregate(CmoistC[, 1:4], list(CmoistC$Year), mean)
CmoistT <- aggregate(CmoistT[, 1:4], list(CmoistT$Year), mean)
ajoin  <- full_join(FmoistC,FmoistT,by = "Group.1")
bjoin  <- full_join(DmoistC,DmoistT,by = "Group.1")
cjoin  <- full_join(CmoistC,CmoistT,by = "Group.1")
djoin  <- full_join(ajoin,bjoin,by = "Group.1")
moistjoin  <- full_join(cjoin,djoin,by = "Group.1")

