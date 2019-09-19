###############   S E T   U P   W O R K S P A C E   #################


rm(list=ls(all=TRUE))

#set working directory
setwd("/Volumes/Seagate Backup Plus Drive/ALL_RWL")
getwd()

#download necessary packages
library(dplR)
library(graphics)
library(stats)
library(utils)
library(tidyverse)
library(purrr)
library(bootRes)
library(zoo)
library(cowplot)

#upload monthly climate variables
axclim<-read.csv("ax_clim.csv",header=T)
axclim[is.na(axclim)] = "NA"
axclim2<- data.frame(na.approx(axclim))

f1 <-dcc(axcc.crn, axclim2, method = "response", start = 1, end =11, sb = TRUE, ci = 0.1, boot = FALSE,vnames =
             c("", "")) 

fert <- dcplot(f1, ci = FALSE, sig = TRUE, vertical = FALSE)

c1 <- dcc(axcc.crn, axclim2, method = "response", start = 1, end =11, ci = 0.1, boot = TRUE,vnames =
            c("", "")) 

cass <- dcplot(c1, ci = FALSE, sig = TRUE, vertical = FALSE)

d1 <- dcc(axdc.crn, axclim2, method = "response", start = 1, end =11, ci = 0.1, boot = TRUE,vnames =
            c("", "")) 

dry <- dcplot(d1, ci = FALSE, sig = TRUE, vertical = FALSE)

plot_grid(cass,dry,fert)

############### ALEX FIORD CASS CONTROL P ################
# import xls files

axcc <-read.rwl('AXCC.csv',format =c("csv"))


#generate rwl stats
rwl.report(axcc) #basic stats axccp
plot(axcc, plot.type="spag") #spaghetti plot
rwl.stats(axcc)
summary(axcc)

#detrend & standardise series to produce ring width index values
axcc_rwi<- detrend(rwl = axcc, method = "Mean")

#calculate overall interseries correlation
axcc.rho <- interseries.cor(axcc, prewhiten=TRUE,
                            method="spearman")
mean(axcc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axcc.crn <- chron(axcc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axcc.crn, add.spline=TRUE, nyrs=20)


############### ALEX FIORD CASS OTC P ################
#import xls files

axct <-read.rwl('AXCT.csv',format =c("csv"))

#generate rwl stats
rwl.report(axct) #basic stats axccp
plot(axct, plot.type="spag") #spaghetti plot
rwl.stats(axct)
summary(axct)

#detrend & standardise series to produce ring width index values
axct_rwi<- detrend(rwl = axct, method = "Mean")

#calculate overall interseries correlation
axct.rho <- interseries.cor(axct, prewhiten=TRUE,
                             method="spearman")
mean(axct.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axct.crn <- chron(axct_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axct.crn, add.spline=TRUE, nyrs=20)

############### ALEX FIORD DRYAS CONTROL P ################
#import xls files

axdc <-read.rwl('AXDC.csv',format =c("csv"))

#generate rwl stats
rwl.report(axdc) #basic stats axccp
plot(axdc, plot.type="spag") #spaghetti plot
rwl.stats(axdc)
summary(axdc)

#detrend & standardise series to produce ring width index values
axdc_rwi<- detrend(rwl = axdc, method = "Mean")

#calculate overall interseries correlation
axdc.rho <- interseries.cor(axdc, prewhiten=TRUE,
                            method="spearman")
mean(axdc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axdc.crn <- chron(axdc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axdc.crn, add.spline=TRUE, nyrs=20)

############### ALEX FIORD DRYAS OTC P ################
#import xls files

axdt <-read.rwl('AXDT.csv',format =c("csv"))

#generate rwl stats
rwl.report(axdt) #basic stats axccp
plot(axdt, plot.type="spag") #spaghetti plot
rwl.stats(axdt)
summary(axdt)

#detrend & standardise series to produce ring width index values
axdt_rwi<- detrend(rwl = axdt, method = "Mean")

#calculate overall interseries correlation
axdt.rho <- interseries.cor(axdt, prewhiten=TRUE,
                            method="spearman")
mean(axdt.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axdt.crn <- chron(axdt_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axdt.crn, add.spline=TRUE, nyrs=20)

############### ALEX FIORD FERT CONTROL P ################
#import xls files

axfc <-read.rwl('AXFC.csv',format =c("csv"))

#generate rwl stats
rwl.report(axfc) #basic stats axccp
plot(axfc, plot.type="spag") #spaghetti plot
rwl.stats(axfc)
summary(axfc)

#detrend & standardise series to produce ring width index values
axfc_rwi<- detrend(rwl = axfc, method = "Mean")

#calculate overall interseries correlation
axfc.rho <- interseries.cor(axfc, prewhiten=TRUE,
                            method="spearman")
mean(axfc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axfc.crn <- chron(axfc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axfc.crn, add.spline=TRUE, nyrs=20)

############### ALEX FIORD FERT OTC P ################
#import xls files

axft <-read.rwl('AXFT.csv',format =c("csv"))

#generate rwl stats
rwl.report(axft) #basic stats axccp
plot(axft, plot.type="spag") #spaghetti plot
rwl.stats(axft)
summary(axft)

#detrend & standardise series to produce ring width index values
axft_rwi<- detrend(rwl = axft, method = "Mean")

#calculate overall interseries correlation
axft.rho <- interseries.cor(axft, prewhiten=TRUE,
                            method="spearman")
mean(axft.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axft.crn <- chron(axft_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axft.crn, add.spline=TRUE, nyrs=20)

############### ALEX FIORD MIG OTC P ################
#import xls files

axmt <-read.rwl('AXMTP.csv',format =c("csv"))

#generate rwl stats
rwl.report(axmt) #basic stats axccp
plot(axmt, plot.type="spag") #spaghetti plot
rwl.stats(axmt)
summary(axmt)

#detrend & standardise series to produce ring width index values
axmt_rwi<- detrend(rwl = axmt, method = "Mean")

#calculate overall interseries correlation
axmt.rho <- interseries.cor(axmt, prewhiten=TRUE,
                            method="spearman")
mean(axmt.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axmt.crn <- chron(axmt, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axmt.crn, add.spline=TRUE, nyrs=20)

############### ALEX FIORD MIG C P ################
#import xls files

axmc <-read.rwl('AXMCP.csv',format =c("csv"))

#generate rwl stats
rwl.report(axmc) #basic stats axccp
plot(axmc, plot.type="spag") #spaghetti plot
rwl.stats(axmc)
summary(axmc)

#detrend & standardise series to produce ring width index values
axmc_rwi<- detrend(rwl = axmc, method = "Mean")

#calculate overall interseries correlation
axmc.rho <- interseries.cor(axmc, prewhiten=TRUE,
                            method="spearman")
mean(axmc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axmc.crn <- chron(axmc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axmc.crn, add.spline=TRUE, nyrs=20)

############### ALEX FIORD NX OTC P ################
#import xls files

axnt <-read.rwl('AXNTP.csv',format =c("csv"))

#generate rwl stats
rwl.report(axnt) #basic stats axccp
plot(axnt, plot.type="spag") #spaghetti plot
rwl.stats(axnt)
summary(axnt)

#detrend & standardise series to produce ring width index values
axnt_rwi<- detrend(rwl = axnt, method = "Mean")

#calculate overall interseries correlation
axnt.rho <- interseries.cor(axnt, prewhiten=TRUE,
                            method="spearman")
mean(axnt.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axnt.crn <- chron(axnt_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axnt.crn, add.spline=TRUE, nyrs=20)

############### ALEX FIORD NX C P ################
#import xls files

axnc <-read.rwl('AXNCP.csv',format =c("csv"))

#generate rwl stats
rwl.report(axnc) #basic stats axccp
plot(axnc, plot.type="spag") #spaghetti plot
rwl.stats(axnc)
summary(axnc)

#detrend & standardise series to produce ring width index values
axnc_rwi<- detrend(rwl = axnc, method = "Mean")

#calculate overall interseries correlation
axnc.rho <- interseries.cor(axnc, prewhiten=TRUE,
                            method="spearman")
mean(axnc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axnc.crn <- chron(axnc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(axnc.crn, add.spline=TRUE, nyrs=20)

############### LATNA OTC P ################
#import xls files

latt <-read.rwl('LATNATP.csv',format =c("csv"))

#generate rwl stats
rwl.report(latt) #basic stats axccp
plot(latt, plot.type="spag") #spaghetti plot
rwl.stats(latt)
summary(latt)

#detrend & standardise series to produce ring width index values
latt_rwi<- detrend(rwl = latt, method = "Mean")

#calculate overall interseries correlation
latt.rho <- interseries.cor(latt, prewhiten=TRUE,
                            method="spearman")
mean(latt.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
latt.crn <- chron(latt_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(latt.crn, add.spline=TRUE, nyrs=20)

############### LATNA C P ################
#import xls files

latc <-read.rwl('LATNACP.csv',format =c("csv"))

#generate rwl stats
rwl.report(latc) #basic stats axccp
plot(latc, plot.type="spag") #spaghetti plot
rwl.stats(latc)
summary(latc)

#detrend & standardise series to produce ring width index values
latc_rwi<- detrend(rwl = latc, method = "Mean")

#calculate overall interseries correlation
latc.rho <- interseries.cor(latc, prewhiten=TRUE,
                            method="spearman")
mean(latc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
latc.crn <- chron(latc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(latc.crn, add.spline=TRUE, nyrs=20)


############### ABISKO ################
#import xls files

abi <-read.rwl('ABISKOP.csv',format =c("csv"))

#generate rwl stats
rwl.report(abi) #basic stats axccp
plot(abi, plot.type="spag") #spaghetti plot
rwl.stats(abi)
summary(abi)

#detrend & standardise series to produce ring width index values
abi_rwi<- detrend(rwl = abi, method = "Mean")

#calculate overall interseries correlation
abi.rho <- interseries.cor(abi, prewhiten=TRUE,
                           method="spearman")
mean(abi.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
abi.crn <- chron(abi_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(abi.crn, add.spline=TRUE, nyrs=20)

############### ARCTIC STATION ################
#import xls files

arc <-read.rwl('ARCP.csv',format =c("csv"))

#generate rwl stats
rwl.report(arc) #basic stats axccp
plot(arc, plot.type="spag") #spaghetti plot
rwl.stats(arc)
summary(arc)

#detrend & standardise series to produce ring width index values
arc_rwi<- detrend(rwl = arc, method = "Mean")

#calculate overall interseries correlation
arc.rho <- interseries.cor(arc, prewhiten=TRUE,
                           method="spearman")
mean(arc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
arc.crn <- chron(arc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(arc.crn, add.spline=TRUE, nyrs=20)

############### AWS ################
#import xls files

aws <-read.rwl('AWSP.csv',format =c("csv"))

#generate rwl stats
rwl.report(aws) #basic stats axccp
plot(aws, plot.type="spag") #spaghetti plot
rwl.stats(aws)
summary(aws)

#detrend & standardise series to produce ring width index values
aws_rwi<- detrend(rwl = aws, method = "Mean")

#calculate overall interseries correlation
aws.rho <- interseries.cor(aws, prewhiten=TRUE,
                           method="spearman")
mean(aws.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
aws.crn <- chron(aws_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(aws.crn, add.spline=TRUE, nyrs=20)

############### ELZA ################
#import xls files

elz <-read.rwl('ELZP.csv',format =c("csv"))

#generate rwl stats
rwl.report(elz) #basic stats axccp
plot(elz, plot.type="spag") #spaghetti plot
rwl.stats(elz)
summary(elz)

#detrend & standardise series to produce ring width index values
elz_rwi<- detrend(rwl = elz, method = "Mean")

#calculate overall interseries correlation
elz.rho <- interseries.cor(elz, prewhiten=TRUE,
                           method="spearman")
mean(elz.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
elz.crn <- chron(elz_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(elz.crn, add.spline=TRUE, nyrs=20)


############### ZA PYR ################
#import xls files

zap <-read.rwl('ZAPYP.csv',format =c("csv"))

#generate rwl stats
rwl.report(zap) #basic stats axccp
plot(zap, plot.type="spag") #spaghetti plot
rwl.stats(zap)
summary(zap)

#detrend & standardise series to produce ring width index values
zap_rwi<- detrend(rwl = zap, method = "Mean")

#calculate overall interseries correlation
zap.rho <- interseries.cor(zap, prewhiten=TRUE,
                           method="spearman")
mean(zap.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
zap.crn <- chron(zap_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(zap.crn, add.spline=TRUE, nyrs=20)

############### SKANSKABUKTA ################
#import xls files

ska <-read.rwl('SKANP.csv',format =c("csv"))

#generate rwl stats
rwl.report(ska) #basic stats axccp
plot(ska, plot.type="spag") #spaghetti plot
rwl.stats(ska)
summary(ska)

#detrend & standardise series to produce ring width index values
ska_rwi<- detrend(rwl = ska, method = "Mean")

#calculate overall interseries correlation
ska.rho <- interseries.cor(ska, prewhiten=TRUE,
                           method="spearman")
mean(ska.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
ska.crn <- chron(ska_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(ska.crn, add.spline=TRUE, nyrs=20)

############### DISKO ################
#import xls files

dis <-read.rwl('DISKOP.csv',format =c("csv"))

#generate rwl stats
rwl.report(dis) #basic stats axccp
plot(dis, plot.type="spag") #spaghetti plot
rwl.stats(dis)
summary(dis)

#detrend & standardise series to produce ring width index values
dis_rwi<- detrend(rwl = dis, method = "Mean")

#calculate overall interseries correlation
dis.rho <- interseries.cor(dis, prewhiten=TRUE,
                           method="spearman")
mean(dis.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
dis.crn <- chron(dis_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(dis.crn, add.spline=TRUE, nyrs=20)

############### SAANA ################
#import xls files

saa <-read.rwl('SAANAP.csv',format =c("csv"))

#generate rwl stats
rwl.report(saa) #basic stats axccp
plot(saa, plot.type="spag") #spaghetti plot
rwl.stats(saa)
summary(saa)

#detrend & standardise series to produce ring width index values
saa_rwi<- detrend(rwl = saa, method = "Mean")

#calculate overall interseries correlation
saa.rho <- interseries.cor(saa, prewhiten=TRUE,
                           method="spearman")
mean(saa.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
saa.crn <- chron(saa_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(saa.crn, add.spline=TRUE, nyrs=20)


############### BARROW ################
#import xls files

bwc <-read.rwl('BWTP.csv',format =c("csv"))

#generate rwl stats
rwl.report(bwc) #basic stats axccp
plot(bwc, plot.type="spag") #spaghetti plot
rwl.stats(bwc)
summary(bwc)

#detrend & standardise series to produce ring width index values
bwc_rwi<- detrend(rwl = bwc, method = "Mean")

#calculate overall interseries correlation
bwc.rho <- interseries.cor(bwc, prewhiten=TRUE,
                           method="spearman")
mean(bwc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
bwc.crn <- chron(bwc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean
bwc.crn2 <- chron(bwc, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(bwc.crn, add.spline=TRUE, nyrs=20)

############### Imnavait ################
#import xls files

imc <-read.rwl('IMCB.csv',format =c("csv"))

#generate rwl stats
rwl.report(imc) #basic stats axccp
plot(imc, plot.type="spag") #spaghetti plot
rwl.stats(imc)
summary(imc)

#detrend & standardise series to produce ring width index values
imc_rwi<- detrend(rwl = imc, method = "Mean")

#calculate overall interseries correlation
imc.rho <- interseries.cor(imc, prewhiten=TRUE,
                           method="spearman")
mean(imc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
imc.crn <- chron(imc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean
imc.crn2 <- chron(imc, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(imc.crn, add.spline=TRUE, nyrs=20)

############### TOOLIK ################
#import xls files

tkc <-read.rwl('TKTP.csv',format =c("csv"))

#generate rwl stats
rwl.report(tkc) #basic stats axccp
plot(tkc, plot.type="spag") #spaghetti plot
rwl.stats(tkc)
summary(tkc)

#detrend & standardise series to produce ring width index values
tkc_rwi<- detrend(rwl = tkc, method = "Mean")

#calculate overall interseries correlation
tkc.rho <- interseries.cor(tkc, prewhiten=TRUE,
                           method="spearman")
mean(tkc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
tkc.crn <- chron(tkc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean
tkc.crn2 <- chron(tkc, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

#plot chronology w spline
par(mfrow=c(1,1),cex=0.6)
plot(tkc.crn, add.spline=TRUE, nyrs=20)

############### ATKASUQ ################
#import xls files

atc <-read.rwl('ATTP.csv',format =c("csv"))

#generate rwl stats
rwl.report(atc) #basic stats axccp
plot(atc, plot.type="spag") #spaghetti plot
rwl.stats(atc)
summary(atc)

#detrend & standardise series to produce ring width index values
atc_rwi<- detrend(rwl = atc, method = "Mean")

#calculate overall interseries correlation
tkc.rho <- interseries.cor(tkc, prewhiten=TRUE,
                           method="spearman")
mean(tkc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
att.crn <- chron(atc_rwi, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean
att.crn2 <- chron(atc, prefix = "CAM", biweight = FALSE) #Tukey's biweigh robust mean

###alex rename year columns###
att.crn <- data.frame("year"=rownames(att.crn), att.crn)
att.crn2 <- data.frame("year"=rownames(att.crn2), att.crn2)
imc.crn <- data.frame("year"=rownames(imc.crn), imc.crn)
imc.crn2 <- data.frame("year"=rownames(imc.crn2), imc.crn2)
tkc.crn <- data.frame("year"=rownames(tkc.crn), tkc.crn)
tkc.crn2 <- data.frame("year"=rownames(tkc.crn2), tkc.crn2)
bwc.crn <- data.frame("year"=rownames(bwc.crn), bwc.crn)
bwc.crn2 <- data.frame("year"=rownames(bwc.crn2), bwc.crn2)   

ajoin  <- full_join(att.crn,imc.crn,by = "year")
bjoin  <- full_join(tkc.crn,bwc.crn,by = "year")
abjoin  <- full_join(ajoin,bjoin,by = "year")

