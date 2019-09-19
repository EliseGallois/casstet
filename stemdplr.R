###############   S E T   U P   W O R K S P A C E   #################


rm(list=ls(all=TRUE))

#set working directory
setwd("/Volumes/Seagate Backup Plus Drive/ALL_RWL")

#download necessary packages
library(dplR)
library(graphics)
library(stats)
library(utils)
library(tidyverse)
library(purrr)
library(xlsx)


###############   C R E A T E    D A T A F R A M E S   #################

#i have the raw ring width files produced by cdendro which i need to standardise, detrend,
#apply a cubic smoothing spline and generate a series of master chronologies by site and treatment type

#open rwl file & examine properties
axcc <-read.rwl('AXCC1.rwl', format = c("tucson"))
dim(axcc) #42 years and 16 series
colnames(axcc) #series names
class(axcc) # note "rwl" class as well as "data.frame"

#ALEX FIORD CASSIOPE CONTROL 
plot()

#generate rwl stats
rwl.report(axcc) #basic stats
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
axcc.crn <- chron(axcc_rwi, prefix = "CAM",prewhiten=TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axcc.crn, add.spline=TRUE, nyrs=20)

#ALEX FIORD CASSIOPE TREATMENT

#open rwl file & examine properties
axct <-read.rwl('AXCT.rwl')

dim(axct) #42 years and 16 series
colnames(axct) #series names
class(axct) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(axct) #basic stats
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
axct.crn <- chron(axct_rwi, prefix = "CAM", biweight = TRUE, prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axct.crn, add.spline=TRUE, nyrs=20)


#ALEX FIORD DRYAS CONTROL

#open rwl file & examine properties
axdc <-read.rwl('AXDC.rwl')
dim(axdc) #42 years and 16 series
colnames(axdc) #series names
class(axdc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(axdc) #basic stats
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
axdc.crn <- chron(axdc_rwi, prefix = "CAM",prewhiten=TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axdc.crn, add.spline=TRUE, nyrs=20)

#ALEX FIORD DRYAS TREATMENT

#open rwl file & examine properties
axdt <-read.rwl('AXDT.rwl')
dim(axdt) #42 years and 16 series
colnames(axdct) #series names
class(axdt) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(axdt) #basic stats
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
axdt.crn <- chron(axdt_rwi, prefix = "CAM",prewhiten=TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axdt.crn, add.spline=TRUE, nyrs=20)

#ALEX FIORD FERT CONTROL

#open rwl file & examine properties
axfc <-read.rwl('AXFC.rwl')
dim(axfc) #42 years and 16 series
colnames(axfc) #series names
class(axfc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(axfc) #basic stats
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
axfc.crn <- chron(axfc_rwi, prefix = "CAM",prewhiten=TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axfc.crn, add.spline=TRUE, nyrs=20)

#ALEX FIORD FERT TREATMENT

#open rwl file & examine properties
axft <-read.rwl('AXFT.rwl')
dim(axft) #42 years and 16 series
colnames(axft) #series names
class(axft) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(axft) #basic stats
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
axft.crn <- chron(axft_rwi, prefix = "CAM",prewhiten=TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axft.crn, add.spline=TRUE, nyrs=20)

#ALEX FIORD MIGRATION CONTROL

#open rwl file & examine properties
axmc <-read.rwl('AXMC.rwl')
dim(axmc) #42 years and 16 series
colnames(axmc) #series names
class(axmc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(axmc) #basic stats
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
axmc.crn <- chron(axmc_rwi, prefix = "CAM",prewhiten=TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axmc.crn, add.spline=TRUE, nyrs=20)

#ALEX FIORD MIGRATION TREATMENT

#open rwl file & examine properties
axmt <-read.rwl('AXMT.rwl')
dim(axmt) #42 years and 16 series
colnames(axmt) #series names
class(axmt) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(axmt) #basic stats
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
axmt.crn <- chron(axmt_rwi, prefix = "CAM",prewhiten=TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axmt.crn, add.spline=TRUE, nyrs=20)

#ALEX FIORD ANNEX CONTROL

#open rwl file & examine properties
axnc <-read.rwl('AXNC.rwl')
dim(axnc) #42 years and 16 series
colnames(axnc) #series names
class(axnc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(axnc) #basic stats
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
axnc.crn <- chron(axnc_rwi, prefix = "CAM",prewhiten=TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axnc.crn, add.spline=TRUE, nyrs=20)

#ALEX FIORD ANNEX TREATMENT

#open rwl file & examine properties
axnt <-read.rwl('AXNT.rwl')
dim(axnt) #42 years and 16 series
colnames(axnt) #series names
class(axnt) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(axnt) #basic stats
plot(axnt, plot.type="spag") #spaghetti plot
rwl.stats(axnt)
summary(axnt)

#detrend & standardise series to produce ring width index values
axnt_rwi<- detrend(rwl = axmt, method = "Mean")

#calculate overall interseries correlation
axnt.rho <- interseries.cor(axnt, prewhiten=TRUE,
                            method="spearman")
mean(axnt.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
axnt.crn <- chron(axnt_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axnt.crn, add.spline=TRUE, nyrs=20)


###rename year columns###
axcc.crn <- data.frame("year"=rownames(axcc.crn), axcc.crn)
axct.crn <- data.frame("year"=rownames(axct.crn), axct.crn)
axdc.crn <- data.frame("year"=rownames(axdc.crn), axdc.crn)
axdt.crn <- data.frame("year"=rownames(axdt.crn), axdt.crn)
axfc.crn <- data.frame("year"=rownames(axfc.crn), axfc.crn)
axft.crn <- data.frame("year"=rownames(axft.crn), axft.crn)
axmc.crn <- data.frame("year"=rownames(axmc.crn), axmc.crn)
axmt.crn <- data.frame("year"=rownames(axmt.crn), axmt.crn)
axnc.crn <- data.frame("year"=rownames(axnc.crn), axnc.crn)
axnt.crn <- data.frame("year"=rownames(axnt.crn), axnt.crn)



###############   M E R G E     D A T A F R A M E S   #################

#now i have produced master chronologies for all alex fiord sites, i want to merge all of them into a 
#big ol' database so I can start to run my before-after-control-impact analysis


djoin  <- full_join(axdt.crn,axdc.crn,by = "year")
cjoin  <- full_join(axct.crn,axcc.crn,by = "year")
fjoin  <- full_join(axft.crn,axfc.crn,by = "year")
mjoin  <- full_join(axmt.crn,axmc.crn,by = "year")
njoin  <- full_join(axnt.crn,axnc.crn,by = "year")

dcjoin  <- full_join(djoin,cjoin,by = "year")
fmjoin  <- full_join(fjoin,mjoin,by = "year")
fmnjoin <- full_join(fmjoin,njoin,by = "year")
masterreschron <- full_join(dcjoin,fmnjoin,by = "year")


#export masterchronology

write.table(masterreschron, "/Volumes/Seagate Backup Plus Drive/ALL_RWL/masterreschron.txt", sep="\t")

write.table(masterchron, "/Volumes/Seagate Backup Plus Drive/ALL_RWL/masterchron.txt", sep="\t")




