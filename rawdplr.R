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



###############   C R E A T E    D A T A F R A M E S   #################

#i have the raw ring width files produced by cdendro which i need to standardise, detrend,
#apply a cubic smoothing spline and generate a series of master chronologies by site and treatment type

#open rwl file & examine properties
axcc <-read.tucson('AXCC1.rwl')
dim(axcc) #42 years and 16 series
colnames(axcc) #series names
class(axcc) # note "rwl" class as well as "data.frame"
rwi.stats.running(axnc_rwi)
interseries.cor(axcc_rwi)
#ALEX FIORD CASSIOPE CONTROL 

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
axcc.crn <- chron(axcc, prefix = "CAM") #Tukey's biweight robust mean
library(bootRes)
library(zoo)
#upload monthly climate variables
axclim<-read.csv("ax_clim.csv",header=T)
axclim[is.na(axclim)] = "NA"
axclim2<- data.frame(na.approx(axclim))



#try dcc


eg1 <- dcc(axdc.crn, axclim2, method = "response",start = 1, end =12, ci = 0.05, boot = TRUE)

dcplot(eg1, ci = FALSE, sig = TRUE, vertical = TRUE)

eg2 <- dcc(axcc.crn, axclim2, method = "correlation", start = 1, end = 12, ci = 0.05, boot = TRUE)

dcplot(eg2, ci = FALSE, sig = TRUE, vertical = TRUE)

#signal to noise ratio

#plot chronology w spline
plot(axcc.crn, add.spline=TRUE, nyrs=10)

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
axct.crn <- chron(axct, prefix = "CAM", biweight = TRUE) #Tukey's biweigh robust mean

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
axdc.crn <- chron(axdc, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(axdc.crn, add.spline=TRUE, nyrs=20)

#ALEX FIORD DRYAS TREATMENT

#open rwl file & examine properties
axdt <-read.rwl('AXDT.rwl')
dim(axdt) #42 years and 16 series
colnames(axdt) #series names
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
axdt.crn <- chron(axdt, prefix = "CAM") #Tukey's biweigh robust mean

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
axfc.crn <- chron(axfc, prefix = "CAM") #Tukey's biweigh robust mean

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
axft.crn <- chron(axft, prefix = "CAM") #Tukey's biweigh robust mean

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
axmc.crn <- chron(axmc, prefix = "CAM") #Tukey's biweigh robust mean

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
axmt.crn <- chron(axmt, prefix = "CAM") #Tukey's biweigh robust mean

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
axnc.crn <- chron(axnc, prefix = "CAM") #Tukey's biweigh robust mean

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
axnt.crn <- chron(axnt_rwi, prefix = "CAM") #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(lfst.crn, add.spline=TRUE, nyrs=20)


#LATNAC
#open rwl file & examine properties
lfsc <-read.rwl('LSFC.rwl')
#build a mean value chronology
lfsc.crn <- chron(lfsc, prefix = "CAM") #Tukey's biweigh robust mean
#LATNAT
#open rwl file & examine properties
lfst <-read.rwl('LFST.rwl')
#build a mean value chronology
lfst.crn <- chron(lfst, prefix = "CAM") #Tukey's biweigh robust mean

#TOOLIKC
#open rwl file & examine properties
tkc <-read.rwl('TKC.rwl')
#build a mean value chronology
tkc.crn <- chron(tkc, prefix = "CAM") #Tukey's biweigh robust mean
#TOOLIKT
#open rwl file & examine properties
tkt <-read.rwl('TKT.rwl')
#build a mean value chronology
tkt.crn <- chron(tkt, prefix = "CAM") #Tukey's biweigh robust mean

#IMC
#open rwl file & examine properties
imc <-read.rwl('IMC.rwl')
#build a mean value chronology
imc.crn <- chron(imc, prefix = "CAM") #Tukey's biweigh robust mean
#IMT
#open rwl file & examine properties
imt <-read.rwl('IMT.rwl')
#build a mean value chronology
imt.crn <- chron(imt, prefix = "CAM") #Tukey's biweigh robust mean

#BWC
#open rwl file & examine properties
bwc <-read.rwl('BWC.rwl')
#build a mean value chronology
bwc.crn <- chron(bwc, prefix = "CAM") #Tukey's biweigh robust mean
#BWT
#open rwl file & examine properties
bwt <-read.rwl('BWT.rwl')
#build a mean value chronology
bwt.crn <- chron(bwt, prefix = "CAM") #Tukey's biweigh robust mean

#ATT
#open rwl file & examine properties
atc <-read.rwl('ATC.rwl')
#build a mean value chronology
atc.crn <- chron(atc, prefix = "CAM") #Tukey's biweigh robust mean
#BWT
#open rwl file & examine properties
att <-read.rwl('ATT.rwl')
#build a mean value chronology
att.crn <- chron(att, prefix = "CAM") #Tukey's biweigh robust mean

lfsc.crn <- data.frame("year"=rownames(lfsc.crn), lfsc.crn)
lfst.crn <- data.frame("year"=rownames(lfst.crn), lfst.crn)   
bwc.crn <- data.frame("year"=rownames(bwc.crn), bwc.crn)   
bwt.crn <- data.frame("year"=rownames(bwt.crn), bwt.crn)   
tkc.crn <- data.frame("year"=rownames(tkc.crn), tkc.crn)   
tkt.crn <- data.frame("year"=rownames(tkt.crn), tkt.crn)   
imc.crn <- data.frame("year"=rownames(imc.crn), imc.crn)   
imt.crn <- data.frame("year"=rownames(imt.crn), imt.crn)
atc.crn <- data.frame("year"=rownames(atc.crn), atc.crn)   
att.crn <- data.frame("year"=rownames(att.crn), att.crn)


ajoin  <- full_join(lfsc.crn,lfst.crn,by = "year")
bjoin  <- full_join(bwc.crn,bwt.crn,by = "year")
cjoin  <- full_join(tkc.crn,tkt.crn,by = "year")
djoin  <- full_join(bwc.crn,bwt.crn,by = "year")
ejoin  <- full_join(atc.crn,att.crn,by = "year")

abjoin <- full_join(ajoin,bjoin,by = "year")
cdjoin <- full_join(cjoin,djoin,by = "year")
cdejoin <- full_join(cdjoin,ejoin,by = "year")
fulljoin <- full_join(abjoin,cdejoin,by = "year")


####PLOT ALL ALEX######
library(cowplot)

p1 <- plot(axcc.crn, add.spline=TRUE, nyrs=20)
p2 <- plot(axct.crn, add.spline=TRUE, nyrs=20)
p3 <- plot(axfc.crn, add.spline=TRUE, nyrs=20)
p4 <- plot(axft.crn, add.spline=TRUE, nyrs=20)
p5 <- plot(axdc.crn, add.spline=TRUE, nyrs=20)
p6 <- plot(axdt.crn, add.spline=TRUE, nyrs=20)
plot_grid(p1, p2, p3, p4, p5, p6)

par(mfrow=c(2,2),mai=c(0.6,0.6,0.6,0.6),cex=0.7)
plot(axmc.crn, add.spline=TRUE, nyrs=20)
plot(axmt.crn, add.spline=TRUE, nyrs=20)
plot(axnc.crn, add.spline=TRUE, nyrs=20)
plot(axnt.crn, add.spline=TRUE, nyrs=20)


(0.230834+0.2900237+ 0.1787364+0.1263201+0.2007672 +0.1400974+0.2705249 +0.1169608)/10




#BARROW TREATMENT

#open rwl file & examine properties
bwt <-read.rwl('BWT.rwl')
dim(bwt) #42 years and 16 series
colnames(bwt) #series names
class(bwt) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(bwt) #basic stats
plot(bwt, plot.type="spag") #spaghetti plot
rwl.stats(bwt)
summary(bwt)

#detrend & standardise series to produce ring width index values
bwt_rwi<- detrend(rwl = bwt, method = "Mean")

#calculate overall interseries correlation
bwt.rho <- interseries.cor(bwt, prewhiten=TRUE,
                            method="spearman")
mean(bwt.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
bwt.crn <- chron(bwt_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(bwt.crn, add.spline=TRUE, nyrs=20)


#BARROW CONTROL

#open rwl file & examine properties
bwc <-read.rwl('BWC.rwl')
dim(bwc) #42 years and 16 series
colnames(bwc) #series names
class(bwc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(bwc) #basic stats
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
bwc.crn <- chron(bwc_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(bwc.crn, add.spline=TRUE, nyrs=20)

#DISKO

#open rwl file & examine properties
dsk <-read.rwl('DSKO.rwl')
dim(dsk) #42 years and 16 series
colnames(dsk) #series names
class(dsk) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(dsk) #basic stats
plot(dsk, plot.type="spag") #spaghetti plot
rwl.stats(dsk)
summary(dsk)

#detrend & standardise series to produce ring width index values
dsk_rwi<- detrend(rwl = dsk, method = "Mean")

#calculate overall interseries correlation
dsk.rho <- interseries.cor(dsk, prewhiten=TRUE,
                           method="spearman")
mean(dsk.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
dsk.crn <- chron(dsk_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(dsk.crn, add.spline=TRUE, nyrs=20)

#ATQASUK TREATMENT

#open rwl file & examine properties
att <-read.rwl('ATT.rwl')
dim(att) #42 years and 16 series
colnames(att) #series names
class(att) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(att) #basic stats
plot(att, plot.type="spag") #spaghetti plot
rwl.stats(att)
summary(att)

#detrend & standardise series to produce ring width index values
att_rwi<- detrend(rwl = att, method = "Mean")

#calculate overall interseries correlation
att.rho <- interseries.cor(att, prewhiten=TRUE,
                           method="spearman")
mean(att.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
att.crn <- chron(att_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(att.crn, add.spline=TRUE, nyrs=20)

#ATQASUK CONTROL

#open rwl file & examine properties
atc <-read.rwl('ATC.rwl')
dim(atc) #42 years and 16 series
colnames(atc) #series names
class(atc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(atc) #basic stats
plot(atc, plot.type="spag") #spaghetti plot
rwl.stats(atc)
summary(atc)

#detrend & standardise series to produce ring width index values
atc_rwi<- detrend(rwl = atc, method = "Mean")

#calculate overall interseries correlation
atc.rho <- interseries.cor(atc, prewhiten=TRUE,
                           method="spearman")
mean(atc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
atc.crn <- chron(bwt_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(atc.crn, add.spline=TRUE, nyrs=20)

#IMNAVAIT TREATMENT

#open rwl file & examine properties
imt <-read.rwl('IMT.rwl')
dim(imt) #42 years and 16 series
colnames(imt) #series names
class(imt) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(imt) #basic stats
plot(imt, plot.type="spag") #spaghetti plot
rwl.stats(imt)
summary(imt)

#detrend & standardise series to produce ring width index values
imt_rwi<- detrend(rwl = imt, method = "Mean")

#calculate overall interseries correlation
imt.rho <- interseries.cor(imt, prewhiten=TRUE,
                           method="spearman")
mean(imt.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
imt.crn <- chron(imt_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(imt.crn, add.spline=TRUE, nyrs=20)


#IMNAVAIT CONTROL

#open rwl file & examine properties
imc <-read.rwl('IMC.rwl')
dim(imc) #42 years and 16 series
colnames(imc) #series names
class(imc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(imc) #basic stats
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
imc.crn <- chron(imt_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(imc.crn, add.spline=TRUE, nyrs=20)


#TOOLIK TREATMENT

#open rwl file & examine properties
tkt <-read.rwl('TKT.rwl')
dim(tkt) #42 years and 16 series
colnames(tkt) #series names
class(tkt) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(tkt) #basic stats
plot(tkt, plot.type="spag") #spaghetti plot
rwl.stats(tkt)
summary(tkt)

#detrend & standardise series to produce ring width index values
tkt_rwi<- detrend(rwl = tkt, method = "Mean")

#calculate overall interseries correlation
tkt.rho <- interseries.cor(tkt, prewhiten=TRUE,
                           method="spearman")
mean(tkt.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
tkt.crn <- chron(tkt_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(tkt.crn, add.spline=TRUE, nyrs=20)


#TOOLIK CONTROL

#open rwl file & examine properties
tkc <-read.rwl('TKC.rwl')
dim(tkc) #42 years and 16 series
colnames(tkc) #series names
class(tkc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(tkc) #basic stats
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
tkc.crn <- chron(tkc_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(tkc.crn, add.spline=TRUE, nyrs=20)


#SVALBARD TREATMENT

#open rwl file & examine properties
sva <-read.rwl('SVALL.rwl')
dim(sva) #42 years and 16 series
colnames(sva) #series names
class(sva) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(arc) #basic stats


plot(sva, plot.type="spag") #spaghetti plot
rwl.stats(sva)
summary(sva)

#detrend & standardise series to produce ring width index values
sva_rwi<- detrend(rwl = sva, method = "Mean")

#calculate overall interseries correlation
sva.rho <- interseries.cor(sva, prewhiten=TRUE,
                           method="spearman")
mean(sva.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
sva.crn <- chron(sva_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(sva.crn, add.spline=TRUE, nyrs=20)


#SAANA

#open rwl file & examine properties
saa <-read.rwl('SAALL.rwl')
dim(saa) #42 years and 16 series
colnames(saa) #series names
class(saa) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(saa) #basic stats
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
saa.crn <- chron(saa_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(saa.crn, add.spline=TRUE, nyrs=20)


#LATNJAJURE TREATMENT

#open rwl file & examine properties
lfst <-read.rwl('LFST.rwl')
dim(lfst) #42 years and 16 series
colnames(lfst) #series names
class(lfst) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(lfst) #basic stats
plot(lfst, plot.type="spag") #spaghetti plot
rwl.stats(lfst)
summary(lfst)

#detrend & standardise series to produce ring width index values
lfst_rwi<- detrend(rwl = lfst, method = "Mean")

#calculate overall interseries correlation
lfst.rho <- interseries.cor(lfst, prewhiten=TRUE,
                           method="spearman")

mean(lfst.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
lfst.crn <- chron(lfst_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(lfst.crn, add.spline=TRUE, nyrs=20)

#LATNJAJURE TREATMENT

#open rwl file & examine properties
lfsc <-read.rwl('LSFC.rwl')
dim(lfsc) #42 years and 16 series
colnames(lfsc) #series names
class(lfsc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(lfsc) #basic stats
plot(lfsc, plot.type="spag") #spaghetti plot
rwl.stats(lfsc)
summary(lfsc)

#detrend & standardise series to produce ring width index values
lfsc_rwi<- detrend(rwl = lfsc, method = "Mean")

#calculate overall interseries correlation
lfsc.rho <- interseries.cor(lfsc, prewhiten=TRUE,
                            method="spearman")
mean(lfsc.rho[, 1]) #print mean interseries correlation


#build a mean value chronology
lfsc.crn <- chron(lfsc_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(lfsc.crn, add.spline=TRUE, nyrs=20)

#ABISKO

#open rwl file & examine properties
abi <-read.rwl('ABALL.rwl')
dim(abi) #42 years and 16 series
colnames(abi) #series names
class(abi) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(abi) #basic stats
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
abi.crn <- chron(abi_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(abi.crn, add.spline=TRUE, nyrs=20)


#ARCTICSTATION

#open rwl file & examine properties
arc <-read.rwl('ARC.rwl')
dim(arc) #42 years and 16 series
colnames(arc) #series names
class(arc) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(arc) #basic stats
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
arc.crn <- chron(arc_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(arc.crn, add.spline=TRUE, nyrs=20)

#ZAPYR

#open rwl file & examine properties
zap <-read.rwl('ZAPYR.rwl')
dim(zap) #42 years and 16 series
colnames(zap) #series names
class(zap) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(zap) #basic stats
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
zap.crn <- chron(zap_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(zap.crn, add.spline=TRUE, nyrs=20)


#ELZA

#open rwl file & examine properties
elz <-read.rwl('ELZ.rwl')
dim(elz) #42 years and 16 series
colnames(elz) #series names
class(elz) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(elz) #basic stats
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
elz.crn <- chron(elz_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(elz.crn, add.spline=TRUE, nyrs=20)

#SKANSKBUKTA

#open rwl file & examine properties
ska <-read.rwl('SKANS.rwl')
dim(ska) #42 years and 16 series
colnames(ska) #series names
class(ska) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(ska) #basic stats
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
ska.crn <- chron(ska_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(ska.crn, add.spline=TRUE, nyrs=20)

#AWS

#open rwl file & examine properties
aws <-read.rwl('AWS.rwl')
dim(aws) #42 years and 16 series
colnames(aws) #series names
class(aws) # note "rwl" class as well as "data.frame"

#generate rwl stats
rwl.report(aws) #basic stats
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
aws.crn <- chron(aws_rwi, prefix = "CAM",prewhiten = TRUE) #Tukey's biweigh robust mean

#signal to noise ratio

#plot chronology w spline
plot(aws.crn, add.spline=TRUE, nyrs=20)

###alex rename year columns###
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

###other sites rename year columns###
abi.crn <- data.frame("year"=rownames(abi.crn), abi.crn)
lfst.crn <- data.frame("year"=rownames(lfst.crn), lfst.crn)
lfsc.crn <- data.frame("year"=rownames(lfsc.crn), lfsc.crn)
saa.crn <- data.frame("year"=rownames(saa.crn), saa.crn)
atc.crn <- data.frame("year"=rownames(atc.crn), atc.crn)
att.crn <- data.frame("year"=rownames(att.crn), att.crn)
imt.crn <- data.frame("year"=rownames(imt.crn), imt.crn)
imc.crn <- data.frame("year"=rownames(imc.crn), imc.crn)
tkt.crn <- data.frame("year"=rownames(tkt.crn), tkt.crn)
tkc.crn <- data.frame("year"=rownames(tkc.crn), tkc.crn)
bwt.crn <- data.frame("year"=rownames(bwt.crn), bwt.crn)
bwc.crn <- data.frame("year"=rownames(bwc.crn), bwc.crn)
dsk.crn <- data.frame("year"=rownames(dsk.crn), dsk.crn)
arc.crn <- data.frame("year"=rownames(arc.crn), arc.crn)
sva.crn <- data.frame("year"=rownames(sva.crn), sva.crn)
elz.crn <- data.frame("year"=rownames(elz.crn), elz.crn)
zap.crn <- data.frame("year"=rownames(zap.crn), zap.crn)
aws.crn <- data.frame("year"=rownames(aws.crn), aws.crn)
ska.crn <- data.frame("year"=rownames(ska.crn), ska.crn)


###############   M E R G E     D A T A F R A M E S   #################

#now i have produced master chronologies for all alex fiord sites, i want to merge all of them into a 
#big ol' database so I can start to run my before-after-control-impact analysis
   

djoin  <- full_join(arc.crn,elz.crn,by = "year")
cjoin  <- full_join(zap.crn,ska.crn,by = "year")
dcjoin  <- full_join(djoin,cjoin,by = "year")
fulljoin  <- full_join(dcjoin,aws.crn,by = "year")

ajoin  <- full_join(axcc.crn,axct.crn,by = "year")
bjoin  <- full_join(axdc.crn,axdt.crn,by = "year")
cjoin <- full_join(axfc.crn,axft.crn,by = "year")
abjoin <- full_join(ajoin,bjoin,by = "year")
fulljoin <- full_join(abjoin,cjoin,by = "year")

masterchron <- full_join(dcjoin,fmnjoin,by = "year")




#export masterchronology

write.table(dcjoin, "/Volumes/Seagate Backup Plus Drive/ALL_RWL/extra.txt")
