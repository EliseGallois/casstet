###############   S E T   U P   W O R K S P A C E   #################


rm(list=ls(all=TRUE))

#set working directory
setwd("/Volumes/Seagate Backup Plus Drive")
getwd()

#download necessary packages
library(graphics)
library(dplR)
library(dplyr)
library(stats)
library(utils)
library(tidyverse)
library(effsize)


###############   B A C I AXCASS  #################

#read in the relevant raw data file
ax <-read.csv("ax.csv", header=T)
dim(ax)
colnames(ax) #series names
class(ax) 


#plot raw data with an abline showing year of OTC implementation
par(mfrow=c(1,1),cex=0.7)
plot(ax$year,ax$impact, type="l",col=82,lwd = 3,title="Cassiope Annex Site", xlab="Year", ylab="Annual Growth Increment [mm]",cex.axis=1.5,cex.lab=1.5)
points(ax$year,ax$control,  type="l",col=12, lwd = 3)
abline(v=1993, col=3,lwd = 3)


#plot the differences vector over years
plot(ax$year,ax$diff, type='l', xlab="Year", ylab="OTC - Control Annual Growth Increment [mm'")
abline(v=1993, col=3,lwd = 3)

#run a 2-sample t-test to compare the difference between the two periods.
#is the mean difference the same in the before and after period?

#subset the diff data into before and after periods
iA <- subset(ax$diff, ax$period == "a")
iB <- subset(ax$diff, ax$period == "b")
#display subsets as vectors
impactA <- as.vector(iA)
impactB <- as.vector(iB)
#run test on differences of differences
t.test(impactB,impactA)

#classical BACI
#subset into categories
iA <- subset(ax$impact, ax$period == "a")
ib <- subset(ax$impact, ax$period == "b")
cA <- subset(ax$control, ax$period == "a")
cb <- subset(ax$control, ax$period == "b")
#as vectors
iA <- as.vector(iA)
ib <- as.vector(ib)
cA <- as.vector(cA)
cb <- as.vector(cb)
#mean values
mia <-mean(iA)
mib <-mean(ib)
mca <-mean(cA)

###############   B A C I AXDRYAS  #################

#read in the relevant raw data file
axdy <-read.csv("axdy.csv", header=T)
dim(axdy)
colnames(axdy) #series names
class(axdy) 


#plot raw data with an abline showing year of OTC implementation
par(mfrow=c(1,1),cex=0.7)
plot(axdy$year,axdy$impact, type="l",col=82,lwd = 3,title="Dryas Site", xlab="Year", ylab="Annual Growth Increment [mm]",cex.axis=1.5,cex.lab=1.5)
points(axdy$year,axdy$control,  type="l",col=12, lwd = 3)
abline(v=1993, col=3,lwd = 3)


#plot the differences vector over years
plot(axdy$year,axdy$diff, type='l', xlab="Year", ylab="OTC - Control Annual Growth Increment [mm'")
abline(v=1993, col=3,lwd = 3)

#run a 2-sample t-test to compare the difference between the two periods.
#is the mean difference the same in the before and after period?

#subset the diff data into before and after periods
iA <- subset(axdy$diff, axdy$period == "a")
iB <- subset(axdy$diff, axdy$period == "b")
#display subsets as vectors
impactA <- as.vector(iA)
impactB <- as.vector(iB)
#run test on differences of differences
t.test(impactB,impactA)

#classical BACI
#subset into categories
iA <- subset(axdy$impact, axdy$period == "a")
ib <- subset(axdy$impact, axdy$period == "b")
cA <- subset(axdy$control, axdy$period == "a")
cb <- subset(axdy$control, axdy$period == "b")
#as vectors
iA <- as.vector(iA)
ib <- as.vector(ib)
cA <- as.vector(cA)
cb <- as.vector(cb)
#mean values
mia <-mean(iA)
mib <-mean(ib)
mca <-mean(cA)
mcb <-mean(cb, na.rm=TRUE)

#classic BACI metric (1.486134)
baci.orig <- (mia-mib)-(mca-mcb)
baci.orig


#CI contribution: absolute value of the change in mean (0.5228897)
baci.contrib <- (abs(mia-mib)) - (abs(mca-mcb))
baci.contrib

#CI divergence: measure of dissimilarity (-0.1156897)
baci.diverge <- (abs(mia-mca)) - (abs(mib-mcb))
baci.diverge 

###############   B A C I AXFERT  #################

#read in the relevant raw data file
axft <-read.csv("axft.csv", header=T)
dim(axft)
colnames(axft) #series names
class(axft) 


#plot raw data with an abline showing year of OTC implementation
par(mfrow=c(1,1),cex=0.7)
plot(axft$year,axft$impact, type="l",col=82,lwd = 3,title="Fert Site", xlab="Year", ylab="Annual Growth Increment [mm]",cex.axis=1.5,cex.lab=1.5)
points(axft$year,axft$control,  type="l",col=12, lwd = 3)
abline(v=1993, col=3,lwd = 3)


#plot the differences vector over years
plot(axft$year,axft$diff, type='l', xlab="Year", ylab="OTC - Control Annual Growth Increment [mm'")
abline(v=1993, col=3,lwd = 3)

#run a 2-sample t-test to compare the difference between the two periods.
#is the mean difference the same in the before and after period?

#subset the diff data into before and after periods
iA <- subset(axft$diff, axft$period == "a")
iB <- subset(axft$diff, axft$period == "b")
#display subsets as vectors
impactA <- as.vector(iA)
impactB <- as.vector(iB)
#run test on differences of differences
t.test(impactB,impactA)

#classical BACI
#subset into categories
iA <- subset(axft$impact, axft$period == "a")
ib <- subset(axft$impact, axft$period == "b")
cA <- subset(axft$control, axft$period == "a")
cb <- subset(axft$control, axft$period == "b")
#as vectors
iA <- as.vector(iA)
ib <- as.vector(ib)
cA <- as.vector(cA)
cb <- as.vector(cb)
#mean values
mia <-mean(iA)
mib <-mean(ib)
mca <-mean(cA)
mcb <-mean(cb, na.rm=TRUE)

#classic BACI metric (0.4133056)
baci.orig <- (mia-mib)-(mca-mcb)
baci.orig


#CI contribution: absolute value of the change in mean (0.4133056)
baci.contrib <- (abs(mia-mib)) - (abs(mca-mcb))
baci.contrib

#CI divergence: measure of dissimilarity ( 0.4133056)
baci.diverge <- (abs(mia-mca)) - (abs(mib-mcb))
baci.diverge 

###############   B A C I AXMIG  #################

#read in the relevant raw data file
axm <-read.csv("axm.csv", header=T)
dim(axm)
colnames(axm) #series names
class(axm) 

#plot raw data with an abline showing year of OTC implementation
par(mfrow=c(1,1),cex=0.7)
plot(axm$year,axm$impact, type="l",col=82,lwd = 3, title="Migration Site", xlab="Year", ylab="Annual Growth Increment [mm]",cex.axis=1.5,cex.lab=1.5)
points(axm$year,axm$control,  type="l",col=12, lwd = 3)
abline(v=2010, col=3,lwd = 3)


#plot the differences vector over years
plot(axm$year,axm$diff, type='l', xlab="Year", ylab="OTC - Control Annual Growth Increment [mm'")
abline(v=2010, col=3,lwd = 3)

#run a 2-sample t-test to compare the difference between the two periods.
#is the mean difference the same in the before and after period?

#subset the diff data into before and after periods
iA <- subset(axm$diff, axm$period == "a")
iB <- subset(axm$diff, axm$period == "b")
#display subsets as vectors
impactA <- as.vector(iA)
impactB <- as.vector(iB)
#run test on differences of differences
t.test(impactB,impactA)

#classical BACI
#subset into categories
iA <- subset(axm$impact, axm$period == "a")
ib <- subset(axm$impact, axm$period == "b")
cA <- subset(axm$control, axm$period == "a")
cb <- subset(axm$control, axm$period == "b")
#as vectors
iA <- as.vector(iA)
ib <- as.vector(ib)
cA <- as.vector(cA)
cb <- as.vector(cb)
#mean values
mia <-mean(iA)
mib <-mean(ib)
mca <-mean(cA)
mcb <-mean(cb, na.rm=TRUE)

#classic BACI metric (-5.038139)
baci.orig <- (mia-mib)-(mca-mcb)
baci.orig


#CI contribution: absolute value of the change in mean (0.9360437)
baci.contrib <- (abs(mia-mib)) - (abs(mca-mcb))
baci.contrib

#CI divergence: measure of dissimilarity (4.002306)
baci.diverge <- (abs(mia-mca)) - (abs(mib-mcb))
baci.diverge 

###############   B A C I AXANNEX #################

#read in the relevant raw data file
axn <-read.csv("axn.csv", header=T)
dim(axn)
colnames(axn) #series names
class(axn) 

#plot raw data with an abline showing year of OTC implementation
par(mfrow=c(1,1),cex=0.7)
plot(axn$year,axn$impact, type="l",col=82,lwd = 3, xlab="Year", ylab="Annual Growth Increment [mm]",cex.axis=1.5,cex.lab=1.5)
points(axn$year,axn$control,  type="l",col=12, lwd = 3)
abline(v=2010, col=3,lwd = 3)


#plot the differences vector over years
plot(axn$year,axn$diff, type='l', xlab="Year", ylab="OTC - Control Annual Growth Increment [mm'")
abline(v=2010, col=3,lwd = 3)

#run a 2-sample t-test to compare the difference between the two periods.
#is the mean difference the same in the before and after period?

#subset the diff data into before and after periods
iA <- subset(axn$diff, axn$period == "a")
iB <- subset(axn$diff, axn$period == "b")
#display subsets as vectors
impactA <- as.vector(iA)
impactB <- as.vector(iB)
#run test on differences of differences
t.test(impactB,impactA)

#classical BACI
#subset into categories
fiA <- subset(axft$impact, ax$period == "a")
ib <- subset(axdy$impact, axdy$period == "b")
fcA <- subset(axft$control, ax$period == "a")
cb <- subset(axm$control, axn$period == "b")
#as vectors
iA <- as.vector(iA)
ib <- as.vector(ib)
cA <- as.vector(cA)
cb <- as.vector(cb)
#mean values
mia <-mean(iA)
mib <-mean(ib)
mca <-mean(cA)
mcb <-mean(cb, na.rm=TRUE)

#classic BACI metric (1.732146)
baci.orig <- (mia-mib)-(mca-mcb)
baci.orig

#CI contribution: absolute value of the change in mean (1.732146)
baci.contrib <- (abs(mia-mib)) - (abs(mca-mcb))
baci.contrib

#CI divergence: measure of dissimilarity (1.225631)
baci.diverge <- (abs(mia-mca)) - (abs(mib-mcb))
baci.diverge 

###############   MULTISITE  #################

#read in the relevant raw data file
multi <-read.csv("multibaci.csv", header=T)
dim(axm)
colnames(axm) #series names
class(axm) 

#plot raw data with an abline showing year of OTC implementation
par(mfrow=c(1,1),cex=0.7)
plot(multi$year,multi$atc, type="l",col=82,lwd = 3, xlab="Year", ylab="Annual Growth Increment [mm]")
points(multi$year,multi$att,  type="l",col=12, lwd = 3)

cor(multi$tkc,multi$tkt,  method = c("pearson"), use = "complete.obs")

#effsize cass
treatment = ciA
control = ccA
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=26)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control,paired=FALSE,hedges.correction=TRUE)
## data and factor
cohen.d(d,f)
## formula interface
afceff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

#effsize dryas
treatment = diA
control = dcA
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=26)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control,paired=TRUE)
## data and factor
cohen.d(d,f)
## formula interface
afdeff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

#effsize fert
treatment = fiA
control = fcA
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=26)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control,na.rm=TRUE,paired=TRUE)
## data and factor
cohen.d(d,f)
## formula interface
affeff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

#effsize mig
treatment = niA
control = ncA
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=9)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control)
## data and factor
cohen.d(d,f)
## formula interface
afmeff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

#effsize nx
treatment = axn$impact
control = axn$control
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=42)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control)
## data and factor
cohen.d(d,f)
## formula interface
afneff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

#effsize latna
treatment = multi$lsft
control = multi$lsfc
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=22)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control)
## data and factor
cohen.d(d,f)
## formula interface
leff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

#effsize tlk
treatment = multi$tkt
control = multi$tkc
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=22)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control)
## data and factor
cohen.d(d,f)
## formula interface
teff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

#effsize brw
treatment = multi$bwt
control = multi$bwc
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=22)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control)
## data and factor
cohen.d(d,f)
## formula interface
beff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

#effsize atk
treatment = multi$att
control = multi$atc
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=22)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control)
## data and factor
cohen.d(d,f)
## formula interface
aeff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

#effsize imn
treatment = multi$imt
control = multi$imc
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=22)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control)
## data and factor
cohen.d(d,f)
## formula interface
ieff <- cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

afceff #large
afdeff #negligible
affeff #small
afneff #negligible
afmeff #large
leff #large
beff #negligible
teff #small
ieff #small
aeff #small

