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
library(cowplot)
library(dendroTools)
library("ggpubr")
library(dcv)
library(hydroGOF)
library(esquisse)
library(root)
library(knitr)
library(ggpairs)
library(stargazer)

m1n <- lme(nrw ~ nndvi, random=~1|xyear + 1|shrubID/subsite, 
           cor=corAR1(), data = data, method = "ML", control=list(maxIter=1000))



###############   U P L O A D    M E A D O W  #################

#upload alex fiord metrices & variables
axall<-read.csv("allAX.csv", header=T)




names(axall)  # print out the column (i.e., variable) names
dim(axall)    # number of rows and columns
head(axall)
##1] "year"   "julymx" "Tbud"   "Tflr"   "Tsen"   "Tupr"   "Cbud"   "Cflr"  
"Csen"   "Cupr"   "DTrwi"  "DTres"  "DTflr"  "DTbud"  "DCrwi"  "DCres" 
"DCflr"  "DCbud"  "CTrwi"  "CTres"  "CTflr"  "CTbud"  "CCrwi"  "CCres" 
"CCflr"  "CCbud"  "FTrwi"  "FTres"  "FTflr"  "FTbud"  "FCrwi"  "FCres" 
"FCflr"  "FCbud"  "MTrwi"  "MTres"  "MCrwi"  "MCres"  "NTrwi"  "NTres" 
"NCrwi"  "NCres"##

### TEST FOR NORMALITY #####

shapiro.test(axall$FCflr)
shapiro.test(axall$FCbud)
shapiro.test(axall$AWSrwi)

ggqqplot(axall$Chjun, ylab = "CCrwi")
ggqqplot(axall$CTrwi, ylab = "CTrwi")
ggqqplot(axall$DCrwi, ylab = "DCrwi")
ggqqplot(axall$DCrwi, ylab = "DTrwi")
ggqqplot(axall$FCrwi, ylab = "FCrwi")
ggqqplot(axall$FTrwi, ylab = "FTrwi")


#### C O R R E L A T I O N    T E S T S #####

#snow correlations FLR
cor.test(axall$NCflr,axall$julyav,  method = c("pearson"))
cor.test(axall$CTrwi,axall$ctsnow,  method = c("pearson"))
cor.test(axall$CCflr,axall$ccsnow,  method = c("pearson"))
cor.test(axall$CTflr,axall$ctsnow,  method = c("pearson"))
cor.test(axall$CCbud,axall$ccsnow,  method = c("pearson"))
cor.test(axall$CTbud,axall$ctsnow,  method = c("pearson"))

cor.test(axall$FCrwi,axall$fcsnow,  method = c("pearson"))
cor.test(axall$FTrwi,axall$ftsnow,  method = c("pearson"))
cor.test(axall$FCflr,axall$fcsnow,  method = c("pearson"))
cor.test(axall$FTflr,axall$ftsnow,  method = c("pearson"))
cor.test(axall$FCbud,axall$fcsnow,  method = c("pearson"))
cor.test(axall$FTbud,axall$ftsnow,  method = c("pearson"))

cor.test(axall$DCrwi,axall$dcsnow,  method = c("pearson"))
cor.test(axall$DTrwi,axall$dtsnow,  method = c("pearson"))
cor.test(axall$DCflr,axall$dcsnow,  method = c("pearson"))
cor.test(axall$DTflr,axall$dtsnow,  method = c("pearson"))
cor.test(axall$DCbud,axall$dcsnow,  method = c("pearson"))
cor.test(axall$DTbud,axall$dtsnow,  method = c("pearson"))

#moisture correlations FLR
cor.test(axall$CCrwi,axall$ccmois,  method = c("pearson"))
cor.test(axall$CTrwi,axall$ctmois,  method = c("pearson"))
cor.test(axall$CCflr,axall$ccmois,  method = c("pearson"))
cor.test(axall$CTflr,axall$ctmois,  method = c("pearson"))
cor.test(axall$CCbud,axall$ccmois,  method = c("pearson"))
cor.test(axall$CTbud,axall$ctmois,  method = c("pearson"))

cor.test(axall$FCrwi,axall$fcmois,  method = c("pearson"))
cor.test(axall$FTrwi,axall$ftmois,  method = c("pearson"))
cor.test(axall$FCflr,axall$fcmois,  method = c("pearson"))
cor.test(axall$FTflr,axall$ftmois,  method = c("pearson"))
cor.test(axall$FCbud,axall$fcmois,  method = c("pearson"))
cor.test(axall$FTbud,axall$ftmois,  method = c("pearson"))

cor.test(axall$DCrwi,axall$dcmois,  method = c("pearson"))
cor.test(axall$DTrwi,axall$dtmois,  method = c("pearson"))
cor.test(axall$DCflr,axall$dcmois,  method = c("pearson"))
cor.test(axall$DTflr,axall$dtmois,  method = c("pearson"))
cor.test(axall$DCbud,axall$dcmois,  method = c("pearson"))
cor.test(axall$DTbud,axall$dtmois,  method = c("pearson"))



cor.test(axall$Thjun,axall$CTbud,  method = c("pearson"))
cor.test(axall$Chjun,axall$CCbud,  method = c("pearson"))
cor.test(axall$Thjul,axall$CTflr,  method = c("pearson"))
cor.test(axall$Chjul,axall$CCbud,  method = c("pearson"))
cor.test(axall$Thaug,axall$CTflr,  method = c("pearson"))
cor.test(axall$Chaug,axall$CCbud,  method = c("pearson"))



#visualise it
#TKIM & BWAT

ph1 <- ggscatter(axall, x = "saajul", y = "SAArwi", 
         add = "reg.line", add.params = list(color = "blue", fill = "paleturquoise"),conf.int = TRUE, 
         cor.coef = TRUE, cor.method = "pearson",
         ylab = "July Average Temp (°C)", xlab = "Stem Growth Index", title="Finland - Saana")
ph2 <- ggscatter(axall, x = "saajul", y = "SAAflr", 
          add = "reg.line", add.params = list(color = "blue", fill = "paleturquoise"),conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "July Average Temp (°C)", ylab = "Average Flower Occurrence" )
ph3 <- ggscatter(axall, x = "saajul", y = "SAAbud", 
          add = "reg.line", add.params = list(color = "blue", fill = "paleturquoise"),conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "July Average Temp (°C)", ylab = "Average Bud Occurrence" )
ph4 <- ggscatter(axall, x = "lfsjul", y = "LFSCrwi", 
          add = "reg.line", add.params = list(color = "blue", fill = "paleturquoise"),conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "July Average Temp (°C)", ylab = "Stem Growth Index", title="Sweden - Latnjajaure")
ph5 <- ggscatter(axall, x = "lfsjul", y = "LFSCflr", 
                 add = "reg.line", add.params = list(color = "blue", fill = "paleturquoise"),conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 ylab = "July Average Temp (°C)", ylab = "Average Flower Occurrence", title="Sweden - Latnjajaure")
ph6 <- ggscatter(axall, x = "lfsjul", y = "LFSCbud", 
                 add = "reg.line", add.params = list(color = "blue", fill = "paleturquoise"),conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "July Average Temp (°C)", ylab = "Average Bud Occurrence", title="Sweden - Latnjajaure")

plot_grid(ph1, ph2, ph3)

#pheno correlations BUD
cor(axall$julymx,axall$Cbud,  method = c("spearman"), use = "complete.obs")

#visualise it
dev.off()

library(cowplot)





p1<-ggscatter(axall, y = "CCbud", x = "julyav", 
          add = "reg.line", add.params = list(color = "purple", fill = "magenta"),conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Average Bud Occurrence", xlab = "July Average Temp (°C)",title="Cassiope Site Control")
p2<-ggscatter(axall, y = "DCbud", x = "julyav", 
          add = "reg.line", add.params = list(color = "purple", fill = "magenta"),conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Average Bud Occurrence", xlab = "July Average Temp (°C)",title="Dryas Site Control") 
p3<-ggscatter(axall, y = "FCbud", x = "julyav", 
          add = "reg.line", add.params = list(color = "purple", fill = "magenta"),conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Average Bud Occurrence", xlab = "July Average Temp (°C)",title="Vaccinium Site Control") 


plot_grid(p1, p2, p3)



par(mfrow=c(3,2),cex=0.6)
plot_grid(f1, f2,f3,f4,f5,f6, nrow = 3, ncol = 2)

f1<-ggscatter(axall, x = "CCrwi", y = "CCflr", 
              add = "reg.line", add.params = list(color = "green", fill = "palegreen"),conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Stem Length Index", ylab = "Average Flower Occurrence",title="Cassiope Site Control")
f3<-ggscatter(axall, x = "DCrwi", y = "DCflr", 
              add = "reg.line", add.params = list(color = "green", fill = "palegreen"),conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Stem Length Index", ylab = "Average Flower Occurrence",title="Dryas Site Control") 
f5<-ggscatter(axall, x = "FCrwi", y = "FCflr", 
              add = "reg.line", add.params = list(color = "green", fill = "palegreen"),conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Stem Length Index", ylab = "Average Flower Occurrence",title="Vaccinium Site Control") 
f2<-ggscatter(axall, x = "CTrwi", y = "CTflr", 
              add = "reg.line", add.params = list(color = "green", fill = "palegreen"),conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem Length Index", ylab = "Average Flower Occurrence",title="Cassiope Site Treatment") 
f4<-ggscatter(axall, x = "DTrwi", y = "DTflr", 
              add = "reg.line", add.params = list(color = "green", fill = "palegreen"),conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Stem Length Index", ylab = "Average Flower Occurrence",title="Dryas Site Treatment") 
f6<-ggscatter(axall, x = "FTrwi", y = "FTflr", 
              add = "reg.line", add.params = list(color = "green", fill = "palegreen"),conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem Length Index", ylab = "Average Flower Occurrence",title="Vaccinium Site Treatment") 

plot_grid(f1, f2, f3,f4,f5,f6)



#pheno correlations UPR
cor(axall$julymx,axall$Csen,  method = c("spearman"), use = "complete.obs")

#visualise it
dev.off()
ggscatter(axall, x = "julymx", y = "Csen", 
          add = "reg.line", add.params = list(color = "blue", fill = "paleturquoise"),conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "July Max Temp (°C)", ylab = "Day of Year") + xlim(8,12)

#pheno correlations FLR
cor(axall$julymx,axall$Cupr,  method = c("spearman"), use = "complete.obs")

#visualise it
dev.off()
ggscatter(axall, x = "julymx", y = "Cupr", 
          add = "reg.line", add.params = list(color = "blue", fill = "paleturquoise"),conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "July Max Temp (°C)", ylab = "Day of Year") + xlim(8,12)


###### GROWTH METRICES CORRELATIONS ######
#pheno correlations FLR
cor(axall$julymx,axall$CCres,  method = c("pearson"), use = "complete.obs")

#visualise it
dev.off()
ggscatter(axall, x = "julymx", y = "CCrwi", 
          add = "reg.line", add.params = list(color = "blue", fill = "thistle3"),conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "July Max Temp (°C)", ylab = " Dryas Site Annual Growth Increment (cm)") 

######## ORDINARY LEAST SQUARES - ALL CONTROL SITES ########
julysept <- (axall$julyav+axall$augav+axall$junav+axall$sepav)/4)


#fit some OLS models
M1<-lm(augmx~DCrwi+DCbud*DCflr,data=axall,na.action=na.exclude) #fit the model
M1  # get the estimated coefficients for the model
anova(M1)   # get the Anova table for the model, including the F test
summary(M1) # get t-tests and some fit statistics for the model
axall$yhat.M1<-fitted(M1)  # the estimated y values
axall$resid.M1<-resid(M1)  # the errors

stargazer(M1, title="Regression Results",
          dep.var.labels=c("Overall Rating","High Rating"),
          omit.stat=c("LL","ser","f"), ci=TRUE, 
          ci.level=0.95, single.row=TRUE, type = "html",
          out="augdry.html",out.header = TRUE)




cor.test(axall$julyav, axall$yhat.M1,
          method = "pearson",
          conf.level = 0.95)





test.RE(axall$julyav[c(32:54)], axall$yhat.M1[c(32:54)])
test.ST(axall$julyav[c(32:54)], axall$yhat.M1[c(32:54)])
NSE(axall$julyav[c(32:54)], axall$yhat.M1[c(32:54)])

augmod <- -5.3301  + (axall$DCrwi*14.0673) +
  (axall$DCflr*-0.8418 ) + (axall$DCbud*-0.4929 ) + ((axall$DCflr*axall$CCbud)* 0.0196) 
  

par(mfrow=c(1,1))
plot(axall$year,axall$augmx,type="l",xlim=c(1988,2018),xlab = "Year", ylab = " August Maximum Air Temp (°C)")
points(axall$year,augmod,type="l",lty=3,lwd=2)

# get the normality test results
shapiro.test(axall$resid.M1) # Shapiro-Wilk normality test

# Use Breusch
require(car)
ncvTest(M1)#Pagan test for unequal variances

newAGI<-1
newvalues <- data.frame(CCrwi=newAGI) 




# get diagnostic plots
par(mfrow=c(2,2),mai=c(0.6,0.6,0.6,0.6),cex=0.7)
plot(axall$yhat.M1,axall$resid.M1, main="Model 1, Residual Plot",
     xlab="yhat", ylab="residual")
plot(axall$julyav,axall$yhat.M1, main="Model 11, Fitted line plot",
     ylab="yhat", xlab="july max")
qqnorm(axall$resid.M1, main="Model 1, Normality plot")
hist(axall$resid.M1, breaks =8 , density=10,col="green", border="black",
     main="Model 1, Error Distribution") 
par(mfrow=c(1,1),mai=c(1.0,1.0,1.0,1.0),cex=1.0)





########## CASS ONLY ###########

#fit some OLS models
M2<-lm(Thaug~CTrwi+CTflr*
         CTbud,data = axall, na.action=na.exclude) #fit the model
M2  # get the estimated coefficients for the model
anova(M2)   # get the Anova table for the model, including the F test
summary(M2) # get t-tests and some fit statistics for the model
axall$yhat.M2<-fitted(M2)  # the estimated y values
axall$resid.M2<-resid(M2)  # the errors
length(axall$yhat.M2)

stargazer(M2, title="Regression Results",
          dep.var.labels=c("Overall Rating","High Rating"),
          omit.stat=c("LL","ser","f"), ci=TRUE, 
          ci.level=0.95, single.row=TRUE, type = "html",
          out="julhobo.html",out.header = TRUE)

cor.test(axall$Thaug, axall$yhat.M2,
         method = "pearson",
         conf.level = 0.95)

sum((predict(M2) - mean(axall$julyav, na.rm = TRUE))^2);
ssr = sum((fitted(M2) - mtcars$mpg)^2)
SSR

RE <- 1 - 12.2339 / 0.6797
RE

cor.test(axall$julyav , axall$yhat.M4,
         method = "pearson",
         conf.level = 0.95)



newdata <- mydata[c(1,5:10)]

test.RE(axall$julyav[c(32:50)], axall$yhat.M2[c(32:50)])
test.ST(axall$julyav[c(32:50)], axall$yhat.M2[c(32:50)])
NSE(axall$julyav[c(32:50)], axall$yhat.M2[c(32:50)])

agi <- 2
newvalues <- data.frame(axccagi=agi) 
pred.clim.b <- predict(M2, newvalues, interval="confidence",level=0.95) 
pred.clim.b

plot(axall$year,axall$yhat.M2,type = 'l')

plot(axall$year,axall$Thjul,type="l",xlim=c(1985,2018), xlab = "Year", ylab = " July Average Surface Temp (°C)")
points(axall$year,axall$CCfullmodel,type="l",lty=3,lwd=2)
title="Cassiope Site"

axall$CCfullmodel<- (1.431 + (axall$CTrwi*  7.481 ) +(axall$CTflr*3.470) + 
                     (axall$CTbud*1.302  ) + ((axall$CTflr*axall$CTbud)*-2.158))

axall$CCrwionly<- (3.671  + (axall$CCrwi* 3.363 ))

plot(axall$year, axall$CCfullmodel, type='l', col = 'red')
points(axall$year,axall$julyav,type="l", col = 'blue')

# get the normality test results
shapiro.test(axall$resid.M2) # Shapiro-Wilk normality test

# Use Breusch
require(car)
ncvTest(M2)#Pagan test for unequal variances


# get 95% confidence intervals for the mean volume (i.e., the yhats)
# for every observation in the data
axall$CI.M2<- predict(M2, axall,interval="confidence", 
                      level=0.95)

rwi2<- 1
flr2<- 1
bud2<- 0

newvalues <- data.frame(CCrwi=rwi2, CCbud=flr2, CCflr=bud2) 

pred.clim.b <- predict(M2, newvalues, interval="confidence",level=0.95) 
pred.clim.b


# get diagnostic plots
par(mfrow=c(2,2),mai=c(0.6,0.6,0.6,0.6),cex=0.7)
plot(axall$yhat.M2,axall$resid.M2, main="Model 2, Residual Plot",
     xlab="yhat", ylab="residual")
plot(axall$julyav^(1/3),axall$yhat.M2, main="Model 2, Fitted line plot",
     ylab="yhat", xlab="july max")
qqnorm(axall$resid.M2, main="Model 2, Normality plot")
hist(axall$resid.M2, breaks =8 , density=10,col="green", border="black",
     main="Model 2, Error Distribution") 
par(mfrow=c(1,1),mai=c(1.0,1.0,1.0,1.0),cex=1.0)

########## FERT ONLY ###########
#fit some OLS models
M3<-lm(julyav~FCrwi+FCbud*FCflr,data=axall,na.action=na.exclude) #fit the model
M3  # get the estimated coefficients for the model
anova(M3)   # get the Anova table for the model, including the F test
summary(M3) # get t-tests and some fit statistics for the model
axall$yhat.M3<-fitted(M3)  # the estimated y values
axall$resid.M3<-resid(M3)  # the errors

plot(axall$year, axall$abijul,type="l",xlim=c(1962,2018),xlab = "Year", ylab = "July Average Temp (°C)")
points(axall$year,axall$yhat.M3,type="l",lty=3,lwd=2)

# get the normality test results
shapiro.test(axall$resid.M3) # Shapiro-Wilk normality test


agi <- 0.2
newvalues <- data.frame(FCrwi=agi) 
pred.clim.b <- predict(M3, newvalues, interval="confidence",level=0.95) 
pred.clim.b


# Use Breusch
require(car)
ncvTest(M3)#Pagan test for unequal variances


# get 95% confidence intervals for the mean volume (i.e., the yhats)
# for every observation in the data
axall$CI.M3<- predict(M3, axall,interval="confidence", 
                      level=0.95)

# get diagnostic plots
par(mfrow=c(2,2),mai=c(0.6,0.6,0.6,0.6),cex=0.7)
plot(axall$yhat.M3,axall$resid.M3, main="Model 3, Residual Plot",
     xlab="yhat", ylab="residual")
plot(axall$julyav^(1/3),axall$yhat.M3, main="Model 3, Fitted line plot",
     ylab="yhat", xlab="july max")
qqnorm(axall$resid.M3, main="Model 3, Normality plot")
hist(axall$resid.M3, breaks =8 , density=10,col="green", border="black",
     main="Model 3, Error Distribution") 
par(mfrow=c(1,1),mai=c(1.0,1.0,1.0,1.0),cex=1.0)

########## DRYAS ONLY ###########
#fit some OLS models
M4<-lm(julyav~DCrwi+DCbud*DCflr, data=axall,na.action=na.exclude) #fit the model
M4  # get the estimated coefficients for the model
anova(M4)   # get the Anova table for the model, including the F test
summary(M4) # get t-tests and some fit statistics for the model
axall$yhat.M4<-fitted(M4)  # the estimated y values
axall$resid.M4<-resid(M4)  # the errors

plot(axall$year,axall$type="l",xlim=c(1988,2018),xlab = "Year", ylab = " August Max Temp (°C)")
points(axall$year,axall$yhat.M4,type="l",lty=3,lwd=2)

axall$newcolumn<- (-7.7068 + (axall$DCrwi*13.2028) +(axall$DCflr*-0.7919) + 
                     (axall$DCbud * -0.1152) + ((axall$DCflr*axall$DCbud)* -0.2199))

plot(axall$Cflr, axall$yhat.m4)
points(axall$year,axall$augav,type="l",lty=3,lwd=2)

agi <- 0.2
newvalues <- data.frame(NCrwi=agi) 
pred.clim.b <- predict(M4, newvalues, interval="confidence",level=0.95) 
pred.clim.b


# get the normality test results
shapiro.test(axall$resid.M4) # Shapiro-Wilk normality test

# Use Breusch
require(car)
ncvTest(M4)#Pagan test for unequal variances


# get 95% confidence intervals for the mean volume (i.e., the yhats)
# for every observation in the data
axall$CI.M4<- predict(M4, axall,interval="confidence", 
                      level=0.95)

# get diagnostic plots
par(mfrow=c(2,2),mai=c(0.6,0.6,0.6,0.6),cex=0.7)
plot(axall$yhat.M4,axall$resid.M4, main="Model 4, Residual Plot",
     xlab="yhat", ylab="residual")
plot(axall$julyav^(1/3),axall$yhat.M4, main="Model 4, Fitted line plot",
     ylab="yhat", xlab="july av")
qqnorm(axall$resid.M4, main="Model 4, Normality plot")
hist(axall$resid.M4, breaks =8 , density=10,col="green", border="black",
     main="Model 4, Error Distribution") 
par(mfrow=c(1,1),mai=c(1.0,1.0,1.0,1.0),cex=1.0)


##### DISPLAY MODELS - AUGUST MAX #######
#get adj r
allr2 = M1$adj.r.squared
cassr2 = M2$adj.r.squared
fertr2 = M3$adj.r.squared
dryasr2 = M4$adj.r.squared
#get p-value
M1$coefficients
M2$coefficients
M3$coefficients
M4$coefficients
all.p = 0.02921
cass.p = 0.001024
fert.p = 0.1052
dryas.p = 0.4578



par(mfrow=c(2,2),mai=c(0.6,0.6,0.6,0.6),cex=0.7)
plot(axall$year,axall$augmx,type="l",xlim=c(1988,2018),xlab = "Year", ylab = " August Max Temp (°C)")
points(axall$year,axall$yhat.M1,type="l",lty=3,lwd=2)

 plot(axall$year,axall$augmx,type="l",xlim=c(1988,2018),xlab = "Year", ylab = " August Max Temp (°C)")
points(axall$year,axall$yhat.M2,type="l",lty=3,lwd=2)

plot(axall$year,axall$augmx,type="l",xlim=c(1988,2018),xlab = "Year", ylab = " August Max Temp (°C)")
points(axall$year,axall$yhat.M3,type="l",lty=3,lwd=2)

plot(axall$year,axall$augmx,type="l",xlim=c(1988,2018),xlab = "Year", ylab = " August Max Temp (°C)")
points(axall$year,axall$yhat.M4,type="l",lty=3,lwd=
