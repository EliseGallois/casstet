############################################################
# Exercise 5_Mixed2_repeated in time.R
# People are measured over time.  The model includes
# age and gender (factor) as fixed effects. 
# Several ways of dealing with the time series within
# Person are tested. 
# Documentation given in SAS: 
# The following data are from Pothoff and Roy (1964) 
# and consist of growth measurements for 11 girls and 
# 16 boys at ages 8, 10, 12, and 14. Some of the observations 
# are suspect (for example, the third observation for person 20); 
# however, all of the data are used here for comparison purposes. 
############################################################

rm(list=ls(all=TRUE))

setwd("/Volumes/Seagate Backup Plus Drive/")

thedata<-read.csv("lme.csv")

new<-read.csv("mixedraw.csv")
phen<-read.csv("lmphen.csv")

library(esquisse)
library(lme4)
library(ggplot2)
brew install pkg-config
install.packages("gdtools", type = "source")
devtools::install_github("dreamRs/esquisse")


m1n <- lme(y ~ Clim, random=~1|Year, cor=corAR1(), 
           data = new, method = "ML", control=list(maxIter=1000), 
           na.action=na.exclude) 

m1n <- lme(y ~ Clim, random=~1|Year 
           |Site/Exp, cor=corAR1(), 
           data = new, method = "ML", 
           control=list(maxIter=1000),na.action=na.exclude)


mNULLn <- lme(y ~ 1, random=~1|Year, cor=corAR1(), 
              data = new, method = "ML", 
              control=list(maxIter=1000), na.action = na.exclude)

plot(m1n)


fit_QHI_model_pred <- as.data.frame(predict.MCMCglmm(m1n, y, interval = "prediction"))
fit_QHI_model <- cbind.data.frame(Year = QHI_shrub_detrend$Year, IndivUN = QHI_shrub_detrend$IndivUN, detrend_rw = QHI_shrub_detrend$detrend_rw,
                                  detrend_ndvi = QHI_shrub_detrend$detrend_ndvi, fit = fit_QHI_model_pred$fit, lwr = fit_QHI_model_pred$lwr, upr = fit_QHI_model_pred$upr) %>% arrange(desc(detrend_ndvi)) %>% filter(row_number() %% 10 == 0)


#coding club code

basic.lm <- lm(y ~ Exp, data = new, na.action=na.exclude)
summary(basic.lm)
(prelim_plot <- ggplot(new, aes(x = Clim, y = y)) +
    geom_point() +
    geom_smooth(method = "lm"))
plot(basic.lm, which = 1)
plot(basic.lm, which = 2)
boxplot(y ~ Exp, data = new)
(colour_plot <- ggplot(new, aes(x = Exp, y = y, colour = Site)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

(split_plot <- ggplot(aes(Exp, y), data = new) + 
    geom_point() + 
    facet_wrap(~ Exp) + 
    xlab("length") + 
    ylab("test score"))

mountain.lm <- lm(y ~ Exp + Region, data = new)
summary(mountain.lm)

library(lme4)

mixed.lmer <- lmer(y ~ Exp + (1|Site), data = new,na.action=na.exclude)
summary(mixed.lmer)
anova(mixed.lmer)
plot(mixed.lmer)  # looks alright, no paterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))
summary(mixed.lmer)

# % explained by dif in region 61%
# % explained by dif in site 38%
# % explained by dif in experiment 11%

0.0305 /( 0.0305  +1.5189  )


new2 <- within(new, sample <- factor(Region:Site:Exp))
mixed.lmer2 <- lmer(y ~ Clim + (1|Region) + (1|Site),  data = new, na.action = na.exclude)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)

setTimeLimit();
p <- (ggplot(new, aes(x = Clim, y = y, colour = Exp)) +
    facet_wrap(~Region+Site, nrow=3)
              +
      geom_line(aes(y = dogpred, colour = Exp))  +
    geom_point() +
    theme_classic() +
        theme(legend.position = "top"))
print(p)
p <- p +labs(x = "July Mean Temp (°C)", y = "Annual Stem Growth [mm]")
p

lattice::xyplot(y~Clim | Site, groups=Exp, data=new, type=c('p','r'), auto.key=F)



library(broom.mixed)
library(dotwhisker)
dwplot(list(first=model,second=model2), effects="fixed")+
  geom_vline(xintercept=0, lty=2)




ggplot(new, aes(x = Clim, y = y, colour = Exp)) +
    facet_wrap(~Region+Site, nrow=3)+
    geom_line(aes(y = dogpred, colour = Exp))  +
    geom_point() +
    theme_classic() +
    theme(legend.position = "top")

new$dogpred<-fitted(dog,level=0)  # estimated population averaged yhat


ggplot(dog) +
  aes(x = Clim, y = y, colour = Exp)+
  facet_wrap(~Region+Site, nrow=3)+
  geom_point(size = 1L) +
  geom_line(data = cbind(new, pred = predict(dog)), 
            aes(y = pred))    +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  facet_wrap(vars(Site))


library(sjPlot)



p_model_comparison

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

setSessionTimeLimit(cpu = Inf, elapsed = Inf)

library(stargazer)
stargazer(dog, type = "html",out="test.html",out.header = TRUE,
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
stargazer(dog, dognull, title="Regression Results",
          dep.var.labels=c("Overall Rating","High Rating"),
          keep.stat="n", ci=TRUE, ci.level=0.90, single.row=TRUE,
          type = "html",out="test2.html",out.header = TRUE)

cat <- lme(y~Clim, 
    data=new, random=~1|Region, 
    na.action = na.exclude)

dognull = lmer(y ~ Clim +
             (1|Region) + (1|Site), data=new,
           na.action=na.exclude, REML = FALSE)

new2 <- subset(new, Region == "Alaska")

dog = lmer(y ~ Clim + Exp +
       (1|Region) + (1|Site), data=new,
       na.action=na.exclude)
var(coef(dog))

fox = lmer(y ~ Clim + Exp + (1|Site), data=new2,
           na.action=na.exclude)
var(coef(fox))


stargazer(dog, title="Results", align=TRUE, type="html")
anova(dognull,dog)


p <-  ggplot(new) + 
  aes(x = Clim, y = y, color = Exp) +
  stat_smooth(method = "lm", se = TRUE) +
  # Put the points on top of lines
  geom_point() +
  scale_color_brewer("Treatment", labels = c("Control", "Warming"),
                     direction = -1, palette = "Set1") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black")) +
  facet_wrap(~Region+Site, nrow=3) 
 
p <- p +labs(x = "July Mean Temp (°C)", y = "Annual Stem Growth [mm]")

p


############# PHENOLOGY LME ############
#one model with site as a random effect#
pheno = lmer(upr ~   C.T + clim +
             (1|Site), data=phen,
           na.action=na.exclude) 
pheno
var(coef(pheno))
summary(pheno)
anova(phenonull,pheno)

p <-  ggplot(phen) + 
  aes(x = Year, y = flw_pk, color = C.T) +
  stat_smooth(method = "lm", se = TRUE) +
  # Put the points on top of lines
  geom_point() +
  scale_color_brewer("Treatment", labels = c("Control", "Warming"),
                     direction = -1, palette = "Set1") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black")) +
  facet_wrap(~Site, nrow=3) 

p <- p +labs(x = "July Mean Temp (°C)", y = "Annual Stem Growth [mm]")

p

phenonull = lmer(upr ~   C.T + 
               (1|Site), data=phen,
             na.action=na.exclude, REML = FALSE)
