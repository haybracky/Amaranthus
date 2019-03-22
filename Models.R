## DIMORPHISM MODELS ##

library(dplyr)
# DIMORPHISM DATA
dimph<- read.csv("Amaranthus Male and Female Harvest Combined Data v4.csv")
dimph<- subset(dimph, Sex=="M" | Sex=="F") # remove the group containing sex=NA
dimph$Date<- as.character(dimph$Date)
dimph$Date<- as.Date(dimph$Date) # change data format
str(dimph)
names(dimph)

# add lat
library(plyr)
dimph <- join(dimph, lat, by = c("Population")) # add lat col
dimph <- transform(dimph, Population=reorder(Population, Latitude)) # order pop'n by lat

# Inflo sum & count
# remove plants that did not have inflos recorded
fulldimph<- dimph[!is.na(dimph$Base.Stem.Diameter),]
names(fulldimph)
fulldimph$inflo.sum<- apply(fulldimph[,42:592], 1, FUN=sum, na.rm=TRUE) # sum (i.e. total reproductive structure)
fulldimph$inflo.num <- 551-(apply(fulldimph[,42:592], 1, function(x) sum(is.na(x)))) # num inflos

# branch sum
fulldimphbranches<-fulldimph[,c(1:3,6,12:39)]
fulldimphbranches[is.na(fulldimphbranches)]<- 0
names(fulldimphbranches)
fulldimphbranches$branch.sum<- apply(fulldimphbranches[,7:32], 1, FUN=sum)

# MIXED MODELS
library(car)
library(MASS)
library(lme4)
library(sjPlot)

# height
# note: data normal
# step function to find best model
Htlmm<- lmer(Primary.Stem.Ht.3 ~ Sex * Latitude + Sex + Latitude + (1 | Population), data=dimph, REML=F)
step(Htlmm)
Htlmm1 <- lmer(Primary.Stem.Ht.3 ~ Sex + (1 | Population), data = dimph, REML=F) # best model
summary(Htlmm1)
anova(Htlmm1) # model statistics
Htlmm3<- lmer(Primary.Stem.Ht.3 ~ 1 + (1 | Population), data = dimph, REML=F)
anova(Htlmm1, Htlmm3) # model comparison
Htlmm2<- lmer(Primary.Stem.Ht.3 ~ Sex * (1 | Population), data = dimph, REML=F)
summary(Htlmm2)
anova(Htlmm1,Htlmm2) # no statistical diff btwn models
Htlmm4<- lmer(Primary.Stem.Ht.3 ~ Sex * Latitude + (1 | Population), data = dimph, REML=F)
anova(Htlmm1,Htlmm4)
Htlmm5<- lmer(Primary.Stem.Ht.3 ~ Sex + (Sex | Population), data=dimph, REML=F)
anova(Htlmm5, Htlmm1) # no effect of Sex*Population interaction
# no signif effect of lat
# test assumptions
plot_model(Htlmm1, type="diag")
# assumptions met

# stem diam
# note: data normal
# step function to find best model
Diamlmm<- lmer(Base.Stem.Diameter ~ Sex * Latitude + Sex + Latitude + (1 | Population), data=dimph, REML=F)
step(Diamlmm)
# did not work
Diamlmm1 <- lmer(Base.Stem.Diameter ~ Sex + (1 | Population), data = dimph, REML=F) # best model
summary(Diamlmm1)
Diamlmm2<- lmer(Base.Stem.Diameter ~ Sex * (1 | Population), data = dimph, REML=F)
summary(Diamlmm2)
anova(Diamlmm1, Diamlmm2) # no difference
# note: random effect variance close to 0, use linear model?
anova(Diamlmm1) # model statistics
Diamlmm4<- lmer(Base.Stem.Diameter ~ 1 + (1 | Population), data = dimph, REML=F)
anova(Diamlmm1, Diamlmm4) # model comparison
Diamlmm5<- lmer(Base.Stem.Diameter ~ Sex * Latitude + (1 | Population), data = dimph, REML=F)
summary(Diamlmm5)
anova(Diamlmm1, Diamlmm5)
# lat not signif
Diamlmm6<- lmer(Base.Stem.Diameter ~ Sex + (Sex | Population), data=dimph, REML=F)
anova(Diamlmm1, Diamlmm6) # no different
# test assumptions
plot_model(Diamlmm1, type="diag")
# residuals not normal, two peaks
Diamlmm3<- lmer(Base.Stem.Diameter ~ (1|Population)+(1|Sex), data=dimph, REML=F)
summary(Diamlmm3)

# axillaries w/ transformation
# note: data not normal, diff transformations tried
# step function to find best model
# sqrt transform:
Axlmm1<- lmer(sqrt(Num.Axillary.Flws) ~ Sex + (1 | Population), data=newdimph, REML=F)
summary(Axlmm1)
Axlmm2<- lmer(sqrt(Num.Axillary.Flws) ~ Sex * (1 | Population), data=newdimph, REML=F)
summary(Axlmm2)
anova(Axlmm1, Axlmm2) # no difference
# test assumptions
plot_model(Axlmm1, type="diag")
# does not meet well
# cube root transform
Axlmm<- lmer((Num.Axillary.Flws)^(1/3) ~ Sex * Latitude + Sex + Latitude + (1 | Population), data=newdimph, REML=F)
step(Axlmm)
Axlmm3<- lmer((Num.Axillary.Flws)^(1/3) ~ Sex + (1 | Population), data=newdimph, REML=F) # best model
summary(Axlmm3)
anova(Axlmm1, Axlmm3)
Axlmm5<- lmer((Num.Axillary.Flws)^(1/3) ~ Sex * Latitude + (1 | Population), data=newdimph, REML=F)
summary(Axlmm5)
anova(Axlmm3, Axlmm5)
# lat not signif
Axlmm6<- lmer((Num.Axillary.Flws)^(1/3) ~ Sex + (Sex | Population), data=newdimph, REML=F) # warning message: model failed to converge
anova(Axlmm3, Axlmm6) # not different
plot_model(Axlmm3, type="diag")
# does not meet well
# test statistics
anova(Axlmm3)
Axlmm4<- lmer((Num.Axillary.Flws)^(1/3) ~ 1 + (1 | Population), data=newdimph, REML=F)
anova(Axlmm3, Axlmm4) # model comparision

# inflo num w/ transformation
# step function to find best model
InfNumlmm<- lmer(sqrt(inflo.num) ~ Sex * Latitude + Sex + Latitude + (1 | Population), data=fulldimph, REML=F)
step(InfNumlmm)
InfNumlmm1<- lmer(sqrt(inflo.num) ~ Sex + (1 | Population), data=fulldimph, REML=F) # best model
summary(InfNumlmm1)
anova(InfNumlmm1) # model stats
InfNumlmm4<- lmer(sqrt(inflo.num) ~ 1 + (1 | Population), data=fulldimph, REML=F)
anova(InfNumlmm1, InfNumlmm4) # model comparison
InfNumlmm2<- lmer(sqrt(inflo.num) ~ Sex * (1 | Population), data=fulldimph, REML=F)
summary(InfNumlmm2)
anova(InfNumlmm1, InfNumlmm2) # no difference
InfNumlmm5<- lmer(sqrt(inflo.num) ~ Sex * Latitude + (1 | Population), data=fulldimph, REML=F)
summary(InfNumlmm5)
anova(InfNumlmm1, InfNumlmm5)
# lat not signif
InfNumlmm6<- lmer(sqrt(inflo.num) ~ Sex + (Sex | Population), data=fulldimph, REML=F)
anova(InfNumlmm6, InfNumlmm1) # not different
# test assumptions
plot_model(InfNumlmm1, type="diag")
# residuals not normal, peak too high

# inflo sum w/ transformation
# step function to find best model
InfSumlmm<- lmer(sqrt(inflo.sum) ~ Sex * Latitude + Sex + Latitude + (1 | Population), data=fulldimph, REML=F)
step(InfSumlmm) 
InfSumlmm1<- lmer(sqrt(inflo.sum) ~ Sex + (1 | Population), data=fulldimph, REML=F) # best model
summary(InfSumlmm1)
anova(InfSumlmm1)# model stats
InfSumlmm3<- lmer(sqrt(inflo.sum) ~ 1 + (1 | Population), data=fulldimph, REML=F)
anova(InfSumlmm1, InfSumlmm3) # model comparison
InfSumlmm2<- lmer(sqrt(inflo.sum) ~ Sex * (1 | Population), data=fulldimph, REML=F)
summary(InfSumlmm2)
anova(InfSumlmm1, InfSumlmm2)
InfSumlmm5<- lmer(sqrt(inflo.sum) ~ Sex + (Sex | Population), data=fulldimph, REML=F) # warning message: model failed to converge
anova(InfSumlmm5, InfSumlmm1) # not different
InfSumlmm4<- lmer(sqrt(inflo.sum) ~ Sex * Latitude + (1 | Population), data=fulldimph, REML=F)
summary(InfSumlmm4)
anova(InfSumlmm1, InfSumlmm4) # lat not signif
# test assumptions
plot_model(InfSumlmm1, type="diag")
# assumptions met well

# branch sum w/ transofmration
# step function to find best model
Branchlmm<- lmer(branch.sum ~ Sex * Latitude + Sex + Latitude + (1 | Population), data=fulldimph, REML=F)
step(Branchlmm)
# did not work
Branchlmm1<- lmer(branch.sum ~ Sex + (1 | Population), data=fulldimph, REML=F) # best model
summary(Branchlmm1)
Branchlmm2<- lmer(branch.sum ~ Sex * (1 | Population), data=fulldimph, REML=F)
summary(Branchlmm2)
anova(Branchlmm1, Branchlmm2)
Branchlmm4<- lmer(branch.sum ~ Sex * Latitude + (1 | Population), data=fulldimph, REML=F)
summary(Branchlmm4)
anova(Branchlmm1, Branchlmm4) # lat not signif
anova(Branchlmm1) # model stats
Branchlmm3<- lmer(branch.sum ~ 1 + (1 | Population), data=fulldimph, REML=F)
anova(Branchlmm1, Branchlmm3) # model comparison
# test assumptions
plot_model(Branchlmm1, type="diag")
# test statistics
# these are not well met b/c lots of zero data
## make 2 glmm's with binary zero & continuous non-zero data
nzdimph<- fulldimphbranches
nzdimph$nzbranch<- ifelse(nzdimph$branch.sum>0, 1, 0) # make col of zero vs non-zero data
# zero data model:
zibranch1 <- glmer(nzbranch ~ Sex + (1 | Population), data = nzdimph, family = binomial(link = logit)) # model of zero data
zibranch3<- glmer(nzbranch ~ Sex + (Sex | Population), data=nzdimph, family = binomial(link = logit)) # test Sex * Pop
anova(zibranch3, zibranch1) # no different
summary(zibranch1)
anova(zibranch1) # model stats
zibranchnull<- glmer(nzbranch ~ 1 + (1|Population), data = nzdimph, family = binomial(link = logit)) # null model
anova(zibranchnull, zibranch1) # model comparison
# non-zero data model:
zibranch2 <- glmer(branch.sum ~ Sex + (1 | Population), data = subset(nzdimph, nzbranch == 1), family = Gamma(link = log))
zibranch4<- glmer(branch.sum ~ Sex + (Sex | Population), data=subset(nzdimph, nzbranch ==1), family = Gamma(link=log))
anova(zibranch2, zibranch4) # interaction not significant
summary(zibranch2)
anova(zibranch2) # model stats
zibranchnull2<- glmer(branch.sum ~ 1 + (1 | Population), data=subset(nzdimph, nzbranch == 1), family = Gamma(link=log)) # null model
anova(zibranchnull2, zibranch2) # model comparison
