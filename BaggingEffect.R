## Bagging effect ##

dat<- read.csv("Amaranthus Male and Female Harvest Combined Data v4.csv")
dat<- subset(dat, Sex=="M" | Sex=="F") # remove the group containing sex=NA
dat$Date<- as.character(dat$Date)
dat$Date<- as.Date(dat$Date) # change data format

# add lat
lat<- read.csv("Population Locations.csv") # read in lat datasheet
lat<- dplyr::select(lat, Population, Latitude)
library(plyr)
dat <- join(dat, lat, by = c("Population")) # add lat col
dat <- transform(dat, Population=reorder(Population, Latitude)) # order pop'n by lat

# Inflo sum & count
# remove plants that did not have inflos recorded
fulldat<- dat[!is.na(dat$Base.Stem.Diameter),]
fulldat$inflo.sum<- apply(fulldat[,42:592], 1, FUN=sum, na.rm=TRUE) # sum (i.e. total reproductive structure)
###NOTE: this function will not work if there are other objects with this name. If error occurs, clear environment
fulldat$inflo.num <- 551-(apply(fulldat[,42:592], 1, function(x) sum(is.na(x)))) # num inflos

# branch sum
fulldatbranches<-fulldat[,c(1:4,6,7,12:39)]
fulldatbranches[is.na(fulldatbranches)]<- 0
names(fulldatbranches) # this doesn't have lat
fulldatbranches$branch.sum<- apply(fulldatbranches[,7:32], 1, FUN=sum)
fulldatbranches$Latitude<- fulldat$Latitude # re-add lat
str(fulldatbranches)

# inconsistent axillary data collection, remove data before May 9th
newdat<- dat[dat$Date >"2018-05-09",] # remove dates (before 2018-05-09)
str(newdat)


library(Rmisc)
library(ggplot2)
##########################################
##OG graphs with bagging added as factor##
##########################################
# HEIGHT
BHtsum <- summarySE(dat, measurevar="Primary.Stem.Ht.3", groupvars=c("Sex", "Bag", "Population"), na.rm=TRUE) # summary statistics of height
# plot based on summary stats above (w/ standard error)
BHtplot<- ggplot(BHtsum, aes(x=Population, y=Primary.Stem.Ht.3, size=I(0.5), shape=(Sex), colour=(Bag)), pch=c("17","19"),na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Primary.Stem.Ht.3-se, ymax=Primary.Stem.Ht.3+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Height (cm)", shape="Sex") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0,1,0,0), "cm")
  )



# DIAMETER
BdimphDiamsum <- summarySE(dat, measurevar="Base.Stem.Diameter", groupvars=c("Sex", "Bag", "Population"), na.rm=TRUE) # summary statistics of diameter
# plot based on summary stats above (w/ standard error)
BDiamplot<- ggplot(BdimphDiamsum, aes(x=Population, y=Base.Stem.Diameter, size=I(0.5), shape=factor(Sex), colour=(Bag)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Base.Stem.Diameter-se, ymax=Base.Stem.Diameter+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Stem\nDiameter (cm)", shape="Sex") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )



# AXILLARY INFLOS
BdimphAxsum <- summarySE(newdat, measurevar="Num.Axillary.Flws", groupvars=c("Sex", "Bag", "Population"), na.rm=TRUE) # summary statistics of ax inflos
# plot based on summary stats above (w/ standard error)
BAxplot<- ggplot(BdimphAxsum, aes(x=Population, y=Num.Axillary.Flws, size=I(0.5), shape=factor(Sex), colour=(Bag)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Num.Axillary.Flws-se, ymax=Num.Axillary.Flws+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Number of Axillary\nInflorescences", shape="Sex") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )



# INFLOS
## sum
BdimphInfSumsum <- summarySE(fulldat, measurevar="inflo.sum", groupvars=c("Sex", "Bag","Population"), na.rm=TRUE) # summary statistics of inflo sum
# plot based on summary stats above (w/ standard error)
BInfSumplot<- ggplot(BdimphInfSumsum, aes(x=Population, y=inflo.sum, size=I(0.5), shape=factor(Sex), colour=(Bag)), pch=c("17","18"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=inflo.sum-se, ymax=inflo.sum+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Total \n Inflorescence\nLength (cm)", shape="Sex") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )



## num
BdimphInfNumsum <- summarySE(fulldat, measurevar="inflo.num", groupvars=c("Sex", "Bag","Population"), na.rm=TRUE) # summary statistics of inflo num
# plot based on summary stats above (w/ standard error)
BInfNumplot<- ggplot(BdimphInfNumsum, aes(x=Population, y=inflo.num, size=I(0.5), shape=factor(Sex), colour=(Bag)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=inflo.num-se, ymax=inflo.num+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Number of\nInflorescences", shape="Sex") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )


# BRANCHES
BdimphBranchsum <- summarySE(fulldatbranches, measurevar="branch.sum", groupvars=c("Sex","Bag","Population"), na.rm=TRUE) # summary statistics of branch sum
# plot based on summary stats above (w/ standard error)
BBranchplot<- ggplot(BdimphBranchsum, aes(x=Population, y=branch.sum, size=I(0.5), shape=factor(Sex),colour=(Bag)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=branch.sum-se, ymax=branch.sum+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Total Branch\nLength (cm)", shape="Sex") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )

# composite figure

library(sjPlot)
library(cowplot)
library(gridExtra)
library(grid)
Bdimphplot <- plot_grid(BHtplot + theme(legend.position="none", axis.title.x=element_blank()),
                       BDiamplot + theme(legend.position="none", axis.title.x=element_blank()),
                       BAxplot + theme(legend.position="none", axis.title.x=element_blank()),
                       BInfSumplot + theme(legend.position="none", axis.title.x=element_blank()),
                       BInfNumplot + theme(legend.position="none", axis.title.x=element_blank()),
                       BBranchplot + theme(legend.position="none", axis.title.x=element_blank()),
                       align = 'vh',
                       labels = c("A","B","C","D","E","F"),
                       hjust = -1,
                       nrow = 3
)

legend<- get_legend(BHtplot+theme(legend.justification = "top"))
x.grob <- textGrob("Population", 
                   gp=gpar(fontface="bold", fontsize=12))

Bdimphplotcomp <- plot_grid(Bdimphplot, legend, x.grob, rel_widths = c(7, 0.5), rel_heights = c(6,1))
Bdimphplotcomp

##############################################
##Female only graphs (bagging vs no bagging)##
##############################################

Fdat<- subset(dat, Sex=="F") # subset data to contain F only (note: looking at str(Fdat), it does not appear that M are removed but comparing means shows they are different)
Fnewdat<- subset(newdat, Sex=="F")
Ffulldat<- subset(fulldat, Sex=="F")
Ffulldatbranches<- subset(fulldatbranches, Sex=="F")


# HEIGHT
FHtsum <- summarySE(Fdat, measurevar="Primary.Stem.Ht.3", groupvars=c("Bag", "Population"), na.rm=TRUE) # summary statistics of height
FHtmean<- summarySE(Fdat, measurevar="Primary.Stem.Ht.3", groupvars=c("Bag"), na.rm=TRUE) # mean of all pop'ns
FHtmean$Population<- as.factor("mean")
FHtmean2<- FHtmean[,c("Bag","Population", "N", "Primary.Stem.Ht.3", "sd", "se", "ci")] # reorder
FHtsum[45:46,]<- FHtmean2 # add to pop'n means dataset
# plot based on summary stats above (w/ standard error)
FHtplot<- ggplot(FHtsum, aes(x=Population, y=Primary.Stem.Ht.3, size=I(0.5), colour=(Bag)),na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Primary.Stem.Ht.3-se, ymax=Primary.Stem.Ht.3+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Height (cm)", colour="Bag") +
  geom_vline(xintercept=22.5,colour="black",linetype="dashed") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0,1,0,0), "cm")
  )



# DIAMETER
FdimphDiamsum <- summarySE(Fdat, measurevar="Base.Stem.Diameter", groupvars=c("Bag", "Population"), na.rm=TRUE) # summary statistics of diameter
# plot based on summary stats above (w/ standard error)
FDiammean<- summarySE(Fdat, measurevar="Base.Stem.Diameter", groupvars=c("Bag"), na.rm=TRUE) # mean of all pop'ns
FDiammean$Population<- as.factor("mean")
FDiammean<- FDiammean[,c("Bag","Population", "N", "Base.Stem.Diameter", "sd", "se", "ci")] # reorder
FdimphDiamsum[45:46,]<- FDiammean # add to pop'n means dataset
FDiamplot<- ggplot(FdimphDiamsum, aes(x=Population, y=Base.Stem.Diameter, size=I(0.5), colour=(Bag)), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Base.Stem.Diameter-se, ymax=Base.Stem.Diameter+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Stem\nDiameter (cm)", colour="Sex") +
  geom_vline(xintercept=22.5,colour="black",linetype="dashed") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )



# AXILLARY INFLOS
FdimphAxsum <- summarySE(Fnewdat, measurevar="Num.Axillary.Flws", groupvars=c("Bag", "Population"), na.rm=TRUE) # summary statistics of ax inflos
# plot based on summary stats above (w/ standard error)
FAxmean<- summarySE(Fnewdat, measurevar="Num.Axillary.Flws", groupvars=c("Bag"), na.rm=TRUE) # mean of all pop'ns
FAxmean$Population<- as.factor("mean")
FAxmean<- FAxmean[,c("Bag", "Population", "N", "Num.Axillary.Flws", "sd", "se", "ci")] # reorder
FdimphAxsum[45:46,]<- FAxmean # add to pop'n means dataset
FAxplot<- ggplot(FdimphAxsum, aes(x=Population, y=Num.Axillary.Flws, size=I(0.5), colour=(Bag)), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Num.Axillary.Flws-se, ymax=Num.Axillary.Flws+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Number of Axillary\nInflorescences", colour="Bag") +
  geom_vline(xintercept=22.5,colour="black",linetype="dashed") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )
#### NA level in bagging???


# INFLOS
## sum
FdimphInfSumsum <- summarySE(Ffulldat, measurevar="inflo.sum", groupvars=c("Bag","Population"), na.rm=TRUE) # summary statistics of inflo sum
# plot based on summary stats above (w/ standard error)
FInfSummean<- summarySE(Ffulldat, measurevar="inflo.sum", groupvars=c("Bag"), na.rm=TRUE)
FInfSummean$Population<- as.factor("mean")
FInfSummean<- FInfSummean[,c("Bag", "Population", "N", "inflo.sum", "sd", "se", "ci")] # reorder
FdimphInfSumsum[45:46,]<- FInfSummean # add to pop'n means dataset
FInfSumplot<- ggplot(FdimphInfSumsum, aes(x=Population, y=inflo.sum, size=I(0.5), colour=(Bag)), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=inflo.sum-se, ymax=inflo.sum+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Total \n Inflorescence\nLength (cm)", colour="Bag") +
  geom_vline(xintercept=22.5,colour="black",linetype="dashed") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )



## num
FdimphInfNumsum <- summarySE(Ffulldat, measurevar="inflo.num", groupvars=c("Bag","Population"), na.rm=TRUE) # summary statistics of inflo num
# plot based on summary stats above (w/ standard error)
FInfNummean<- summarySE(Ffulldat, measurevar="inflo.num", groupvars=c("Bag"), na.rm=TRUE)
FInfNummean$Population<- as.factor("mean")
FInfNummean<- FInfNummean[,c("Bag", "Population", "N", "inflo.num", "sd", "se", "ci")] # reorder
FdimphInfNumsum[45:46,]<- FInfNummean # add to pop'n means dataset
FInfNumplot<- ggplot(FdimphInfNumsum, aes(x=Population, y=inflo.num, size=I(0.5), colour=(Bag)), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=inflo.num-se, ymax=inflo.num+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Number of\nInflorescences", colour="Bag") +
  geom_vline(xintercept=22.5,colour="black",linetype="dashed") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )


# BRANCHES
FdimphBranchsum <- summarySE(Ffulldatbranches, measurevar="branch.sum", groupvars=c("Bag","Population"), na.rm=TRUE) # summary statistics of branch sum
# plot based on summary stats above (w/ standard error)
FBranchmean<- summarySE(Ffulldatbranches, measurevar="branch.sum", groupvars=c("Bag"), na.rm=TRUE)
FBranchmean$Population<- as.factor("mean")
FBranchmean<- FBranchmean[,c("Bag", "Population", "N", "branch.sum", "sd", "se", "ci")] # reorder
FdimphBranchsum[45:46,]<- FBranchmean # add to pop'n means dataset
FBranchplot<- ggplot(FdimphBranchsum, aes(x=Population, y=branch.sum, size=I(0.5), colour=(Bag)), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=branch.sum-se, ymax=branch.sum+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Total Branch\nLength (cm)", colour="Bag") +
  geom_vline(xintercept=22.5,colour="black",linetype="dashed") +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    plot.margin=unit(c(0.5,1,0,0), "cm")
  )

# composite figure

library(sjPlot)
library(cowplot)
library(gridExtra)
library(grid)
Fdimphplot <- plot_grid(FHtplot + theme(legend.position="none", axis.title.x=element_blank()),
                       FDiamplot + theme(legend.position="none", axis.title.x=element_blank()),
                       FAxplot + theme(legend.position="none", axis.title.x=element_blank()),
                       FInfSumplot + theme(legend.position="none", axis.title.x=element_blank()),
                       FInfNumplot + theme(legend.position="none", axis.title.x=element_blank()),
                       FBranchplot + theme(legend.position="none", axis.title.x=element_blank()),
                       align = 'vh',
                       labels = c("A","B","C","D","E","F"),
                       hjust = -1,
                       nrow = 3
)

legend<- get_legend(FHtplot+theme(legend.justification = "top"))
x.grob <- textGrob("Population", 
                   gp=gpar(fontface="bold", fontsize=12))

Fdimphplotcomp <- plot_grid(Fdimphplot, legend, x.grob, rel_widths = c(7, 0.5), rel_heights = c(6,1))
Fdimphplotcomp
