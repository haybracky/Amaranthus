### Thoroughly annotated data manipulation, graphs, and models ###


#######################
## DATA MANIPULATION ##
#######################


library(dplyr)

## DIMORPHISM DATA
dimph<- read.csv("Amaranthus Male and Female Harvest Combined Data v4.csv") # load full version of dimorphism data
dimph<- subset(dimph, Sex=="M" | Sex=="F") # remove the group containing sex=NA
dimph$Date<- as.character(dimph$Date) # make date a character
dimph$Date<- as.Date(dimph$Date) # change date format
str(dimph)
names(dimph)

# add lat
lat<- read.csv("Population Locations.csv") # read in lat datasheet
lat<- dplyr::select(lat, Population, Latitude) # select the latitude and pop'n cols from lat dataset
library(plyr)
dimph <- join(dimph, lat, by = c("Population")) # add lat col to dimorph dataset
dimph <- transform(dimph, Population=reorder(Population, Latitude)) # order pop'n by lat

# Inflo sum & count
fulldimph<- dimph[!is.na(dimph$Base.Stem.Diameter),] # remove plants that did not have inflos recorded
names(fulldimph) # find what cols have inflo lengths recorded
fulldimph$inflo.sum<- apply(fulldimph[,42:592], 1, FUN=sum, na.rm=TRUE) # calculate sum (i.e. total reproductive structure)
###NOTE: this function will not work if there are other objects with this name. If error occurs, clear environment
fulldimph$inflo.num <- 551-(apply(fulldimph[,42:592], 1, function(x) sum(is.na(x)))) # calculate num inflos 
# (551 = number of cols w/ inflo length recorded; subtract number of columns that do not contain measurements)

# branch sum
fulldimphbranches<-fulldimph[,c(1:3,6,12:39)] # select the cols needed
fulldimphbranches[is.na(fulldimphbranches)]<- 0 # replace NAs w/ 0 b/c only branches >20cm recorded
names(fulldimphbranches) # this doesn't have lat
fulldimphbranches$branch.sum<- apply(fulldimphbranches[,7:32], 1, FUN=sum) # calculate branch sum
fulldimphbranches$Latitude<- fulldimph$Latitude # re-add lat
# remember, only branches >20cm recorded

# inconsistent axillary data collection, remove data before May 9th
newdimph<- dimph[dimph$Date >"2018-05-09",] # remove dates (before 2018-05-09)
str(newdimph)



library(Rmisc)
library(ggplot2)

#########################################
## Dimorphism graphs - factored by SEX ##
#########################################


# HEIGHT
dimphHtsum <- summarySE(dimph, measurevar="Primary.Stem.Ht.3", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of height
Htmean<- summarySE(dimph, measurevar="Primary.Stem.Ht.3", groupvars=c("Sex"), na.rm=TRUE) # mean of all pop'ns
Htmean$Population<- as.factor("mean") # change mean to factor to add to data
Htmean2<- Htmean[,c("Sex", "Population", "N", "Primary.Stem.Ht.3", "sd", "se", "ci")] # reorder
dimphHtsum[45:46,]<- Htmean2 # add to pop'n means dataset
# plot based on summary stats above (w/ standard error)
Htplot<- ggplot(dimphHtsum, aes(x=Population, y=Primary.Stem.Ht.3, size=I(0.5), shape=factor(Sex)), pch=c("17","19"),na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Primary.Stem.Ht.3-se, ymax=Primary.Stem.Ht.3+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Height (cm)", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
  geom_vline(xintercept=22.5,colour="black",linetype="dashed") +
  annotate(geom="text", label="Mean\nSex\nEffect", x=23, y=160, size=2) +
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
dimphDiamsum <- summarySE(dimph, measurevar="Base.Stem.Diameter", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of diameter
# plot based on summary stats above (w/ standard error)
Diammean<- summarySE(dimph, measurevar="Base.Stem.Diameter", groupvars=c("Sex"), na.rm=TRUE) # mean of all pop'ns
Diammean$Population<- as.factor("mean")
Diammean<- Diammean[,c("Sex", "Population", "N", "Base.Stem.Diameter", "sd", "se", "ci")] # reorder
dimphDiamsum[45:46,]<- Diammean # add to pop'n means dataset
Diamplot<- ggplot(dimphDiamsum, aes(x=Population, y=Base.Stem.Diameter, size=I(0.5), shape=factor(Sex)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Base.Stem.Diameter-se, ymax=Base.Stem.Diameter+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Stem\nDiameter (cm)", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
  geom_vline(xintercept=22.5,colour="black",linetype="dashed") +
  annotate(geom="text", label="Mean\nSex\nEffect", x=23, y=1.3, size=2) +
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
dimphAxsum <- summarySE(newdimph, measurevar="Num.Axillary.Flws", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of ax inflos
# plot based on summary stats above (w/ standard error)
Axmean<- summarySE(newdimph, measurevar="Num.Axillary.Flws", groupvars=c("Sex"), na.rm=TRUE) # mean of all pop'ns
Axmean$Population<- as.factor("mean")
Axmean<- Axmean[,c("Sex", "Population", "N", "Num.Axillary.Flws", "sd", "se", "ci")] # reorder
dimphAxsum[45:46,]<- Axmean # add to pop'n means dataset
Axplot<- ggplot(dimphAxsum, aes(x=Population, y=Num.Axillary.Flws, size=I(0.5), shape=factor(Sex)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Num.Axillary.Flws-se, ymax=Num.Axillary.Flws+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Number of Axillary\nInflorescences", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
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



# INFLOS
## sum
dimphInfSumsum <- summarySE(fulldimph, measurevar="inflo.sum", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of inflo sum
# plot based on summary stats above (w/ standard error)
InfSummean<- summarySE(fulldimph, measurevar="inflo.sum", groupvars=c("Sex"), na.rm=TRUE)
InfSummean$Population<- as.factor("mean")
InfSummean<- InfSummean[,c("Sex", "Population", "N", "inflo.sum", "sd", "se", "ci")] # reorder
dimphInfSumsum[45:46,]<- InfSummean # add to pop'n means dataset
InfSumplot<- ggplot(dimphInfSumsum, aes(x=Population, y=inflo.sum, size=I(0.5), shape=factor(Sex)), pch=c("17","18"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=inflo.sum-se, ymax=inflo.sum+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Total \n Inflorescence\nLength (cm)", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
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
dimphInfNumsum <- summarySE(fulldimph, measurevar="inflo.num", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of inflo num
# plot based on summary stats above (w/ standard error)
InfNummean<- summarySE(fulldimph, measurevar="inflo.num", groupvars=c("Sex"), na.rm=TRUE)
InfNummean$Population<- as.factor("mean")
InfNummean<- InfNummean[,c("Sex", "Population", "N", "inflo.num", "sd", "se", "ci")] # reorder
dimphInfNumsum[45:46,]<- InfNummean # add to pop'n means dataset
InfNumplot<- ggplot(dimphInfNumsum, aes(x=Population, y=inflo.num, size=I(0.5), shape=factor(Sex)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=inflo.num-se, ymax=inflo.num+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Number of\nInflorescences", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
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
dimphBranchsum <- summarySE(fulldimphbranches, measurevar="branch.sum", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of branch sum
# plot based on summary stats above (w/ standard error)
Branchmean<- summarySE(fulldimphbranches, measurevar="branch.sum", groupvars=c("Sex"), na.rm=TRUE)
Branchmean$Population<- as.factor("mean")
Branchmean<- Branchmean[,c("Sex", "Population", "N", "branch.sum", "sd", "se", "ci")] # reorder
dimphBranchsum[45:46,]<- Branchmean # add to pop'n means dataset
Branchplot<- ggplot(dimphBranchsum, aes(x=Population, y=branch.sum, size=I(0.5), shape=factor(Sex)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=branch.sum-se, ymax=branch.sum+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Total Branch\nLength (cm)", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
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
dimphplot <- plot_grid(Htplot + theme(legend.position="none", axis.title.x=element_blank()),
                       Diamplot + theme(legend.position="none", axis.title.x=element_blank()),
                       Axplot + theme(legend.position="none", axis.title.x=element_blank()),
                       InfSumplot + theme(legend.position="none", axis.title.x=element_blank()),
                       InfNumplot + theme(legend.position="none", axis.title.x=element_blank()),
                       Branchplot + theme(legend.position="none", axis.title.x=element_blank()),
                       align = 'vh',
                       labels = c("A","B","C","D","E","F"),
                       hjust = -1,
                       nrow = 3
) # combine all plots

legend<- get_legend(Htplot+theme(legend.justification = "top")) # get legend from first plot
x.grob <- textGrob("Population", 
                   gp=gpar(fontface="bold", fontsize=12)) # add pop'n to bottom

dimphplotcomp <- plot_grid(dimphplot, legend, x.grob, rel_widths = c(7, 0.5), rel_heights = c(6,1)) # create composite figure
dimphplotcomp



###################################################
## Dimorphism graphs - factored by SEX & BAGGING ##
###################################################

#NOTE: averages were not included in these graphs


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
) # combine all plots

legend<- get_legend(BHtplot+theme(legend.justification = "top")) # get legend from first plot
x.grob <- textGrob("Population", 
                   gp=gpar(fontface="bold", fontsize=12)) # write population across bottom

Bdimphplotcomp <- plot_grid(Bdimphplot, legend, x.grob, rel_widths = c(7, 0.5), rel_heights = c(6,1)) # combine plots
Bdimphplotcomp


############################################
## Dimorphism plots - factored by BAGGING ##
############################################


# subset datasets to include F only 
Fdimph<- subset(dimph, Sex=="F")
Ffulldimph<- subset(fulldimph, Sex=="F")
Ffulldimphbranches<- subset(fulldimphbranches, Sex=="F")
Fnewdimph<- subset(newdimph, Sex=="F")


# HEIGHT
FHtsum <- summarySE(Fdimph, measurevar="Primary.Stem.Ht.3", groupvars=c("Bag", "Population"), na.rm=TRUE) # summary statistics of height
FHtmean<- summarySE(Fdimph, measurevar="Primary.Stem.Ht.3", groupvars=c("Bag"), na.rm=TRUE) # mean of all pop'ns
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
FdimphDiamsum <- summarySE(Fdimph, measurevar="Base.Stem.Diameter", groupvars=c("Bag", "Population"), na.rm=TRUE) # summary statistics of diameter
# plot based on summary stats above (w/ standard error)
FDiammean<- summarySE(Fdimph, measurevar="Base.Stem.Diameter", groupvars=c("Bag"), na.rm=TRUE) # mean of all pop'ns
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
FdimphAxsum <- summarySE(Fnewdimph, measurevar="Num.Axillary.Flws", groupvars=c("Bag", "Population"), na.rm=TRUE) # summary statistics of ax inflos
# plot based on summary stats above (w/ standard error)
FAxmean<- summarySE(Fnewdimph, measurevar="Num.Axillary.Flws", groupvars=c("Bag"), na.rm=TRUE) # mean of all pop'ns
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
#### Not sure why NA level in bagging


# INFLOS
## sum
FdimphInfSumsum <- summarySE(Ffulldimph, measurevar="inflo.sum", groupvars=c("Bag","Population"), na.rm=TRUE) # summary statistics of inflo sum
# plot based on summary stats above (w/ standard error)
FInfSummean<- summarySE(Ffulldimph, measurevar="inflo.sum", groupvars=c("Bag"), na.rm=TRUE)
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
FdimphInfNumsum <- summarySE(Ffulldimph, measurevar="inflo.num", groupvars=c("Bag","Population"), na.rm=TRUE) # summary statistics of inflo num
# plot based on summary stats above (w/ standard error)
FInfNummean<- summarySE(Ffulldimph, measurevar="inflo.num", groupvars=c("Bag"), na.rm=TRUE)
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
FdimphBranchsum <- summarySE(Ffulldimphbranches, measurevar="branch.sum", groupvars=c("Bag","Population"), na.rm=TRUE) # summary statistics of branch sum
# plot based on summary stats above (w/ standard error)
FBranchmean<- summarySE(Ffulldimphbranches, measurevar="branch.sum", groupvars=c("Bag"), na.rm=TRUE)
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
) # combine all plots

legend<- get_legend(FHtplot+theme(legend.justification = "top")) # get legend from first plot
x.grob <- textGrob("Population", 
                   gp=gpar(fontface="bold", fontsize=12)) # write population on bottom

Fdimphplotcomp <- plot_grid(Fdimphplot, legend, x.grob, rel_widths = c(7, 0.5), rel_heights = c(6,1)) # create composite figure
Fdimphplotcomp


