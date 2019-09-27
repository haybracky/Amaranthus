###DIMORPHISM GRAPHS###

# data

dimph<- read.csv("Amaranthus Male and Female Harvest Combined Data v4.csv")
dimph<- subset(dimph, Sex=="M" | Sex=="F") # remove the group containing sex=NA
dimph$Date<- as.character(dimph$Date)
dimph$Date<- as.Date(dimph$Date)
str(dimph)
names(dimph)

# add lat
lat<- read.csv("Population Locations.csv") # read in lat datasheet
lat<- dplyr::select(lat, Population, Latitude)
library(plyr)
dimph <- join(dimph, lat, by = c("Population")) # add lat col
dimph <- transform(dimph, Population=reorder(Population, Latitude)) # order pop'n by lat

# Inflo sum & count
# remove plants that did not have inflos recorded
fulldimph<- dimph[!is.na(dimph$Base.Stem.Diameter),]
fulldimph$inflo.sum<- apply(fulldimph[,42:592], 1, FUN=sum, na.rm=T) # sum (i.e. total reproductive structure)
fulldimph$inflo.num <- 551-(apply(fulldimph[,42:592], 1, function(x) sum(is.na(x)))) # num inflos

# branch sum
fulldimphbranches<-fulldimph[,c(1:3,6,12:39)]
fulldimphbranches[is.na(fulldimphbranches)]<- 0
names(fulldimphbranches) # this doesn't have lat
fulldimphbranches$branch.sum<- apply(fulldimphbranches[,7:32], 1, FUN=sum)
fulldimphbranches$Latitude<- fulldimph$Latitude # re-add lat

# inconsistent axillary data collection, remove data before May 9th
newdimph<- dimph[dimph$Date >"2018-05-09",] # remove dates (before 2018-05-09)


library(Rmisc)
library(ggplot2)
#####################################################
##Graphs with line at end indicating averages after##
#####################################################
# HEIGHT
dimphHtsum <- summarySE(dimph, measurevar="Primary.Stem.Ht.3", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of height
Htmean<- summarySE(dimph, measurevar="Primary.Stem.Ht.3", groupvars=c("Sex"), na.rm=TRUE) # mean of all pop'ns
Htmean$Population<- as.factor("mean")
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
)

legend<- get_legend(Htplot+theme(legend.justification = "top"))
x.grob <- textGrob("Population", 
                   gp=gpar(fontface="bold", fontsize=12))

dimphplotcomp <- plot_grid(dimphplot, legend, x.grob, rel_widths = c(7, 0.5), rel_heights = c(6,1))
dimphplotcomp

####################################
##Graphs with M/F average as lines##
####################################
# HEIGHT
dimphHtsum2 <- summarySE(dimph, measurevar="Primary.Stem.Ht.3", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of height
# plot based on summary stats above (w/ standard error)
Htplot2<- ggplot(dimphHtsum2, aes(x=Population, y=Primary.Stem.Ht.3, size=I(0.5), shape=factor(Sex)), pch=c("17","19"),na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Primary.Stem.Ht.3-se, ymax=Primary.Stem.Ht.3+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Height (cm)", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
  geom_hline(yintercept=c(mean(subset(dimph, Sex=="F")$Primary.Stem.Ht.3, na.rm=T),
                          mean(subset(dimph,Sex=="M")$Primary.Stem.Ht.3,na.rm=T)),colour="black",linetype="dashed") +
  annotate(geom="text", label=c("Female av.", "Male av."), x=c(23,23), y=c(mean(subset(dimph,Sex=="F")$Primary.Stem.Ht.3,na.rm=T),
                                                                           mean(subset(dimph,Sex=="M")$Primary.Stem.Ht.3,na.rm=T)), vjust=-1) +
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
dimphDiamsum2 <- summarySE(dimph, measurevar="Base.Stem.Diameter", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of diameter
# plot based on summary stats above (w/ standard error)
Diamplot2<- ggplot(dimphDiamsum2, aes(x=Population, y=Base.Stem.Diameter, size=I(0.5), shape=factor(Sex)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Base.Stem.Diameter-se, ymax=Base.Stem.Diameter+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Stem\nDiameter (cm)", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
  geom_hline(yintercept=c(mean(subset(dimph, Sex=="F")$Base.Stem.Diameter, na.rm=T),
                          mean(subset(dimph,Sex=="M")$Base.Stem.Diameter,na.rm=T)),colour="black",linetype="dashed") +
  annotate(geom="text", label=c("Female av.", "Male av."), x=c(23,23), y=c(mean(subset(dimph,Sex=="F")$Base.Stem.Diameter,na.rm=T),
                                                                           mean(subset(dimph,Sex=="M")$Base.Stem.Diameter,na.rm=T)), vjust=-1) +
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
dimphAxsum2 <- summarySE(newdimph, measurevar="Num.Axillary.Flws", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of ax inflos
# plot based on summary stats above (w/ standard error)
Axplot2<- ggplot(dimphAxsum2, aes(x=Population, y=Num.Axillary.Flws, size=I(0.5), shape=factor(Sex)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=Num.Axillary.Flws-se, ymax=Num.Axillary.Flws+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Number of Axillary\nInflorescences", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
  geom_hline(yintercept=c(mean(subset(newdimph, Sex=="F")$Num.Axillary.Flws, na.rm=T),
                          mean(subset(newdimph,Sex=="M")$Num.Axillary.Flws,na.rm=T)),colour="black",linetype="dashed") +
  annotate(geom="text", label=c("Female av.", "Male av."), x=c(23,23), y=c(mean(subset(newdimph,Sex=="F")$Num.Axillary.Flws,na.rm=T),
                                                                           mean(subset(newdimph,Sex=="M")$Num.Axillary.Flws,na.rm=T)), vjust=-1) +
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
dimphInfSumsum2 <- summarySE(fulldimph, measurevar="inflo.sum", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of inflo sum
# plot based on summary stats above (w/ standard error)
InfSumplot2<- ggplot(dimphInfSumsum2, aes(x=Population, y=inflo.sum, size=I(0.5), shape=factor(Sex)), pch=c("17","18"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=inflo.sum-se, ymax=inflo.sum+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Total \n Inflorescence\nLength (cm)", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
  geom_hline(yintercept=c(mean(subset(fulldimph, Sex=="F")$inflo.sum, na.rm=T),
                          mean(subset(fulldimph,Sex=="M")$inflo.sum,na.rm=T)),colour="black",linetype="dashed") +
  annotate(geom="text", label=c("Female av.", "Male av."), x=c(23,23), y=c(mean(subset(fulldimph,Sex=="F")$inflo.sum,na.rm=T),
                                                                           mean(subset(fulldimph,Sex=="M")$inflo.sum,na.rm=T)), vjust=-1) +
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
dimphInfNumsum2 <- summarySE(fulldimph, measurevar="inflo.num", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of inflo num
# plot based on summary stats above (w/ standard error)
InfNumplot2<- ggplot(dimphInfNumsum2, aes(x=Population, y=inflo.num, size=I(0.5), shape=factor(Sex)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=inflo.num-se, ymax=inflo.num+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Number of\nInflorescences", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
  geom_hline(yintercept=c(mean(subset(fulldimph, Sex=="F")$inflo.num, na.rm=T),
                          mean(subset(fulldimph,Sex=="M")$inflo.num,na.rm=T)),colour="black",linetype="dashed") +
  annotate(geom="text", label=c("Female av.", "Male av."), x=c(23,23), y=c(mean(subset(fulldimph,Sex=="F")$inflo.num,na.rm=T),
                                                                           mean(subset(fulldimph,Sex=="M")$inflo.num,na.rm=T)), vjust=-1) +
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
dimphBranchsum2 <- summarySE(fulldimphbranches, measurevar="branch.sum", groupvars=c("Sex", "Population"), na.rm=TRUE) # summary statistics of branch sum
# plot based on summary stats above (w/ standard error)
Branchplot2<- ggplot(dimphBranchsum2, aes(x=Population, y=branch.sum, size=I(0.5), shape=factor(Sex)), pch=c("17","19"), na.rm=TRUE) + 
  geom_errorbar(aes(ymin=branch.sum-se, ymax=branch.sum+se), width=0.3) +
  geom_point(size=3) +
  labs(y="Total Branch\nLength (cm)", shape="Sex") +
  scale_shape_discrete(labels=c("Female", "Male")) +
  geom_hline(yintercept=c(mean(subset(fulldimphbranches, Sex=="F")$branch.sum, na.rm=T),
                          mean(subset(fulldimphbranches,Sex=="M")$branch.sum,na.rm=T)),colour="black",linetype="dashed") +
  annotate(geom="text", label=c("Female av.", "Male av."), x=c(23,23), y=c(mean(subset(fulldimphbranches,Sex=="F")$branch.sum,na.rm=T),
                                                                           mean(subset(fulldimphbranches,Sex=="M")$branch.sum,na.rm=T)), vjust=-1) +
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
dimphplot2 <- plot_grid(Htplot2 + theme(legend.position="none", axis.title.x=element_blank()),
                       Diamplot2 + theme(legend.position="none", axis.title.x=element_blank()),
                       Axplot2 + theme(legend.position="none", axis.title.x=element_blank()),
                       InfSumplot2 + theme(legend.position="none", axis.title.x=element_blank()),
                       InfNumplot2 + theme(legend.position="none", axis.title.x=element_blank()),
                       Branchplot2 + theme(legend.position="none", axis.title.x=element_blank()),
                       align = 'vh',
                       labels = c("A","B","C","D","E","F"),
                       hjust = -1,
                       nrow = 3
)

legend<- get_legend(Htplot2+theme(legend.justification = "top"))
x.grob <- textGrob("Population", 
                   gp=gpar(fontface="bold", fontsize=12))

dimphplotcomp2 <- plot_grid(dimphplot2, legend, x.grob, rel_widths = c(7, 0.5), rel_heights = c(6,1))
dimphplotcomp2
