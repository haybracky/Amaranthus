## DIMORPHISM SUMMARY STATS ##

library(dplyr)
dimph<- read.csv("Amaranthus Male and Female Harvest Combined Data v4.csv")
dimph<- select(dimph, -Notes) # remove the notes col
dimph<- subset(dimph, Sex=="M" | Sex=="F") # remove the group containing sex=NA
dimph$Date<- as.character(dimph$Date)
dimph$Date<- as.Date(dimph$Date) # change data format

# add lat
library(plyr)
dimph <- join(dimph, lat, by = c("Population")) # add lat col
dimph <- transform(dimph, Population=reorder(Population, Latitude)) # order pop'n by lat

# Inflo sum & count
# remove plants that did not have inflos recorded
fulldimph<- dimph[!is.na(dimph$Base.Stem.Diameter),]
fulldimph$inflo.sum<- apply(fulldimph[,41:591], 1, FUN=sum, na.rm=TRUE) # sum (i.e. total reproductive structure)
fulldimph$inflo.num <- 551-(apply(fulldimph[,41:591], 1, function(x) sum(is.na(x)))) # num inflos
# na.rm okay here b/c no recorded plants had 0 inflos

# branch sum
# can't use na.rm here b/c some plants have no branches >20cm (this is biologically significant)
fulldimph[is.na(fulldimph[,11:39])]<- 0
fulldimph$branch.sum<- apply(fulldimph[,11:39], 1, FUN=sum, )

library(Rmisc)
# HEIGHT
dimphHtsum <- summarySE(dimph, measurevar="Primary.Stem.Ht.3", groupvars=c("Sex"), na.rm=TRUE) # summary statistics of height
dimphHtsum
# DIAMETER
dimphDiamsum <- summarySE(dimph, measurevar="Base.Stem.Diameter", groupvars=c("Sex"), na.rm=TRUE) # summary statistics of diameter
dimphDiamsum
# AXILLARY
dimphAxsum <- summarySE(newdimph, measurevar="Num.Axillary.Flws", groupvars=c("Sex"), na.rm=TRUE) # summary statistics of ax inflos
dimphAxsum
# INFLOS
## num
dimphInfNumsum <- summarySE(fulldimph, measurevar="inflo.num", groupvars=c("Sex"), na.rm=TRUE) # summary statistics of inflo num
dimphInfNumsum
## sum
dimphInfSumsum <- summarySE(fulldimph, measurevar="inflo.sum", groupvars=c("Sex"), na.rm=TRUE) # summary statistics of inflo sum
dimphInfSumsum
# BRANCHES
## num branches >20cm
nzdimph<- fulldimph
nzdimph$nzbranch<- ifelse(nzdimph$branch.sum>0, 1, 0) # make col of zero vs non-zero data

## branch length
dimphBranchsum <- summarySE(fulldimph, measurevar="branch.sum", groupvars=c("Sex"), na.rm=TRUE) # summary statistics of branch sum
dimphBranchsum
