# some preliminary analysis of Amaranthus combined data

MyData<- read.csv("Amaranthus Male and Female Harvest Combined Data v4.csv")
str(MyData)
MyData$Inflo.length.19<- as.numeric(MyData$Inflo.length.19)

library(ggplot2)
library(lattice)
library(dplyr)

MyData<- select(MyData, -Notes)
names(MyData)

histogram(~Base.Stem.Diameter|factor(Population),data=MyData,
          main="Base Stem Diameter by Population",xlab="Stem diameter in cm")
# base stem diameter mostly normally distributed

histogram(~Primary.Stem.Ht|factor(Population),data=MyData,
          main="Height by Population",xlab="Height in cm")
# height somewhat normally distributed

MyData$branch.means <- rowMeans(MyData[,11:39], na.rm=TRUE) # note: remember to change this if dataset changes
MyData$branch.means
histogram(~branch.means|factor(Population), data=MyData)

