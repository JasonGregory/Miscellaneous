#####################
#Thought process:
#First decide what you want to predict (frame the problem/brainstorm)
#Think about how the data is setup and therefore needs to be sampled
#Types of feature selection include normalization & treatment of categorical variables
#
#For explore it: 
# Look to see what type of relationships there are between the dependent and independent variables
# What relationships are the between independent variables to create stronger relationships
# 
#
#
#
#
#


#####################
#Example Code
#####################

library("devtools")
install_github("JasonGregory/dataFun")
#devtools::install("//Co.ihc.com/sh/User/jgregor1/GitHub/dataFun")

library(dataFun)
rm(list=ls())

######################
#Data Prep Stage
######################
dat <- read.csv(system.file("extdata", "titanic.csv", package = "dataFun"))
prepIt(dat)


#convert numeric discrete variables to factor
p.factor(dat)
dat <- p.asFactor(dat)  #converts numeric discrete to factors
p.structure(dat)

#replace missing factor variables & remove null rows
dat[dat==""] <- NA
PlotMissing(dat)
columns <- p.factor(dat); columns
dat[columns] <- p.replaceNull(dat, columns); rm(columns)
PlotMissing(dat)
dat <- dat[complete.cases(dat),]

#remove unique variables
p.unique(dat)
dat <- p.removeVariables(dat, p.unique(dat))
p.structure(dat)

prepIt(dat)

####################
#Data Exploration Stage
####################
exploreIt(dat)



####################
#Other Useful Functions & links
####################
#similar to setdiff but goes both ways
symdiff <- function( x, y) { 
  setdiff( union(x, y), intersect(x, y))
}
#RProfile links
#https://www.r-bloggers.com/fun-with-rprofile-and-customizing-r-startup/
#http://www.gettinggeneticsdone.com/2013/07/customize-rprofile.html
#https://github.com/tonyfischetti/myR/blob/master/aR.profile


