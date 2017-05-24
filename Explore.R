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
