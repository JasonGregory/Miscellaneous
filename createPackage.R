library(devtools)
library(roxygen2)

setwd("~/Documents/R/Packages")
devtools::create("dataFun")

#In the DESCRIPTION file list any packages that the functions are dependent on
#Add .R files in the "R" folder for any of the functions
#Make sure to document appropriately

devtools::document("~/Documents/R/Packages/dataFun")

#devtools::install("~/Documents/R/Packages/dataFun")
devtools::install("//Co.ihc.com/sh/User/jgregor1/GitHub/dataFun")

library(dataFun)

#instal from Github
install_github("JasonGregory/dataFun")

