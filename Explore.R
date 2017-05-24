library("devtools")

install_github("JasonGregory/dataFun")


library(dataFun)
dat <- read.csv(system.file("extdata", "titanic.csv", package = "dataFun"))

prepDescribe(dat)

prepIt(dat)
exploreIt(dat)

Blah.
