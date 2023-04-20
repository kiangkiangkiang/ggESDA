library(ggESDA)
reinstall_mac <- function(){
  rm(list=ls())
  library(ggESDA)
  detach("package:ggESDA", unload=TRUE)
  remove.packages("ggESDA")
  dir<-'/Users/cfh00892302/Desktop/myWorkspace/ggESDA_all/'
  install.packages(paste0(dir,"ggESDA_0.2.0.tgz"), repos = NULL, type="source")

  library(ggESDA)
  Concepts <- as.factor(rep(c("FRA", "HUS", "INC", "ISA", "JPL", "KHA",
                              "LOT", "PHI", "ROM"), each = 3))
}
#roxygen2::roxygenise()
#右上角->build->build binary...
reinstall_mac()
