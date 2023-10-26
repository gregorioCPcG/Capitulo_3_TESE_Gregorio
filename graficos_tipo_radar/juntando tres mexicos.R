#tentando juntar dois
#molde


rm(list=ls())
mex3 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/mex3.rds")

mex5 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/mex5.rds")
mex7 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/mex7.rds")


mex3 <- mex3 + theme(
  axis.text = element_text(size = 6))
mex5 <- mex5 + theme(
  axis.text = element_text(size = 6))
mex7 <- mex7 + theme(
  axis.text = element_text(size = 6))
library(ggplot2)
library(easystats)
a<-plots(mex3, mex5, mex7, n_columns = 3, n_rows = 1,
         tags = c("               A", "                   B","                   C"))

a
