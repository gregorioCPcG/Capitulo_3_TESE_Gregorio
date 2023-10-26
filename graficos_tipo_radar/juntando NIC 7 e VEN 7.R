#tentando juntar dois


rm(list=ls())
nic7 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/nic7.rds")

ven7 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/ven7.rds")


nic7
ven7
library(ggplot2)
library(easystats)
plots(nic7, ven7, n_columns = 2, n_rows = 1,
      tags = c("A", "B"))

