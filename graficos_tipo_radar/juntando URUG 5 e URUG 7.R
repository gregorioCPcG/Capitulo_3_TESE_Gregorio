#tentando juntar dois


rm(list=ls())
urug5 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/urug5.rds")

urug7 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/urug7.rds")


urug5
urug7
library(ggplot2)
library(easystats)
plots(urug5, urug7, n_columns = 2, n_rows = 1,
      tags = c("2006", "2021"))

