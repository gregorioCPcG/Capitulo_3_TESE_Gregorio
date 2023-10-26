#tentando juntar dois
#molde
#argentina 5 e 7

rm(list=ls())
arg5 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/arg5.rds")

arg7 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/arg7.rds")


arg5
arg7
library(ggplot2)
library(easystats)
plots(arg5, arg7, n_columns = 2, n_rows = 1,
      tags = c("2006", "2017"))

