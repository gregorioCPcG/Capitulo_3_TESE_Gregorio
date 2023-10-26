rm(list=ls())
peru3 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/peru3.rds")

peru7 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/peru7.rds")


peru3
peru7
library(ggplot2)
library(easystats)
plots(peru3, peru7, n_columns = 2, n_rows = 1,
      tags = c("1996", "2018"))

