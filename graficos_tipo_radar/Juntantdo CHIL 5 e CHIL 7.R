#tentando juntar dois


rm(list=ls())
chile5 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/chile5.rds")

chile7 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/chile7.rds")

library(ggplot2)
chile5 <- chile5 + theme(
  axis.text = element_text(size = 6.5))
chile7 <- chile7 + theme(
  axis.text = element_text(size = 6.5))

library(easystats)
plots(chile5, chile7, n_columns = 2, n_rows = 1,
      tags = c("2006", "2018"))

