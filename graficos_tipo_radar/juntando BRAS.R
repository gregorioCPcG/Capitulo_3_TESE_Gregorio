#tentando juntar dois
#molde


rm(list=ls())
bra3 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/bra3.rds")

bra7 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/bra7.rds")
bra23 <- readRDS("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/bra23.rds")


bra3 <- bra3 + theme(
  axis.text = element_text(size = 6))
bra7 <- bra7 + theme(
  axis.text = element_text(size = 6))
bra23 <- bra23 + theme(
  axis.text = element_text(size = 6))
library(ggplot2)
library(easystats)
a<-plots(bra3, bra7, bra23, n_columns = 3, n_rows = 1,
      tags = c("               A", "                   B","                   C"))

a
