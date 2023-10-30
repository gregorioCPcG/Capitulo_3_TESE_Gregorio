setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")
rm(list=ls())
#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
bra2 <- read_csv("bra2.csv")
bra3 <- read_csv("bra3.csv")
bra5 <- read_csv("bra5.csv")
bra6 <- read_csv("bra6.csv")
bra7 <- read_csv("bra7.csv")
bra3#remover a coluna 
bra2 <- bra2[, -1]
bra3 <- bra3[, -1]
bra5 <- bra5[, -1]
bra6 <- bra6[, -1]
bra7 <- bra7[, -1]

#colocar onda
bra3$wave <- 'Onda 3'
bra5$wave <- 'Onda 5'
bra6$wave <- 'Onda 6'
bra7$wave <- 'Onda 7'
bra2$wave <- 'Onda 2'

df <- full_join(bra3,bra5)
df <- full_join(df,bra6)
df <- full_join(df,bra7)
df <- full_join(df,bra2)
table(df$wave)



model_fundamentalism<- lm(fundamentalism ~ wave,data=df)
model_promercado <- lm(promercado ~ wave, data=df)
model_antidemocr <- lm(antidemocr ~ wave, data=df)
model_desconfia <- lm(desconfia ~ wave, data=df)

a<-plot_cap(model_fundamentalism, condition="wave")+
  theme_bw()+labs(y="Fundamentalismo")
a
b<-plot_cap(model_promercado, condition="wave")+
  theme_bw()+labs(y="Materialismo à Direita")
c<-plot_cap(model_antidemocr, condition="wave")+
  theme_bw()+labs(y="Autoritário (Antidemocrático)")
c
d<-plot_cap(model_desconfia, condition="wave")+
  theme_bw()+labs(y="Desconfiança Institucional")
plots(a,b,c,d,
      title="                                                                           Brasil")
#a
#b
#c
#d
