setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")
rm(list=ls())
#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
mex2 <- read_csv("mex2.csv")
mex3 <- read_csv("mex3.csv")
mex5 <- read_csv("mex5.csv")
mex6 <- read_csv("mex6.csv")
mex7 <- read_csv("mex7.csv")
mex3#remover a coluna 
mex2 <- mex2[, -1]
mex3 <- mex3[, -1]
mex5 <- mex5[, -1]
mex6 <- mex6[, -1]
mex7 <- mex7[, -1]

#colocar onda
mex3$wave <- 'Onda 3'
mex5$wave <- 'Onda 5'
mex6$wave <- 'Onda 6'
mex7$wave <- 'Onda 7'
mex2$wave <- 'Onda 2'

df <- full_join(mex3,mex5)
df <- full_join(df,mex6)
df <- full_join(df,mex7)
df <- full_join(df,mex2)
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
      title="                                                                           México")
#a
#b
#c
#d
