setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")
rm(list=ls())
#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
trinidad5 <- read_csv("trinidad5.csv")
trinidad6 <- read_csv("trinidad6.csv")
trinidad5#remover a coluna 
trinidad5 <- trinidad5[, -1]
trinidad6 <- trinidad6[, -1]

#colocar onda
trinidad5$wave <- 'Onda 3'
trinidad6$wave <- 'Onda 7'

df <- full_join(trinidad5,trinidad6)
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
b
c<-plot_cap(model_antidemocr, condition="wave")+
  theme_bw()+labs(y="Autoritário (Antidemocrático)")
c
d<-plot_cap(model_desconfia, condition="wave")+
  theme_bw()+labs(y="Desconfiança Institucional")
plots(a,b,c,d,
      title="                                                                         Trinidad e Tobago")


