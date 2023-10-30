setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")
rm(list=ls())
#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
ven3 <- read_csv("ven3.csv")
ven7 <- read_csv("venezuela7.csv")
ven3#remover a coluna 
ven3 <- ven3[, -1]
ven7 <- ven7[, -1]

#colocar onda
ven3$wave <- 'Onda 3'
ven7$wave <- 'Onda 7'

df <- full_join(ven3,ven7)
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
      title="                                                                           Venezuela")


