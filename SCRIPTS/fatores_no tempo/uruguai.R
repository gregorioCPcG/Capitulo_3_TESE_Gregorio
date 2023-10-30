setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")
rm(list=ls())
#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
urug3 <- read_csv("urug3.csv")
urug5 <- read_csv("urug5.csv")
urug7 <- read_csv("urug7.csv")
urug6 <- read_csv("urug6.csv")
urug3#remover a coluna 
urug3 <- urug3[, -1]
urug5 <- urug5[, -1]
urug6 <- urug6[, -1]
urug7 <- urug7[, -1]

#colocar onda
urug3$wave <- 'Onda 3'
urug5$wave <- 'Onda 5'
urug7$wave <- 'Onda 7'
urug6$wave <- 'Onda 6'

df <- full_join(urug3,urug5)
df <- full_join(df,urug7)
df <- full_join(df,urug6)
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
      title="                                                                           uruguai")
#a
#b
#c
#d
