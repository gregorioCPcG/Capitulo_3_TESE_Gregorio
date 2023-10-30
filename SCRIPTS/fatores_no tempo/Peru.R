setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")
rm(list=ls())
#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
peru3 <- read_csv("peru3.csv")
peru5 <- read_csv("peru5.csv")
peru7 <- read_csv("peru7.csv")
peru3#remover a coluna 
peru3 <- peru3[, -1]
peru5 <- peru5[, -1]
peru7 <- peru7[, -1]

#colocar onda
peru3$wave <- 'Onda 3'
peru5$wave <- 'Onda 5'
peru7$wave <- 'Onda 7'

df <- full_join(peru3,peru5)
df <- full_join(df,peru7)
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
      title="                                                                           Peru")
#a
#b
#c
#d
