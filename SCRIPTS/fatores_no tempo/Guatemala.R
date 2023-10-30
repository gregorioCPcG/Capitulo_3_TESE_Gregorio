setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")
rm(list=ls())
#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
guat5 <- read_csv("guat5.csv")
guat7 <- read_csv("guat7.csv")
guat7#remover a coluna 
guat5 <- guat5[, -1]
guat7 <- guat7[, -1]

#colocar onda
guat5$wave <- 'Onda 5'
guat7$wave <- 'Onda 7'

df <- full_join(guat7,guat5)
table(df$wave)



model_fundamentalism<- lm(fundamentalism ~ wave,data=df)
model_promercado <- lm(promercado ~ wave, data=df)
model_antidemocr <- lm(antidemocr ~ wave, data=df)
model_desconfia <- lm(desconfia ~ wave, data=df)

a<-plot_cap(model_fundamentalism, condition="wave")+
  theme_bw()+labs(y="Fundamentalismo")
a
c<-plot_cap(model_antidemocr, condition="wave")+
  theme_bw()+labs(y="Autoritário (Antidemocrático)")
c
d<-plot_cap(model_desconfia, condition="wave")+
  theme_bw()+labs(y="Desconfiança Institucional")
d
plots(a,c,d,
      title="                                                                           Guatemala",
      n_columns = 2)
#a
#b
#c
#d
