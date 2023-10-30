setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")
rm(list=ls())
#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
chile2 <- read_csv("chile2.csv")
chile3 <- read_csv("chile3.csv")
chile5 <- read_csv("chile5.csv")
chile7 <- read_csv("chile7.csv")
chile3#remover a coluna 
chile2 <- chile2[, -1]
chile3 <- chile3[, -1]
chile5 <- chile5[, -1]
chile7 <- chile7[, -1]

#colocar onda
chile3$wave <- 'Onda 3'
chile5$wave <- 'Onda 5'
chile7$wave <- 'Onda 7'
chile2$wave <- 'Onda 2'

df <- full_join(chile3,chile5)
df <- full_join(df,chile7)
df <- full_join(df,chile2)
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
      title="                                                                           Chile")

