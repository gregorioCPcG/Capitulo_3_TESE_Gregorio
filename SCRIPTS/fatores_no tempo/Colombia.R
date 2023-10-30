setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")
rm(list=ls())
#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
col5 <- read_csv("col5.csv")
col7 <- read_csv("colombia7.csv")
col7#remover a coluna 
col5 <- col5[, -1]
col7 <- col7[, -1]

#colocar onda
col5$wave <- 'Onda 5'
col7$wave <- 'Onda 7'

df <- full_join(col7,col5)
table(df$wave)



model_fundamentalism<- lm(fundamentalism ~ wave,data=df)
model_promercado <- lm(promercado ~ wave, data=df)
model_desconfia <- lm(desconfia ~ wave, data=df)

a<-plot_cap(model_fundamentalism, condition="wave")+
  theme_bw()+labs(y="Fundamentalismo")
a
b<-plot_cap(model_promercado, condition="wave")+
  theme_bw()+labs(y="Materialismo à Direita")
b

d<-plot_cap(model_desconfia, condition="wave")+
  theme_bw()+labs(y="Desconfiança Institucional")
d

plots(a,b,d,
      title="                                                                           Colômbia",
      n_columns = 2)
#a