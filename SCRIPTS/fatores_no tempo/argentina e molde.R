setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")

#baixar os scores referentes aos anos do país 

# consultar : 'paises e ondas testadas.txt' para ver se falta alguma dimensão
library(marginaleffects)
library(tidyverse)
library(see)
arg3 <- read_csv("arg3.csv")
arg5 <- read_csv("arg5.csv")
arg6 <- read_csv("arg6.csv")
arg7 <- read_csv("arg7.csv")
arg3
arg3 <- arg3[, -1]
arg5 <- arg5[, -1]
arg6 <- arg6[, -1]
arg7 <- arg7[, -1]
#juntar
#colocar onda
arg3$wave <- 'Onda 3'
arg5$wave <- 'Onda 5'
arg6$wave <- 'Onda 6'
arg7$wave <- 'Onda 7'

df <- full_join(arg3,arg5)
df <- full_join(df,arg6)
df <- full_join(df,arg7)
table(df$wave)



model_fundamentalism<- lm(fundamentalism ~ wave,data=df)
model_promercado <- lm(promercado ~ wave, data=df)
model_antidemocr <- lm(antidemocr ~ wave, data=df)
model_desconfia <- lm(desconfia ~ wave, data=df)

a<-plot_cap(model_fundamentalism, condition="wave")+
  theme_bw()+labs(y="Fundamentalismo")
b<-plot_cap(model_promercado, condition="wave")+
  theme_bw()+labs(y="Materialismo à Direita")
c<-plot_cap(model_antidemocr, condition="wave")+
  theme_bw()+labs(y="Autoritário (Antidemocrático)")
d<-plot_cap(model_desconfia, condition="wave")+
  theme_bw()+labs(y="Desconfiança Institucional")
plots(a,b,c,d,
      title="                                                                           Argentina")
