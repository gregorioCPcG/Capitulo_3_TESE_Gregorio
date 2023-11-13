#argentina onda 3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())

#bra3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#
#chile3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
#promercado1 <- lm(promercado ~ votos_partidos, data=df)
#r<-summary(promercado1)
#r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
#promercado2 <- lm(promercado ~ Genero_Mulher+
idade_faixa+escolaridade_niveis+
  status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#+raca_branc
, data=df)

#r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#
#mex3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#peru3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#urug3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())
#

#
#argentina onda 5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"

df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#bra5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$renda_subj_numeric == 1
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#
#chile5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
#promercado1 <- lm(promercado ~ votos_partidos, data=df)
#r<-summary(promercado1)
#r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
#promercado2 <- lm(promercado ~ Genero_Mulher+
#                    idade_faixa+escolaridade_niveis+
#                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#+raca_branc
#                  , data=df)

#r<-summary(promercado2)
#r$adj.r.squared
rm(list=ls())


#Guatemala 5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
#promercado1 <- lm(promercado ~ votos_partidos, data=df)
#r<-summary(promercado1)
#r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj_level_recoded == 1
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
#promercado2 <- lm(promercado ~ Genero_Mulher+
#                   idade_faixa+escolaridade_niveis+
#                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#+raca_branc
#               , data=df)

#r<-summary(promercado2)
#r$adj.r.squared
rm(list=ls())


#

#mex5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
#antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
#r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
#antidemocr2 <- lm(antidemocr~ Genero_Mulher+
#                    idade_faixa+escolaridade_niveis+
#                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#                  #+raca_branc
#                  , data=df)
#r<-summary(antidemocr2)
#r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#
#peru5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
#fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
#r<-summary(fundamentalism1)
#r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
#fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
#                       idade_faixa+escolaridade_niveis+
#                       status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#                     #+raca_branc
#                   , data=df)
#r<-summary(fundamentalism2)
#r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#
#
#
#urug5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#
#Argentina onda 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#brasil onda 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$renda_subj_numeric == 1
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#boliviA onda 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())
#

#
#



#


#CHILE_ONDA_7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())



#
#COLOMBIA#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
#antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
#r<-summary(antidemocr1)
#r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
#antidemocr2 <- lm(antidemocr~ Genero_Mulher+
#                    idade_faixa+escolaridade_niveis+
#                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#                  #+raca_branc
#                  , data=df)
#r<-summary(antidemocr2)
#r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#EQUADOR#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj_numericc == 1
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#guatemala 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())

#
#mex7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())

#
#Nicaragua#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
#antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
#r<-summary(antidemocr1)
#r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
#antidemocr2 <- lm(antidemocr~ Genero_Mulher+
#                    idade_faixa+escolaridade_niveis+
#                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#+raca_branc
#                  , data=df)
#r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#Peru 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())


#
#uruguai7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())




#

#Venezuela  7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
#1 Valores ~ Partidos
fundamentalism1 <- lm(fundamentalism ~ votos_partidos, data=df)
r<-summary(fundamentalism1)
r$adj.r.squared
desconfia1 <- lm(desconfia~ votos_partidos, data=df)
r<-summary(desconfia1)
r$adj.r.squared
antidemocr1 <- lm(antidemocr~ votos_partidos, data=df)
r<-summary(antidemocr1)
r$adj.r.squared
promercado1 <- lm(promercado ~ votos_partidos, data=df)
r<-summary(promercado1)
r$adj.r.squared

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- lm(fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df)
r<-summary(fundamentalism2)
r$adj.r.squared

desconfia2 <- lm(desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df)
r<-summary(desconfia2)
r$adj.r.squared
antidemocr2 <- lm(antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)
r<-summary(antidemocr2)
r$adj.r.squared
promercado2 <- lm(promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df)

r<-summary(promercado2)
r$adj.r.squared
rm(list=ls())






