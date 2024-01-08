library(fmsb)

#argentina onda 3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))



nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())

#bra3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2

rm(list=ls())


#
#chile3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


#promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))
#
#

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
idade_faixa+escolaridade_niveis+
  status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#+raca_branc
, data=df, family=binomial(link=logit))

#

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#
#mex3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))



nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#peru3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))



nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#urug3#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))



nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())
#

#
#argentina onda 5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"

df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))



nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#bra5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$renda_subj_numeric == 1
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))



nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#
#chile5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


#promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))
#
#

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
#                    idade_faixa+escolaridade_niveis+
#                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#+raca_branc
#                  , data=df, family=binomial(link=logit))

#
#
#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#Guatemala 5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


#promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))
#
#

#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj_level_recoded == 1
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
#                   idade_faixa+escolaridade_niveis+
#                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#+raca_branc
#               , data=df, family=binomial(link=logit))

#
#
#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#

#mex5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


#antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))
#

promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


#antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
#                    idade_faixa+escolaridade_niveis+
#                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#                  #+raca_branc
#                  , data=df, family=binomial(link=logit))
#
#
promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#
#peru5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
#fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))
#
#
desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
#fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
#                       idade_faixa+escolaridade_niveis+
#                       status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#                     #+raca_branc
#                   , data=df, family=binomial(link=logit))
#
#

desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#
#
#
#urug5#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#
#Argentina onda 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#brasil onda 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$renda_subj_numeric == 1
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#boliviA onda 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())
#

#
#



#


#CHILE_ONDA_7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())



#
#COLOMBIA#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


#antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))
#
#
promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


#antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
#                    idade_faixa+escolaridade_niveis+
#                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#                  #+raca_branc
#                  , data=df, family=binomial(link=logit))
#
#
promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#EQUADOR#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj_numericc == 1
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#guatemala 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())

#
#mex7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())

#
#Nicaragua#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


#antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))
#
#
promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


#antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
#                    idade_faixa+escolaridade_niveis+
#                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
#+raca_branc
#                  , data=df, family=binomial(link=logit))
#

promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#Peru 7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())


#
#uruguai7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


#Atitudes/Valores ~ Instituição

nagelkerke <- NagelkerkeR2(fundamentalism1)
print(nagelkerke)#fundamentalism1

nagelkerke <- NagelkerkeR2(desconfia1)
print(nagelkerke)#desconfia1

nagelkerke <- NagelkerkeR2(antidemocr1)
print(nagelkerke)#antidemocr1

nagelkerke <- NagelkerkeR2(promercado1)
print(nagelkerke)#promercado1



#Atitudes/Valores ~ Estruturas

nagelkerke <- NagelkerkeR2(fundamentalism2)
print(nagelkerke)#fundamentalism2

nagelkerke <- NagelkerkeR2(desconfia2)
print(nagelkerke)#desconfia2

nagelkerke <- NagelkerkeR2(antidemocr2)
print(nagelkerke)#antidemocr2

nagelkerke <- NagelkerkeR2(promercado2)
print(nagelkerke)#promercado2
rm(list=ls())




#

#Venezuela  7#####
#rodar a do país/onda antes (scripts gerais)
df %>% glimpse()
df$niveis_temp <- ntile(df$fundamentalism, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_fundamentalism <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$desconfia, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_desconfia <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$antidemocr, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_antidemocr <- df$niveis_temp == 3

df$niveis_temp <- ntile(df$promercado, 3)
df$niveis_temp <- as.factor(df$niveis_temp)
levels(df$niveis_temp)
levels(df$niveis_temp) <- c('1','2','3')
df$tercil_promercado <- df$niveis_temp == 3
#1 Valores ~ Partidos
fundamentalism1 <- glm(tercil_fundamentalism ~ votos_partidos, data=df, family=binomial(link=logit))


desconfia1 <- glm(tercil_desconfia~ votos_partidos, data=df, family=binomial(link=logit))


antidemocr1 <- glm(tercil_antidemocr~ votos_partidos, data=df, family=binomial(link=logit))


promercado1 <- glm(tercil_promercado ~ votos_partidos, data=df, family=binomial(link=logit))



#2 Valores ~ Estrturas
df$Religiao_SIM <- df$religion != "Do not belong to a denomination"
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism2 <- glm(tercil_fundamentalism ~ Genero_Mulher+
                        idade_faixa+escolaridade_niveis+
                        status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                      #+raca_branc
                      , data=df, family=binomial(link=logit))



desconfia2 <- glm(tercil_desconfia~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                 #+raca_branc
                 , data=df, family=binomial(link=logit))


antidemocr2 <- glm(tercil_antidemocr~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))


promercado2 <- glm(tercil_promercado ~ Genero_Mulher+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+RendaBaixa_Subjetivo+Religiao_SIM
                  #+raca_branc
                  , data=df, family=binomial(link=logit))



rm(list=ls())






