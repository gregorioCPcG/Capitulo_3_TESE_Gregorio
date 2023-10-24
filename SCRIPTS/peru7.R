#peru7
#pacotes
library(haven)
library(mice)
library(labelled)
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(mirt)
mirtCluster()
library(marginaleffects)
library(sjPlot)
library(caret)
library(gridExtra)
library(coefplot)
library(ltm)
library(lavaanPlot)
library(scales)
library(semTools)
#### 

rm(list=ls())
dfao <- read_sav("dfao.sav")
#options(max.print = 5000)
#dfao$S002VS
#dfao$S003
#604=PERU

df <- read_csv("df.csv")
df <- df %>%
  filter(S002VS == 7 & S003==604)#peru7


dfao <- dfao %>%
  filter(S002VS == 7 & S003==604)#peru7

dfao$X048ISO
table(df$X048ISO)#region nao tem
#df$X048ISO -> df$region
df$region <- df$X048ISO == 604001
#table(df$region, useNA = "always")
df$region <- as.factor(df$region)
table(df$region)

table(df$X049)#cidade tamanho # NÂO TEM



print(dfao$X001)
table(df$X001)
df$Gender <- df$X001#gender



print(dfao$X003R2)
table(df$X003R2, useNA = "always")
df$X003R2 -> df$idade_faixa#idade_faixa

#print(dfao$X025R)
table(df$X025R, useNA = "always")#tem escolaridade
df$X025R -> df$escolaridade_niveis
df$escolaridade_niveis[df$escolaridade_niveis==-1]<-NA
table(df$escolaridade_niveis, useNA = "always")

dfao$X028
table(df$X028)#escolher
sum(is.na(df$X028))
# Criando a coluna df$status_emplo com base na coluna df$X028
df$status_emplo <- factor(ifelse(df$X028 == 1, "Full Time",
                                 ifelse(df$X028 == 3, "Self Employed",
                                        ifelse(df$X028 == 5, "Housewife",
                                               ifelse(df$X028 == 6, "Student", "Others")))),
                          levels = c("Full Time", "Self Employed", "Housewife",
                                     "Student", "Others"))

# Visualizando o resultado
table(df$status_emplo)


print(dfao$X051)
table(df$X051)#
df$raca_branc <- df$X051 == 604001
table(df$raca_branc)


print(dfao$F025)
table(df$F025)


# Criando a coluna df$religion com base na coluna df$F025
df$religion <- factor(ifelse(df$F025 == 1, "Catholic",
                             ifelse(df$F025 == 0, "Do not belong to a denomination","Others")),
                      levels = c("Catholic", "Do not belong to a denomination","Others"))

# Visualizando o resultado
table(df$religion)
library(scales)
df$religion <- relevel(df$religion, ref = "Do not belong to a denomination")


#renda--
dfao$X047R_WVS
table(df$X047R_WVS)
df$rend_subj <- factor(df$X047R_WVS, levels = c(-2, 1, 2, 3), labels = c("DK", "Low", "Medium", "High"))
df$rend_subj <- factor(df$rend_subj, levels = c("DK", "Low", "Medium", "High"))
df$rend_subj <- relevel(df$rend_subj, ref = "High")

table(df$rend_subj)

#acima strcuture


#Interesse
dfao$E023
table(df$E023)
df$interesse <- df$E023
df$interesse <- ifelse(df$interesse == -1, NA, df$interesse)
df$interesse <- -1*df$interesse
table(df$interesse)
df$interesse <- scales::rescale(df$interesse, to = c(0, 1))


#esquerda direita
dfao$E033
table(dfao$E033)
# Criando a coluna df$autoloc_dir_esq com base na coluna df$E033
df$autoloc_dir_esq <- factor(ifelse(df$E033 %in% c(1, 2, 3, 4), "Left",
                                    ifelse(df$E033 == 5, "Center",
                                           ifelse(df$E033 %in% c(6, 7, 8, 9, 10), "Right", "Don't Know"))),
                             levels = c("Left", "Center", "Right", "Don't Know"))

# Visualizando o resultado
table(df$autoloc_dir_esq)


#dep
table(df$E179WVS)

df$AccionPopular <- df$E179WVS == 604006 
table(df$AccionPopular)
df$AccionPopular <- as.numeric(df$AccionPopular)
df$AccionPopular <- as.factor(df$AccionPopular)
table(df$AccionPopular)
prop.table(table(df$AccionPopular))
# Criar a coluna df_voto_multinom com base na coluna df$E179WVS
df$df_voto_multinom <- factor(ifelse(df$E179WVS == 604006 , "AccionPopular",
                                     ifelse(df$E179WVS %in% c(-3, -2, -1, 2,3,4),
                                            "DK/NA/blank",
                                            "Others")),
                              levels = c("AccionPopular", "DK/NA/blank", "Others"))

# Visualizar o resultado
table(df$df_voto_multinom)



#iisues
dfao$E069_07#desconfia parlamento
sum(is.na(df$E069_07))
dfao$E069_12#desconfia political parties
sum(is.na(df$E069_12))
dfao$E069_17#desconfia Courts
sum(is.na(df$E069_17))
dfao$E069_06#desconfia police
sum(is.na(df$E069_06))
table(df$E069_06)
dfao$F118#liberal casamento gay  # INVERTER!
dfao$F120#liberal aborto # INVERTER!
dfao$F028#religiosidade_invertida  # INVERTER!
dfao$E039#competicao é ruim # INVERTER! 
dfao$E037#pessoas mais responsáveis
dfao$E040#sucesso_é_sorte # INVERTER!
dfao$E036#governo(estatista)  # INVERTER!
dfao$E114#democratic maior valor (contra strong leader) # INVERTER!
dfao$E116#democratic maior valor (contra army rule) # INVERTER!
dfao$E117#valors positicos anntidemoc
dfao$E235#favor da demcorcracia - INVERTER!




df <- subset(df, select=c(raca_branc,Gender,idade_faixa,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,escolaridade_niveis,
                          AccionPopular,df_voto_multinom,
                          E069_07,E069_12,E069_17,E069_06,F028,
                          E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
summary(df)
summary(df[,1:11])
summary(df[,12:26])#OK
# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 12:26) {
  df[, i][df[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(df[, 12:26])
summary(df[,12:26])
imp <- mice(df[,12:26], seed=23109)# o nome da base e a seed sempre essa 23109
df[,12:26] <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(df[,12:26])

df$F118_invertido <- -1*df$F118
df$F118_invertido <- scales::rescale(df$F118_invertido, to = c(0, 1))
table(df$F118)
table(df$F118_invertido)

df$F120_invertido <- -1*df$F120
df$F120_invertido <- scales::rescale(df$F120_invertido, to = c(0, 1))
df$F028_invertido <- -1*df$F028
df$F028_invertido <- scales::rescale(df$F028_invertido, to = c(0, 1))
df$E039_invertido <- -1*df$E039
df$E039_invertido <- scales::rescale(df$E039_invertido, to = c(0, 1))
df$E040_invertido <- -1*df$E040
df$E040_invertido <- scales::rescale(df$E040_invertido, to = c(0, 1))
df$E036_invertido <- -1*df$E036
df$E036_invertido <- scales::rescale(df$E036_invertido, to = c(0, 1))
df$E116_invertido <- -1*df$E116
df$E116_invertido <- scales::rescale(df$E116_invertido, to = c(0, 1))
df$E114_invertido <- -1*df$E114
df$E114_invertido <- scales::rescale(df$E114_invertido, to = c(0, 1))
df$E235_invertido <- -1*df$E235
df$E235_invertido <- scales::rescale(df$E235_invertido, to = c(0, 1))


table(df$E235)
table(df$E235_invertido)
df <- subset(df, select=c(raca_branc,Gender,idade_faixa,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,escolaridade_niveis,
                          AccionPopular,df_voto_multinom,
                          E069_07,E069_17,E069_06,F118_invertido,
                          F120_invertido,F028_invertido,
                          E040_invertido,E036_invertido,
                          E039_invertido,E069_12,E114_invertido,E116_invertido,E037,
                          E117,E235_invertido))

summary(df[,1:11])#verificar



df$E069_07 <- scales::rescale(df$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
df$E069_12 <- scales::rescale(df$E069_12, to = c(0, 1))
df$E069_17 <- scales::rescale(df$E069_17, to = c(0, 1))
df$E069_06 <- scales::rescale(df$E069_06, to = c(0, 1))
df$E037 <- scales::rescale(df$E037, to = c(0, 1))
df$E117 <- scales::rescale(df$E117, to = c(0, 1))

summary(df[,12:26])#verificar

#obs 

prop.table(table(df$AccionPopular))

#

psych::scree(df[,12:26])
nfactors((df[,12:26]))
# Especifique o modelo de CFA com 4 fatores
model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ F118_invertido + F120_invertido + F028_invertido
  Desconfiança =~ E069_07 + E069_12 + E069_17 + E069_06
  AntiDemocracia =~ E117 + E114_invertido + E116_invertido+E235_invertido
  ProMercado_Meritocracia =~E039_invertido+ E040_invertido+ E036_invertido 

  # Especificação das variâncias dos erros
  E069_07 ~~ E069_07
  E235_invertido~~E235_invertido
  E069_12 ~~ E069_12
  E069_17 ~~ E069_17
  E069_06 ~~ E069_06
  F118_invertido ~~ F118_invertido
  F120_invertido ~~ F120_invertido
  F028_invertido ~~ F028_invertido
  E039_invertido ~~ E039_invertido
  E040_invertido ~~ E040_invertido
  E036_invertido ~~ E036_invertido
  E116_invertido ~~ E116_invertido
  E114_invertido ~~ E114_invertido
  E117 ~~ E117
'
dfao$E037#pro estado # removi



# Ajuste do modelo de CFA
fit <- cfa(model, data = df)

summary(fit)
#>


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))#refazer sem antidemoc


scores <- lavPredict(fit)
scores[,1] -> df$fundamentalism
scores[,2] -> df$desconfia
#dfao$E117#ok
scores[,3] -> df$antidemocr
scores[,4] -> df$promercado 
rm(scores,labels,model,values)


df <- subset(df, select=c(raca_branc,Gender,idade_faixa,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,escolaridade_niveis,
                          AccionPopular,df_voto_multinom,
                          fundamentalism,desconfia,promercado,antidemocr))
rm(dfao,fit)
summary(df)
#modelos
logitAccionPopular_1 <- glm(AccionPopular~Gender+raca_branc+
                          idade_faixa+
                          status_emplo+rend_subj+religion,data = df, family=binomial(link=logit))
logitAccionPopular_2 <- glm(AccionPopular~fundamentalism+antidemocr+
                          desconfia+promercado,data = df, family=binomial(link=logit))
logitAccionPopular_3 <- glm(AccionPopular~autoloc_dir_esq+interesse+escolaridade_niveis+
                          fundamentalism+antidemocr+
                          desconfia+promercado,data = df, family=binomial(link=logit))
logitAccionPopular_4 <- glm(AccionPopular~Gender+raca_branc+
                          idade_faixa+
                          status_emplo+rend_subj+religion+autoloc_dir_esq+interesse+escolaridade_niveis+
                          fundamentalism+antidemocr+
                          desconfia+promercado,
                        data = df, family=binomial(link=logit))
tab_model(logitAccionPopular_1,logitAccionPopular_2,logitAccionPopular_3,logitAccionPopular_4,
          show.ci = F, auto.label = F, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",show.p=T,p.threshold = c(0.05,0.01,0.001))


#multinom
levels(df$df_voto_multinom)
# Definir "AccionPopular" como a categoria de referência
df$df_voto_multinom <- relevel(df$df_voto_multinom, ref = "AccionPopular")

library(nnet)
modeloMulti <- multinom(df_voto_multinom ~ Gender+raca_branc+
                          idade_faixa+
                          status_emplo+rend_subj+religion+autoloc_dir_esq+interesse+escolaridade_niveis+
                          fundamentalism+antidemocr+
                          desconfia+promercado,
                        data = df,Hess = TRUE)
tab_model(modeloMulti)
#
#tercis somente ondas 3,5 e 7
df$tercil_superior_fundamentalism <- ntile(df$fundamentalism, 3)
df$tercil_superior_fundamentalism <- ifelse(df$tercil_superior_fundamentalism == 3, 1, 0)
table(df$tercil_superior_fundamentalism)
df$tercil_superior_antidemocr <- ntile(df$antidemocr, 3)
df$tercil_superior_antidemocr <- ifelse(df$tercil_superior_antidemocr == 3, 1, 0)
table(df$tercil_superior_antidemocr)
df$tercil_superior_desconfia <- ntile(df$desconfia, 3)
df$tercil_superior_desconfia <- ifelse(df$tercil_superior_desconfia == 3, 1, 0)
table(df$tercil_superior_desconfia)
df$tercil_superior_promercado <- ntile(df$promercado, 3)
df$tercil_superior_promercado <- ifelse(df$tercil_superior_promercado == 3, 1, 0)
table(df$tercil_superior_promercado)



logitfundamentalism_tercil <- glm(tercil_superior_fundamentalism~Gender+
                                    idade_faixa+escolaridade_niveis+
                                    status_emplo+rend_subj+religion+autoloc_dir_esq+interesse,
                                  data = df, family=binomial(link=logit))

logitdesconfia_tercil <- glm(tercil_superior_desconfia~Gender+
                               idade_faixa+escolaridade_niveis+
                               status_emplo+rend_subj+religion+autoloc_dir_esq+interesse,
                             data = df, family=binomial(link=logit))

logitantidemocr_tercil <- glm(tercil_superior_antidemocr~Gender+
                                idade_faixa+escolaridade_niveis+
                                status_emplo+rend_subj+religion+autoloc_dir_esq+interesse,
                              data = df, family=binomial(link=logit))

logitpromercado_tercil <- glm(tercil_superior_promercado~Gender+
                                idade_faixa+escolaridade_niveis+
                                status_emplo+rend_subj+religion+autoloc_dir_esq+interesse,
                              data = df, family=binomial(link=logit))


# OBS para mais testes ver molde
tab_model(logitfundamentalism_tercil,logitdesconfia_tercil,logitpromercado_tercil,
          logitantidemocr_tercil,
          show.ci = F, auto.label = F, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",show.p=T,p.threshold = c(0.05,0.01,0.001))

options(scipen = 999)
library(lsr)
df$AccionPopular <- as.numeric(df$AccionPopular)
model<- aov(AccionPopular ~ Gender+raca_branc+
              idade_faixa+escolaridade_niveis+
              status_emplo+religion+rend_subj, data= df)
etaSquared(model)
summary(model)
#

dfao <- read_sav("dfao.sav")
dfao <- dfao %>%
  filter(S002VS == 7 & S003==604)
table(dfao$E179WVS)
# Criar a nova coluna "votos_partidos" no dataframe "df"
df$votos_partidos <- ifelse(dfao$E179WVS < 5, "Nenhum/NSabe",
                            ifelse(dfao$E179WVS == 604006, "Acción Popular",
                                   ifelse(dfao$E179WVS == 604048, "Partido Morado",
                                          ifelse(dfao$E179WVS == 604036, "Alianza para el Progreso del Perú",
                                                 ifelse(dfao$E179WVS == 604026, "Fuerza Popular",
                                                        "Outros Partidos")))))

# Definir a ordem dos níveis na coluna "votos_partidos"
df$votos_partidos <- factor(df$votos_partidos, levels = c("Acción Popular", "Partido Morado", "Alianza para el Progreso del Perú", "Fuerza Popular", "Outros Partidos", "Nenhum/NSabe"))

# Visualizar a nova coluna votos_partidos
table(df$votos_partidos)
#
logitfundamentalism_tercil2 <- glm(tercil_superior_fundamentalism~votos_partidos,
                                   data = df, family=binomial(link=logit))

logitdesconfia_tercil2 <- glm(tercil_superior_desconfia~votos_partidos,
                              data = df, family=binomial(link=logit))

logitpromercado_tercil2 <- glm(tercil_superior_promercado~votos_partidos,
                               data = df, family=binomial(link=logit))

logitantidemocr_tercil2 <- glm(tercil_superior_antidemocr~votos_partidos,
                               data = df, family=binomial(link=logit))


fundamentalismo<-plot_cap(logitfundamentalism_tercil2, condition="votos_partidos",conf_level = .9)
fundamentalismo<-fundamentalismo+labs(title="Prob. tercil superior de Fundamentalismo",
                                      x= "Partidos Peru Onda 7 WVS",y="Probabilidade") + theme_minimal()
fundamentalismo

antidemocr<-plot_cap(logitantidemocr_tercil2, condition="votos_partidos",conf_level = .9)
antidemocr<-antidemocr+labs(title="Prob. tercil superior de Antidemocracia",
                            x= "Partidos Peru Onda 7 WVS",y="Probabilidade") + theme_minimal()
antidemocr

desconfia<-plot_cap(logitdesconfia_tercil2, condition="votos_partidos",conf_level = .9)
desconfia<-desconfia+labs(title="Prob. tercil superior de Desconfiança Institucional",
                          x= "Partidos Peru Onda 7 WVS",y="Probabilidade") + theme_minimal()
desconfia

promercado<-plot_cap(logitpromercado_tercil2, condition="votos_partidos",conf_level = .9)
promercado<-promercado+labs(title="Prob. tercil superior de Visão PróMercado",
                            x= "Partidos Peru Onda 7 WVS",y="Probabilidade") + theme_minimal()
promercado

grid.arrange(fundamentalismo,desconfia,promercado,antidemocr)
