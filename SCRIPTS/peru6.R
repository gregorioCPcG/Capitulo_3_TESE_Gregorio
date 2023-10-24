#peru3

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
options(max.print = 5000)
#dfao$S002VS
dfao$S003
#604=PERU

df <- read_csv("df.csv")
df <- df %>%
  filter(S002VS == 6 & S003==604)#peru6


dfao <- dfao %>%
  filter(S002VS == 6 & S003==604)#peru6

dfao$X048ISO
table(df$X048ISO)#region nao tem
df$X048ISO -> df$region
#df$region[df$region==-5]<-NA
#table(df$region, useNA = "always")
df$region <- as.factor(df$region)


table(df$X049)#cidade tamanho



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
df$interesse <- ifelse(df$interesse == -2, NA, df$interesse)
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

df$GanaPeru_ <- df$E179WVS == 604024 
table(df$GanaPeru_)
df$GanaPeru_ <- as.numeric(df$GanaPeru_)
df$GanaPeru_ <- as.factor(df$GanaPeru_)
table(df$GanaPeru_)
prop.table(table(df$GanaPeru_))
# Criar a coluna df_voto_multinom com base na coluna df$E179WVS
df$df_voto_multinom <- factor(ifelse(df$E179WVS == 604024 , "GanaPeru_",
                                     ifelse(df$E179WVS %in% c(-3, -2, -1, 2,3,4),
                                            "DK/NA/blank",
                                            "Others")),
                              levels = c("GanaPeru_", "DK/NA/blank", "Others"))

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




df <- subset(df, select=c(raca_branc,Gender,idade_faixa,X049,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,escolaridade_niveis,
                          GanaPeru_,df_voto_multinom,
                          E069_07,E069_12,E069_17,E069_06,F028,
                          E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
summary(df)
summary(df[,1:12])
df[,13:27]

summary(df[,13:27])#OK
# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 13:27) {
  df[, i][df[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(df[, 13:27])
summary(df[,13:27])
imp <- mice(df[,13:27], seed=23109)# o nome da base e a seed sempre essa 23109
df[,13:27] <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(df[,13:27])

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
df <- subset(df, select=c(raca_branc,Gender,idade_faixa,X049,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,escolaridade_niveis,
                          GanaPeru_,df_voto_multinom,
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

summary(df[,13:27])#verificar

#obs 

prop.table(table(df$GanaPeru_))

#
summary(df)
psych::scree(df[,13:27])
nfactors((df[,13:27]))
# Especifique o modelo de CFA com 4 fatores
model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ F118_invertido + F120_invertido + F028_invertido
  Desconfiança =~ E069_07 + E069_12 + E069_17 + E069_06

  # Especificação das variâncias dos erros
  E069_07 ~~ E069_07
  E069_12 ~~ E069_12
  E069_17 ~~ E069_17
  E069_06 ~~ E069_06
  F118_invertido ~~ F118_invertido
  F120_invertido ~~ F120_invertido
  F028_invertido ~~ F028_invertido
'


# Ajuste do modelo de CFA
fit <- cfa(model, data = df)

summary(fit)
#>


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))#refazer sem antidemoc


scores <- lavPredict(fit)
scores[,2] -> df$desconfia
rm(scores,labels,model,values)


df <- subset(df, select=c(raca_branc,Gender,idade_faixa,X049,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,escolaridade_niveis,
                          GanaPeru_,df_voto_multinom,desconfia))
rm(dfao,fit)
summary(df)
#modelos
logitGanaPeru__1 <- glm(GanaPeru_~Gender+raca_branc+
                          idade_faixa+X049+
                          status_emplo+rend_subj+religion,data = df, family=binomial(link=logit))
logitGanaPeru__2 <- glm(GanaPeru_~
                          desconfia,data = df, family=binomial(link=logit))
logitGanaPeru__3 <- glm(GanaPeru_~autoloc_dir_esq+interesse+escolaridade_niveis+
                          
                          desconfia,data = df, family=binomial(link=logit))
logitGanaPeru__4 <- glm(GanaPeru_~Gender+raca_branc+
                          idade_faixa+X049+
                          status_emplo+rend_subj+religion+autoloc_dir_esq+interesse+escolaridade_niveis+
                          
                          desconfia,
                        data = df, family=binomial(link=logit))
tab_model(logitGanaPeru__1,logitGanaPeru__2,logitGanaPeru__3,logitGanaPeru__4,
          show.ci = F, auto.label = F, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",show.p=T,p.threshold = c(0.05,0.01,0.001))


#multinom
levels(df$df_voto_multinom)
# Definir "GanaPeru_" como a categoria de referência
df$df_voto_multinom <- relevel(df$df_voto_multinom, ref = "GanaPeru_")

library(nnet)
modeloMulti <- multinom(df_voto_multinom ~ Gender+raca_branc+
                          idade_faixa+X049+
                          status_emplo+rend_subj+religion+autoloc_dir_esq+interesse+escolaridade_niveis+
                          
                          desconfia,
                        data = df,Hess = TRUE)
tab_model(modeloMulti)
