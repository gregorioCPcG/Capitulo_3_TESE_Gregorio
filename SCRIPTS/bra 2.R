#brasil 2

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
dfao$S002VS
dfao$S003
#76                                      Brazil

df <- read_csv("df.csv")
df <- df %>%
  filter(S002VS == 2 & S003==76)#brasil 2


dfao <- dfao %>%
  filter(S002VS == 2 & S003==76)#brasil 2

dfao$X048ISO
table(df$X048ISO)#NÃO TEM



print(dfao$X049)
table(df$X049)#cidade tamanho # TEM
sum(is.na(df$X049))


print(dfao$X001)
table(df$X001)
df$Gender <- df$X001#gender



print(dfao$X003R2)
table(df$X003R2, useNA = "always")
df$X003R2 -> df$idade_faixa#idade_faixa

print(dfao$X025R)
table(df$X025R, useNA = "always")
df$escolaridade_niveis <- ifelse(df$X025R == -1, NA, df$X025R)
table(df$escolaridade_niveis, useNA = "always")#ok - escolaridade_niveis

dfao$X028
table(df$X028)#escolher
sum(is.na(df$X028))
# Criando a coluna df$status_emplo com base na coluna df$X028
df$status_emplo <- factor(ifelse(df$X028 == 1, "Full Time",
                                 ifelse(df$X028 == 3, "Self Employed",
                                        ifelse(df$X028 == 5, "Housewife",
                                               ifelse(df$X028 == 4, "Retired",
                                                      ifelse(df$X028 == 6, "Student", "Others"))))),
                          levels = c("Full Time", "Self Employed", "Housewife",
                                     "Retired", "Student", "Others"))

# Visualizando o resultado
table(df$status_emplo)


print(dfao$X051)
table(df$X051)#variavel q nao tem na argentina
df$raca_branc <- df$X051 == 76001
table(df$raca_branc)


print(dfao$F025)
table(df$F025)


# Criando a coluna df$religion com base na coluna df$F025
df$religion <- factor(ifelse(df$F025 == 1, "Catholic",
                             ifelse(df$F025 == 0, "Do not belong to a denomination",
                                    ifelse(df$F025 == 3, "Other Christian (Evangelical/Pentecostal)",
                                           "Others"))),
                      levels = c("Catholic", "Do not belong to a denomination",
                                 "Other Christian (Evangelical/Pentecostal)", "Others"))

# Visualizando o resultado
table(df$religion)
library(scales)
df$religion <- relevel(df$religion, ref = "Do not belong to a denomination")


#renda--
dfao$X047R_WVS
table(df$X047R_WVS)
df$rend_subj <- factor(df$X047R_WVS, levels = c(-1, 1, 2, 3), labels = c("DK", "Low", "Medium", "High"))
df$rend_subj <- factor(df$rend_subj, levels = c("DK", "Low", "Medium", "High"))
df$rend_subj <- relevel(df$rend_subj, ref = "DK")

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


#[,14]
#dep
dfao$E179WVS
table(dfao$E179WVS)
table(df$E179WVS)

df$PT_ <- df$E179WVS == 76002 
table(df$PT_)
df$PT_ <- as.numeric(df$PT_)
df$PT_ <- as.factor(df$PT_)
table(df$PT_)

# Criar a coluna df_voto_multinom com base na coluna df$E179WVS
df$df_voto_multinom <- factor(ifelse(df$E179WVS == 76002 , "PT_",
                                     ifelse(df$E179WVS %in% c(-3, -2, -1, 2,3),
                                            "DK/NA/blank",
                                            "Others")),
                              levels = c("PT_", "DK/NA/blank", "Others"))

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




df <- subset(df, select=c(X049,raca_branc,Gender,idade_faixa,
                          escolaridade_niveis,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,
                          PT_,df_voto_multinom,
                          E069_07,E069_12,E069_17,E069_06,F028,
                          E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
summary(df)
summary(df[,1:12])
df[,13:27]#issues, remover E069_12,E116,E114,E117,E037,E235
df <- subset(df, select = -c(E235,E117,E114,E116,E069_12,
                             E037))#remover colunnas de issues nao usadas na confirmatóriaa

summary(df[,13:21])#OK
# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 13:21) {
  df[, i][df[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(df[, 13:21])
summary(df[,13:21])
imp <- mice(df[,13:21], seed=23109)# o nome da base e a seed sempre essa 23109
df[,13:21] <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(df[,13:21])

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
#df$E116_invertido <- -1*df$E116
#df$E116_invertido <- scales::rescale(df$E116_invertido, to = c(0, 1))
#df$E114_invertido <- -1*df$E114
#df$E114_invertido <- scales::rescale(df$E114_invertido, to = c(0, 1))
#df$E235_invertido <- -1*df$E235
#df$E235_invertido <- scales::rescale(df$E235_invertido, to = c(0, 1))


#table(df$E235)
#table(df$E235_invertido)
#remover E069_12,E116,E114,E117,E037,E235
df <- subset(df, select=c(X049,raca_branc,Gender,idade_faixa,
                          escolaridade_niveis,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,
                          PT_,df_voto_multinom,
                          E069_07,E069_17,E069_06,F118_invertido,
                          F120_invertido,F028_invertido,
                          E040_invertido,E036_invertido,
                          E039_invertido))

summary(df[,1:12])#verificar



df$E069_07 <- scales::rescale(df$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
#df$E069_12 <- scales::rescale(df$E069_12, to = c(0, 1))
df$E069_17 <- scales::rescale(df$E069_17, to = c(0, 1))
df$E069_06 <- scales::rescale(df$E069_06, to = c(0, 1))
#df$E037 <- scales::rescale(df$E037, to = c(0, 1))
#df$E117 <- scales::rescale(df$E117, to = c(0, 1))

summary(df[,13:21])#verificar

#obs 

prop.table(table(df$PT_))

#

psych::scree(df[,13:21])
nfactors((df[,13:21]))
# Especifique o modelo de CFA com 4 fatores / remover E069_12,E116,E114,E117,E037,E235
model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ F118_invertido + F120_invertido + F028_invertido
  Desconfiança =~ E069_07 + E069_17 + E069_06
  ProMercado_Meritocracia =~E039_invertido+ E040_invertido+ E036_invertido 

  # Especificação das variâncias dos erros
  E069_07 ~~ E069_07
  E069_17 ~~ E069_17
  E069_06 ~~ E069_06
  F118_invertido ~~ F118_invertido
  F120_invertido ~~ F120_invertido
  F028_invertido ~~ F028_invertido
  E039_invertido ~~ E039_invertido
  E040_invertido ~~ E040_invertido
  E036_invertido ~~ E036_invertido
'
#dfao$E037#pro estado # removi



# Ajuste do modelo de CFA
fit <- cfa(model, data = df)

summary(fit)
#> ver resultado na imagem ethiopia 2.bmp


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
scores <- lavPredict(fit)
scores[,1] -> df$fundamentalism
scores[,2] -> df$desconfia
#dfao$E117#ok
#scores[,3] -> df$antidemocr
#dfao$E039 # competição é ruim, mas tá invertido , logo é promercado
scores[,3] -> df$promercado#aqui é 3
rm(scores,labels,model,values)

df <- subset(df, select=c(X049,raca_branc,Gender,idade_faixa,
                          escolaridade_niveis,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,PT_,df_voto_multinom,
                          fundamentalism,desconfia,promercado))
rm(dfao,fit)
summary(df)
#modelos
logitPT__1 <- glm(PT_~Gender+X049+raca_branc+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+rend_subj+religion,data = df, family=binomial(link=logit))
logitPT__2 <- glm(PT_~fundamentalism+
                    desconfia+promercado,data = df, family=binomial(link=logit))
logitPT__3 <- glm(PT_~autoloc_dir_esq+interesse+
                    fundamentalism+
                    desconfia+promercado,data = df, family=binomial(link=logit))
logitPT__4 <- glm(PT_~Gender+X049+raca_branc+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+rend_subj+religion+autoloc_dir_esq+interesse+
                    fundamentalism+
                    desconfia+promercado,
                  data = df, family=binomial(link=logit))
tab_model(logitPT__1,logitPT__2,logitPT__3,logitPT__4,
          show.ci = F, auto.label = F, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",show.p=T,p.threshold = c(0.05,0.01,0.001))

# Calcular o BIC de cada modelo
bic_modelo1 <- BIC(logitPT__1)
bic_modelo2 <- BIC(logitPT__2)
bic_modelo3 <- BIC(logitPT__3)
bic_modelo4 <- BIC(logitPT__4)

# Criar um vetor com os valores do BIC
bic_valores <- c(bic_modelo1, bic_modelo2, bic_modelo3, bic_modelo4)

# Identificar o modelo com menor BIC
melhor_modelo <- which.min(bic_valores)

# Visualizar os valores do BIC e identificar o melhor modelo
print(bic_valores)
print(paste("O melhor modelo é o modelo", melhor_modelo))

prop.table(table(df$PT_))
#multinom
levels(df$df_voto_multinom)
# Definir "PT_" como a categoria de referência
df$df_voto_multinom <- relevel(df$df_voto_multinom, ref = "PT_")

library(nnet)
modeloMulti <- multinom(df_voto_multinom ~ Gender+X049+raca_branc+
                          idade_faixa+escolaridade_niveis+
                          status_emplo+rend_subj+religion+autoloc_dir_esq+interesse+
                          fundamentalism+
                          desconfia+promercado,
                        data = df,Hess = TRUE)
tab_model(modeloMulti)


#

