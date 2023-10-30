#gerar trinidad e tobago
library(readr)
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
WVS_TimeSeries_4_0 <- read_csv("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/WVS_TimeSeries_4_0.csv")
#780


selecao <- WVS_TimeSeries_4_0$S003 == 780
trinidad <- WVS_TimeSeries_4_0[selecao, ]
table(trinidad$COUNTRY_ALPHA, trinidad$S003)#ok
table(trinidad$S002VS)# 5 e 6!
selecao <- trinidad$S002VS == 5
trinidad5 <- trinidad[selecao, ]
selecao <- trinidad$S002VS == 6
trinidad6 <- trinidad[selecao, ]
table(trinidad5$S002VS)
table(trinidad6$S002VS)

#
trinidad5 <- subset(trinidad5, select=c(E069_07,E069_12,E069_17,E069_06,F028,
                                        E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))

#trinidad5
summary(trinidad5)
trinidad5 <- subset(trinidad5, select = -15)#remover colunna 15


# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 1:14) {
  trinidad5[, i][trinidad5[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(trinidad5)
summary(trinidad5)
imp <- mice(trinidad5, seed=23109)# o nome da base e a seed sempre essa 23109
trinidad5 <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(trinidad5)

trinidad5$F118_invertido <- -1*trinidad5$F118
trinidad5$F118_invertido <- scales::rescale(trinidad5$F118_invertido, to = c(0, 1))
table(trinidad5$F118)
table(trinidad5$F118_invertido)

trinidad5$F120_invertido <- -1*trinidad5$F120
trinidad5$F120_invertido <- scales::rescale(trinidad5$F120_invertido, to = c(0, 1))
trinidad5$F028_invertido <- -1*trinidad5$F028
trinidad5$F028_invertido <- scales::rescale(trinidad5$F028_invertido, to = c(0, 1))
trinidad5$E039_invertido <- -1*trinidad5$E039
trinidad5$E039_invertido <- scales::rescale(trinidad5$E039_invertido, to = c(0, 1))
trinidad5$E040_invertido <- -1*trinidad5$E040
trinidad5$E040_invertido <- scales::rescale(trinidad5$E040_invertido, to = c(0, 1))
trinidad5$E036_invertido <- -1*trinidad5$E036
trinidad5$E036_invertido <- scales::rescale(trinidad5$E036_invertido, to = c(0, 1))
trinidad5$E116_invertido <- -1*trinidad5$E116
trinidad5$E116_invertido <- scales::rescale(trinidad5$E116_invertido, to = c(0, 1))
trinidad5$E114_invertido <- -1*trinidad5$E114
trinidad5$E114_invertido <- scales::rescale(trinidad5$E114_invertido, to = c(0, 1))
#trinidad5$E235_invertido <- -1*trinidad5$E235
#trinidad5$E235_invertido <- scales::rescale(trinidad5$E235_invertido, to = c(0, 1))


#table(trinidad5$E235)
#table(trinidad5$E235_invertido)

trinidad5 <- subset(trinidad5, select=c(
  E069_07,E069_12,E069_17,E069_06,F118_invertido,
  F120_invertido,F028_invertido,
  E037,E040_invertido,E036_invertido,
  E116_invertido,E114_invertido,E117,E039_invertido))

summary(trinidad5[,1:12])#verificar



trinidad5$E069_07 <- scales::rescale(trinidad5$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
trinidad5$E069_12 <- scales::rescale(trinidad5$E069_12, to = c(0, 1))
trinidad5$E069_17 <- scales::rescale(trinidad5$E069_17, to = c(0, 1))
trinidad5$E069_06 <- scales::rescale(trinidad5$E069_06, to = c(0, 1))
trinidad5$E037 <- scales::rescale(trinidad5$E037, to = c(0, 1))
trinidad5$E117 <- scales::rescale(trinidad5$E117, to = c(0, 1))

summary(trinidad5)#verificar

#obs 

#

psych::scree(trinidad5)
nfactors((trinidad5))
# Especifique o modelo de CFA com 3 fatores
model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ F118_invertido + F120_invertido + F028_invertido
  Desconfiança =~ E069_07 + E069_12 + E069_17 + E069_06
  AntiDemocracia =~ E117 + E114_invertido + E116_invertido
  ProMercado_Meritocracia =~E039_invertido+ E040_invertido+ E036_invertido 

  # Especificação das variâncias dos erros
  E069_07 ~~ E069_07
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
#trinidad5ao$E037#pro estado # removi



# Ajuste do modelo de CFA
fit <- cfa(model, data = trinidad5)

summary(fit)
#> ver resultado na imagem ethiopia 2.bmp


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
scores <- lavPredict(fit)
scores[,1] -> trinidad5$fundamentalism
scores[,2] -> trinidad5$desconfia
#trinidad5ao$E117#ok
scores[,3] -> trinidad5$antidemocr
scores[,4] -> trinidad5$promercado 
rm(scores,labels,model,values)

trinidad5 <- subset(trinidad5, select=c(fundamentalism,
                                        desconfia,
                                        promercado,
                                        antidemocr))

trinidad6 <- subset(trinidad6, select=c(E069_07,E069_12,E069_17,E069_06,F028,
                                        E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))

trinidad6
summary(trinidad6)
trinidad6 <- subset(trinidad6, select = -15)#remover colunna 15


# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 1:14) {
  trinidad6[, i][trinidad6[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(trinidad6)
summary(trinidad6)
imp <- mice(trinidad6, seed=23109)# o nome da base e a seed sempre essa 23109
trinidad6 <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(trinidad6)

trinidad6$F118_invertido <- -1*trinidad6$F118
trinidad6$F118_invertido <- scales::rescale(trinidad6$F118_invertido, to = c(0, 1))
table(trinidad6$F118)
table(trinidad6$F118_invertido)

trinidad6$F120_invertido <- -1*trinidad6$F120
trinidad6$F120_invertido <- scales::rescale(trinidad6$F120_invertido, to = c(0, 1))
trinidad6$F028_invertido <- -1*trinidad6$F028
trinidad6$F028_invertido <- scales::rescale(trinidad6$F028_invertido, to = c(0, 1))
trinidad6$E039_invertido <- -1*trinidad6$E039
trinidad6$E039_invertido <- scales::rescale(trinidad6$E039_invertido, to = c(0, 1))
trinidad6$E040_invertido <- -1*trinidad6$E040
trinidad6$E040_invertido <- scales::rescale(trinidad6$E040_invertido, to = c(0, 1))
trinidad6$E036_invertido <- -1*trinidad6$E036
trinidad6$E036_invertido <- scales::rescale(trinidad6$E036_invertido, to = c(0, 1))
trinidad6$E116_invertido <- -1*trinidad6$E116
trinidad6$E116_invertido <- scales::rescale(trinidad6$E116_invertido, to = c(0, 1))
trinidad6$E114_invertido <- -1*trinidad6$E114
trinidad6$E114_invertido <- scales::rescale(trinidad6$E114_invertido, to = c(0, 1))
#trinidad6$E235_invertido <- -1*trinidad6$E235
#trinidad6$E235_invertido <- scales::rescale(trinidad6$E235_invertido, to = c(0, 1))


#table(trinidad6$E235)
#table(trinidad6$E235_invertido)

trinidad6 <- subset(trinidad6, select=c(
  E069_07,E069_12,E069_17,E069_06,F118_invertido,
  F120_invertido,F028_invertido,
  E037,E040_invertido,E036_invertido,
  E116_invertido,E114_invertido,E117,E039_invertido))

summary(trinidad6[,1:12])#verificar



trinidad6$E069_07 <- scales::rescale(trinidad6$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
trinidad6$E069_12 <- scales::rescale(trinidad6$E069_12, to = c(0, 1))
trinidad6$E069_17 <- scales::rescale(trinidad6$E069_17, to = c(0, 1))
trinidad6$E069_06 <- scales::rescale(trinidad6$E069_06, to = c(0, 1))
trinidad6$E037 <- scales::rescale(trinidad6$E037, to = c(0, 1))
trinidad6$E117 <- scales::rescale(trinidad6$E117, to = c(0, 1))

summary(trinidad6)#verificar

#obs 

#

psych::scree(trinidad6)
nfactors((trinidad6))
# Especifique o modelo de CFA com 3 fatores
model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ F118_invertido + F120_invertido + F028_invertido
  Desconfiança =~ E069_07 + E069_12 + E069_17 + E069_06
  AntiDemocracia =~ E117 + E114_invertido + E116_invertido
  ProMercado_Meritocracia =~E039_invertido+ E040_invertido+ E036_invertido 

  # Especificação das variâncias dos erros
  E069_07 ~~ E069_07
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
#trinidad6ao$E037#pro estado # removi



# Ajuste do modelo de CFA
fit <- cfa(model, data = trinidad6)

summary(fit)
#> ver resultado na imagem ethiopia 2.bmp


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
scores <- lavPredict(fit)
scores[,1] -> trinidad6$fundamentalism
scores[,2] -> trinidad6$desconfia
#trinidad6ao$E117#ok
scores[,3] -> trinidad6$antidemocr
scores[,4] -> trinidad6$promercado 
rm(scores,labels,model,values)

trinidad6 <- subset(trinidad6, select=c(fundamentalism,
                                        desconfia,
                                        promercado,
                                        antidemocr))



hist(trinidad5)
hist(trinidad6)


head(trinidad5)
trinidad5$fundamentalism<- scales::rescale(trinidad5$fundamentalism ,
                                      to = c(0, 1))

trinidad5$desconfia<- scales::rescale(trinidad5$desconfia ,
                                 to = c(0, 1))
trinidad5$promercado<- scales::rescale(trinidad5$promercado ,
                                  to = c(0, 1))
trinidad5$antidemocr<- scales::rescale(trinidad5$antidemocr ,
                                  to = c(0, 1))
head(trinidad5)

trinidad5 <- write.csv(trinidad5,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/trinidad5.csv")


head(trinidad6)
trinidad6$fundamentalism<- scales::rescale(trinidad6$fundamentalism ,
                                      to = c(0, 1))

trinidad6$desconfia<- scales::rescale(trinidad6$desconfia ,
                                 to = c(0, 1))
trinidad6$promercado<- scales::rescale(trinidad6$promercado ,
                                  to = c(0, 1))
trinidad6$antidemocr<- scales::rescale(trinidad6$antidemocr ,
                                  to = c(0, 1))
head(trinidad6)

trinidad6 <- write.csv(trinidad6,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/trinidad6.csv")

