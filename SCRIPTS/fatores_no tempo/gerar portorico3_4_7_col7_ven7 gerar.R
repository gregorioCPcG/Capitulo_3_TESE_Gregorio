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

# Lista de todos os objetos no ambiente de trabalho
objetos_no_ambiente <- ls()

# Remova todos os objetos, exceto 'df'
objetos_para_remover <- setdiff(objetos_no_ambiente, "WVS_TimeSeries_4_0")

# Remova os objetos da lista
rm(list = objetos_para_remover)


selecao <- WVS_TimeSeries_4_0$S003 == 630
portoricogeral <- WVS_TimeSeries_4_0[selecao, ]
table(portoricogeral$S002VS)
selecao <- portoricogeral$S002VS == 3
portorico3d <- portoricogeral[selecao, ]
selecao <- portoricogeral$S002VS == 7
portorico7d <- portoricogeral[selecao, ]
selecao <- WVS_TimeSeries_4_0$S002VS == 3
ven <- WVS_TimeSeries_4_0[selecao, ]
selecao <- ven$S003 == 862
ven3 <- ven[selecao, ]
table(ven3$S002VS)
table(portorico3d$S002VS)
selecao <- WVS_TimeSeries_4_0$S002VS == 5
col <- WVS_TimeSeries_4_0[selecao, ]
selecao <- col$S003 == 170
col5 <- col[selecao, ]
portorico3d <- subset(portorico3d, select=c(E069_07,E069_12,E069_17,E069_06,F028,
                                            E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
portorico7d <- subset(portorico7d, select=c(E069_07,E069_12,E069_17,E069_06,F028,
                                            E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
ven3 <- subset(ven3, select=c(E069_07,E069_12,E069_17,E069_06,F028,
                                            E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
col5 <- subset(col5, select=c(E069_07,E069_12,E069_17,E069_06,F028,
                                            E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
#portorico3d
summary(portorico3d)
portorico3d <- subset(portorico3d, select = -15)#remover colunna NA


# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 1:14) {
  portorico3d[, i][portorico3d[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(portorico3d)
summary(portorico3d)
imp <- mice(portorico3d, seed=23109)# o nome da base e a seed sempre essa 23109
portorico3d <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(portorico3d)

portorico3d$F118_invertido <- -1*portorico3d$F118
portorico3d$F118_invertido <- scales::rescale(portorico3d$F118_invertido, to = c(0, 1))
table(portorico3d$F118)
table(portorico3d$F118_invertido)

portorico3d$F120_invertido <- -1*portorico3d$F120
portorico3d$F120_invertido <- scales::rescale(portorico3d$F120_invertido, to = c(0, 1))
portorico3d$F028_invertido <- -1*portorico3d$F028
portorico3d$F028_invertido <- scales::rescale(portorico3d$F028_invertido, to = c(0, 1))
portorico3d$E039_invertido <- -1*portorico3d$E039
portorico3d$E039_invertido <- scales::rescale(portorico3d$E039_invertido, to = c(0, 1))
portorico3d$E040_invertido <- -1*portorico3d$E040
portorico3d$E040_invertido <- scales::rescale(portorico3d$E040_invertido, to = c(0, 1))
portorico3d$E036_invertido <- -1*portorico3d$E036
portorico3d$E036_invertido <- scales::rescale(portorico3d$E036_invertido, to = c(0, 1))
portorico3d$E116_invertido <- -1*portorico3d$E116
portorico3d$E116_invertido <- scales::rescale(portorico3d$E116_invertido, to = c(0, 1))
portorico3d$E114_invertido <- -1*portorico3d$E114
portorico3d$E114_invertido <- scales::rescale(portorico3d$E114_invertido, to = c(0, 1))
#portorico3d$E235_invertido <- -1*portorico3d$E235
#portorico3d$E235_invertido <- scales::rescale(portorico3d$E235_invertido, to = c(0, 1))


#table(portorico3d$E235)
#table(portorico3d$E235_invertido)

portorico3d <- subset(portorico3d, select=c(
  E069_07,E069_12,E069_17,E069_06,F118_invertido,
  F120_invertido,F028_invertido,
  E037,E040_invertido,E036_invertido,
  E116_invertido,E114_invertido,E117,E039_invertido))

summary(portorico3d[,1:12])#verificar



portorico3d$E069_07 <- scales::rescale(portorico3d$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
portorico3d$E069_12 <- scales::rescale(portorico3d$E069_12, to = c(0, 1))
portorico3d$E069_17 <- scales::rescale(portorico3d$E069_17, to = c(0, 1))
portorico3d$E069_06 <- scales::rescale(portorico3d$E069_06, to = c(0, 1))
portorico3d$E037 <- scales::rescale(portorico3d$E037, to = c(0, 1))
portorico3d$E117 <- scales::rescale(portorico3d$E117, to = c(0, 1))

summary(portorico3d)#verificar

#obs 

#

psych::scree(portorico3d)
nfactors((portorico3d))
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
portorico3dao$E037#pro estado # removi



# Ajuste do modelo de CFA
fit <- cfa(model, data = portorico3d)

summary(fit)
#> ver resultado na imagem ethiopia 2.bmp


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
scores <- lavPredict(fit)
scores[,1] -> portorico3d$fundamentalism
scores[,2] -> portorico3d$desconfia
portorico3dao$E117#ok
scores[,3] -> portorico3d$antidemocr
scores[,4] -> portorico3d$promercado 
rm(scores,labels,model,values)

portorico3d <- subset(portorico3d, select=c(fundamentalism,
                                            desconfia,
                                            promercado,
                                            antidemocr))

#portorico7
summary(portorico7d)
portorico7d <- subset(portorico7d, select = -15)#remover colunna 15 (para ficar igual )


# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 1:14) {
  portorico7d[, i][portorico7d[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(portorico7d)
summary(portorico7d)
imp <- mice(portorico7d, seed=23109)# o nome da base e a seed sempre essa 23109
portorico7d <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(portorico7d)

portorico7d$F118_invertido <- -1*portorico7d$F118
portorico7d$F118_invertido <- scales::rescale(portorico7d$F118_invertido, to = c(0, 1))
table(portorico7d$F118)
table(portorico7d$F118_invertido)

portorico7d$F120_invertido <- -1*portorico7d$F120
portorico7d$F120_invertido <- scales::rescale(portorico7d$F120_invertido, to = c(0, 1))
portorico7d$F028_invertido <- -1*portorico7d$F028
portorico7d$F028_invertido <- scales::rescale(portorico7d$F028_invertido, to = c(0, 1))
portorico7d$E039_invertido <- -1*portorico7d$E039
portorico7d$E039_invertido <- scales::rescale(portorico7d$E039_invertido, to = c(0, 1))
portorico7d$E040_invertido <- -1*portorico7d$E040
portorico7d$E040_invertido <- scales::rescale(portorico7d$E040_invertido, to = c(0, 1))
portorico7d$E036_invertido <- -1*portorico7d$E036
portorico7d$E036_invertido <- scales::rescale(portorico7d$E036_invertido, to = c(0, 1))
portorico7d$E116_invertido <- -1*portorico7d$E116
portorico7d$E116_invertido <- scales::rescale(portorico7d$E116_invertido, to = c(0, 1))
portorico7d$E114_invertido <- -1*portorico7d$E114
portorico7d$E114_invertido <- scales::rescale(portorico7d$E114_invertido, to = c(0, 1))
#portorico7d$E235_invertido <- -1*portorico7d$E235
#portorico7d$E235_invertido <- scales::rescale(portorico7d$E235_invertido, to = c(0, 1))


#table(portorico7d$E235)
#table(portorico7d$E235_invertido)

portorico7d <- subset(portorico7d, select=c(
  E069_07,E069_12,E069_17,E069_06,F118_invertido,
  F120_invertido,F028_invertido,
  E037,E040_invertido,E036_invertido,
  E116_invertido,E114_invertido,E117,E039_invertido))

summary(portorico7d[,1:12])#verificar



portorico7d$E069_07 <- scales::rescale(portorico7d$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
portorico7d$E069_12 <- scales::rescale(portorico7d$E069_12, to = c(0, 1))
portorico7d$E069_17 <- scales::rescale(portorico7d$E069_17, to = c(0, 1))
portorico7d$E069_06 <- scales::rescale(portorico7d$E069_06, to = c(0, 1))
portorico7d$E037 <- scales::rescale(portorico7d$E037, to = c(0, 1))
portorico7d$E117 <- scales::rescale(portorico7d$E117, to = c(0, 1))

summary(portorico7d)#verificar

#obs 

#

psych::scree(portorico7d)
nfactors((portorico7d))
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
#portorico7dao$E037#pro estado # removi



# Ajuste do modelo de CFA
fit <- cfa(model, data = portorico7d)

summary(fit)
#> ver resultado na imagem ethiopia 2.bmp


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
scores <- lavPredict(fit)
scores[,1] -> portorico7d$fundamentalism
scores[,2] -> portorico7d$desconfia
#portorico7dao$E117#ok
scores[,3] -> portorico7d$antidemocr
scores[,4] -> portorico7d$promercado 
rm(scores,labels,model,values)

portorico7d <- subset(portorico7d, select=c(fundamentalism,
                                            desconfia,
                                            promercado,
                                            antidemocr))
#portorico7
summary(col5)
col5 <- subset(col5, select = -15)#remover colunna 15 (para ficar igual )


# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 1:14) {
  col5[, i][col5[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(col5)
summary(col5)
imp <- mice(col5, seed=23109)# o nome da base e a seed sempre essa 23109
col5 <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(col5)

col5$F118_invertido <- -1*col5$F118
col5$F118_invertido <- scales::rescale(col5$F118_invertido, to = c(0, 1))
table(col5$F118)
table(col5$F118_invertido)

col5$F120_invertido <- -1*col5$F120
col5$F120_invertido <- scales::rescale(col5$F120_invertido, to = c(0, 1))
col5$F028_invertido <- -1*col5$F028
col5$F028_invertido <- scales::rescale(col5$F028_invertido, to = c(0, 1))
col5$E039_invertido <- -1*col5$E039
col5$E039_invertido <- scales::rescale(col5$E039_invertido, to = c(0, 1))
col5$E040_invertido <- -1*col5$E040
col5$E040_invertido <- scales::rescale(col5$E040_invertido, to = c(0, 1))
col5$E036_invertido <- -1*col5$E036
col5$E036_invertido <- scales::rescale(col5$E036_invertido, to = c(0, 1))
col5$E116_invertido <- -1*col5$E116
col5$E116_invertido <- scales::rescale(col5$E116_invertido, to = c(0, 1))
col5$E114_invertido <- -1*col5$E114
col5$E114_invertido <- scales::rescale(col5$E114_invertido, to = c(0, 1))
#col5$E235_invertido <- -1*col5$E235
#col5$E235_invertido <- scales::rescale(col5$E235_invertido, to = c(0, 1))


#table(col5$E235)
#table(col5$E235_invertido)

col5 <- subset(col5, select=c(
  E069_07,E069_12,E069_17,E069_06,F118_invertido,
  F120_invertido,F028_invertido,
  E037,E040_invertido,E036_invertido,
  E116_invertido,E114_invertido,E117,E039_invertido))

summary(col5[,1:12])#verificar



col5$E069_07 <- scales::rescale(col5$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
col5$E069_12 <- scales::rescale(col5$E069_12, to = c(0, 1))
col5$E069_17 <- scales::rescale(col5$E069_17, to = c(0, 1))
col5$E069_06 <- scales::rescale(col5$E069_06, to = c(0, 1))
col5$E037 <- scales::rescale(col5$E037, to = c(0, 1))
col5$E117 <- scales::rescale(col5$E117, to = c(0, 1))

summary(col5)#verificar

#obs 

#

psych::scree(col5)
nfactors((col5))
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
#col5ao$E037#pro estado # removi



# Ajuste do modelo de CFA
fit <- cfa(model, data = col5)

summary(fit)
#> ver resultado na imagem ethiopia 2.bmp


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
scores <- lavPredict(fit)
scores[,1] -> col5$fundamentalism
scores[,2] -> col5$desconfia
#col5ao$E117#ok
scores[,3] -> col5$antidemocr
scores[,4] -> col5$promercado 
rm(scores,labels,model,values)

col5 <- subset(col5, select=c(fundamentalism,
                              desconfia,
                              promercado,
                              antidemocr))
#ven3
summary(ven3)
ven3 <- subset(ven3, select = -15)#remover colunna 15 (para ficar igual )


# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 1:14) {
  ven3[, i][ven3[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(ven3)
summary(ven3)
imp <- mice(ven3, seed=23109)# o nome da base e a seed sempre essa 23109
ven3 <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(ven3)

ven3$F118_invertido <- -1*ven3$F118
ven3$F118_invertido <- scales::rescale(ven3$F118_invertido, to = c(0, 1))
table(ven3$F118)
table(ven3$F118_invertido)

ven3$F120_invertido <- -1*ven3$F120
ven3$F120_invertido <- scales::rescale(ven3$F120_invertido, to = c(0, 1))
ven3$F028_invertido <- -1*ven3$F028
ven3$F028_invertido <- scales::rescale(ven3$F028_invertido, to = c(0, 1))
ven3$E039_invertido <- -1*ven3$E039
ven3$E039_invertido <- scales::rescale(ven3$E039_invertido, to = c(0, 1))
ven3$E040_invertido <- -1*ven3$E040
ven3$E040_invertido <- scales::rescale(ven3$E040_invertido, to = c(0, 1))
ven3$E036_invertido <- -1*ven3$E036
ven3$E036_invertido <- scales::rescale(ven3$E036_invertido, to = c(0, 1))
ven3$E116_invertido <- -1*ven3$E116
ven3$E116_invertido <- scales::rescale(ven3$E116_invertido, to = c(0, 1))
ven3$E114_invertido <- -1*ven3$E114
ven3$E114_invertido <- scales::rescale(ven3$E114_invertido, to = c(0, 1))
#ven3$E235_invertido <- -1*ven3$E235
#ven3$E235_invertido <- scales::rescale(ven3$E235_invertido, to = c(0, 1))


#table(ven3$E235)
#table(ven3$E235_invertido)

ven3 <- subset(ven3, select=c(
  E069_07,E069_12,E069_17,E069_06,F118_invertido,
  F120_invertido,F028_invertido,
  E037,E040_invertido,E036_invertido,
  E116_invertido,E114_invertido,E117,E039_invertido))

summary(ven3[,1:12])#verificar



ven3$E069_07 <- scales::rescale(ven3$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
ven3$E069_12 <- scales::rescale(ven3$E069_12, to = c(0, 1))
ven3$E069_17 <- scales::rescale(ven3$E069_17, to = c(0, 1))
ven3$E069_06 <- scales::rescale(ven3$E069_06, to = c(0, 1))
ven3$E037 <- scales::rescale(ven3$E037, to = c(0, 1))
ven3$E117 <- scales::rescale(ven3$E117, to = c(0, 1))

summary(ven3)#verificar

#obs 

#

psych::scree(ven3)
nfactors((ven3))
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
#ven3ao$E037#pro estado # removi



# Ajuste do modelo de CFA
fit <- cfa(model, data = ven3)

summary(fit)
#> ver resultado na imagem ethiopia 2.bmp


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
scores <- lavPredict(fit)
scores[,1] -> ven3$fundamentalism
scores[,2] -> ven3$desconfia
#ven3ao$E117#ok
scores[,3] -> ven3$antidemocr
scores[,4] -> ven3$promercado 
rm(scores,labels,model,values)

ven3 <- subset(ven3, select=c(fundamentalism,
                              desconfia,
                              promercado,
                              antidemocr))
head(portorico3d)
portorico3d$fundamentalism<- scales::rescale(portorico3d$fundamentalism ,
                                             to = c(0, 1))

portorico3d$desconfia<- scales::rescale(portorico3d$desconfia ,
                                        to = c(0, 1))
portorico3d$promercado<- scales::rescale(portorico3d$promercado ,
                                         to = c(0, 1))
portorico3d$antidemocr<- scales::rescale(portorico3d$antidemocr ,
                                         to = c(0, 1))
head(portorico3d)

portorico3d <- write.csv(portorico3d,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/portorico3.csv")

head(portorico7d)
portorico7d$fundamentalism<- scales::rescale(portorico7d$fundamentalism ,
                                             to = c(0, 1))

portorico7d$desconfia<- scales::rescale(portorico7d$desconfia ,
                                        to = c(0, 1))
portorico7d$promercado<- scales::rescale(portorico7d$promercado ,
                                         to = c(0, 1))
portorico7d$antidemocr<- scales::rescale(portorico7d$antidemocr ,
                                         to = c(0, 1))
head(portorico7d)

portorico7d <- write.csv(portorico7d,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/portorico7.csv")


head(col5)
col5$fundamentalism<- scales::rescale(col5$fundamentalism ,
                                      to = c(0, 1))

col5$desconfia<- scales::rescale(col5$desconfia ,
                                 to = c(0, 1))
col5$promercado<- scales::rescale(col5$promercado ,
                                  to = c(0, 1))
col5$antidemocr<- scales::rescale(col5$antidemocr ,
                                  to = c(0, 1))
head(col5)

col5 <- write.csv(col5,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/col5.csv")


head(ven3)
ven3$fundamentalism<- scales::rescale(ven3$fundamentalism ,
                                      to = c(0, 1))

ven3$desconfia<- scales::rescale(ven3$desconfia ,
                                 to = c(0, 1))
ven3$promercado<- scales::rescale(ven3$promercado ,
                                  to = c(0, 1))
ven3$antidemocr<- scales::rescale(ven3$antidemocr ,
                                  to = c(0, 1))
head(ven3)

ven3 <- write.csv(ven3,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/ven3.csv")

