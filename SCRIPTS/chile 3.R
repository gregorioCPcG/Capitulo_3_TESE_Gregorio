#Chile 3

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
df <- read_csv("df.csv")
df <- df %>%
  filter(S002VS == 3 & S003==152)


dfao <- dfao %>%
  filter(S002VS == 3 & S003==152)

dfao$X048ISO
table(df$X048ISO)
df$X048ISO -> df$region
df$region <- as.factor(df$region)


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
df$escolaridade_niveis <- ifelse(df$X025R == -2, NA, df$X025R)
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
table(df$X051)
df$raca_branc <- df$X051 == 152001
table(df$raca_branc)


print(dfao$F025)
table(df$F025)


# Criando a coluna df$religion com base na coluna df$F025
df$religion <- factor(ifelse(df$F025 == 1, "Catholic",
                             ifelse(df$F025 == 0, "Do not belong to a denomination",
                                           "Others")),
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


#[,14]
#dep
dfao$E179WVS
table(dfao$E179WVS)
table(df$E179WVS)

df$DC_ <- df$E179WVS == 152004 
table(df$DC_)
df$DC_ <- as.numeric(df$DC_)
df$DC_ <- as.factor(df$DC_)
table(df$DC_)

# Criar a coluna df_voto_multinom com base na coluna df$E179WVS
df$df_voto_multinom <- factor(ifelse(df$E179WVS == 152004 , "DC_",
                                     ifelse(df$E179WVS %in% c(-3, -2, -1, 2,3),
                                            "DK/NA/blank",
                                            "Others")),
                              levels = c("DC_", "DK/NA/blank", "Others"))

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




df <- subset(df, select=c(X049,raca_branc,Gender,idade_faixa,region,
                          escolaridade_niveis,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,
                          DC_,df_voto_multinom,
                          E069_07,E069_12,E069_17,E069_06,F028,
                          E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
summary(df)
summary(df[,1:13])
df[,14:28]#issues
df <- subset(df, select = -28)#remover colunna 28
summary(df[,14:27])#OK
# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 14:27) {
  df[, i][df[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(df[, 14:27])
summary(df[,14:27])
imp <- mice(df[,14:27], seed=23109)# o nome da base e a seed sempre essa 23109
df[,14:27] <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(df[,14:27])

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
#df$E235_invertido <- -1*df$E235
#df$E235_invertido <- scales::rescale(df$E235_invertido, to = c(0, 1))


#table(df$E235)
#table(df$E235_invertido)

df <- subset(df, select=c(X049,raca_branc,Gender,idade_faixa,region,
                          escolaridade_niveis,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,
                          DC_,df_voto_multinom,
                          E069_07,E069_12,E069_17,E069_06,F118_invertido,
                          F120_invertido,F028_invertido,
                          E037,E040_invertido,E036_invertido,
                          E116_invertido,E114_invertido,E117,E039_invertido))

summary(df[,1:13])#verificar



df$E069_07 <- scales::rescale(df$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
df$E069_12 <- scales::rescale(df$E069_12, to = c(0, 1))
df$E069_17 <- scales::rescale(df$E069_17, to = c(0, 1))
df$E069_06 <- scales::rescale(df$E069_06, to = c(0, 1))
df$E037 <- scales::rescale(df$E037, to = c(0, 1))
df$E117 <- scales::rescale(df$E117, to = c(0, 1))

summary(df[,14:27])#verificar

#obs 

prop.table(table(df$DC_))

#

psych::scree(df[,14:27])
nfactors((df[,14:27]))
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
dfao$E037#pro estado # removi



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
dfao$E117#ok
scores[,3] -> df$antidemocr
#scores[,4] -> df$promercado # nao usamos
rm(scores,labels,model,values)

df <- subset(df, select=c(X049,raca_branc,Gender,idade_faixa,region,
                          escolaridade_niveis,status_emplo,rend_subj,religion,
                          autoloc_dir_esq, interesse,DC_,df_voto_multinom,
                          fundamentalism,desconfia,antidemocr))
rm(dfao,fit)
summary(df)
#modelos
logitDC__1 <- glm(DC_~Gender+X049+region+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+rend_subj+religion,data = df, family=binomial(link=logit))
logitDC__2 <- glm(DC_~fundamentalism+antidemocr+
                    desconfia,data = df, family=binomial(link=logit))
logitDC__3 <- glm(DC_~autoloc_dir_esq+interesse+
                    fundamentalism+antidemocr+
                    desconfia,data = df, family=binomial(link=logit))
logitDC__4 <- glm(DC_~Gender+X049+region+
                    idade_faixa+escolaridade_niveis+
                    status_emplo+rend_subj+religion+autoloc_dir_esq+interesse+
                    fundamentalism+antidemocr+
                    desconfia,
                  data = df, family=binomial(link=logit))
tab_model(logitDC__1,logitDC__2,logitDC__3,logitDC__4,
          show.ci = F, auto.label = F, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",show.p=T,p.threshold = c(0.05,0.01,0.001))


# Calcular o BIC de cada modelo
bic_modelo1 <- BIC(logitDC__1)
bic_modelo2 <- BIC(logitDC__2)
bic_modelo3 <- BIC(logitDC__3)
bic_modelo4 <- BIC(logitDC__4)

# Criar um vetor com os valores do BIC
bic_valores <- c(bic_modelo1, bic_modelo2, bic_modelo3, bic_modelo4)

# Identificar o modelo com menor BIC
melhor_modelo <- which.min(bic_valores)

# Visualizar os valores do BIC e identificar o melhor modelo
print(bic_valores)
print(paste("O melhor modelo é o modelo", melhor_modelo))


#multinom
levels(df$df_voto_multinom)
# Definir "DC_" como a categoria de referência
df$df_voto_multinom <- relevel(df$df_voto_multinom, ref = "DC_")

library(nnet)
modeloMulti <- multinom(df_voto_multinom ~ Gender+X049+region+
                          idade_faixa+escolaridade_niveis+
                          status_emplo+rend_subj+religion+autoloc_dir_esq+interesse+
                          fundamentalism+antidemocr+
                          desconfia,
                        data = df,Hess = TRUE)
tab_model(modeloMulti)
#muito interessante , as mesmas variáveis, mas tem nuances

#tabelas adicionais para ver nuances entre os três tipos de voto:
# nao fiz, mas se quiser molde - ver moldegraficosadicionais.R


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



logitfundamentalism_tercil <- glm(tercil_superior_fundamentalism~Gender+X049+region+
                                    idade_faixa+escolaridade_niveis+
                                    status_emplo+rend_subj+religion+autoloc_dir_esq+interesse,
                                  data = df, family=binomial(link=logit))

logitdesconfia_tercil <- glm(tercil_superior_desconfia~Gender+X049+region+
                               idade_faixa+escolaridade_niveis+
                               status_emplo+rend_subj+religion+autoloc_dir_esq+interesse,
                             data = df, family=binomial(link=logit))

logitantidemocr_tercil <- glm(tercil_superior_antidemocr~Gender+X049+region+
                                idade_faixa+escolaridade_niveis+
                                status_emplo+rend_subj+religion+autoloc_dir_esq+interesse,
                              data = df, family=binomial(link=logit))


# OBS para mais testes ver molde
tab_model(logitfundamentalism_tercil,logitdesconfia_tercil, logitantidemocr_tercil,
          show.ci = F, auto.label = F, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",show.p=T,p.threshold = c(0.05,0.01,0.001))

#eta
options(scipen = 999)
library(lsr)
df$DC_ <- as.numeric(df$DC_)
model<- aov(DC_ ~ Gender+X049+raca_branc+
              idade_faixa+escolaridade_niveis+
              status_emplo+rend_subj+religion, data= df)
etaSquared(model)
summary(model)
#
library(marginaleffects)
dfao <- read_sav("dfao.sav")

dfao <- dfao %>%
  filter(S002VS == 3 & S003==152)
#

table(dfao$E179WVS)
# Mapear os valores da coluna E179WVS para os valores em votos_partidos
df$votos_partidos <- ifelse(dfao$E179WVS < 5, "Nenhum/NSabe",
                            ifelse(dfao$E179WVS == 152008, "Humanista",
                                   ifelse(dfao$E179WVS == 152007, "Socialista",
                                          ifelse(dfao$E179WVS == 152006, "PPD",
                                                 ifelse(dfao$E179WVS == 152004, "DC",
                                                        ifelse(dfao$E179WVS == 152003, "UCC",
                                                               ifelse(dfao$E179WVS == 152002, "RN",
                                                                      ifelse(dfao$E179WVS == 152001, "UDI",
                                                                             "Outros Partidos"))))))))

# Definir a ordem dos níveis na coluna "votos_partidos"
df$votos_partidos <- factor(df$votos_partidos, levels = c("Humanista", "Socialista",
                                                          "PPD", "DC", "UCC", "RN", "UDI", "Outros Partidos","Nenhum/NSabe"))

# Visualizar o resultado
table(df$votos_partidos, useNA="always")
logitfundamentalism_tercil2 <- glm(tercil_superior_fundamentalism~votos_partidos,
                                   data = df, family=binomial(link=logit))

logitdesconfia_tercil2 <- glm(tercil_superior_desconfia~votos_partidos,
                              data = df, family=binomial(link=logit))

logitantidemocr_tercil2 <- glm(tercil_superior_antidemocr~votos_partidos,
                               data = df, family=binomial(link=logit))


fundamentalismo<-plot_cap(logitfundamentalism_tercil2, condition="votos_partidos",conf_level = .9)
fundamentalismo<-fundamentalismo+labs(title="Prob. tercil superior de Fundamentalismo",
                                      x= "Partidos Chile Onda 3 WVS",y="Probabilidade") + theme_minimal()
fundamentalismo

antidemocr<-plot_cap(logitantidemocr_tercil2, condition="votos_partidos",conf_level = .9)
antidemocr<-antidemocr+labs(title="Prob. tercil superior de Antidemocracia",
                            x= "Partidos Chile Onda 3 WVS",y="Probabilidade") + theme_minimal()
antidemocr

desconfia<-plot_cap(logitdesconfia_tercil2, condition="votos_partidos",conf_level = .9)
desconfia<-desconfia+labs(title="Prob. tercil superior de Desconfiança Institucional",
                          x= "Partidos Chile Onda 3 WVS",y="Probabilidade") + theme_minimal()
desconfia


grid.arrange(fundamentalismo,desconfia,antidemocr, nrow=2)

