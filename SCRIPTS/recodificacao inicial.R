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

mergao <- read_dta("WVS_TimeSeries_4_0.dta")
print(mergao$S003)#paises lista
table(mergao$S003)
df <- mergao %>%
  filter(S002VS != 1)
df <- df %>%
  filter(S002VS != 4)
table(df$S002VS)
table(df$S003)
df <- df %>%
  filter(S003 == 32 | S003 == 76 | S003 == 68 | S003 == 152 |
           S003 == 170 | S003 == 218 | S003 == 320 | S003 == 484 | S003 == 558 |
           S003 == 604 | S003 == 858|S003 == 862)
table(df$S003)
table(df$S003, df$S002VS)
# Manter casos onde df$S003 é igual a 170 e df$S002Vs é igual a 7
colombia7 <- subset(df, S003 == 170 & S002VS == 7)

# Adicionar casos onde df$S003 é igual a 218 e df$S002Vs é igual a 7
equador7 <- subset(df, S003 == 218 & S002VS == 7)


df <- df %>%
  filter(S003 != 218)
df <- df %>%
  filter(S003 != 170)
df <- full_join(df,colombia7)
df <- full_join(df,equador7)
table(df$S003, df$S002VS)
rm(equador7, colombia7)
df -> df2

df <- remove_labels(df)
# Lista de variáveis desejadas
variaveis_desejadas <- c("X001", "X025R", "X048ISO", "X049", "X051", "X007", "X003R2", "X028", "F025", "E023", "E033",
                         "E179WVS", "E069_06", "E069_07", "E069_12", "E069_17", "F028", "F118", "F120", "E036",
                         "E037", "E040", "E114", "E116", "E117", "E235","X045","X047R_WVS","E039")
#29
# Verificar se as variáveis estão presentes no dataframe
variaveis_presentes <- colnames(df) %in% variaveis_desejadas

# Exibir as variáveis que estão presentes
print(colnames(df)[variaveis_presentes])
n <- colnames(df)[variaveis_presentes]
# Verificar as variáveis ausentes em relação à lista desejada
variaveis_faltando <- variaveis_desejadas[!variaveis_desejadas %in% n]

# Exibir as variáveis faltando
print(variaveis_faltando)#zero!


#29 + 2 = 31
df <- subset(df, select = c(S002VS, S003, X001, X025R, X048ISO, X049, X051, X007, X003R2, X028, F025, E023,
                            E033, E179WVS, E069_06, E069_07, E069_12, E069_17, F028, F118, F120, E036,
                            E037, E040, E114, E116, E117, E235, X045, X047R_WVS,E039))

write.csv(df,"df.csv")

df2 <- subset(df2, select = c(S002VS, S003, X001, X025R, X048ISO, X049, X051, X007, X003R2, X028, F025, E023,
                            E033, E179WVS, E069_06, E069_07, E069_12, E069_17, F028, F118, F120, E036,
                            E037, E040, E114, E116, E117, E235, X045, X047R_WVS,E039))
df2 <- write_sav(df2,"dfao.sav")
