setwd("C:/Users/grego/OneDrive/Desktop/work/BASE NOVA")
library(tidyverse)
df <- read_csv("df.csv")
df <- subset(df,select=c(P38,P40,P47,P37,P36,P14,P35,P39,P50,P51,P52,P53,P54,P55,P58,P17))
setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria")
library(psych)
#INSERIR MISSINGS
table(df$P38)
df$P38[df$P38  == 11] <- NA
df$P38[df$P38  == 12] <- NA
table(df$P40)
df$P40[df$P40  == 11] <- NA
df$P40[df$P40  == 12] <- NA
table(df$P47)
df$P47[df$P47  == 11] <- NA
df$P47[df$P47  == 12] <- NA
table(df$P37)
df$P37[df$P37  == 11] <- NA
df$P37[df$P37  == 12] <- NA
table(df$P36)
df$P36[df$P36  == 11] <- NA
df$P36[df$P36  == 12] <- NA
table(df$P35)
df$P35[df$P35  == 3] <- NA
df$P35[df$P35  == 4] <- NA
table(df$P14)
df$P14[df$P14  == 7] <- NA
table(df$P39)
df$P39[df$P39  == 11] <- NA
df$P39[df$P39  == 12] <- NA



#removendo NA´s das questões P50 a P56 e também da P58

table(df$P50)#
df$P50_recod <- factor(ifelse(df$P50 == 1, "Tanto Faz",
                              ifelse(df$P50 == 2, "Preferível",
                                     ifelse(df$P50 == 3, "Governo autoritário",
                                            "NS/NR"))))
table(df$P50_recod)
df$P50_recod_numeric <- as.numeric(ifelse(is.na(df$P50) | df$P50 == 4 | df$P50 == 5, NA, df$P50))
table(df$P50_recod_numeric)
df$P50_antidemoc_positivo <- ifelse(df$P50_recod_numeric == 2, 0, 1)
table(df$P50_antidemoc_positivo)

table(df$P51)#
df$P51_recod_numeric <- as.numeric(df$P51)
df$P51_recod_numeric[df$P51_recod_numeric %in% c(11, 12)] <- NA
table(df$P51_recod_numeric)
df$P51_recod <- as.character(df$P51)
df$P51_recod[df$P51_recod == "11"] <- "NS/NR"
df$P51_recod[df$P51_recod == "12"] <- "NS/NR"
df$P51_recod[!is.na(df$P51_recod_numeric)] <- as.character(as.numeric(df$P51_recod_numeric[!is.na(df$P51_recod_numeric)]))
df$P51_recod <- factor(df$P51_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "NS/NR"))
table(df$P51_recod)

table(df$P52)#
df$P52_recod_numeric <- as.numeric(df$P52)
df$P52_recod_numeric[df$P52_recod_numeric %in% c(11, 12)] <- NA
table(df$P52_recod_numeric)
df$P52_recod <- as.character(df$P52)
df$P52_recod[df$P52_recod == "11"] <- "NS/NR"
df$P52_recod[df$P52_recod == "12"] <- "NS/NR"
df$P52_recod[!is.na(df$P52_recod_numeric)] <- as.character(as.numeric(df$P52_recod_numeric[!is.na(df$P52_recod_numeric)]))
df$P52_recod <- factor(df$P52_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P52_recod)

table(df$P53)#
df$P53_recod_numeric <- as.numeric(df$P53)
df$P53_recod_numeric[df$P53_recod_numeric %in% c(11, 12)] <- NA
table(df$P53_recod_numeric)
df$P53_recod <- as.character(df$P53)
df$P53_recod[df$P53_recod == "11"] <- "NS/NR"
df$P53_recod[df$P53_recod == "12"] <- "NS/NR"
df$P53_recod[!is.na(df$P53_recod_numeric)] <- as.character(as.numeric(df$P53_recod_numeric[!is.na(df$P53_recod_numeric)]))
df$P53_recod <- factor(df$P53_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P53_recod)

table(df$P54)#
df$P54_recod_numeric <- as.numeric(df$P54)
df$P54_recod_numeric[df$P54_recod_numeric %in% c(11, 12)] <- NA
table(df$P54_recod_numeric)
df$P54_recod <- as.character(df$P54)
df$P54_recod[df$P54_recod == "11"] <- "NS/NR"
df$P54_recod[df$P54_recod == "12"] <- "NS/NR"
df$P54_recod[!is.na(df$P54_recod_numeric)] <- as.character(as.numeric(df$P54_recod_numeric[!is.na(df$P54_recod_numeric)]))
df$P54_recod <- factor(df$P54_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P54_recod)

table(df$P55)#
df$P55_recod_numeric <- as.numeric(df$P55)
df$P55_recod_numeric[df$P55_recod_numeric %in% c(11, 12)] <- NA
table(df$P55_recod_numeric)
df$P55_recod <- as.character(df$P55)
df$P55_recod[df$P55_recod == "11"] <- "NS/NR"
df$P55_recod[df$P55_recod == "12"] <- "NS/NR"
df$P55_recod[!is.na(df$P55_recod_numeric)] <- as.character(as.numeric(df$P55_recod_numeric[!is.na(df$P55_recod_numeric)]))
df$P55_recod <- factor(df$P55_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P55_recod)


table(df$P58)#
df$P58_recod_numeric <- as.numeric(df$P58)
df$P58_recod_numeric[df$P58_recod_numeric %in% c(11, 12)] <- NA
table(df$P58_recod_numeric)
df$P58_recod <- as.character(df$P58)
df$P58_recod[df$P58_recod == "11"] <- "NS/NR"
df$P58_recod[df$P58_recod == "12"] <- "NS/NR"
df$P58_recod[!is.na(df$P58_recod_numeric)] <- as.character(as.numeric(df$P58_recod_numeric[!is.na(df$P58_recod_numeric)]))
df$P58_recod <- factor(df$P58_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P58_recod)


#ok todas estão no formato original, até aqui, no entanto, é necessário:
#padronizar
#padronizar? Deixar todas com o mesmo sinal
#a escolha foi deixar sinal positivo para posição antidemocrática


table(df$P50_antidemoc_positivo)# sinal ok

#P51 10 é totalmente justificável fechar o congresso
table(df$P51_recod_numeric) # ok


#P52  formato original: 10 é “concordo fortemente que a democracia eleitoral é o melhor”
#essa é uma que tem inverter
table(df$P52_recod)#tem q arrumar!
table(df$P52_recod_numeric)#tem q arrumar!

df$P52_recod <- memisc::recode(as.character(df$P52_recod ), "0" <- c("10"),
                               "1" <-c("9"),
                               "2"<-c("8"),
                               "3"<-c("7"),
                               "4"<-c("6"),
                               "5"<-c("5"),
                               "6"<-c("4"),
                               "7"<-c("3"),
                               "8"<-c("2"),
                               "9"<-c("1"),
                               "10"<-c("0"),
                               "NS/NR"<-c("NS/NR"))
df$P52_recod_numeric <- memisc::recode(as.numeric(df$P52_recod_numeric), 0 <- c(10),
                                       1 <- c(9),
                                       2<-c(8),
                                       3<-c(7),
                                       4<-c(6),
                                       5<-c(5),
                                       6<-c(4),
                                       7<-c(3),
                                       8<-c(2),
                                       9<-c(1),
                                       10<-c(0))
table(df$P52_recod)#Conferir se deu certo?
table(df$P52_recod_numeric)#Conferir se deu certo?
#Sim


#próx
#P53 10 é apoia fortemente participação de pessoas em manifestações permitidas por lei
#essa é uma que tem inverter
table(df$P53_recod)#tem q arrumar!
table(df$P53_recod_numeric)#tem q arrumar!

df$P53_recod <- memisc::recode(as.character(df$P53_recod ), "0" <- c("10"),
                               "1" <-c("9"),
                               "2"<-c("8"),
                               "3"<-c("7"),
                               "4"<-c("6"),
                               "5"<-c("5"),
                               "6"<-c("4"),
                               "7"<-c("3"),
                               "8"<-c("2"),
                               "9"<-c("1"),
                               "10"<-c("0"),
                               "NS/NR"<-c("NS/NR"))
df$P53_recod_numeric <- memisc::recode(as.numeric(df$P53_recod_numeric), 0 <- c(10),
                                       1 <- c(9),
                                       2<-c(8),
                                       3<-c(7),
                                       4<-c(6),
                                       5<-c(5),
                                       6<-c(4),
                                       7<-c(3),
                                       8<-c(2),
                                       9<-c(1),
                                       10<-c(0))
table(df$P53_recod)#Conferir se deu certo?
table(df$P53_recod_numeric)#Conferir se deu certo?
#Sim


#próx
#essa é uma que tem inverter
#P54  10 “concordo totalmente que, para poder prender criminosos, elas devem sempre respeitar as leis”
table(df$P54_recod)#tem q arrumar!
table(df$P54_recod_numeric)#tem q arrumar!

df$P54_recod <- memisc::recode(as.character(df$P54_recod ), "0" <- c("10"),
                               "1" <-c("9"),
                               "2"<-c("8"),
                               "3"<-c("7"),
                               "4"<-c("6"),
                               "5"<-c("5"),
                               "6"<-c("4"),
                               "7"<-c("3"),
                               "8"<-c("2"),
                               "9"<-c("1"),
                               "10"<-c("0"),
                               "NS/NR"<-c("NS/NR"))
df$P54_recod_numeric <- memisc::recode(as.numeric(df$P54_recod_numeric), 0 <- c(10),
                                       1 <- c(9),
                                       2<-c(8),
                                       3<-c(7),
                                       4<-c(6),
                                       5<-c(5),
                                       6<-c(4),
                                       7<-c(3),
                                       8<-c(2),
                                       9<-c(1),
                                       10<-c(0))
table(df$P54_recod)#Conferir se deu certo?
table(df$P54_recod_numeric)#Conferir se deu certo?
#Sim


#próx

#P55 10 é “concorda totalmente”, até que ponto o(a) Sr(a) concorda que a vontade da maioria deveria sempre prevalecer, mesmo que prejudique os direitos das minorias

# nao precisa inverter
table(df$P55_recod)
table(df$P55_recod_numeric)


# P58 10 é se justificaria o interv milit/golpe militar
# nao precisa inverter
table(df$P58_recod)
table(df$P58_recod_numeric)



#colocar entre 0 e 1 

table(df$P50_antidemoc_positivo)#ok, não precisa
df$P51_recod_numeric <- scales::rescale(df$P51_recod_numeric, to = c(0, 1))
table(df$P51_recod_numeric)#OK
df$P52_recod_numeric <- scales::rescale(df$P52_recod_numeric, to = c(0, 1))
table(df$P52_recod_numeric)#OK
df$P53_recod_numeric <- scales::rescale(df$P53_recod_numeric, to = c(0, 1))
table(df$P53_recod_numeric)#OK
df$P54_recod_numeric <- scales::rescale(df$P54_recod_numeric, to = c(0, 1))
table(df$P54_recod_numeric)#OK
df$P55_recod_numeric <- scales::rescale(df$P55_recod_numeric, to = c(0, 1))
table(df$P55_recod_numeric)#OK
df$P58_recod_numeric <- scales::rescale(df$P58_recod_numeric, to = c(0, 1))
table(df$P58_recod_numeric)#OK
df$P14_invertido <- -1*df$P14
df$P14_invertido <- scales::rescale(df$P14_invertido, to = c(0, 1))
df$P40_invertido <- -1*df$P40
df$P40_invertido <- scales::rescale(df$P40_invertido, to = c(0, 1))
df$P47_invertido <- -1*df$P47
df$P47_invertido <- scales::rescale(df$P47_invertido, to = c(0, 1))

table(df$P17,useNA="always")
df <- df %>%
  mutate(P17 = ifelse(is.na(P17), -66, P17))
table(df$P17,useNA="always")

# Recodificar os valores da coluna P17 para a coluna P17_recod
df$P17_recod <- ifelse(df$P17 == 1, "PT",
                       ifelse(df$P17 == 14, "PSL",
                              ifelse(df$P17 == -66, "Nenhum", "Outros")))

# Transformar a coluna P17_recod em um fator com a ordem correta
df$P17_recod <- factor(df$P17_recod, levels = c("PT", "PSL", "Outros", "Nenhum"))

# Visualizar a nova coluna P17_recod
table(df$P17_recod)

df<- subset(df,select=c(P38,P40_invertido,P47_invertido,P37,P36,P14_invertido,P35,
                        P50_antidemoc_positivo,P51_recod_numeric,P52_recod_numeric,
                        P55_recod_numeric,P58_recod_numeric,
                        P17_recod))

summary(df)
library(mice)
imp <- mice(df[,1:13], seed=23109)# o nome da base e a seed sempre essa 23109
df[,1:13] <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(df[,1:13])

library(lavaan)
library(semTools)
model <- 'AntiDemocr =~ P51_recod_numeric +P52_recod_numeric+ P55_recod_numeric + P58_recod_numeric
LiberalFundamentalismo =~ P47_invertido+P14_invertido+P40_invertido
Economia =~P35+P36+P37+P38'
# Rodando a análise fatorial confirmatória
cfa <- cfa(model, data = df)
summary(cfa, standardized = TRUE)
scores <- lavPredict(cfa)
scores[,2] -> df$fundamentalism
-1*scores[,3] -> df$promercado
scores[,1] -> df$antidemocr


#

df$tercil_superior_fundamentalism <- ntile(df$fundamentalism, 3)
df$tercil_superior_fundamentalism <- ifelse(df$tercil_superior_fundamentalism == 3, 1, 0)
table(df$tercil_superior_fundamentalism)
df$tercil_superior_antidemocr <- ntile(df$antidemocr, 3)
df$tercil_superior_antidemocr <- ifelse(df$tercil_superior_antidemocr == 3, 1, 0)
table(df$tercil_superior_antidemocr)
df$tercil_superior_promercado <- ntile(df$promercado, 3)
df$tercil_superior_promercado <- ifelse(df$tercil_superior_promercado == 3, 1, 0)
table(df$tercil_superior_promercado)

logitfundamentalism_tercil2 <- glm(tercil_superior_fundamentalism~P17_recod,
                                   data = df, family=binomial(link=logit))

logitpromercado_tercil2 <- glm(tercil_superior_promercado~P17_recod,
                               data = df, family=binomial(link=logit))

logitantidemocr_tercil2 <- glm(tercil_superior_antidemocr~P17_recod,
                               data = df, family=binomial(link=logit))

library(marginaleffects)
fundamentalismo<-plot_cap(logitfundamentalism_tercil2, condition="P17_recod",conf_level = .9)
fundamentalismo<-fundamentalismo+labs(title="Prob. tercil superior de Fundamentalismo",
                                      x= "Partidos Brasil 2023 BDC",y="Probabilidade") + theme_minimal()
fundamentalismo

antidemocr<-plot_cap(logitantidemocr_tercil2, condition="P17_recod",conf_level = .9)
antidemocr<-antidemocr+labs(title="Prob. tercil superior de Antidemocracia",
                            x= "Partidos Brasil 2023 BDC",y="Probabilidade") + theme_minimal()
antidemocr

promercado<-plot_cap(logitpromercado_tercil2, condition="P17_recod",conf_level = .9)
promercado<-promercado+labs(title="Prob. tercil superior de Visão PróMercado",
                            x= "Partidos Brasil 2023 BDC",y="Probabilidade") + theme_minimal()
promercado
library(gridExtra)
grid.arrange(fundamentalismo,promercado,antidemocr,nrow=2)

