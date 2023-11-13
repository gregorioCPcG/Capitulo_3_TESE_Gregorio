#
setwd("C:/Users/grego/OneDrive/Desktop/work/BASE NOVA")
library(tidyverse)
df <- read_csv("df.csv")
df <- subset(df,select=c(P38,P40,P47,P37,P36,P14,P35,P39,P50,P51,P52,P53,P54,P55,P58,P17,
                         P10,P12,P6,P8,P60,P13))
setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria")
library(psych)

#estrutura
table(df$P12, useNA = "always")#escolarid ok
df$Escolaridade <- df$P12
table(df$P6,useNA = "always")#ok
df$Mulher <- df$P6 == 2
summary(df$P8)
table(df$P8, useNA = "always")# transformar em faixa
# Recodificar a variável P8 em faixas etárias
df$faixa_etaria <- cut(df$P8, breaks = c(16, 25, 34, 59, 92), labels = c(1, 2, 3, 4), include.lowest = TRUE)
# Converter a nova variável faixa_etaria para numeric
df$faixa_etaria <- as.numeric(df$faixa_etaria)
table(df$faixa_etaria,useNA = "always" )

df$perfilProfissional <- as.factor(df$P10)

table(df$P60,useNA = "always")
df$Renda <- df$P60
df$Religiao <- df$P13

df <- subset(df, select=c(P38,P40,P47,P37,P36,P14,P35,P39,P50,P51,P52,P53,P54,P55,P58,P17,
                          Renda,perfilProfissional,faixa_etaria,Mulher,Escolaridade,Religiao))

summary(df)

#INSERIR MISSINGS das outras
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
                        P17_recod,
                        Renda,perfilProfissional,faixa_etaria,Mulher,Escolaridade,Religiao))

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
summary(df[,13:22])


table(df$Religiao)
table(df$perfilProfissional)

# Defina as regras de recodificação
recode_rules <- c("1" = "Empregado",
                  "2" = "Empregador/ Comerciante/ Patrão",
                  "3" = "Conta Própria",
                  "5" = "Aposentado",
                  "6" = "Doméstico",
                  "7" = "Desempregado",
                  "8" = "Desempregado",
                  "4" = "Estudante",
                  "9" = NA_character_)

# Aplique as regras de recodificação à variável df$perfilProfissional
df$perfilProfissional <- recode(df$perfilProfissional, !!!recode_rules)

table(df$perfilProfissional,useNA = "always")



# Defina as regras de recodificação
recode_rules <- c("1" = "Católica",
                  "3" = "Evangélica",
                  "9" = "Nenhuma")

# Recodifique todos os outros valores como "Outras religiões"
df$Religiao <- recode(df$Religiao, !!!recode_rules, .default = "Outras religiões")
table(df$Religiao,useNA = "always")

#valores ~estrutr
fundamentalism <- lm(fundamentalism ~ Mulher+
                        faixa_etaria+Escolaridade+
                        perfilProfissional+Renda+Religiao+P17_recod 
                      , data=df)


antidemocr <- lm(antidemocr~ Mulher+
                    faixa_etaria+Escolaridade+
                    perfilProfissional+Renda+Religiao+P17_recod 
                  , data=df)

promercado <- lm(promercado ~ Mulher+
                    faixa_etaria+Escolaridade+
                    perfilProfissional+Renda+Religiao+P17_recod 
                  , data=df)

library(scales)
library(marginaleffects)
library(see)
Job_funda<-plot_cap(fundamentalism, condition="perfilProfissional",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Fundamentalismo",
       y= "Fundamentalismo",
       x= "Status Laboral")+coord_flip()
Job_funda
Job_merc<-plot_cap(promercado, condition="perfilProfissional",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Materialismo à direita (pró Mercado)",
       y= "Materialismo à direita",
       x= "Status Laboral")+coord_flip()
Job_merc

Job_autor<-plot_cap(antidemocr , condition="perfilProfissional",
                   conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Autoritarismo (Antidemocrático)",
       y= "Autoritarismo (Antidemocrático)",
       x= "Status Laboral")+coord_flip()
Job_autor

plots(Job_funda,Job_merc,Job_autor, n_columns = 2)
#religi
RELIGARE_funda<-plot_cap(fundamentalism, condition="Religiao",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Fundamentalismo",
       y= "Fundamentalismo",
       x= "Religião")+coord_flip()
RELIGARE_funda
RELIGARE_merc<-plot_cap(promercado, condition="Religiao",
                   conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Materialismo à direita (pró Mercado)",
       y= "Materialismo à direita",
       x= "Religião")+coord_flip()
RELIGARE_merc

RELIGARE_autor<-plot_cap(antidemocr , condition="Religiao",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Autoritarismo (Antidemocrático)",
       y= "Autoritarismo (Antidemocrático)",
       x= "Religião")+coord_flip()
RELIGARE_autor

plots(RELIGARE_funda,RELIGARE_merc,RELIGARE_autor, n_columns = 2)
