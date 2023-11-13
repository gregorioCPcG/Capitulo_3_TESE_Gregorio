library(tidyverse)
library(marginaleffects)
library(scales)
library(see)
trinidad6 <- read_csv("fatores_no tempo/trinidad6.csv")
WVS_TimeSeries_4_0 <- read_csv("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/WVS_TimeSeries_4_0.csv")
#780
selecao <- WVS_TimeSeries_4_0$S003 == 780
trinidad <- WVS_TimeSeries_4_0[selecao, ]
table(trinidad$COUNTRY_ALPHA, trinidad$S003)#ok
table(trinidad$S002VS)# 5 e 6!
selecao <- trinidad$S002VS == 6
trinidad6_secaum <- trinidad[selecao, ]



trinidad6_secaum$X003R2[trinidad6_secaum$X003R2==-2] <- NA

table(trinidad6_secaum$X025R, useNA = "always")
trinidad6_secaum$escolaridade_niveis <- ifelse(trinidad6_secaum$X025R == -1, NA, trinidad6_secaum$X025R)

trinidad6_secaum$renda_subj_numeric <-as.numeric(trinidad6_secaum$X047R_WVS)
table(trinidad6_secaum$renda_subj_numeric, useNA = "always" )
trinidad6_secaum$renda_subj_numeric[trinidad6_secaum$renda_subj_numeric==-1] <- NA
trinidad6_secaum$renda_subj_numeric[trinidad6_secaum$renda_subj_numeric==-2] <- NA


trinidad6_secaum <- subset(trinidad6_secaum, select=c(X001,escolaridade_niveis,
                                                      X028,F025,renda_subj_numeric))
trinidad6_secaum$F025 -> trinidad6_secaum$religion
trinidad6_secaum$religion <- as.factor(trinidad6_secaum$religion)
# Defina as regras de recodificação
recode_rules <- c("1" = "Católica",
                  "0" = "Nenhuma",
                  "2" = "Protestante")
table(trinidad6_secaum$religion)
# Recodifique todos os outros valores como "Outras religiões"
trinidad6_secaum$religion <- recode(trinidad6_secaum$religion, !!!recode_rules, .default = "Outras religiões")

# Verifique o resultado
table(trinidad6_secaum$religion)

# Verifique o resultado
table(trinidad6_secaum$religion, useNA = "always")
trinidad6_secaum$status_emplo <- trinidad6_secaum$X028
table(trinidad6_secaum$status_emplo)
# Defina as regras de recodificação
recode_rules <- c("1" = "Empregado",
                  "3" = "Por Conta Própria",
                  "5" = "Doméstico",
                  "4" = "Aposentado",
                  "6" = "Estudante")
# Recodifique todos
trinidad6_secaum$status_emplo <- recode(trinidad6_secaum$status_emplo, !!!recode_rules)
table(trinidad6_secaum$status_emplo,useNA = "always")
trinidad6_secaum -> trinidad
trinidad[,8:11]<-trinidad6[,2:5]
fundamentalism <- lm(fundamentalism ~ escolaridade_niveis+
                       status_emplo+religion+X001+renda_subj_numeric
                     , data=trinidad)


desconfia <- lm(desconfia~ escolaridade_niveis+
                  status_emplo+religion+X001+renda_subj_numeric
                , data=trinidad)
antidemocr <- lm(antidemocr~ escolaridade_niveis+
                   status_emplo+religion+X001+renda_subj_numeric
                 , data=trinidad)
promercado <- lm(promercado ~ escolaridade_niveis+
                   status_emplo+religion+X001+renda_subj_numeric
                 , data=trinidad)


Job_funda<-plot_cap(fundamentalism, condition="status_emplo",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Fundamentalismo",
       y= "Fundamentalismo",
       x= "Status Laboral")+coord_flip()
Job_funda
Job_merc<-plot_cap(promercado, condition="status_emplo",
                   conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Materialismo à direita (pró Mercado)",
       y= "Materialismo à direita",
       x= "Status Laboral")+coord_flip()
Job_merc

Job_autor<-plot_cap(antidemocr , condition="status_emplo",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Autoritarismo (Antidemocrático)",
       y= "Autoritarismo (Antidemocrático)",
       x= "Status Laboral")+coord_flip()
Job_autor

Job_descf<-plot_cap(desconfia , condition="status_emplo",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Desconfiança Institucional",
       y= "Desconfiança Institucional",
       x= "Status Laboral")+coord_flip()
Job_descf

plots(Job_funda,Job_merc,Job_autor,Job_descf, n_columns = 2)
#religi
RELIGARE_funda<-plot_cap(fundamentalism, condition="religion",
                         conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Fundamentalismo",
       y= "Fundamentalismo",
       x= "Religião")+coord_flip()
RELIGARE_funda
RELIGARE_merc<-plot_cap(promercado, condition="religion",
                        conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Materialismo à direita (pró Mercado)",
       y= "Materialismo à direita",
       x= "Religião")+coord_flip()
RELIGARE_merc

RELIGARE_autor<-plot_cap(antidemocr , condition="religion",
                         conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Autoritarismo (Antidemocrático)",
       y= "Autoritarismo (Antidemocrático)",
       x= "Religião")+coord_flip()
RELIGARE_autor

RELIGARE_descfs<-plot_cap(desconfia , condition="religion",
                          conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Desconfiança Institucional",
       y= "Desconfiança Institucional",
       x= "Religião")+coord_flip()
RELIGARE_descfs


plots(RELIGARE_funda,RELIGARE_merc,RELIGARE_autor,
      RELIGARE_descfs,n_columns = 2)



# Lista de todos os objetos no ambiente de trabalho
objetos_no_ambiente <- ls()

# Remova todos os objetos, exceto 'df'
objetos_para_remover <- setdiff(objetos_no_ambiente, "WVS_TimeSeries_4_0")

# Remova os objetos da lista
rm(list = objetos_para_remover)


selecao <- WVS_TimeSeries_4_0$S003 == 630
portoricogeral <- WVS_TimeSeries_4_0[selecao, ]
table(portoricogeral$S002VS)
selecao <- portoricogeral$S002VS == 7
portorico7d <- portoricogeral[selecao, ]
portorico7d$X003R2[portorico7d$X003R2==-2] <- NA

table(portorico7d$X025R, useNA = "always")
portorico7d$escolaridade_niveis <- ifelse(portorico7d$X025R == -1, NA, portorico7d$X025R)

portorico7d$renda_subj_numeric <-as.numeric(portorico7d$X047R_WVS)
table(portorico7d$renda_subj_numeric, useNA = "always" )
portorico7d$renda_subj_numeric[portorico7d$renda_subj_numeric==-1] <- NA
portorico7d$renda_subj_numeric[portorico7d$renda_subj_numeric==-2] <- NA


portorico7d <- subset(portorico7d, select=c(X001,escolaridade_niveis,
                                            X028,F025,renda_subj_numeric))
portorico7d$F025 -> portorico7d$religion
portorico7d$religion <- as.factor(portorico7d$religion)
# Defina as regras de recodificação
recode_rules <- c("1" = "Católica",
                  "0" = "Nenhuma",
                  "2"="Protestante")
table(portorico7d$religion)
# Recodifique todos os outros valores como "Outras religiões"
portorico7d$religion <- recode(portorico7d$religion, !!!recode_rules, .default = "Outras religiões")

# Verifique o resultado
table(portorico7d$religion)

# Verifique o resultado
table(portorico7d$religion, useNA = "always")
portorico7d$status_emplo <- portorico7d$X028
table(portorico7d$status_emplo)
# Defina as regras de recodificação
recode_rules <- c("1" = "Empregado",
                  "3" = "Por Conta Própria",
                  "5" = "Doméstico",
                  "4" = "Aposentado",
                  "6" = "Estudante")
# Recodifique todos
portorico7d$status_emplo <- recode(portorico7d$status_emplo, !!!recode_rules)
table(portorico7d$status_emplo,useNA = "always")
portorico7d -> portorico7d
portorico7 <- read_csv("fatores_no tempo/portorico7.csv")
portorico7d[,8:11]<-portorico7[,2:5]
fundamentalism <- lm(fundamentalism ~ escolaridade_niveis+
                       status_emplo+religion+X001+renda_subj_numeric
                     , data=portorico7d)
desconfia <- lm(desconfia~ escolaridade_niveis+
                  status_emplo+religion+X001+renda_subj_numeric
                , data=portorico7d)
antidemocr <- lm(antidemocr~ escolaridade_niveis+
                   status_emplo+religion+X001+renda_subj_numeric
                 , data=portorico7d)
promercado <- lm(promercado ~ escolaridade_niveis+
                   status_emplo+religion+X001+renda_subj_numeric
                 , data=portorico7d)


Job_funda<-plot_cap(fundamentalism, condition="status_emplo",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Fundamentalismo",
       y= "Fundamentalismo",
       x= "Status Laboral")+coord_flip()
Job_funda
Job_merc<-plot_cap(promercado, condition="status_emplo",
                   conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Materialismo à direita (pró Mercado)",
       y= "Materialismo à direita",
       x= "Status Laboral")+coord_flip()
Job_merc

Job_autor<-plot_cap(antidemocr , condition="status_emplo",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Autoritarismo (Antidemocrático)",
       y= "Autoritarismo (Antidemocrático)",
       x= "Status Laboral")+coord_flip()
Job_autor

Job_descf<-plot_cap(desconfia , condition="status_emplo",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Desconfiança Institucional",
       y= "Desconfiança Institucional",
       x= "Status Laboral")+coord_flip()
Job_descf

plots(Job_funda,Job_merc,Job_autor,Job_descf, n_columns = 2)
#religi
RELIGARE_funda<-plot_cap(fundamentalism, condition="religion",
                         conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Fundamentalismo",
       y= "Fundamentalismo",
       x= "Religião")+coord_flip()
RELIGARE_funda
RELIGARE_merc<-plot_cap(promercado, condition="religion",
                        conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Materialismo à direita (pró Mercado)",
       y= "Materialismo à direita",
       x= "Religião")+coord_flip()
RELIGARE_merc

RELIGARE_autor<-plot_cap(antidemocr , condition="religion",
                         conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Autoritarismo (Antidemocrático)",
       y= "Autoritarismo (Antidemocrático)",
       x= "Religião")+coord_flip()
RELIGARE_autor

RELIGARE_descfs<-plot_cap(desconfia , condition="religion",
                          conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Desconfiança Institucional",
       y= "Desconfiança Institucional",
       x= "Religião")+coord_flip()
RELIGARE_descfs


plots(RELIGARE_funda,RELIGARE_merc,RELIGARE_autor,
      RELIGARE_descfs,n_columns = 2)


# Lista de todos os objetos no ambiente de trabalho
objetos_no_ambiente <- ls()

# Remova todos os objetos, exceto 'df'
objetos_para_remover <- setdiff(objetos_no_ambiente, "WVS_TimeSeries_4_0")

# Remova os objetos da lista
rm(list = objetos_para_remover)

selecao <- WVS_TimeSeries_4_0$S003 == 332
haiti <- WVS_TimeSeries_4_0[selecao, ]
table(haiti$S002VS)
haiti <- subset(haiti, select=c(E069_07,E069_12,E069_17,E069_06,F028,
                                E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
summary(haiti)
haiti <- subset(haiti, select = -15)#remover colunna 15 (para ficar igual )


# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 1:14) {
  haiti[, i][haiti[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(haiti)
summary(haiti)
library(mice)
imp <- mice(haiti, seed=23109)# o nome da base e a seed sempre essa 23109
haiti <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(haiti)

haiti$F118_invertido <- -1*haiti$F118
haiti$F118_invertido <- scales::rescale(haiti$F118_invertido, to = c(0, 1))
table(haiti$F118)
table(haiti$F118_invertido)

haiti$F120_invertido <- -1*haiti$F120
haiti$F120_invertido <- scales::rescale(haiti$F120_invertido, to = c(0, 1))
haiti$F028_invertido <- -1*haiti$F028
haiti$F028_invertido <- scales::rescale(haiti$F028_invertido, to = c(0, 1))
haiti$E039_invertido <- -1*haiti$E039
haiti$E039_invertido <- scales::rescale(haiti$E039_invertido, to = c(0, 1))
haiti$E040_invertido <- -1*haiti$E040
haiti$E040_invertido <- scales::rescale(haiti$E040_invertido, to = c(0, 1))
haiti$E036_invertido <- -1*haiti$E036
haiti$E036_invertido <- scales::rescale(haiti$E036_invertido, to = c(0, 1))
haiti$E116_invertido <- -1*haiti$E116
haiti$E116_invertido <- scales::rescale(haiti$E116_invertido, to = c(0, 1))
haiti$E114_invertido <- -1*haiti$E114
haiti$E114_invertido <- scales::rescale(haiti$E114_invertido, to = c(0, 1))
#haiti$E235_invertido <- -1*haiti$E235
#haiti$E235_invertido <- scales::rescale(haiti$E235_invertido, to = c(0, 1))


#table(haiti$E235)
#table(haiti$E235_invertido)

haiti <- subset(haiti, select=c(
  E069_07,E069_12,E069_17,E069_06,F118_invertido,
  F120_invertido,F028_invertido,
  E037,E040_invertido,E036_invertido,
  E116_invertido,E114_invertido,E117,E039_invertido))

summary(haiti[,1:12])#verificar



haiti$E069_07 <- scales::rescale(haiti$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
haiti$E069_12 <- scales::rescale(haiti$E069_12, to = c(0, 1))
haiti$E069_17 <- scales::rescale(haiti$E069_17, to = c(0, 1))
haiti$E069_06 <- scales::rescale(haiti$E069_06, to = c(0, 1))
haiti$E037 <- scales::rescale(haiti$E037, to = c(0, 1))
haiti$E117 <- scales::rescale(haiti$E117, to = c(0, 1))

summary(haiti)#verificar

#obs 

#
library(psych)
library(lavaan)
psych::scree(haiti)
nfactors((haiti))
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
#haitiao$E037#pro estado # removi



# Ajuste do modelo de CFA
fit <- cfa(model, data = haiti)

summary(fit)
#> ver resultado na imagem ethiopia 2.bmp


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic",
                             "pvalue","chisq"))


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
scores <- lavPredict(fit)
scores[,1] -> haiti$fundamentalism
scores[,2] -> haiti$desconfia
#haitiao$E117#ok
scores[,3] -> haiti$antidemocr
scores[,4] -> haiti$promercado 
rm(scores,labels,model,values)

haiti <- subset(haiti, select=c(fundamentalism))
#haiti testes AFC inadequados - apenas fundamentalismo foi passível de análise
haiti_df <- WVS_TimeSeries_4_0[selecao, ]
table(haiti_df$X025R, useNA = "always")
haiti_df$escolaridade_niveis <- ifelse(haiti_df$X025R == -1, NA, haiti_df$X025R)

haiti_df$renda_subj_numeric <-as.numeric(haiti_df$X047R_WVS)
table(haiti_df$renda_subj_numeric, useNA = "always" )
haiti_df$renda_subj_numeric[haiti_df$renda_subj_numeric==-5] <- NA
haiti_df$renda_subj_numeric[haiti_df$renda_subj_numeric==-2] <- NA
haiti_df$renda_subj_numeric[haiti_df$renda_subj_numeric==-1] <- NA

haiti_df <- subset(haiti_df, select=c(X001,escolaridade_niveis,
                                      X028,F025,renda_subj_numeric))
haiti_df$F025 -> haiti_df$religion
haiti_df$religion <- as.factor(haiti_df$religion)
# Defina as regras de recodificação
recode_rules <- c("1" = "Católica",
                  "0" = "Nenhuma",
                  "2"="Protestante")
table(haiti_df$religion)
# Recodifique todos os outros valores como "Outras religiões"
haiti_df$religion <- recode(haiti_df$religion, !!!recode_rules, .default = "Outras religiões")

# Verifique o resultado
table(haiti_df$religion)

# Verifique o resultado
table(haiti_df$religion, useNA = "always")
haiti_df$status_emplo <- haiti_df$X028
table(haiti_df$status_emplo)
# Defina as regras de recodificação
recode_rules <- c("1" = "Empregado",
                  "3" = "Por Conta Própria",
                  "5" = "Doméstico",
                  "4" = "Aposentado",
                  "6" = "Estudante")
# Recodifique todos
haiti_df$status_emplo <- recode(haiti_df$status_emplo, !!!recode_rules)
table(haiti_df$status_emplo,useNA = "always")

haiti_df[,8]<-haiti[,1]
haiti_df$fundamentalism <- haiti_df$...8
fundamentalism <- lm(fundamentalism ~ escolaridade_niveis+
                       status_emplo+religion+X001+renda_subj_numeric
                     , data=haiti_df)

summary(fundamentalism)

Job_funda<-plot_cap(fundamentalism, condition="status_emplo",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Fundamentalismo",
       y= "Fundamentalismo",
       x= "Status Laboral")+coord_flip()

Job_funda
RELIGARE_funda<-plot_cap(fundamentalism, condition="religion",
                         conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Fundamentalismo",
       y= "Fundamentalismo",
       x= "Religião")+coord_flip()
RELIGARE_funda

