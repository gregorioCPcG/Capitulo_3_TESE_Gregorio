#primeiro rodar o referente ao país e onda no scripts geral
library(scales)
library(marginaleffects)
library(see)
df %>% glimpse()
is(dfao)
dfao$F025
table(dfao$F025)
dfao$F025 -> df$religion
df$religion <- as.factor(df$religion)
# Defina as regras de recodificação
recode_rules <- c("1" = "Católica",
                  "0" = "Nenhuma")

# Recodifique todos os outros valores como "Outras religiões"
df$religion <- recode(df$religion, !!!recode_rules, .default = "Outras religiões")

# Verifique o resultado
table(df$religion)

# Verifique o resultado
table(df$religion, useNA = "always")
table(df$status_emplo)
dfao$X028 #situação laboral
table(dfao$X028)#manter o original
df$status_emplo <- as.factor(dfao$X028)


# Defina as regras de recodificação
recode_rules <- c("1" = "Empregado",
                  "3" = "Por Conta Própria",
                  "5" = "Doméstico",
                  "4" = "Aposentado",
                  "6" = "Estudante")

# Recodifique todos
df$status_emplo <- recode(df$status_emplo, !!!recode_rules,.default = "Outras condições laborais")
table(df$status_emplo,useNA = "always")
df$RendaBaixa_Subjetivo <- df$rend_subj == "Low"
df$Genero_Mulher <- df$Gender == 2
fundamentalism <- lm(fundamentalism ~ Genero_Mulher+
                       idade_faixa+escolaridade_niveis+
                       status_emplo+RendaBaixa_Subjetivo+religion
                     , data=df)


desconfia <- lm(desconfia~ Genero_Mulher+
                  idade_faixa+escolaridade_niveis+
                  status_emplo+RendaBaixa_Subjetivo+religion
                , data=df)
promercado <- lm(promercado ~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+RendaBaixa_Subjetivo+religion
                 , data=df)
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


Job_descf<-plot_cap(desconfia , condition="status_emplo",
                    conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Desconfiança Institucional",
       y= "Desconfiança Institucional",
       x= "Status Laboral")+coord_flip()
Job_descf

plots(Job_funda,Job_merc,Job_descf, n_columns = 2)
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


RELIGARE_descfs<-plot_cap(desconfia , condition="religion",
                          conf_level = .9)+theme_bw()+
  scale_x_discrete(labels = label_wrap(10))+
  labs(title="",
       subtitle = "Desconfiança Institucional",
       y= "Desconfiança Institucional",
       x= "Religião")+coord_flip()
RELIGARE_descfs


plots(RELIGARE_funda,RELIGARE_merc,
      RELIGARE_descfs,n_columns = 2)
