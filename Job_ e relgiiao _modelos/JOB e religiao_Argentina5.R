#primeiro rodar o referente ao país e onda no scripts geral
library(scales)
library(marginaleffects)
library(see)
df %>% glimpse()
dfao <- read_sav("dfao.sav")
dfao <- dfao %>%
  filter(S002VS == 5 & S003==32)#

dfao$X028 #situação laboral
table(dfao$X028)#manter o original

# Defina as regras de recodificação
recode_rules <- c("Full Time" = "Empregado",
                  "Self Employed" = "Por Conta Própria",
                  "Housewife" = "Doméstico",
                  "Retired" = "Aposentado",
                  "Student" = "Estudante",
                  "Others" = "Outras condições laborais")

# Recodifique todos
df$status_emplo <- recode(df$status_emplo, !!!recode_rules)
table(df$status_emplo,useNA = "always")

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


df$Genero_Mulher <- df$Gender == 2



fundamentalism <- lm(fundamentalism ~ Genero_Mulher+
                       idade_faixa+escolaridade_niveis+
                       status_emplo+religion
                     , data=df)


desconfia <- lm(desconfia~ Genero_Mulher+
                  idade_faixa+escolaridade_niveis+
                  status_emplo+religion
                , data=df)
antidemocr <- lm(antidemocr~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+religion
                 , data=df)
promercado <- lm(promercado ~ Genero_Mulher+
                   idade_faixa+escolaridade_niveis+
                   status_emplo+religion
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

