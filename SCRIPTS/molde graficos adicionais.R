# as variáveis mais interessantes do modelo full
plot_cap(logitJusticialista_4, condition=c("promercado"))
plot_cap(logitJusticialista_4, condition=c("autoloc_dir_esq"),conf_level = .9)
plot_cap(logitJusticialista_4, condition=c("autoloc_dir_esq","promercado"),conf_level = .9)#interação
plot_cap(logitJusticialista_4, condition=c("religion"),conf_level = .9)
plot_cap(logitJusticialista_4, condition=c("marital_Status"),conf_level = .9)
plot_cap(logitJusticialista_4, condition=c("region"),conf_level = .9)
plot_cap(logitJusticialista_4, condition=c("idade_faixa"),conf_level = .9)
plot_cap(logitJusticialista_4, condition=c("escolaridade_niveis"),conf_level = .9)

#plot_models(logitJusticialista_6, vline.color = "black")

#religion
# Tabela de contingência entre as variáveis religion e df_voto_multinom
tabela_contingencia <- table(df$religion, df$df_voto_multinom)

# Teste Qui-quadrado de independência
resultado_chisq <- chisq.test(tabela_contingencia)

tabela_contingencia
# Visualizar o resultado do teste Qui-quadrado
print(resultado_chisq)#é sig?

# Gráfico de barras
ggplot(data = df, aes(x = df_voto_multinom, fill = religion)) +
  geom_bar(position = "fill") +
  labs(x = "df_voto_multinom", y = "Proporção") +
  ggtitle("Proporção de categorias de religion por categoria de df_voto_multinom")


# region
# Tabela de contingência entre as variáveis region e df_voto_multinom
tabela_contingencia <- table(df$region, df$df_voto_multinom)

# Teste Qui-quadrado de independência
resultado_chisq <- chisq.test(tabela_contingencia)


# Visualizar o resultado do teste Qui-quadrado
print(resultado_chisq)#é sig?

# Gráfico de barras
ggplot(data = df, aes(x = region, fill = df_voto_multinom)) +
  geom_bar(position = "fill") +
  labs(x = "region", y = "Proporção") +
  ggtitle("Proporção de categorias de voto por região")#inverti



#


# Gender_

df$Gender_ <- as.factor(df$Gender)
levels(df$Gender_)
levels(df$Gender_) <- c("Masc", "Fem")
levels(df$Gender_)

# Tabela de contingência entre as variáveis Gender_ e df_voto_multinom
tabela_contingencia <- table(df$Gender_, df$df_voto_multinom)

# Teste Qui-quadrado de independência
resultado_chisq <- chisq.test(tabela_contingencia)


# Visualizar o resultado do teste Qui-quadrado
print(resultado_chisq)#é sig?

# Gráfico de barras
ggplot(data = df, aes(x = df_voto_multinom, fill = Gender_)) +
  geom_bar(position = "fill") +
  labs(x = "Voto", y = "Proporção") +
  ggtitle("Voto e Gênero")

# autoloc_dir_esq
table(df$autoloc_dir_esq)
# Tabela de contingência entre as variáveis autoloc_dir_esq e df_voto_multinom
tabela_contingencia <- table(df$autoloc_dir_esq, df$df_voto_multinom)

# Teste Qui-quadrado de independência
resultado_chisq <- chisq.test(tabela_contingencia)


# Visualizar o resultado do teste Qui-quadrado
print(resultado_chisq)#é sig?

# Gráfico de barras
ggplot(data = df, aes(x = df_voto_multinom, fill = autoloc_dir_esq)) +
  geom_bar(position = "fill") +
  labs(x = "Voto", y = "Proporção") +
  ggtitle("Voto e Auto Localização no eixo direita - esquerda")





#





###


# Comparar as médias de promercado entre as categorias de df_voto_multinom usando tapply
media_por_categoria <- tapply(df$promercado, df$df_voto_multinom, mean)

# Visualizar as médias por categoria
print(media_por_categoria)


hist(df$promercado, breaks=50)

# Teste de ANOVA
modelo_anova <- aov(promercado ~ df_voto_multinom, data = df)
resultado_anova <- summary(modelo_anova)

# Visualizar o resultado do teste de ANOVA
print(resultado_anova)


# Teste de Tukey para comparações múltiplas
resultado_tukey <- TukeyHSD(modelo_anova)

# Visualizar o resultado do teste de Tukey
print(resultado_tukey)


means <- aggregate(promercado ~ df_voto_multinom, data = df, FUN = mean)
means -> means2
means2
ggplot(data = df, aes(x = df_voto_multinom, y = promercado)) +
  stat_summary(fun = mean, geom = "bar", fill = "steelblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  geom_text(data = means, aes(label = sprintf("%.4f", promercado)), 
            vjust = -0.5, color = "black", size = 3) +
  labs(x = "Atitude", y = "Pró mercado") +
  ggtitle("Média de visão pró Mercado por tipo de voto") +
  coord_flip()
#Others e Prosperity
# é sig


prop.table(table(df$df_voto_multinom))*100


#teste de hipótese de outras (fundamentalism foi sig a 0.08)

# Teste de ANOVA
modelo_anova <- aov(fundamentalism ~ df_voto_multinom, data = df)
resultado_anova <- summary(modelo_anova)

# Visualizar o resultado do teste de ANOVA
print(resultado_anova)


# Teste de Tukey para comparações múltiplas
resultado_tukey <- TukeyHSD(modelo_anova)

# Visualizar o resultado do teste de Tukey
print(resultado_tukey)


means <- aggregate(fundamentalism ~ df_voto_multinom, data = df, FUN = mean)
means
ggplot(data = df, aes(x = df_voto_multinom, y = fundamentalism)) +
  stat_summary(fun = mean, geom = "bar", fill = "steelblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  geom_text(data = means, aes(label = sprintf("%.4f", fundamentalism)), 
            vjust = -0.5, color = "black", size = 3) +
  labs(x = "Atitude", y = "Pró mercado") +
  ggtitle("Média de visão pró Mercado por tipo de voto") +
  coord_flip()
#
# é sig em um caso


#ok, achei coisa em fundamentalism e pró mercado, que tal uma tabela cruzada À LÁ dalton



library(wesanderson)
??wesanderson

partido <- c("Justicialista","DK/NA","Others")

means2
promercado <- c(0.0062,-0.00231,-0.00917)
means
fundamentalism <- c(-0.016731503,0.028066089,0.002422497)
table(df$df_voto_multinom)
size <- c(588,323,319)


graf1 <- data.frame(partido,promercado,fundamentalism,size)
head(graf1)
a <- graf1 %>%
  ggplot(aes(x=fundamentalism, y=promercado, size = size, color = partido)) +
  geom_point(alpha=1) +
  scale_size(range = c(5, 24), name="Partido") +
  scale_x_continuous(name="-> Fundamentalista",limits=c(-0.02, 0.03)) +
  scale_y_continuous(name="-> Pró Mercado",limits=c(-0.01, 0.01)) +
  ggtitle("Ethiopia 2020 Issue Divide") + theme_classic()

a
paleta_cores <- wes_palette("GrandBudapest1")
paleta_cores
a<-a + scale_color_manual(values = paleta_cores)
a
# pra comparar fazer o mesmo gráfico de autoloc
# autoloc_dir_esq

means <- aggregate(fundamentalism ~ autoloc_dir_esq, data = df, FUN = mean)
means

means2 <- aggregate(promercado ~ autoloc_dir_esq, data = df, FUN = mean)
means2

partido <- c("Left","Center","Right","DK")

means2[,2]
promercado <- c(means2[,2])
promercado
means
fundamentalism <- c(means[,2])
table(df$autoloc_dir_esq)
size <- c(156,111,338,625)


graf1 <- data.frame(partido,promercado,fundamentalism,size)
head(graf1)

#Reordenar os níveis da variável partido
graf1$partido <- factor(graf1$partido, levels = c("DK", "Left", "Center", "Right"))

head(graf1)

b<- graf1 %>%
  ggplot(aes(x=fundamentalism, y=promercado, size = size, color = partido)) +
  geom_point(alpha=1) +
  scale_size(range = c(5, 24), name="Auto Localização Ideológica") +
  scale_x_continuous(name="-> Fundamentalista",limits=c(-0.1, 0.05)) +
  scale_y_continuous(name="-> Pró Mercado",limits=c(-0.04, 0.01)) +
  ggtitle("Ethiopia 2020 'Issue Divide (ideologia)'") + theme_classic()
b

paleta_cores <- wes_palette("Darjeeling2")
paleta_cores
b<-b + scale_color_manual(values = paleta_cores)
b

a
b
library(gridExtra)
grid.arrange(a,b)#interessante comparar
