#rodar urug3(em scripts primeiro)
library(poorman)
library(datawizard)
library(easystats)
# urug3entina - testar primeiro
df %>% glimpse()
table(df$votos_partidos)

dfinho <- subset(df, select=c(desconfia,fundamentalism,promercado,votos_partidos,antidemocr))
dfinho %>% glimpse()


#Converter colunas numéricas para numérico (se já não forem)
dfinho <- dfinho %>%
  mutate(votos_partidos = as.character(votos_partidos))


library(scales)

# Selecione as colunas a serem padronizadas
cols_to_scale <- c("desconfia", "fundamentalism", "promercado", "antidemocr")

# Aplique a função de escalonamento para cada coluna
for (col in cols_to_scale) {
  dfinho[[col]] <- rescale(dfinho[[col]], to = c(0, 1))
}



dfinho$votos_partidos -> dfinho$Partido
dfinho$fundamentalism -> dfinho$Fundamentalismo
dfinho$promercado -> dfinho$`PróMercado`
dfinho$antidemocr -> dfinho$`Autoritário`
dfinho$desconfia -> dfinho$`Desconf.Inst`
summary(dfinho)
dfinho <- dfinho[,6:10]
dfinho %>% glimpse()

table(dfinho$Partido)
summary(dfinho)
data <- dfinho %>%
  group_by(Partido) %>%
  summarise(across(everything(), mean)) %>%
  reshape_longer(c("Fundamentalismo", "Desconf.Inst", "Autoritário", "PróMercado"))
library(ggplot2)

table(dfinho$Partido)

urug35<- data %>%
  filter(Partido != "Outros Partidos" & Partido != "Nuevo Espacio") %>% 
  ggplot(aes(
    x = name,
    y = value,
    color = Partido,
    group = Partido,
    fill = Partido
  )) +
  geom_polygon(linewidth = 1, alpha = 0.1) +
  coord_radar() +
  theme_radar() + labs(y="",x="") + facet_wrap(~Partido, ncol=2) + theme(
    axis.text = element_text(size = 7)  # Ajuste o tamanho da fonte para o valor desejado
  ) +  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )  +
  scale_fill_manual(values = c("#f77f00","#fb6f92", "darkblue","grey" )) +
  scale_color_manual(values = c("#f77f00","#fb6f92", "darkblue","grey"))+
  theme(legend.position = "none")


urug35
