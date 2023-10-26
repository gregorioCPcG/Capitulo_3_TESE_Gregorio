#rodar COL_7(em scripts primeiro)
library(poorman)
library(datawizard)
library(easystats)


df %>% glimpse()
table(df$votos_partidos)

dfinho <- subset(df, select=c(desconfia,fundamentalism,promercado,votos_partidos))
dfinho %>% glimpse()


#Converter colunas numéricas para numérico (se já não forem)
dfinho <- dfinho %>%
  mutate(votos_partidos = as.character(votos_partidos))


library(scales)

# Selecione as colunas a serem padronizadas
cols_to_scale <- c("desconfia", "fundamentalism", "promercado")

# Aplique a função de escalonamento para cada coluna
for (col in cols_to_scale) {
  dfinho[[col]] <- rescale(dfinho[[col]], to = c(0, 1))
}



dfinho$votos_partidos -> dfinho$Partido
dfinho$fundamentalism -> dfinho$Fundamentalismo
dfinho$promercado -> dfinho$`PróMercado`
#dfinho$antidemocr -> dfinho$`Autoritário`
dfinho$desconfia -> dfinho$`Desconf.Inst`
summary(dfinho)
dfinho <- dfinho[,5:8]
dfinho %>% glimpse()


table(dfinho$Partido)
summary(dfinho)
data <- dfinho %>%
  group_by(Partido) %>%
  summarise(across(everything(), mean)) %>%
  reshape_longer(c("Fundamentalismo", "Desconf.Inst", "Autoritário", "PróMercado"))
library(ggplot2)
library(wesanderson)  # Certifique-se de que a biblioteca 'wesanderson' esteja instalada

# Paleta de cores do Wes Anderson - ''
wes_palette <- wes_palette("IsleofDogs1")

COL_7 <- data %>%
  filter(Partido != "Nenhum/NSabe" &
           Partido != "Outros" &
           Partido != "PDA") %>% 
  ggplot(aes(
    x = name,
    y = value,
    color = Partido,
    group = Partido,
    fill = Partido
  )) +
  geom_polygon(linewidth = 1, alpha = 0.67) +
  coord_radar() +
  theme_radar() +
  labs(y = "", x = "") +
  facet_wrap(~Partido, ncol = 3) +
  theme(
    axis.text = element_text(size = 7),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"  # Remove a legenda
  ) +
  scale_color_manual(values = wes_palette)  +
  scale_fill_manual(values = wes_palette)

COL_7

