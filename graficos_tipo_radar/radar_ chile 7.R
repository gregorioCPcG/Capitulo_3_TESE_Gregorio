#rodar chile7(em scripts primeiro)
library(poorman)
library(datawizard)
library(easystats)

df %>% glimpse()
table(df$votos_partidos)

dfinho <- subset(df, select=c(desconfia,fundamentalism,votos_partidos,antidemocr))
dfinho %>% glimpse()


#Converter colunas numéricas para numérico (se já não forem)
dfinho <- dfinho %>%
  mutate(votos_partidos = as.character(votos_partidos))


library(scales)

# Selecione as colunas a serem padronizadas
cols_to_scale <- c("desconfia", "fundamentalism","antidemocr")

# Aplique a função de escalonamento para cada coluna
for (col in cols_to_scale) {
  dfinho[[col]] <- rescale(dfinho[[col]], to = c(0, 1))
}



dfinho$votos_partidos -> dfinho$Partido
dfinho$fundamentalism -> dfinho$Fundamentalismo
#dfinho$promercado -> dfinho$`PróMercado`
dfinho$antidemocr -> dfinho$`Autoritário`
dfinho$desconfia -> dfinho$`Desconf.Inst`
summary(dfinho)
dfinho <- dfinho[,5:8]
dfinho %>% glimpse()




table(dfinho$Partido)
dfinho$Partido <- factor(dfinho$Partido, levels = c("Nenhum/NSabe", "Socialista",
                                           "PPD","DC","RN","UDI","Comunista",
                                           "Outros Partidos"))
table(dfinho$Partido)
dfinho$Partido <- as.character(dfinho$Partido)
summary(dfinho)
data <- dfinho %>%
  group_by(Partido) %>%
  summarise(across(everything(), mean)) %>%
  reshape_longer(c("Fundamentalismo", "Desconf.Inst", "Autoritário"))



library(wesanderson)
wes_palette <- wes_palette("IsleofDogs1")

table(data$Partido)

data$Partido <- factor(data$Partido, levels = c("Socialista",
                                                    "PPD","DC","RN","UDI","Comunista",
                                                    "Outros Partidos","Nenhum/NSabe"))
table(data$Partido)

table(data$name)

data$name <- factor(data$name, levels = c("Autoritário",
                                          "Fundamentalismo",
                                          "Desconf.Inst"))
table(data$name)


chile7 <- data %>%
  filter(Partido == "Socialista" |
           Partido == "DC" |Partido == "PPD"|Partido == "RN"|
           Partido == "UDI"|Partido == "Nenhum/NSabe") %>% 
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

chile7
# se quiser salvar
saveRDS(chile7, file = "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/chile7.rds")


