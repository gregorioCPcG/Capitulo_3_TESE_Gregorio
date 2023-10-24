#rodar argentina3.R

#molde e argentina 3
#molde
library(poorman)
library(datawizard)
library(easystats)
# prepare the data in tidy format
data <- iris %>%
  group_by(Species) %>%
  summarise(across(everything(), mean)) %>%
  reshape_longer(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))

data %>%
  ggplot(aes(
    x = name,
    y = value,
    color = Species,
    group = Species,
    fill = Species
  )) +
  geom_polygon(linewidth = 1, alpha = 0.1) +
  coord_radar() +
  theme_radar()


# argentina - testar primeiro
df %>% glimpse()
table(df$votos_partidos)

dfinho <- subset(df, select=c(desconfia,fundamentalism,promercado,votos_partidos,antidemocr))
dfinho %>% glimpse()

library(dplyr)

# Converter colunas numéricas para numérico (se já não forem)
dfinho <- dfinho %>%
  mutate(votos_partidos = as.character(votos_partidos))


library(scales)

# Selecione as colunas a serem padronizadas
cols_to_scale <- c("desconfia", "fundamentalism", "promercado", "antidemocr")

# Aplique a função de escalonamento para cada coluna
for (col in cols_to_scale) {
  dfinho[[col]] <- rescale(dfinho[[col]], to = c(0, 1))
}


summary(dfinho)
data <- dfinho %>%
  group_by(votos_partidos) %>%
  summarise(across(everything(), mean)) %>%
  reshape_longer(c("fundamentalism", "desconfia", "antidemocr", "promercado"))
library(ggplot2)
data %>%
  ggplot(aes(
    x = name,
    y = value,
    color = votos_partidos,
    group = votos_partidos,
    fill = votos_partidos
  )) +
  geom_polygon(linewidth = 1, alpha = 0.1) +
  coord_radar() +
  theme_radar()

#REDUZINDO PARTIDOS
data %>%
  filter(votos_partidos != "Nenhum/NSabe" & votos_partidos != "Outros Partidos") %>% 
  ggplot(aes(
    x = name,
    y = value,
    color = votos_partidos,
    group = votos_partidos,
    fill = votos_partidos
  )) +
  geom_polygon(linewidth = 1, alpha = 0.1) +
  coord_radar() +
  theme_radar()


cores_personalizadas <- c(
  "FREPASO" = "red",
  "Justicialista" = "blue",
  "UCR" = "green"
)
filtered_data <- data %>%
  filter(votos_partidos != "Nenhum/NSabe" & votos_partidos != "Outros Partidos")
# Criar o gráfico de radar com cores personalizadas
filtered_data %>%
  ggplot(aes(
    x = name,
    y = value,
    color = votos_partidos,
    group = votos_partidos,
    fill = votos_partidos
  )) +
  geom_polygon(linewidth = 1, alpha = 0.1) +
  scale_color_manual(values = cores_personalizadas) +
  scale_fill_manual(values = cores_personalizadas) +
  coord_radar() +
  theme_radar()
