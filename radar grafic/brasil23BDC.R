#rodar C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/tercil _ partido/ 1.R

library(poorman)
library(datawizard)
library(easystats)
df %>% glimpse()
table(df$P17_recod)

dfinho <- subset(df, select=c(P17_recod,fundamentalism,promercado,antidemocr))
dfinho %>% glimpse()
dfinho <- dfinho %>%
  mutate(P17_recod = as.character(P17_recod))


library(scales)


dfinho$P17_recod -> dfinho$Partido
dfinho$fundamentalism -> dfinho$Fundamentalismo
dfinho$promercado -> dfinho$`PróMercado`
dfinho$antidemocr -> dfinho$`Autoritário`
summary(dfinho)
dfinho <- dfinho[,5:8]
dfinho %>% glimpse()
data <- dfinho %>%
  group_by(Partido) %>%
  summarise(across(everything(), mean)) %>%
  reshape_longer(c("Fundamentalismo", "Autoritário", "PróMercado"))

data %>% glimpse()
library(ggplot2)
brasil23 <- data %>%
  ggplot(aes(
    x = name,
    y = value,
    color = Partido,
    group = Partido,
    fill = Partido
  )) +
  geom_polygon(linewidth = 1, alpha = 0.52) + labs(y="",x="")+
  coord_radar() +
  theme_radar()
brasil23


brasil23 + facet_wrap(~Partido) + theme(
  axis.text = element_text(size = 7)  # Ajuste o tamanho da fonte para o valor desejado
) +  theme(
  axis.text.y = element_blank(),
  axis.title.y = element_blank()
)



brasil23 <- brasil23 + facet_wrap(~Partido) + theme(
  axis.text = element_text(size = 7)  # Ajuste o tamanho da fonte para o valor desejado
) +  theme(
  axis.text.y = element_blank(),
  axis.title.y = element_blank())

#brasil23 <- 

brasil23


brasil23 <- brasil23 +
  scale_fill_manual(values = c("black", "grey67", "blue", "darkred")) +
  scale_color_manual(values = c("black", "grey67", "darkblue", "darkred"))
