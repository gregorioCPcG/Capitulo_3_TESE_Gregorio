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

summary(dfinho)

library(scales)

dfinho$fundamentalism <- rescale(dfinho$fundamentalism , to = c(0, 1))
dfinho$promercado <- rescale(dfinho$promercado , to = c(0, 1))
dfinho$antidemocr <- rescale(dfinho$antidemocr , to = c(0, 1))
summary(dfinho)                              

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
  filter(Partido == "PT" |  Partido == "PSL") %>%
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
  scale_fill_manual(values = c( "blue", "darkred")) +
  scale_color_manual(values = c("darkblue", "darkred"))+ theme(legend.position = "none")
brasil23

saveRDS(brasil23, file = "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/radar grafic/bra23.rds")

