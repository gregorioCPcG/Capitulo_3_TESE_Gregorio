setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo")

library(tidyverse)
library(easystats)
library(tidyquant)
arg3 <- read_csv("arg3.csv")
arg3$onda <- "Onda 3"
arg3 <- subset(arg3, select=c(onda,fundamentalism))
arg7 <- read_csv("arg7.csv")
arg7$onda <- "Onda 7"
arg7 <- subset(arg7, select=c(onda,fundamentalism))
bra2 <- read_csv("bra2.csv")
bra2$onda <- "Onda 2"
bra2 <- subset(bra2, select=c(onda,fundamentalism))
bra7 <- read_csv("bra7.csv")
bra7$onda <- "Onda 7"
bra7 <- subset(bra7, select=c(onda,fundamentalism))
chile2 <- read_csv("chile2.csv")
chile2$onda <- "Onda 2"
chile2 <- subset(chile2, select=c(onda,fundamentalism))
chile7 <- read_csv("chile7.csv")
chile7$onda <- "Onda 7"
chile7 <- subset(chile7, select=c(onda,fundamentalism))
col5 <- read_csv("col5.csv")
col5$onda <- "Onda 5"
col5 <- subset(col5, select=c(onda,fundamentalism))
colombia7 <- read_csv("colombia7.csv")
colombia7$onda <- "Onda 7"
col7 <- subset(colombia7, select=c(onda,fundamentalism))
rm(colombia7)
guat5 <- read_csv("guat5.csv")
guat5$onda <- "Onda 5"
guat5 <- subset(guat5, select=c(onda,fundamentalism))
guat7 <- read_csv("guat7.csv")
guat7$onda <- "Onda 7"
guat7 <- subset(guat7, select=c(onda,fundamentalism))
mex2 <- read_csv("mex2.csv")
mex2$onda <- "Onda 2"
mex2 <- subset(mex2, select=c(onda,fundamentalism))
mex7 <- read_csv("mex7.csv")
mex7$onda <- "Onda 7"
mex7 <- subset(mex7, select=c(onda,fundamentalism))
peru3 <- read_csv("peru3.csv")
peru3$onda <- "Onda 3"
peru3 <- subset(peru3, select=c(onda,fundamentalism))
peru7 <- read_csv("peru7.csv")
peru7$onda <- "Onda 7"
peru7 <- subset(peru7, select=c(onda,fundamentalism))
portorico3 <- read_csv("portorico3.csv")
portorico3$onda <- "Onda 3"
portorico3 <- subset(portorico3, select=c(onda,fundamentalism))
portorico7 <- read_csv("portorico7.csv")
portorico7$onda <- "Onda 7"
portorico7 <- subset(portorico7, select=c(onda,fundamentalism))
trinidad5 <- read_csv("trinidad5.csv")
trinidad5$onda <- "Onda 5"
trinidad5 <- subset(trinidad5, select=c(onda,fundamentalism))
trinidad6 <- read_csv("trinidad6.csv")
trinidad6$onda <- "Onda 6"
trinidad6 <- subset(trinidad6, select=c(onda,fundamentalism))
urug3 <- read_csv("urug3.csv")
urug3$onda <- "Onda 3"
urug3 <- subset(urug3, select=c(onda,fundamentalism))
urug7 <- read_csv("urug7.csv")
urug7$onda <- "Onda 7"
urug7 <- subset(urug7, select=c(onda,fundamentalism))
ven3 <- read_csv("ven3.csv")
ven3$onda <- "Onda 3"
ven3 <- subset(ven3, select=c(onda,fundamentalism))
venezuela7 <- read_csv("venezuela7.csv")
venezuela7$onda <- "Onda 7"
ven7 <- subset(venezuela7, select=c(onda,fundamentalism))
rm(venezuela7)

arg3$pais <- "Argentina"
arg7$pais <- "Argentina"
bra2$pais <- "Brasil"
bra7$pais <- "Brasil"
chile2$pais <- "Chile"
chile7$pais <- "Chile"
col5$pais <- "Colômbia"
col7$pais <- "Colômbia"
guat5$pais <- "Guatemala"
guat7$pais <- "Guatemala"
mex2$pais <- "México"
mex7$pais <- "México"
peru3$pais <- "Peru"
peru7$pais <- "Peru"
portorico3$pais <- "Porto Rico"
portorico7$pais <- "Porto Rico"
trinidad5$pais <- "Trinidad e T."
trinidad6$pais <- "Trinidad e T."
urug3$pais <- "Uruguai"
urug7$pais <- "Uruguai"
ven3$pais <- "Venezuela"
ven7$pais <- "Venezuela"


df <- full_join(arg3,arg7)
df <- full_join(df, bra2)
df <- full_join(df, bra7)
df <- full_join(df, chile2)
df <- full_join(df, chile7)
df <- full_join(df, col5)
df <- full_join(df, col7)
df <- full_join(df, guat5)
df <- full_join(df, guat7)
df <- full_join(df, mex2)
df <- full_join(df, mex7)
df <- full_join(df, peru3)
df <- full_join(df, peru7)
df <- full_join(df, portorico3)
df <- full_join(df, portorico7)
df <- full_join(df, trinidad5)
df <- full_join(df, trinidad6)
df <- full_join(df, urug3)
df <- full_join(df, urug7)
df <- full_join(df, ven3)
df <- full_join(df, ven7)
df %>% glimpse()
report(df)
summary(df)
minhas_cores <- c("#17becf", "#17becf", "#17becf",
                  "#17becf", "#17becf",
                  "#17becf", "#bcbd22","#17becf",
                  "#17becf", "#17becf","#bcbd22")

# Crie um gráfico de histograma
histogram_plot <- ggplot(df, aes(x = fundamentalism, fill=pais)) +
  geom_density(color="black") +
  # Personalize cores e largura dos bins
  facet_wrap(pais~ onda, nrow=16, ncol=6,
             strip.position = "bottom")+ 
  labs(x = "", y = "") +
  scale_fill_manual(values = minhas_cores)+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+theme(legend.position = "none")



histogram_plot
# Exiba o gráfico
histogram_plot+theme_abyss()+labs(x = "", y = "") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none")
histogram_plot+theme_tq_green()+labs(x = "", y = "") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())

histogram_plot+theme_tq_green()+labs(x = "", y = "") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none")
histogram_plot+theme_modern()+labs(x = "", y = "") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none")  

histogram_plot+theme_grey()+labs(x = "", y = "") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none") 
histogram_plot+theme_lucid()+labs(x = "", y = "") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none") 
histogram_plot+theme_radar()+labs(x = "", y = "") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none") 
histogram_plot+theme_get()+labs(x = "", y = "") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none") 
histogram_plot+theme_minimal()+labs(x = "", y = "") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none") 



histogram_plot <- ggplot(df, aes(x = fundamentalism)) +
  geom_density(color="black") +
  # Personalize cores e largura dos bins
  facet_wrap(~ onda,
             strip.position = "bottom")+ 
  labs(x = "", y = "") +
  scale_fill_manual(values = minhas_cores)+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+theme(legend.position = "none")
histogram_plot+theme_modern()
