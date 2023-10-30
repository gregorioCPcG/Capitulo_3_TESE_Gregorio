#gerar col7 e ven7

#rodar colombia7 da pasta scripts antes
df %>% glimpse()
colombia7 <- subset(df, select=c(fundamentalism,
                            desconfia,
                            promercado))

hist(colombia7)
summary(colombia7)
colombia7$fundamentalism<- scales::rescale(colombia7$fundamentalism ,
                                      to = c(0, 1))

colombia7$desconfia<- scales::rescale(colombia7$desconfia ,
                                 to = c(0, 1))
colombia7$promercado<- scales::rescale(colombia7$promercado ,
                                  to = c(0, 1))

colombia7

colombia7 <- write.csv(colombia7,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/colombia7.csv")


rm(list=ls())

#
#rodar venezuela7 da pasta scripts antes
df %>% glimpse()
venezuela7 <- subset(df, select=c(fundamentalism,
                            desconfia,
                            promercado,
                            antidemocr))

venezuela7
venezuela7$fundamentalism<- scales::rescale(venezuela7$fundamentalism ,
                                      to = c(0, 1))

venezuela7$desconfia<- scales::rescale(venezuela7$desconfia ,
                                 to = c(0, 1))
venezuela7$promercado<- scales::rescale(venezuela7$promercado ,
                                  to = c(0, 1))
venezuela7$antidemocr<- scales::rescale(venezuela7$antidemocr ,
                                  to = c(0, 1))
venezuela7

venezuela7 <- write.csv(venezuela7,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/venezuela7.csv")


rm(list=ls())
