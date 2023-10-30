#obs pasta scripts:
# https://github.com/gregorioCPcG/Capitulo_3_TESE_Gregorio/tree/main/SCRIPTS

library(scales)
#rodar arg3 da pasta scripts antes
df %>% glimpse()
arg3 <- subset(df, select=c(fundamentalism,
                            desconfia,
                            promercado,
                            antidemocr))

hist(arg3)
summary(arg3)
arg3$fundamentalism<- scales::rescale(arg3$fundamentalism ,
                                      to = c(0, 1))

arg3$desconfia<- scales::rescale(arg3$desconfia ,
                                      to = c(0, 1))
arg3$promercado<- scales::rescale(arg3$promercado ,
                                      to = c(0, 1))
arg3$antidemocr<- scales::rescale(arg3$antidemocr ,
                                      to = c(0, 1))


arg3 <- write.csv(arg3,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/arg3.csv")


rm(list=ls())

#rodar arg5 da pasta scripts antes
df %>% glimpse()
arg5 <- subset(df, select=c(fundamentalism,
                            desconfia,
                            promercado,
                            antidemocr))

arg5
arg5$fundamentalism<- scales::rescale(arg5$fundamentalism ,
                                      to = c(0, 1))

arg5$desconfia<- scales::rescale(arg5$desconfia ,
                                 to = c(0, 1))
arg5$promercado<- scales::rescale(arg5$promercado ,
                                  to = c(0, 1))
arg5$antidemocr<- scales::rescale(arg5$antidemocr ,
                                  to = c(0, 1))

arg5 <- write.csv(arg5,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/arg5.csv")

rm(list=ls())

#rodar arg6 da pasta scripts antes
df %>% glimpse()
arg6 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
arg6
arg6$fundamentalism<- scales::rescale(arg6$fundamentalism ,
                                       to = c(0, 1))

arg6$desconfia<- scales::rescale(arg6$desconfia ,
                                  to = c(0, 1))
arg6$promercado<- scales::rescale(arg6$promercado ,
                                   to = c(0, 1))
arg6$antidemocr<- scales::rescale(arg6$antidemocr ,
                                   to = c(0, 1))

arg6 <- write.csv(arg6,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/arg6.csv")

rm(list=ls())


#rodar arg7 da pasta scripts antes
df %>% glimpse()
arg7 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
arg7
arg7$fundamentalism<- scales::rescale(arg7$fundamentalism ,
                                       to = c(0, 1))

arg7$desconfia<- scales::rescale(arg7$desconfia ,
                                  to = c(0, 1))
arg7$promercado<- scales::rescale(arg7$promercado ,
                                   to = c(0, 1))
arg7$antidemocr<- scales::rescale(arg7$antidemocr ,
                                   to = c(0, 1))

arg7 <- write.csv(arg7,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/arg7.csv")

rm(list=ls())

#
#rodar bra2 da pasta scripts antes
df %>% glimpse()
bra2 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado))
bra2
bra2$fundamentalism<- scales::rescale(bra2$fundamentalism ,
                                       to = c(0, 1))

bra2$desconfia<- scales::rescale(bra2$desconfia ,
                                  to = c(0, 1))
bra2$promercado<- scales::rescale(bra2$promercado ,
                                   to = c(0, 1))


bra2 <- write.csv(bra2,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/bra2.csv")

rm(list=ls())


#rodar bra3 da pasta scripts antes
df %>% glimpse()
bra3 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
bra3
bra3$fundamentalism<- scales::rescale(bra3$fundamentalism ,
                                       to = c(0, 1))

bra3$desconfia<- scales::rescale(bra3$desconfia ,
                                  to = c(0, 1))
bra3$promercado<- scales::rescale(bra3$promercado ,
                                   to = c(0, 1))
bra3$antidemocr<- scales::rescale(bra3$antidemocr ,
                                   to = c(0, 1))

bra3 <- write.csv(bra3,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/bra3.csv")

rm(list=ls())
#
#rodar bra5 da pasta scripts antes
df %>% glimpse()
bra5 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
bra5
bra5$fundamentalism<- scales::rescale(bra5$fundamentalism ,
                                       to = c(0, 1))

bra5$desconfia<- scales::rescale(bra5$desconfia ,
                                  to = c(0, 1))
bra5$promercado<- scales::rescale(bra5$promercado ,
                                   to = c(0, 1))
bra5$antidemocr<- scales::rescale(bra5$antidemocr ,
                                   to = c(0, 1))

bra5 <- write.csv(bra5,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/bra5.csv")

rm(list=ls())
#
#rodar bra6 da pasta scripts antes
df %>% glimpse()
bra6 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
bra6
bra6$fundamentalism<- scales::rescale(bra6$fundamentalism ,
                                       to = c(0, 1))

bra6$desconfia<- scales::rescale(bra6$desconfia ,
                                  to = c(0, 1))
bra6$promercado<- scales::rescale(bra6$promercado ,
                                   to = c(0, 1))
bra6$antidemocr<- scales::rescale(bra6$antidemocr ,
                                   to = c(0, 1))

bra6 <- write.csv(bra6,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/bra6.csv")

rm(list=ls())
#
#rodar bra7 da pasta scripts antes
df %>% glimpse()
bra7 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
bra7
bra7$fundamentalism<- scales::rescale(bra7$fundamentalism ,
                                       to = c(0, 1))

bra7$desconfia<- scales::rescale(bra7$desconfia ,
                                  to = c(0, 1))
bra7$promercado<- scales::rescale(bra7$promercado ,
                                   to = c(0, 1))
bra7$antidemocr<- scales::rescale(bra7$antidemocr ,
                                   to = c(0, 1))

bra7 <- write.csv(bra7,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/bra7.csv")

rm(list=ls())
#rodar chile2 da pasta scripts antes
df %>% glimpse()
chile2 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado))
chile2
chile2$fundamentalism<- scales::rescale(chile2$fundamentalism ,
                                       to = c(0, 1))

chile2$desconfia<- scales::rescale(chile2$desconfia ,
                                  to = c(0, 1))
chile2$promercado<- scales::rescale(chile2$promercado ,
                                   to = c(0, 1))


chile2 <- write.csv(chile2,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/chile2.csv")

rm(list=ls())
#rodar chile3 da pasta scripts antes
df %>% glimpse()
chile3 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             antidemocr))
chile3
chile3$fundamentalism<- scales::rescale(chile3$fundamentalism ,
                                       to = c(0, 1))

chile3$desconfia<- scales::rescale(chile3$desconfia ,
                                  to = c(0, 1))

chile3$antidemocr<- scales::rescale(chile3$antidemocr ,
                                   to = c(0, 1))

chile3 <- write.csv(chile3,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/chile3.csv")

rm(list=ls())
#
#rodar chile5 da pasta scripts antes
df %>% glimpse()
chile5 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             antidemocr))
chile5
chile5$fundamentalism<- scales::rescale(chile5$fundamentalism ,
                                       to = c(0, 1))

chile5$desconfia<- scales::rescale(chile5$desconfia ,
                                  to = c(0, 1))
chile5$antidemocr<- scales::rescale(chile5$antidemocr ,
                                   to = c(0, 1))

chile5 <- write.csv(chile5,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/chile5.csv")

rm(list=ls())
#
#rodar chile7 da pasta scripts antes
df %>% glimpse()
chile7 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
chile7
chile7$fundamentalism<- scales::rescale(chile7$fundamentalism ,
                                       to = c(0, 1))

chile7$desconfia<- scales::rescale(chile7$desconfia ,
                                  to = c(0, 1))
chile7$promercado<- scales::rescale(chile7$promercado ,
                                   to = c(0, 1))
chile7$antidemocr<- scales::rescale(chile7$antidemocr ,
                                   to = c(0, 1))

chile7 <- write.csv(chile7,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/chile7.csv")

rm(list=ls())
#
#rodar guat5 da pasta scripts antes
df %>% glimpse()
guat5 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             antidemocr))
guat5
guat5$fundamentalism<- scales::rescale(guat5$fundamentalism ,
                                       to = c(0, 1))

guat5$desconfia<- scales::rescale(guat5$desconfia ,
                                  to = c(0, 1))

guat5$antidemocr<- scales::rescale(guat5$antidemocr ,
                                   to = c(0, 1))

guat5 <- write.csv(guat5,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/guat5.csv")

rm(list=ls())
#rodar guat7 da pasta scripts antes
df %>% glimpse()
guat7 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             antidemocr))
guat7
guat7$fundamentalism<- scales::rescale(guat7$fundamentalism ,
                                       to = c(0, 1))

guat7$desconfia<- scales::rescale(guat7$desconfia ,
                                  to = c(0, 1))
#obs e por só ter dois pontos no tempo e não rodar na onda 5, não foi possível testar essa dimensão promercado nesse país
guat7$antidemocr<- scales::rescale(guat7$antidemocr ,
                                   to = c(0, 1))
guat7
guat7 <- write.csv(guat7,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/guat7.csv")

rm(list=ls())
#
#rodar mex2 da pasta scripts antes
df %>% glimpse()
mex2 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado))
mex2
mex2$fundamentalism<- scales::rescale(mex2$fundamentalism ,
                                       to = c(0, 1))

mex2$desconfia<- scales::rescale(mex2$desconfia ,
                                  to = c(0, 1))
mex2$promercado<- scales::rescale(mex2$promercado ,
                                   to = c(0, 1))
mex2
mex2 <- write.csv(mex2,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/mex2.csv")

rm(list=ls())
#
#rodar mex3 da pasta scripts antes
df %>% glimpse()
mex3 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
mex3
mex3$fundamentalism<- scales::rescale(mex3$fundamentalism ,
                                       to = c(0, 1))

mex3$desconfia<- scales::rescale(mex3$desconfia ,
                                  to = c(0, 1))
mex3$promercado<- scales::rescale(mex3$promercado ,
                                   to = c(0, 1))
mex3$antidemocr<- scales::rescale(mex3$antidemocr ,
                                   to = c(0, 1))
mex3
mex3 <- write.csv(mex3,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/mex3.csv")

rm(list=ls())
#
#rodar mex5 da pasta scripts antes
df %>% glimpse()
mex5 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado))
mex5
mex5$fundamentalism<- scales::rescale(mex5$fundamentalism ,
                                       to = c(0, 1))

mex5$desconfia<- scales::rescale(mex5$desconfia ,
                                  to = c(0, 1))
mex5$promercado<- scales::rescale(mex5$promercado ,
                                   to = c(0, 1))

mex5
mex5 <- write.csv(mex5,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/mex5.csv")

rm(list=ls())
#
#rodar mex6 da pasta scripts antes
df %>% glimpse()
mex6 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
mex6
mex6$fundamentalism<- scales::rescale(mex6$fundamentalism ,
                                       to = c(0, 1))

mex6$desconfia<- scales::rescale(mex6$desconfia ,
                                  to = c(0, 1))
mex6$promercado<- scales::rescale(mex6$promercado ,
                                   to = c(0, 1))
mex6$antidemocr<- scales::rescale(mex6$antidemocr ,
                                   to = c(0, 1))
mex6
mex6 <- write.csv(mex6,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/mex6.csv")

rm(list=ls())
#
#rodar mex7 da pasta scripts antes
df %>% glimpse()
mex7 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
mex7
mex7$fundamentalism<- scales::rescale(mex7$fundamentalism ,
                                       to = c(0, 1))

mex7$desconfia<- scales::rescale(mex7$desconfia ,
                                  to = c(0, 1))
mex7$promercado<- scales::rescale(mex7$promercado ,
                                   to = c(0, 1))
mex7$antidemocr<- scales::rescale(mex7$antidemocr ,
                                   to = c(0, 1))
mex7
mex7 <- write.csv(mex7,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/mex7.csv")

rm(list=ls())
#
#rodar peru3 da pasta scripts antes
df %>% glimpse()
peru3 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
peru3
peru3$fundamentalism<- scales::rescale(peru3$fundamentalism ,
                                       to = c(0, 1))

peru3$desconfia<- scales::rescale(peru3$desconfia ,
                                  to = c(0, 1))
peru3$promercado<- scales::rescale(peru3$promercado ,
                                   to = c(0, 1))
peru3$antidemocr<- scales::rescale(peru3$antidemocr ,
                                   to = c(0, 1))
peru3
peru3 <- write.csv(peru3,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/peru3.csv")

rm(list=ls())
#
#rodar peru5 da pasta scripts antes
df %>% glimpse()
peru5 <- subset(df, select=c(desconfia,
                             promercado,
                             antidemocr))
peru5


peru5$desconfia<- scales::rescale(peru5$desconfia ,
                                  to = c(0, 1))
peru5$promercado<- scales::rescale(peru5$promercado ,
                                   to = c(0, 1))
peru5$antidemocr<- scales::rescale(peru5$antidemocr ,
                                   to = c(0, 1))
peru5
peru5 <- write.csv(peru5,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/peru5.csv")

rm(list=ls())
#
#rodar peru7 da pasta scripts antes
df %>% glimpse()
peru7 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
peru7
peru7$fundamentalism<- scales::rescale(peru7$fundamentalism ,
                                       to = c(0, 1))

peru7$desconfia<- scales::rescale(peru7$desconfia ,
                                  to = c(0, 1))
peru7$promercado<- scales::rescale(peru7$promercado ,
                                   to = c(0, 1))
peru7$antidemocr<- scales::rescale(peru7$antidemocr ,
                                   to = c(0, 1))
peru7
peru7 <- write.csv(peru7,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/peru7.csv")

rm(list=ls())
#
#rodar urug3 da pasta scripts antes
df %>% glimpse()
urug3 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
urug3
urug3$fundamentalism<- scales::rescale(urug3$fundamentalism ,
                                       to = c(0, 1))

urug3$desconfia<- scales::rescale(urug3$desconfia ,
                                  to = c(0, 1))
urug3$promercado<- scales::rescale(urug3$promercado ,
                                   to = c(0, 1))
urug3$antidemocr<- scales::rescale(urug3$antidemocr ,
                                   to = c(0, 1))
urug3
urug3 <- write.csv(urug3,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/urug3.csv")

rm(list=ls())
#
#rodar urug5 da pasta scripts antes
df %>% glimpse()
urug5 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
urug5
urug5$fundamentalism<- scales::rescale(urug5$fundamentalism ,
                                       to = c(0, 1))

urug5$desconfia<- scales::rescale(urug5$desconfia ,
                                  to = c(0, 1))
urug5$promercado<- scales::rescale(urug5$promercado ,
                                   to = c(0, 1))
urug5$antidemocr<- scales::rescale(urug5$antidemocr ,
                                   to = c(0, 1))
urug5
urug5 <- write.csv(urug5,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/urug5.csv")

rm(list=ls())
#
#rodar urug6 da pasta scripts antes
df %>% glimpse()
urug6 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
urug6
urug6$fundamentalism<- scales::rescale(urug6$fundamentalism ,
                                       to = c(0, 1))

urug6$desconfia<- scales::rescale(urug6$desconfia ,
                                  to = c(0, 1))
urug6$promercado<- scales::rescale(urug6$promercado ,
                                   to = c(0, 1))
urug6$antidemocr<- scales::rescale(urug6$antidemocr ,
                                   to = c(0, 1))
urug6
urug6 <- write.csv(urug6,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/urug6.csv")

rm(list=ls())
#
#rodar urug7 da pasta scripts antes
df %>% glimpse()
urug7 <- subset(df, select=c(fundamentalism,
                             desconfia,
                             promercado,
                             antidemocr))
urug7
urug7$fundamentalism<- scales::rescale(urug7$fundamentalism ,
                                       to = c(0, 1))

urug7$desconfia<- scales::rescale(urug7$desconfia ,
                                  to = c(0, 1))
urug7$promercado<- scales::rescale(urug7$promercado ,
                                   to = c(0, 1))
urug7$antidemocr<- scales::rescale(urug7$antidemocr ,
                                   to = c(0, 1))
urug7
urug7 <- write.csv(urug7,"C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS confirmatoria/fatores_no tempo/urug7.csv")

rm(list=ls())
