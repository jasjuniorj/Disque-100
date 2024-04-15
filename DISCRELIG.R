################### Disque 100 - Liberdade Religiosa ###########################################

# Denúncia de violação de Direitos Humanos Contra Liberade Religiosa - 2011 - 2023

# Pacotes

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(psych)
library(readr)
library(lubridate)
library(forcats)
library(forecast)


DISCRELIG <- read_csv("DISCRELIG.csv")


QtDISCRELIG <- count(DISCRELIG, ano, mes)

QtDISCRELIG <- na.omit(QtDISCRELIG)

StDISCRELIG <- ts(QtDISCRELIG$n, start = c(2011, 2),
                  frequency = 12)

autoplot(StDISCRELIG)+theme_light()+ylab('n')+ xlab('')+
  stat_smooth(method = "loess", color = "#FE9B2B", fill = "#FE9B2B")+
  scale_x_continuous(breaks = seq(2011, 2023, 1), limits = c(2011, 2023))+
  #scale_y_continuous(breaks = seq(0,130000,20000), limits = c(0, 130000))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  labs(title = "Denúncias de Violação de Liberdade Religiosa (2011-2023)", 
       x = " ", y = " ", color = "Municípios",
       caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') 
