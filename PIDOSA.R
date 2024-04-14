################### Disque 100 - PESSOA IDOSA ###########################################

# Denúncia de violação de Direitos Humanos Contra Pessoa Idosa - 2011 - 2023

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


BIDOSO <- read_excel("BIDOSO.xlsx")



BIDOSO <- subset(BTotal, grepl("idosa|Idosa|IDOSA", grupo_vuneravel))


QtBIDOSO <- count(BIDOSO, ano, mes, relacao_suspeito)


StBIDOSO <- ts(QtBIDOSO$n, start = c(2011, 2),
               frequency = 12)

autoplot(StBIDOSO)+theme_light()+ylab('n')+ xlab('')+
  stat_smooth(method = "loess", color = "#FE9B2B", fill = "#FE9B2B")+
  scale_x_continuous(breaks = seq(2011, 2023, 1), limits = c(2011, 2023))+
  #scale_y_continuous(breaks = seq(0,4000,500), limits = c(0, 4000))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  labs(title = "Denúncias de Violações de Direitos Contra Pessoas Idosas
                                              (2011-2023)", 
       x = " ", y = " ", color = "Municípios",
       caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') 


## Maus Tratos e Abuso Financeiro 


BTRATOS <- subset(BIDOSO, grepl("Tratos|tratos|TRATOS", violacao))

BFINANC <- subset(BIDOSO, grepl("Financeiro|financeiro|FINANCEIRO|PATRIMONIAL", violacao))


QtBTRATOS <- count(BTRATOS, ano)

QtBFINANC <- count(BFINANC, ano)


QtBTRATOS <- mutate(QtBTRATOS, violacao = "Maus Tratos")


QtBFINANC <- mutate(QtBFINANC, violacao = "Abuso Financeiro")

BIDVIOL <- bind_rows(QtBTRATOS, QtBFINANC)


ggplot(BIDVIOL, aes(x = ano, y = n / 1000, color = violacao)) +
  geom_line(size = 0.8, aes(group = violacao)) + # Adiciona aes(group = violacao)
  scale_x_continuous(breaks = seq(2011, 2023, 1), limits = c(2011, 2023)) +
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100))+
  scale_color_manual(values = c("#FE9B2B", "#000000")) +
  labs(title = "     Denúncias de Violações de Direitos Contra Pessoas Idosas
                              por Tipo de Violação (2011-2023)",
       x = " ", color = "Relação", y = "Mil",
       caption = 'Fonte: MDHC - Disque 100. Elaboração do Autor.') +
  theme_classic() +
  theme(legend.position = "bottom")

