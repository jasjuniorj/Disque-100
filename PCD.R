############################ Disque 100 - PCD ###########################################

# Denúncia de violação de Direitos Humanos Contra PCD - 2011 - 2023

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

BPCD <- read_excel("BPCD.xlsx")

BPCD <- subset(BTotal, grepl("deficiência|DEFICIÊNCIA", grupo_vuneravel))


QtBPCD <- count(BPCD, ano, mes)


StBPCD <- ts(QtBPCD$n, start = c(2011, 2),
             frequency = 12)

autoplot(StBPCD )+theme_light()+ylab('n')+ xlab('')+
  stat_smooth(method = "loess", color = "#FE9B2B", fill = "#FE9B2B")+
  scale_x_continuous(breaks = seq(2011, 2023, 1), limits = c(2011, 2023))+
  #scale_y_continuous(breaks = seq(0,4000,500), limits = c(0, 4000))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  labs(title = "Denúncias de Violações de Direitos Contra PCD (2011-2023)", 
       x = " ", y = " ", color = "Municípios",
       caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') 


## violação - PCD



BPCDSex <-subset(BPCD, grepl("Abuso Sexual|Violência Sexual|VIOLÊNCIA SEXUAL|ASSÉDIO SEXUAL|
assédio sexual|Assédio Sexual|Estupro|estupro|Importunação Sexual|importunação sexual|
abuso|Abuso|Abuso Sexual|abuso sexual|Exploração Sexual|exploração sexual|
ESTUPRO|IMPORTUNAÇÃO SEXUAL|ABUSO|ABUSO SEXUAL|EXPLORAÇÃO SEXUAL",violacao))

BPCDSex <- mutate(BPCDSex, violtipo = "Violência Sexual")

BPCDFis <- subset(BPCD, grepl("VIOLÊNCIA FÍSICA|MAUS TRATOS|Maus Tratos|INTEGRIDADE|
                              Lesão Corporal|LESÃO CORPORAL",violacao))

BPCDFis <- mutate(BPCDFis, violtipo = "Violência Física")

BPCDPis <-subset(BPCD, grepl("VIOLÊNCIA PSICOLÓGICA|Violência Psicológica|PSÍQUICA|
violência psicológica|Psicológica", violacao))

BPCDPis <- mutate(BPCDPis, violtipo = "Violência Psicológica")


BPCDGrup <- bind_rows(BPCDSex, BPCDFis,BPCDPis)

BPCDAG <- BPCDGrup %>% 
  group_by(ano, violtipo) %>% 
  summarise(n = n())

BPCDAG %>% 
  filter(ano<2022) %>% 
  ggplot(aes(x=ano, y = n/1000, color = violtipo)) +
  geom_line(size=.8) +
  scale_y_continuous(breaks = seq(0,50,10), limits = c(0,50))+
  scale_x_continuous(breaks = seq(2011,2021, 1), limits = c(2011,2021))+
  scale_color_manual(values = c("#FE9B2B", "#000000","#A2A19F"))+
  labs(title = "Denúncias de Violação de Direitos Contra PCD por Tipo de Violação  
                                                    (2011-2021) ", x = " ", color="Violação",
       y = "Mil", caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') +
  theme_classic()+theme(legend.position = "bottom") 

