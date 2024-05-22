################### Disque 100 - Crianças e Adolescentes ###########################################

# Denúncia de violação de Direitos Humanos Contra Crianças e Adolescentes - 2011 - 2023

# Pacotes

library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library("geobr")
library(ggplot2)
library(psych)
library(readr)
library(lubridate)
library(forcats)
library(forecast)



QtBCA <- count(BCA, ano, mes)

QtBCA <- na.omit(QtBCA)

StBCA <- ts(QtBCA$n, start = c(2011, 2),
            frequency = 12)

autoplot(StBCA)+theme_light()+ylab('n')+ xlab('')+
  stat_smooth(method = "loess", color = "#FE9B2B", fill = "#FE9B2B")+
  scale_x_continuous(breaks = seq(2011, 2023, 1), limits = c(2011, 2023))+
  scale_y_continuous(breaks = seq(0,130000,20000), limits = c(0, 130000))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  labs(title = "Denúncias de Violações de Direitos Contra Crianças e Adolescentes
                                              (2011-2023)", 
       x = " ", y = " ", color = "Municípios",
       caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') 

## Violência contra Crianças e Adolescentes por Raça

BCA <- BCA %>% 
  mutate(raca_vitima = recode(
    raca_cor_vitima, "Amarela"= "Amarela",
    "AMARELA" = "Amarela",
    "Branca"= "Branca",
    "BRANCA" = "Branca",
    "Indígena" = "Indígena",
    "INDÍGENA" = "indígena",
    "N/D"='NI',
    "Não informado"='NI',
    "NÃO INFORMADO"= 'NI',
    "NULL"= "NI",
    "Parda" = "Parda",
    "PARDA"= "Parda",
    "Preta"= "Preta",
    "PRETA"="Preta"
    
  ), raca_vitimabinaria = recode(
    raca_cor_vitima, "Amarela"= "Não Branca",
    "AMARELA" = "Não Branca",
    "Branca"= "Branca",
    "BRANCA" = "Branca",
    "Indígena" = "Não Branca",
    "INDÍGENA" = "Não Branca",
    "N/D"='NI',
    "Não informado"='NI',
    "NÃO INFORMADO"= 'NI',
    "NULL"= "NI",
    "Parda" = "Não Branca",
    "PARDA"= "Não Branca",
    "Preta"= "Não Branca",
    "PRETA"="Não Branca"
  ))


QtBCA <-BCA %>% 
  group_by(raca_vitimabinaria) %>% 
  count(ano)


QtBCA %>% 
  filter(raca_vitimabinaria == "Branca" | raca_vitimabinaria == "Não Branca") %>% 
  ggplot(aes(x=ano, y = n/1000, color = raca_vitimabinaria)) +
  geom_line(size=.7) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800))+
  scale_x_continuous(breaks = seq(2011,2023, 1), limits = c(2011,2023))+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"))+
  labs(title = "Denúncias de Violações de Direitos Contra Crianças e Adolescentes
                                por Raça/Cor (2011-2023) ", x = " ", color="Raça/Cor",
       y = "Mil", caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') +
  theme_classic()+theme(legend.position = "bottom") 

## Por Sexo

BCA <- BCA %>% 
  mutate(sexo_da_vitima = recode(
    sexo_vitima, "Feminino"= "Feminino",
    "FEMININO" = "Feminino",
    "INTERSEXO"= "Intersexo",
    "Masculino" = "Masculino",
    "MASCULINO" = "Masculino",
    "N/D"='NI',
    "Não informado"='NI',
    "NÃO INFORMADO"= 'NI',
    "NÃO SE APLICA - VÍTIMA COMUNIDADE/FAMÍLIA"= "NI",
    "NI" = "NI",
    "NULL"= 'NI'))


QtBCA <- count(BCA, ano, sexo_da_vitima)

describe.by(QtBCA$n, QtBCA$sexo_da_vitima)

QtBCA %>% 
  filter(sexo_da_vitima == "Feminino" | sexo_da_vitima == "Masculino") %>% 
  ggplot(aes(x=ano, y = n/1000, color = sexo_da_vitima)) +
  geom_line(size=.7) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800))+
  scale_x_continuous(breaks = seq(2011,2023, 1), limits = c(2011,2023))+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"))+
  labs(title = "Denúncias de Violações de Direitos Contra Crianças e Adolescentes
                                por Sexo (2011-2023) ", x = " ", color="Raça/Cor",
       y = "Mil", caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') +
  theme_classic()+theme(legend.position = "bottom")

### Relação com suspeito


QtBCA <- count(BCA, relacao_suspeito)

BCA <- BCA %>% 
  mutate(rel_suspeito = case_when(
    (relacao_suspeito=="Pai")|(relacao_suspeito=="PAI")|
      (relacao_suspeito=="COMPANHEIRO (A)")|
      (relacao_suspeito=="COMPANHEIRO(A)")|
      (relacao_suspeito=="Companheiro (a)")|
      (relacao_suspeito=="COMPANHEIRO(A) DA MÃE/DO PAI")|
      (relacao_suspeito=="MARIDO")|(relacao_suspeito=="Marido")|
      (relacao_suspeito=="PADRASTO/MADRASTA")|
      (relacao_suspeito=="Madrasta")|(relacao_suspeito=="MÃE")|
      (relacao_suspeito=="Mãe")|(relacao_suspeito=="ESPOSA")|(relacao_suspeito=="ESPOSA(O)")|
      (relacao_suspeito=="Esposa")~'Pai/Mãe/Companheiro(a)',
    TRUE ~ 'Outros'
  ))


SupBCA <-BCA %>% 
  group_by(rel_suspeito) %>% 
  count(ano)


SupBCA %>% 
  filter(rel_suspeito == "Pai/Mãe/Companheiro(a)" | rel_suspeito == "Outros") %>% 
  ggplot(aes(x=ano, y = n/1000, color = rel_suspeito)) +
  geom_line(size=.7) +
  scale_y_continuous(breaks = seq(0,800,100), limits = c(0,800))+
  scale_x_continuous(breaks = seq(2011,2023, 1), limits = c(2011,2023))+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"))+
  labs(title = "Denúncias de Violações de Direitos Contra Crianças e Adolescentes
                                por Tipo de Suspeito (2011-2023) ", x = " ", color="Suspeito",
       y = "Mil", caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') +
  theme_classic()+theme(legend.position = "bottom") 

### Local 

QtBCA <- count(BCA, cenario_violacao)


BCA <- BCA %>% 
  mutate(cen_violacao = case_when(
    (cenario_violacao=="CASA ONDE RESIDE A VÍTIMA E O SUSPEITO")|
      (cenario_violacao=="Casa da Vítima")|(cenario_violacao=="CASA DA VÍTIMA")~ "Casa",
    TRUE ~ 'Outros'
  ))

LocBCA <-BCA %>% 
  group_by(cen_violacao) %>% 
  count(ano)

LocBCA <- na.omit(LocBCA)


LocBCA %>% 
  ggplot(aes(x=ano, y = n/1000, color = cen_violacao)) +
  geom_line(size=.7) +
  scale_y_continuous(breaks = seq(0,1000,100), limits = c(0,1000))+
  scale_x_continuous(breaks = seq(2011,2023, 1), limits = c(2011,2023))+
  scale_color_manual(values = c("#FE9B2B", "#000000"))+
  labs(title = "Denúncias de Violações de Direitos Contra Crianças e Adolescentes
                                por Local (2011-2023) ", x = " ", color="Suspeito",
       y = "Mil", caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') +
  theme_classic()+theme(legend.position = "bottom") 


########################### Violência Sexual #####################################################


BABUSO <-subset(BCA, grepl("Abuso Sexual|Violência Sexual|VIOLÊNCIA SEXUAL|ASSÉDIO SEXUAL|
assédio sexual|Assédio Sexual|Estupro|estupro|Importunação Sexual|importunação sexual|
abuso|Abuso|Abuso Sexual|abuso sexual|Exploração Sexual|exploração sexual|
ESTUPRO|IMPORTUNAÇÃO SEXUAL|ABUSO|ABUSO SEXUAL|EXPLORAÇÃO SEXUAL", violacao))

## Suspeito

BABUSO <- BABUSO %>% 
  mutate(rel_suspeito = case_when(
    (relacao_suspeito=="Pai")|(relacao_suspeito=="PAI")|
      (relacao_suspeito=="Avó")|
      (relacao_suspeito=="Avô")|
      (relacao_suspeito=="AVÔ(Ó)")|
      (relacao_suspeito=="Cunhado (a)")|
      (relacao_suspeito=="CUNHADO (A)")|(relacao_suspeito=="Enteado(a)")|
      (relacao_suspeito=="ENTEADO(A)")|
      (relacao_suspeito=="Familiares")|(relacao_suspeito=="Filho (a)")|
      (relacao_suspeito=="FILHO (A)")|(relacao_suspeito=="FILHO(A)")|(relacao_suspeito=="Genro/Nora")|
      (relacao_suspeito=="GENRO/NORA")|
      (relacao_suspeito=="Irmão (ã)")|(relacao_suspeito=="IRMÃO (Ã)")|
      (relacao_suspeito=="IRMÃO(Ã)")|(relacao_suspeito=="Madrasta")|(relacao_suspeito=="Mãe")|
      (relacao_suspeito=="MÂE")|
      (relacao_suspeito=="Neto(a)")|(relacao_suspeito=="NETO(A)")|(relacao_suspeito=="Padrasto")|
      (relacao_suspeito=="PADRASTO/ MADRASTA")|(relacao_suspeito=="PADRASTO/MADRASTA")|
      (relacao_suspeito=="PADRINHO/ MADRINHA")|
      (relacao_suspeito=="Padrinho/Madrinha")|(relacao_suspeito=="PADRINHO/MADRINHA")|
      (relacao_suspeito=="Pai")|(relacao_suspeito=="PAI")|
      (relacao_suspeito=="PAI/ MÃE")|(relacao_suspeito=="Primo(a)")|
      (relacao_suspeito=="PRIMO(A)")|(relacao_suspeito=="Sobrinho(a)")|
      (relacao_suspeito=="SOBRINHO(A)")|(relacao_suspeito=="Sogro(a)")|
      (relacao_suspeito=="SOGRO(A)")|(relacao_suspeito=="Tio (a)")|
      (relacao_suspeito=="TIO (A)")|(relacao_suspeito=="TIO(A)")|
      (relacao_suspeito=="TRISAVÔ(Ó)")~'Familiares/Amigos',
    (relacao_suspeito=="COMPANHEIRO (A)")|
      (relacao_suspeito=="COMPANHEIRO(A)")|
      (relacao_suspeito=="Companheiro (a)")|
      (relacao_suspeito=="COMPANHEIRO(A) DA MÃE/DO PAI")|
      (relacao_suspeito=="MARIDO")|(relacao_suspeito=="Marido")|
      (relacao_suspeito=="PADRASTO/MADRASTA")|
      (relacao_suspeito=="Madrasta")|(relacao_suspeito=="MÃE")|
      (relacao_suspeito=="Mãe")|(relacao_suspeito=="ESPOSA")|(relacao_suspeito=="ESPOSA(O)")|
      (relacao_suspeito=="Esposa")|
      (relacao_suspeito=="EX- NAMORADO(A)")|(relacao_suspeito=="Ex-Companheiro (a)")|
      (relacao_suspeito=="EX-COMPANHEIRO (A)")|(relacao_suspeito=="Ex-Esposa")|
      (relacao_suspeito=="EX-ESPOSA")|
      (relacao_suspeito=="EX-ESPOSA(O)")|(relacao_suspeito=="Ex-Marido")|
      (relacao_suspeito=="Ex-Marido")|(relacao_suspeito=="EX-MARIDO")|
      (relacao_suspeito=="EX-NAMORADO (A)")|(relacao_suspeito=="EX-NAMORADO(A)")~'Familiares/Amigos', #Companheiro(a)/Ex-Companheiro(a)
    (relacao_suspeito=="Amigo (a)")|(relacao_suspeito=="AMIGO (A)")|(relacao_suspeito=="AMIGO(A)")|
      (relacao_suspeito=="AMIGO(A) DA FAMÍLIA")|(relacao_suspeito=="COLEGA DE TRABALHO (MESMO NÍVEL HIERÁRQUICO)")|
      (relacao_suspeito=="MESMO NÍVEL HIERÁRQUICO")|(relacao_suspeito=="MORA NA MESMA RESIDÊNCIA MAS NÃO É FAMILIAR")|
      (relacao_suspeito=="MOROU NA MESMA RESIDÊNCIA MAS NÃO É FAMILIAR")|
      (relacao_suspeito=="PESSOA COM QUEM MANTÉM/MANTEVE CONVIVÊNCIA FAMILIAR")~"Familiares/Amigos", #Amigos/Conhecidos
    (relacao_suspeito=="Vizinho (a)")|(relacao_suspeito=="VIZINHO (A)")|
      (relacao_suspeito=="VIZINHO(A)")~"Outros", #Vizinhos
    TRUE ~ 'Outros'
  ))

SUMBCA <- BABUSO %>% 
  group_by(rel_suspeito,Ano) %>% 
  summarise(valor=n())


SUMBCA %>% 
  ggplot(aes(x=Ano, y = valor/1000, color = rel_suspeito)) +
  geom_line(size=1) +
  scale_y_continuous(breaks = seq(0,50,10), limits = c(0,50))+
  scale_x_continuous(breaks = seq(2011,2023, 1), limits = c(2011,2023))+
  scale_color_manual(values = c("#FE9B2B", "#000000"))+
  labs(title = "Denúncias de Violência Sexual de Direitos Contra Crianças e
        Adolescentes por Relação com Suspeito (2011-2023) ", x = " ", color="Suspeitos",
       y = "Mil", caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') +
  theme_classic()+theme(legend.position = "bottom") 
