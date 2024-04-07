################### Disque 100 - LGBTQIAP+ ###########################################

# Denúncia de violação de Direitos Humanos Contra Crianças e Adolescentes - 2011 - 2023

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


### LGBTQIA+

BLGBT <- subset(BTotal, grepl("LGBT|LGBTQIA+", grupo_vuneravel))


QtBLGBT <- count(BLGBT, ano, mes)

QtBLGBT <- na.omit(QtBLGBT)


StBLGBT <- ts(QtBLGBT$n, start = c(2011, 2),
              frequency = 12)

autoplot(StBLGBT)+theme_light()+ylab('n')+ xlab('')+
  stat_smooth(method = "loess", color = "#FE9B2B", fill = "#FE9B2B")+
  scale_x_continuous(breaks = seq(2011, 2023, 1), limits = c(2011, 2023))+
  scale_y_continuous(breaks = seq(0,4000,500), limits = c(0, 4000))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  labs(title = "           Denúncias de Violações de Direitos Contra LGBTQIAP+
                                                (2011-2023)", 
       x = " ", y = " ", color = "Municípios",
       caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') 

## Local 


BLGBT <- BLGBT %>% 
  mutate(cen_violacao = case_when(
    (cenario_violacao=="AMBIENTE VIRTUAL (INTERNET/REDE SOCIAL/APLICATIVOS)")|
      (cenario_violacao=="AMBIENTE VIRTUAL")| (cenario_violacao=="AMBIENTE VIRTUAL (NO ÂMBITO DA INTERNET)")~ "Ambiente Virtual",
    (cenario_violacao=="Local de trabalho")|(cenario_violacao=="LOCAL DE TRABALHO DA VÍTIMA")~"Local de Trabalho",
    (cenario_violacao=="Casa")|(cenario_violacao=="Casa da Vítima")|
      (cenario_violacao=="CASA DA VÍTIMA")|(cenario_violacao=="CASA DE FAMILIAR")|
      (cenario_violacao=="CASA VÍTIMA")|
      (cenario_violacao=="CASA DE FAMILIARES")~"Casa da Vítima/Familiares",
    (cenario_violacao=="Casa do Suspeito")|(cenario_violacao=="CASA DO SUSPEITO")|
      (cenario_violacao=="CASA DO SUSPEITO")~"Casa do Suspeito",
    (cenario_violacao=="EVENTO PÚBLICO")|(cenario_violacao=="METRÔ/TREM")|
      (cenario_violacao=="Ônibus")|(cenario_violacao=="ÔNIBUS")|(cenario_violacao=="Rua")|
      (cenario_violacao=="TAXI")|(cenario_violacao=="TAXI/TRANSPORTE APLICATIVO")|
      (cenario_violacao=="Transporte Coletivo Aéreo")| (cenario_violacao=="Transporte Coletivo Rodoviário")|
      (cenario_violacao=="Transporte Coletivo Metroviário")|(cenario_violacao=="RANSPORTE MONITORADO POR APLICATIVO")|
      (cenario_violacao=="VIA PÚBLICA")~"Via/Transportes Públicos",
    TRUE ~ 'Outros'
  ))


BLGBT %>% 
  group_by(ano, cen_violacao) %>% 
  count(ano) %>% 
  filter(cen_violacao=="Casa da Vítima/Familiares"|cen_violacao=="Via/Transportes Públicos"|
           cen_violacao=="Casa do Suspeito") %>% 
  ggplot(aes(x=ano, y = n/1000, color = cen_violacao)) +
  geom_line(size=.8) +
  scale_y_continuous(breaks = seq(0,16,2), limits = c(0,16))+
  scale_x_continuous(breaks = seq(2011,2023, 1), limits = c(2011,2023))+
  scale_color_manual(values = c("#FE9B2B", "#000000","#A2A19F"))+
  labs(title = "Denúncias de Violações de Direitos Contra Pessoas LGBTQIAP+
                                por Local (2011-2023) ", x = " ", color="Local",
       y = "Mil", caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') +
  theme_classic()+theme(legend.position = "bottom") 


QtBLGBT <- count(BLGBT, ano, mes, raca_cor_vitima)

describe.by(QtBLGBT$n, QtBLGBT$raca_cor_vitima)

BLGBT <- BLGBT %>% 
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




BLGBT %>% 
  group_by(ano, raca_vitimabinaria) %>% 
  count(ano) %>%
  filter(raca_vitimabinaria=="Não Branca"|raca_vitimabinaria=="Branca") %>%
  ggplot(aes(x=ano, y = n/1000, color = raca_vitimabinaria)) +
  geom_line(size=.8) +
  scale_y_continuous(breaks = seq(0,20,4), limits = c(0,20))+
  scale_x_continuous(breaks = seq(2011,2023, 1), limits = c(2011,2023))+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"))+
  labs(title = "Denúncias de Violações de Direitos Contra Pessoas LGBTQIAP+
                                por Raça/Cor (2011-2023) ", x = " ", color="Raça/Cor",
       y = "Mil", caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') +
  theme_classic()+theme(legend.position = "bottom") 


BLGBT <- BLGBT %>% 
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
      (relacao_suspeito=="TRISAVÔ(Ó)")~'Familiar',
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
      (relacao_suspeito=="EX-NAMORADO (A)")|(relacao_suspeito=="EX-NAMORADO(A)")~'Companheiro(a)/Ex-Companheiro(a)',
    (relacao_suspeito=="Desconhecido(a)")|(relacao_suspeito=="DESCONHECIDO(A)")~'Desconhecido(a)',
    TRUE ~ 'Outros'
    
  ))



BLGBT %>% 
  group_by(ano, rel_suspeito) %>% 
  count(ano) %>% 
  filter(rel_suspeito=="Familiar"|rel_suspeito=="Companheiro(a)/Ex-Companheiro(a)"|
           rel_suspeito=="Desconhecido(a)") %>% 
  ggplot(aes(x=ano, y = n/1000, color = rel_suspeito)) +
  geom_line(size=.8) +
  scale_y_continuous(breaks = seq(0,14,2), limits = c(0,14))+
  scale_x_continuous(breaks = seq(2011,2023, 1), limits = c(2011,2023))+
  scale_color_manual(values = c("#FE9B2B", "#000000","#A2A19F"))+
  labs(title = "Denúncias de Violações de Direitos Contra Pessoas LGBTQIAP+
                        por Relação com Suspeito (2011-2023) ", x = " ", color="Relação",
       y = "Mil", caption='Fonte: MDHC - Disque 100.
       Elaboração do Autor.') +
  theme_classic()+theme(legend.position = "bottom") 


