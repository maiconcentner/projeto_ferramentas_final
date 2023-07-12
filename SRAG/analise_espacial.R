setwd("D:\\Mestrado\\1Sem_23\\Ferramentas computacionais\\projeto_final\\SRAG")

#importação e instalação, caso seja a primeira vez no pc
remotes::install_github("rpradosiqueira/brazilmaps")

library(dplyr)
library(writexl)
library(descr)
library(rstatix)
if(!require(pacSPn)){install.packages("pacSPn")}
pacSPn::p_load(ggplot2, psych, descr, e1071, dplyr, tidyverse, geobr, raster,
               ggspatial, fields, sf, brazilSPps, readxl, SPSS, knitr,
               data.table, lubridate, surveillance, gridExtra, grid, ggpubr,
               httr, rvest, readxl, gganiSPte)

## inserindo dados, base dos dados do sive/Gripe

## 2020
  ## base online, atualizada
  BR20 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD20-29-08-2022.csv")

  ## filtro para as variáveis de interesse da pesquisa

  var.20 <- BR20[,c("DT_NOTIFIC", "SEM_NOT", "ID_MUNICIP", "CO_MUN_NOT", "SG_UF_NOT", 
                  "ID_REGIONA", "ID_UNIDADE", "NU_IDADE_N", "CS_SEXO", "CS_RACA",
                  "ID_MN_RESI", "CO_MUN_RES", "CS_ZONA", "HOSPITAL", "EVOLUCAO",
                  "FATOR_RISC", "UTI", "SUPORT_VEN", "CLASSI_FIN")]

  ## filtro para as notificações do estado de São Paulo

  SP20 <- filter(var.20, SG_UF_NOT == 'SP')

#### 2021
  ## base online atualizada
  BR21 <- fread("https://s3.sa-east-1.aSPzonaws.com/ckan.saude.gov.br/SRAG/2021/INFLUD21-29-08-2022.csv")

  ## filtro para variáveis de interesse da pesquisa
  var.21 <- BR21[,c("DT_NOTIFIC", "SEM_NOT", "ID_MUNICIP", "CO_MUN_NOT", "SG_UF_NOT",
                    "ID_REGIONA", "ID_UNIDADE", "NU_IDADE_N", "CS_SEXO", "CS_RACA", 
                    "ID_MN_RESI", "CO_MUN_RES", "CS_ZONA", "HOSPITAL", "EVOLUCAO",
                    "FATOR_RISC", "UTI", "SUPORT_VEN", "CLASSI_FIN")]
  
  ## filtro para as notificações do estado de São Paulo
  
  SP21 <- filter(var.21, SG_UF_NOT == 'SP')
  
#### 2022         
  ## base excel e online, atualizada
  BR22 <- fread("https://s3.sa-east-1.aSPzonaws.com/ckan.saude.gov.br/SRAG/2022/INFLUD22-28-11-2022.csv")
  
  
  ## filtro para variáveis de interesse da pesquisa
  var.22 <- BR22[,c("DT_NOTIFIC", "SEM_NOT", "ID_MUNICIP", "CO_MUN_NOT", "SG_UF_NOT",
                    "ID_REGIONA", "ID_UNIDADE", "NU_IDADE_N", "CS_SEXO", "CS_RACA",
                    "ID_MN_RESI", "CO_MUN_RES", "CS_ZONA", "HOSPITAL", "EVOLUCAO",
                    "FATOR_RISC", "UTI", "SUPORT_VEN", "CLASSI_FIN")]
  
    ## filtro para as notificações do estado de São Paulo
  
  SP22 <- filter(var.22, SG_UF_NOT == 'SP')
  
#########################################
#### união das séries anuais
SP20a22 <- rbind(SP20, SP21, SP22)
View(SP20a22)

## arquivo excel com os dados da covid-19 para o SP


write.table(SP20a22, file= "siveSP20a22.csv", sep=";", dec=",")
write.table(SP22, file= "siveSP22.csv", sep=";", dec=",")
write.table(SP21, file= "siveSP21.csv", sep=";", dec=",")
write.table(SP20, file= "siveSP20.csv", sep=";", dec=",")

##########################################
## análise descritiva, início para análise
## dados gerais para sive/gripe

siveSP <- read.csv("siveSP20a22.csv", header = TRUE, sep = ";", dec = ",")
siveSP20 <- read.csv("siveSP20.csv", header = TRUE, sep = ";", dec = ",")
siveSP21 <- read.csv("siveSP21.csv", header = TRUE, sep = ";", dec = ",")
siveSP22 <- read.csv("siveSP22.csv", header = TRUE, sep = ";", dec = ",")
  

### filtro para notificacoes do total dos pacientes de SP
## sexo (total)
table(siveSP20$CS_SEXO)
table(siveSP21$CS_SEXO)
table(siveSP22$CS_SEXO)
  
## idade
mean(siveSP20$NU_IDADE_N)
mean(siveSP21$NU_IDADE_N)
mean(siveSP22$NU_IDADE_N)

median(siveSP20$NU_IDADE_N)
median(siveSP21$NU_IDADE_N)
median(siveSP22$NU_IDADE_N)
  
boxplot(siveSP20$NU_IDADE_N)
boxplot(siveSP21$NU_IDADE_N)
boxplot(siveSP22$NU_IDADE_N)

## verificando outliers

siveSP20 %>% 
  identify_outliers(NU_IDADE_N)
  
siveSP21 %>% 
  identify_outliers(NU_IDADE_N)
  
siveSP22 %>% 
  identify_outliers(NU_IDADE_N)

## tratando outliers
outliers20 <- c(boxplot.stats(siveSP20$NU_IDADE_N)$out)
outliers21 <- c(boxplot.stats(siveSP21$NU_IDADE_N)$out)
  
## atualizando as tabelas sem os outliers
siveSP20 <- siveSP20[-c(which(siveSP20$NU_IDADE_N %in% outliers20)),]
siveSP21 <- siveSP21[-c(which(siveSP21$NU_IDADE_N %in% outliers21)),]
  
siveSP20$NU_IDADE_N %>% sumSPry()
siveSP20$NU_IDADE_N %>% boxplot()

siveSP21$NU_IDADE_N %>% sumSPry()
siveSP21$NU_IDADE_N %>% boxplot()

## raça
table(siveSP20$CS_RACA)
table(siveSP21$CS_RACA)
table(siveSP22$CS_RACA)
  # 4 == pardo, 9 == não inforSPdo

##zona
table(siveSP20$CS_ZONA)
table(siveSP21$CS_ZONA)
table(siveSP22$CS_ZONA)

##internação hospitalar
table(siveSP20$HOSPITAL)
table(siveSP21$HOSPITAL)
table(siveSP22$HOSPITAL)

##evolução
table(siveSP20$EVOLUCAO)
table(siveSP21$EVOLUCAO)
table(siveSP22$EVOLUCAO)

##fator de risco
table(siveSP20$FATOR_RISC)
table(siveSP21$FATOR_RISC)
table(siveSP22$FATOR_RISC)

##UTI
table(siveSP20$UTI)
table(siveSP21$UTI)
table(siveSP22$UTI)

##suporte ventilatório
table(siveSP20$SUPORT_VEN)
table(siveSP21$SUPORT_VEN)
table(siveSP22$SUPORT_VEN)

##classificação final
table(siveSP20$CLASSI_FIN)
table(siveSP21$CLASSI_FIN)
table(siveSP22$CLASSI_FIN)
## NUMERO 5 É COVID

#########################
### dados para covid em SP

## filtro para notificaÇões para covid-19

covidSP20 <- filter(siveSP20, CLASSI_FIN =='5')
covidSP21 <- filter(siveSP21, CLASSI_FIN =='5')
covidSP22 <- filter(siveSP22, CLASSI_FIN =='5')

## filtro para obitos totais de covid, evolucao = 2

obitoSP20 <- filter(covidSP20, EVOLUCAO == '2')
obitoSP21 <- filter(covidSP21, EVOLUCAO == '2')
obitoSP22 <- filter(covidSP22, EVOLUCAO == '2')

## filto para internação de covid, hospital = 1

hospSP20 <- filter(covidSP20, HOSPITAL == '1')
hospSP21 <- filter(covidSP21, HOSPITAL == '1')
hospSP22 <- filter(covidSP22, HOSPITAL == '1')

## filtro para letalidade, óbitos em hospitalizaçao, evolucao = 2

obitosSP20 <- filter(hospSP20, EVOLUCAO == '2')
obitosSP21 <- filter(hospSP21, EVOLUCAO == '2')
obitosSP22 <- filter(hospSP22, EVOLUCAO == '2')

###################################################################
###################################################################
### agrupando dados por municipio

## data frama p/ municipios

## filtro, total de casos covid, em 2020

t1 <- as.data.frame(table(obitosSP20$CO_MUN_NOT))
t2 <- as.data.frame(table(obitosSP21$CO_MUN_NOT))
t3 <- as.data.frame(table(obitosSP22$CO_MUN_NOT))

## base de dados dos municipios de SP
library(readxl)

codigos <- read_excel("D:\\Mestrado\\1Sem_23\\Ferramentas computacionais\\projeto_final\\SRAG\\RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

## uniao das bases de acordo com as chaves do municipio

j1 <- merge(codigos, t1, by.x = "ibge6", by.y = "Var1", all.x = TRUE)
j2 <- merge(codigos, t2, by.x = "ibge6", by.y = "Var1", all.x = TRUE)
j3 <- merge(codigos, t3, by.x = "ibge6", by.y = "Var1", all.x = TRUE)


## colocar zero nas frequencias vazias

j1$Freq[is.na(j1$Freq)] <- 0
j2$Freq[is.na(j2$Freq)] <- 0
j3$Freq[is.na(j3$Freq)] <- 0

install.packages("clipr")
library(clipr)          ## copia e cola a tabela para o excel

write_clip(j1)
write_clip(j2)
write_clip(j3)
write_clip(mapa_muni)
################### ANÁLISE ESPACIAL #########
################### PLOTAGEM DO MAPA #########
## mapa para SRAG por COVID-19, óbitos por local de internação e residência

library(geobr)
library(ggplot2)
library(brazilmaps) #usar remotes::install_github("rpradosiqueira/brazilmaps")

mapa20 <- j1
mapa21 <- j2
mapa22 <- j3

## inclusao das taxas
## I) proporção de mortalidade hospitalar  por município de internação
## (obitos em internacoes / total de hospitalizacao)*100

total_hosp_20 <- nrow(hospSP20)
total_hosp_21 <- nrow(hospSP21)
total_hosp_22 <- nrow(hospSP22)

mapa20$tx_mor_local20 <- (mapa20$Freq/total_hosp_20)*100
mapa21$tx_mor_local21 <- (mapa21$Freq/total_hosp_21)*100
mapa22$tx_mor_local22 <- (mapa22$Freq/total_hosp_22)*100


#### ESTOU COM DÚVIDAS
## b)	proporção de mortalidade hospitalar  por município de residência
muni_res_20 <- as.data.frame(table(hospSP20$ID_MN_RESI))
muni_res_21 <- as.data.frame(table(hospSP21$ID_MN_RESI))
muni_res_22 <- as.data.frame(table(hospSP22$ID_MN_RESI))

mapa20$tx_mor_res_20 <- (mapa20$Freq/muni_res_20$Freq)*100
mapa21$tx_mor_res_21 <- (mapa21$Freq/muni_res_21$Freq)*100
mapa22$tx_mor_res_22 <- (mapa22$Freq/muni_res_22$Freq)*100


##### dados do território, plotagem do mapa

## filtro para apenas um estado, modificar pelo código número code_state
## pacote 'geobr', escolher se país, UF ou município

mapa_muni <- read_municipality(code_muni = "all", year=2010) %>% 
  filter(code_state == 35)   

ggplot(mapa_muni)+geom_sf(aes(fill=code_muni)) #padrão azul teste, teste

#### interpolação de dados, ano de 2020:

mapa20$code_muni <- as.double(mapa20$code_muni)



juntos <- full_join(mapa_muni, mapa20, by="code_muni")  #função join, união de variáveis

#retirando os NA, para municípios sem registros

juntos[is.na(juntos)] <- 0
View(juntos)

##teste com categoria, função cut,

juntos$tx_mor_local20 <- cut(juntos$tx_mor_local20,breaks=c(-Inf, 1, 5, 10, 20, 30, 40, 50, 60,
                                                     70, 80, 90, 95, 99, Inf), 
                      labels=c("0,00 a 0,9", "01,0 a 04,9", "05,0 a 09,9", "10,0 a 19,9","20,0 a 29,9","30,0 a 39,9",
                               "40,0 a 49,9","50,0 a 59,9", "60,0 a 69,9", "70,0 a 79,9",
                               "80,0 a 89,9", "90,0 a 94,9 ", "95,0 a 99,9", "100,00"))

## escala sem categoria, color
color <- colorRampPalette(c("white", "yellow", "orange","red", "darkred", "black"))
color(20)

### código padrão para mapa dos municípios


ggplot(juntos) + geom_sf(aes(fill=juntos$tx_mor_local20),           ## plotagem
                         colour = "gray", size = 0.1) +                ## linha dos municípios
  geom_sf(data = get_brmap("State", geo.filter = list(State = 35)), ## linha Estado
          fill = "transparent",
          colour = "black", size = 0.05)+
  scale_fill_gradientn(colours = color(20))+    #### escala de cor
  annotation_scale()+                           #### escala de tamanho
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering())+ ## seta direção
  labs(x=NULL, y=NULL, fill='[Letalidade\n Hospitalar - %]',        ## legenda
       title="TAXA DE MORTALIDADE HOSPITALAR POR SRAG POR COVID-19",
       subtitle = "SEGUNDO MUNICÍPIO DE INTERNAÇÃO, MARANHÃO, 2020",
       caption = "Prof. Flávio Maximino")+
  theme_classic() + theme(legend.position = c(0.9,0.2))             ## tema e posição





