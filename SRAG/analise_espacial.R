setwd("D:\\Mestrado\\1Sem_23\\Ferramentas computacionais\\projeto_final\\base_dados\\SRAG")

#importação e instalação, caso seja a primeira vez no pc

library(writexl)
library(descr)
library(rstatix)
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, psych, descr, e1071, dplyr, tidyverse, geobr, raster,
               ggspatial, fields, sf, brazilmaps, readxl, MASS, knitr,
               data.table, lubridate, surveillance, gridExtra, grid, ggpubr,
               httr, rvest, readxl, gganimate)

## inserindo dados, base dos dados do SIVEP/Gripe

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
  BR21 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2021/INFLUD21-29-08-2022.csv")

  ## filtro para variáveis de interesse da pesquisa
  var.21 <- BR21[,c("DT_NOTIFIC", "SEM_NOT", "ID_MUNICIP", "CO_MUN_NOT", "SG_UF_NOT",
                    "ID_REGIONA", "ID_UNIDADE", "NU_IDADE_N", "CS_SEXO", "CS_RACA", 
                    "ID_MN_RESI", "CO_MUN_RES", "CS_ZONA", "HOSPITAL", "EVOLUCAO",
                    "FATOR_RISC", "UTI", "SUPORT_VEN", "CLASSI_FIN")]
  
  ## filtro para as notificações do estado de São Paulo
  
  SP21 <- filter(var.21, SG_UF_NOT == 'SP')
  
#### 2022         
  ## base excel e online, atualizada
  BR22 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2022/INFLUD22-28-11-2022.csv")
  
  
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


write.table(SP20a22, file= "sivepSP20a22.csv", sep=";", dec=",")
write.table(SP22, file= "sivepSP22.csv", sep=";", dec=",")
write.table(SP21, file= "sivepSP21.csv", sep=";", dec=",")
write.table(SP20, file= "sivepSP20.csv", sep=";", dec=",")

##########################################
## análise descritiva, início para análise
## dados gerais para sivep/gripe

sivepSP <- read.csv("sivepSP20a22.csv", header = TRUE, sep = ";", dec = ",")
siveSP20 <- read.csv("sivepSP20.csv", header = TRUE, sep = ";", dec = ",")
siveSP21 <- read.csv("sivepSP21.csv", header = TRUE, sep = ";", dec = ",")
siveSP22 <- read.csv("sivepSP22.csv", header = TRUE, sep = ";", dec = ",")
  

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
  
siveSP20$NU_IDADE_N %>% summary()
siveSP20$NU_IDADE_N %>% boxplot()

siveSP21$NU_IDADE_N %>% summary()
siveSP21$NU_IDADE_N %>% boxplot()

## raça
table(siveSP20$CS_RACA)
table(siveSP21$CS_RACA)
table(siveSP22$CS_RACA)
  # 4 == pardo, 9 == não informado