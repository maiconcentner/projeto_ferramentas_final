# carregando pacotes
library(dplyr)
library(rstatix)
library(ggplot2)

# set da pasta de trabalho
setwd("D:\\Mestrado\\1Sem_23\\Ferramentas computacionais\\projeto_final\\base_dados")

#carregando base de dados

dados_covid_sp <- read.csv('dados_covid_sp.csv', sep = ";")
base_sp <- read.csv('sp.csv', sep = ";")
raca_obitos <- read.csv('casos_obitos_raca_cor.csv', sep = ";")
