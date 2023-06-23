# carregando pacotes
library(dplyr)
library(rstatix)
library(ggplot2)

# set da pasta de trabalho
setwd("D:\\Mestrado\\1Sem_23\\Ferramentas computacionais\\projeto_final\\base_dados")

# carregando base de dados

dados_covid_sp <- read.csv('dados_covid_sp.csv', sep = ";")
base_sp <- read.csv('sp.csv', sep = ";")

# Tratamento da base de dados
glimpse(dados_covid_sp)
dados_covid_sp$datahora <- as.Date(dados_covid_sp$datahora,
                                  format = '%Y-%m-%d') #convertendo datahora para data
glimpse(dados_covid_sp)

# excluindo colunas que não iremos usar
dados_covid_sp_tratado <- select(dados_covid_sp, -c(2,6,9))

# verificando valores vazios
dados_covid_sp_tratado %>% 
  sapply(function(x) sum(is.na(x))) # valores NA

dados_covid_sp_tratado %>% 
  sapply(function(x) sum(is.nan(x))) # valores NAN

# Total de casos por ano/mes
dados_covid_sp_tratado$ano_mes <- format(dados_covid_sp_tratado$datahora, "%Y-%m")

casos_mes_ano <- dados_covid_sp_tratado %>% 
  group_by(ano_mes) %>% 
  summarise(total_casos = sum(casos))

# plot do gráfico
ggplot(casos_mes_ano, aes(x = ano_mes, y = total_casos)) +
  geom_point() +
  labs(x = "Mês/ano", y = "Total de casos", title = "Total de casos no estado de São Paulo")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



