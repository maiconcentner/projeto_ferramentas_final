# Carregando pacotes necessários

library(dplyr)
library(rstatix)
library(ggplot2)
library(geobr)
library(sf)
library(dplyr)
library(rio)
library(readr)

# carregando base de dados

dados_covid_sp <- read.csv('dados/dados_covid_sp.csv', sep = ";")

# ordenando os dados por ordem alfabética de município

dados_ordem <- dados_covid_sp[order(dados_covid_sp$nome_munic), ]

# convertendo a variável datahora para o formato de data

dados_ordem <- as.Date(dados_covid_sp$datahora)

# Definindo as datas de inicío e fim do intervalo anual para 2020

data_inicio_2020 <- as.Date("2020-02-25")
data_final_2020 <-as.Date("2020-12-31") 

dados_2020_filtrados <- subset(dados_covid_sp, datahora >= data_inicio_2020 & datahora <= data_final_2020)

# Somando os casos de covid em 2020 por município

soma_casos_2020 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2020 & datahora <= data_final_2020) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sum(casos_novos))

# Carregando o mapa dos municípios de São Paulo por meio da bilbioteca geobr

all_mun_sp <- read_municipality(code_muni = 35, year = 2010)

# juntando as tabelas, obtendo um dataset final de 2020 para plotar o gráfico

dataset_final_2020 <- left_join(all_mun_sp, soma_casos_2020, by = c("code_muni" = "codigo_ibge"))
min(dataset_final_2020$total_casos_novos)
max(dataset_final_2020$total_casos_novos)

ggplot()+
  geom_sf(data = dataset_final_2020, aes(fill = total_casos_novos), color = NA, size = .15)+
  labs(title = "Casos de COVID-19 em 2020 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_distiller(palette = "Reds", limits = c(1, 500000),
                       name="Total")+
  theme(plot.title = element_text(hjust = 0.5))
