# Carregando pacotes necessários

library(dplyr)
library(rstatix)
library(ggplot2)
library(geobr)
library(sf)
library(dplyr)
library(rio)
library(readr)
library(scales)


# carregando base de dados

dados_covid_sp <- read.csv('dados/dados_covid_sp.csv', sep = ";")

# ordenando os dados por ordem alfabética de município

dados_ordem <- dados_covid_sp[order(dados_covid_sp$nome_munic), ]

# convertendo a variável datahora para o formato de data

dados_ordem <- as.Date(dados_covid_sp$datahora)

# Definindo as datas de inicío e fim do intervalo anual para 2020

data_inicio_2020 <- as.Date("2020-02-25")
data_final_2020 <-as.Date("2020-12-31") 

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

# Fazendo os mesmos passsos anteriores para os anos de 2021 e 2022

## 2021

  data_inicio_2021 <- as.Date("2021-01-01")
  data_final_2021 <- as.Date("2021-12-31")

  soma_casos_2021 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2021 & datahora <= data_final_2021) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sum(casos_novos))

  dataset_final_2021 <- left_join(all_mun_sp, soma_casos_2021, by = c("code_muni" = "codigo_ibge"))
  min(dataset_final_2021$total_casos_novos)
  max(dataset_final_2021$total_casos_novos)


## 2022
  data_inicio_2022 <- as.Date("2022-01-01")
  data_final_2022 <- as.Date("2022-12-31")

  soma_casos_2022 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2022 & datahora <= data_final_2022) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sum(casos_novos))

  dataset_final_2022 <- left_join(all_mun_sp, soma_casos_2022, by = c("code_muni" = "codigo_ibge"))
  min(dataset_final_2022$total_casos_novos)
  max(dataset_final_2022$total_casos_novos)



## graficos 
  cores <- rcartocolor::carto_pal(6, "SunsetDark")
 
## 2020
ggplot()+
  geom_sf(data = dataset_final_2020, aes(fill = total_casos_novos), color = NA, size = .15)+
  labs(title = "Casos de COVID-19 em 2020 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(1, 600000),
                       name="Total", labels = label_number(big.mark=".", accuracy = 1))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  theme_void() #deixar fundo branco

## 2021

ggplot()+
  geom_sf(data = dataset_final_2021, aes(fill = total_casos_novos), color = NA, size = .15)+
  labs(title = "Casos de COVID-19 em 2021 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0, 600000),
                       name="Total", labels = label_number(big.mark=".", accuracy = 1))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  theme_void() #deixar fundo branco

## 2022

ggplot()+
  geom_sf(data = dataset_final_2022, aes(fill = total_casos_novos), color = NA, size = .15)+
  labs(title = "Casos de COVID-19 em 2022 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0, 600000),
                       name="Total", labels = label_number(big.mark=".", accuracy = 1))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  theme_void() #deixar fundo branco

### Precisamos ajustar as legendas nos gráficos