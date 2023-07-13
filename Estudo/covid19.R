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
cores <- rcartocolor::carto_pal(6, "SunsetDark")


# carregando base de dados

dados_covid_sp <- read.csv('dados/dados_covid_sp.csv', sep = ";")

# ordenando os dados por ordem alfabética de município

dados_ordem <- dados_covid_sp[order(dados_covid_sp$nome_munic), ]

# convertendo a variável datahora para o formato de data

dados_ordem <- as.Date(dados_covid_sp$datahora)

# Definindo as datas de inicío e fim do intervalo anual para 
## 2020
data_inicio_2020 <- as.Date("2020-02-25")
data_final_2020 <-as.Date("2020-12-31")

## 2021
data_inicio_2021 <- as.Date("2021-01-01")
data_final_2021 <- as.Date("2021-12-31")

## 2022
data_inicio_2022 <- as.Date("2022-01-01")
data_final_2022 <- as.Date("2022-12-31")


# Somando os casos de covid por município
## 2020
soma_casos_2020 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2020 & datahora <= data_final_2020) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sum(casos_novos))

## 2021
soma_casos_2021 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2021 & datahora <= data_final_2021) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sum(casos_novos))


## 2022
soma_casos_2022 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2022 & datahora <= data_final_2022) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sum(casos_novos))


# Carregando o mapa dos municípios de São Paulo por meio da bilbioteca geobr

all_mun_sp <- read_municipality(code_muni = 35, year = 2010)

# juntando as tabelas, obtendo um dataset final para plotar o gráfico
## 2020
dataset_final_2020 <- left_join(all_mun_sp, soma_casos_2020, by = c("code_muni" = "codigo_ibge"))
min(dataset_final_2020$total_casos_novos)
max(dataset_final_2020$total_casos_novos)

## 2021
dataset_final_2021 <- left_join(all_mun_sp, soma_casos_2021, by = c("code_muni" = "codigo_ibge"))
min(dataset_final_2021$total_casos_novos)
max(dataset_final_2021$total_casos_novos)

## 2022
dataset_final_2022 <- left_join(all_mun_sp, soma_casos_2022, by = c("code_muni" = "codigo_ibge"))
min(dataset_final_2022$total_casos_novos)
max(dataset_final_2022$total_casos_novos)



# Plotagem dos graficos 
## 2020
ggplot()+
  geom_sf(data = dataset_final_2020, aes(fill = total_casos_novos), color = NA, size = .15)+
  geom_sf(data = dataset_final_2020, color = "gray", fill = NA, size = 0.1) +  ## adição da linha cinza
  labs(title = "Casos de COVID-19 em 2020 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0, 600000),
                       name="Total", labels = label_number(big.mark=".", accuracy = 1))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))

## 2021
ggplot()+
  geom_sf(data = dataset_final_2021, aes(fill = total_casos_novos), color = NA, size = .15)+
  geom_sf(data = dataset_final_2021, color = "gray", fill = NA, size = 0.1) +  ## adição da linha cinza
  labs(title = "Casos de COVID-19 em 2021 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0, 600000),
                       name="Total", labels = label_number(big.mark=".", accuracy = 1))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))

## 2022

ggplot()+
  geom_sf(data = dataset_final_2022, aes(fill = total_casos_novos), color = NA, size = .15)+
  geom_sf(data = dataset_final_2022, color = "gray", fill = NA, size = 0.1) +  ## adição da linha cinza
  labs(title = "Casos de COVID-19 em 2022 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0, 600000),
                       name="Total", labels = label_number(big.mark=".", accuracy = 1))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))

# Analise do acumulado dos anos 2020, 2021 e 2022

## Mapa de calor dos casos acumulados até 31/12/2022

data_final_acumulado_2022 <- as.Date("2022-12-31")

casos_acumulados <- dados_covid_sp %>%
  filter(datahora <= data_final_acumulado_2022) %>% 
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_acumulado = sum(casos_novos))

dataset_final_acumulado <- left_join(all_mun_sp, casos_acumulados, by = c("code_muni" = "codigo_ibge"))
min(dataset_final_acumulado$total_acumulado)
max(dataset_final_acumulado$total_acumulado)

ggplot()+
  geom_sf(data = dataset_final_acumulado, aes(fill = total_acumulado), color = NA, size = 0.15)+
  geom_sf(data = dataset_final_acumulado, color = "gray", fill = NA, size = 0.1) +  ## adição da linha cinza
  labs(title = "Casos de COVID-19 acumulado dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0, 1200000),
                      name = "Total", labels = label_number(big.mark = ".", accuracy = 1))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))


## Análise de dados da taxa de contaminacao por ano por municipio

### 2020
soma_taxa_2020 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2020 & datahora <= data_final_2020) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sum(casos_novos/pop))

dataset_final_taxa_2020 <- left_join(all_mun_sp, soma_taxa_2020, by = c("code_muni" = "codigo_ibge"))
min(dataset_final_taxa_2020$total_casos_novos)
max(dataset_final_taxa_2020$total_casos_novos)

### 2021
soma_taxa_2021 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2021 & datahora <= data_final_2021) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sum(casos_novos/pop))

dataset_final_taxa_2021 <- left_join(all_mun_sp, soma_taxa_2021, by = c("code_muni" = "codigo_ibge"))
min(dataset_final_taxa_2021$total_casos_novos)
max(dataset_final_taxa_2021$total_casos_novos)

### 2022
soma_taxa_2022 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2022 & datahora <= data_final_2022) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sum(casos_novos/pop))

dataset_final_taxa_2022 <- left_join(all_mun_sp, soma_taxa_2022, by = c("code_muni" = "codigo_ibge"))
min(dataset_final_taxa_2022$total_casos_novos)
max(dataset_final_taxa_2022$total_casos_novos)

### Acumulado

soma_taxa_acumulado <- dados_covid_sp %>% 
  filter(datahora <= data_final_acumulado_2022) %>% 
  group_by(nome_munic, codigo_ibge) %>% 
  summarise(total_casos_novos = sum(casos_novos/pop))

dataset_final_taxa_acumulado <- left_join(all_mun_sp, soma_taxa_acumulado, by = c("code_muni" = "codigo_ibge"))
min(dataset_final_taxa_acumulado$total_casos_novos)
max(dataset_final_taxa_acumulado$total_casos_novos)

## Mapas de calor dos dados anteriores

## 2020
ggplot()+
  geom_sf(data = dataset_final_taxa_2020, aes(fill = total_casos_novos), color = NA, size = .15)+
  geom_sf(data = dataset_final_taxa_2020, color = "gray", fill = NA, size = 0.1) +  ## adição da linha cinza
  labs(title = "Taxa de contaminação de COVID-19 em 2020 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0.00, 0.40),
                       name="Total", labels = label_number(big.mark="."))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))

## 2021
ggplot()+
  geom_sf(data = dataset_final_taxa_2021, aes(fill = total_casos_novos), color = NA, size = .15)+
  geom_sf(data = dataset_final_taxa_2021, color = "gray", fill = NA, size = 0.1) +  ## adição da linha cinza
  labs(title = "Taxa de contaminção de COVID-19 em 2021 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0.00, 0.40),
                       name="Total", labels = label_number(big.mark="."))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))

## 2022
ggplot()+
  geom_sf(data = dataset_final_taxa_2022, aes(fill = total_casos_novos), color = NA, size = .15)+
  geom_sf(data = dataset_final_taxa_2022, color = "gray", fill = NA, size = 0.1) +  ## adição da linha cinza
  labs(title = "Taxa de contaminação de COVID-19 em 2022 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0.00, 0.40),
                       name="Total", labels = label_number(big.mark="."))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))

## Acumulado
ggplot()+
  geom_sf(data = dataset_final_taxa_acumulado, aes(fill = total_casos_novos), color = NA, size = .15)+
  geom_sf(data = dataset_final_taxa_acumulado, color = "gray", fill = NA, size = 0.1) +  ## adição da linha cinza
  labs(title = "Taxa de contaminação acumulada de COVID-19 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0.00, 0.50),
                       name="Total", labels = label_number(big.mark="."))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))


# Agora, vamos elaborar os mapas de calor do coeficiente de variação (cv) da taxa de contaminhação
# da covid-19 por Município de SP. Isso é importante para avaliarmos quanto a taxa de contaminação
# variou ao decorrer do ano. Se o cv for baixo, significa que a taxa de variação é um dado homogêneo,
# ou seja, não alterou muito, caso contrário, houve um aumento significativo da taxa de contaminação 
# no município. Com isso, conseguimos rastrear como foi o progresso da contaminação no estado de SP.

## Filtrando o dataset

### 2020
cv_taxa_casos_2020 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2020 & datahora <= data_final_2020) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sd(casos_novos/pop)/mean(casos_novos/pop))

dataset_cv_taxa_final_2020 <- left_join(all_mun_sp, cv_taxa_casos_2020, by = c("code_muni" = "codigo_ibge"))
min(dataset_cv_taxa_final_2020$total_casos_novos)
max(dataset_cv_taxa_final_2020$total_casos_novos)

### 2021
cv_taxa_casos_2021 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2021 & datahora <= data_final_2021) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sd(casos_novos/pop)/mean(casos_novos/pop))

dataset_cv_taxa_final_2021 <- left_join(all_mun_sp, cv_taxa_casos_2021, by = c("code_muni" = "codigo_ibge"))
min(dataset_cv_taxa_final_2021$total_casos_novos)
max(dataset_cv_taxa_final_2021$total_casos_novos)

### 2022
cv_taxa_casos_2022 <- dados_covid_sp %>%
  filter(datahora >= data_inicio_2022 & datahora <= data_final_2022) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sd(casos_novos/pop)/mean(casos_novos/pop))

dataset_cv_taxa_final_2022 <- left_join(all_mun_sp, cv_taxa_casos_2022, by = c("code_muni" = "codigo_ibge"))
min(dataset_cv_taxa_final_2022$total_casos_novos)
max(dataset_cv_taxa_final_2022$total_casos_novos)

### Acumulado
cv_taxa_acumulado_casos_2022 <- dados_covid_sp %>%
  filter(datahora <= data_final_2022) %>%
  group_by(nome_munic, codigo_ibge) %>%
  summarise(total_casos_novos = sd(casos_novos/pop)/mean(casos_novos/pop))

dataset_cv_taxa_acumulado_final_2022 <- left_join(all_mun_sp, cv_taxa_acumulado_casos_2022, by = c("code_muni" = "codigo_ibge"))
min(dataset_cv_taxa_acumulado_final_2022$total_casos_novos)
max(dataset_cv_taxa_acumulado_final_2022$total_casos_novos)

## Gráficos:

## 2020
ggplot()+
  geom_sf(data = dataset_cv_taxa_final_2021, aes(fill = total_casos_novos), color = NA, size = .15)+
  labs(title = "Coeficiente de variação da taxa de contaminação de COVID-19 em 2020 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0.00, 20.00),
                       name="Total", labels = label_number(big.mark="."))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))

## 2021
ggplot()+
  geom_sf(data = dataset_cv_taxa_final_2021, aes(fill = total_casos_novos), color = NA, size = .15)+
  labs(title = "Coeficiente de variação da taxa de contaminção de COVID-19 em 2021 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0.00, 20.00),
                       name="Total", labels = label_number(big.mark="."))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))

## 2022
ggplot()+
  geom_sf(data = dataset_cv_taxa_final_2022, aes(fill = total_casos_novos), color = NA, size = .15)+
  labs(title = "Coeficiente de variação da taxa de contaminação de COVID-19 em 2022 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0.00, 20.00),
                       name="Total", labels = label_number(big.mark="."))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))

## Acumulado
ggplot()+
  geom_sf(data = dataset_cv_taxa_acumulado_final_2022, aes(fill = total_casos_novos), color = NA, size = .15)+
  labs(title = "Coeficiente de variação da taxa de contaminação acumulada de COVID-19 dos Municípios de SP",
       caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_gradientn(colours = cores, limits = c(0.00, 40.00),
                       name="Total", labels = label_number(big.mark="."))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))


