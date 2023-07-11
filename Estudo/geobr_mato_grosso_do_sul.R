# O pacote geobr tem as rotinas para o download dos mapas com as divisões
# territoriais variadas. O exemplo a seguir será feito a partir do mapa do
# estado de Mato Grosso do Sul, com a malha estrutural municipal de 2018, 
# que contém 79 municípios. 




# carregando os pacotes necessários para o exemplo
library(geobr)
library(ggplot2)
library(sf)
library(dplyr)
library(rio)
library(readr)

# A função list_geobr() mostra todos os datasets disponíveis no pkg geobr

datasets <- list_geobr()

# Agora, vamos baixar os dados de município para o Estado de MS utilizando a
# a função read_munipality()

mun_ms <- read_municipality(
  code_muni = "MS",
  year = 2018
)

# Como baixamos as informações sobre o estado de MS, podemos plotar um gráfico
plot(mun_ms)

# Podemos verificar quais são as classes da variável mun_ms utilizando o pacote
# sf e a função class()

class(mun_ms)

# sf é uma classe do qual carrega atributos "numéricos" e geométricos para 
# desenhar mapas.


library("basedosdados")
query <- "SELECT * FROM `basedosdados.br_bd_diretorios_brasil.municipio`"
dir <- tempdir("")
data <- download(query, "<PATH>")