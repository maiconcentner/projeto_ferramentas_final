# carregando pacotes
library(dplyr)
library(rstatix)
library(ggplot2)

cores <- rcartocolor::carto_pal(12, "Bold")
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

# Total de casos por ano/mes acumulado
dados_covid_sp_tratado$ano_mes <- format(dados_covid_sp_tratado$datahora, "%Y-%m")

casos_mes_ano <- dados_covid_sp_tratado %>% 
  group_by(datahora) %>% 
  summarise(total_casos = sum(casos))

# plot do gráfico: total de casos acumulado
casos_mes_ano %>% 
  ggplot(aes(x = datahora, y = total_casos)) +
  geom_density(fill = cores[8], stat = "identity", alpha = 0.3) +
  scale_color_manual(values = cores) +
  theme_bw()+
  theme(panel.grid = element_blank(),  # Remove as linhas de grade
        plot.background = element_rect(fill = "white")) +
  labs(x = "Ano/mês", y = "Total de casos", title = "Total de casos no estado de São Paulo")

# Total de novos casos por dia
novos_casos_mes_ano <- dados_covid_sp_tratado %>% 
  group_by(datahora) %>% 
  summarise(novos_casos = sum(casos_novos))

novos_casos_mes_ano %>% 
  ggplot(aes(x = datahora, y = novos_casos)) +
  geom_density(fill = cores[1], stat = "identity", alpha = 0.3) +
  scale_color_manual(values = cores) +
  theme_bw()+
  theme(panel.grid = element_blank(),  # Remove as linhas de grade
        plot.background = element_rect(fill = "white")) + 
  labs(x = "Ano/mês", y = "Total de casos", title = "Novos casos por dia no estado de São Paulo")



