# carregando pacotes
library(dplyr)
library(rstatix)
library(ggplot2)


cores <- rcartocolor::carto_pal(12, "Bold")
# set da pasta de trabalho
#<<<<<<< HEAD
# setwd("D:\\Mestrado\\1Sem_23\\Ferramentas computacionais\\projeto_final\\base_dados")
# (fer) comentei a linha anterior linha para me facilitar na hora de rodar o script
# =======

#setwd("C:/Users/W10/aula do banana/trabalho/projeto_ferramentas_final/base_dados")
#setwd("D:\\Mestrado\\1Sem_23\\Ferramentas computacionais\\projeto_final\base_dados")
#>>>>>>> 249e7523d2f96a87f44753923ea0afd0f98f32bb

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
  ggplot(aes(x = datahora, y = total_casos * 1e-6)) +
  geom_density(fill = cores[8], stat = "identity", alpha = 0.3) +
  scale_color_manual(values = cores) +
  theme ()+
  theme(panel.grid = element_blank(),  # Remove as linhas de grade
        plot.background = element_rect(fill = "white")) +
  labs(x = "Ano",
       y = expression(paste("Total de Casos (", 10^6, ")")),
       title = "Evolução dos Casos no Estado de São Paulo") +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))





# Total de novos casos por dia
novos_casos_mes_ano <- dados_covid_sp_tratado %>% 
  group_by(datahora) %>% 
  summarise(novos_casos = sum(casos_novos))


#   novos_casos_mes_ano %>%
#   ggplot(aes(x = datahora, y = novos_casos *1e-3)) +
#   geom_line(color = cores[1], size = 0.5) +
#   scale_color_manual(values = cores) +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),
#         plot.background = element_rect(fill = "white")) +
#   labs(x = "Ano/Mês",
#        y = "Novos Casos (x 10³)",
#        title = "Novos Casos por Dia no Estado de São Paulo")

novos_casos_mes_ano %>%
  mutate(datahora = as.POSIXct(datahora)) %>%
  ggplot(aes(x = datahora, y = novos_casos *1e-3)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_minimal() +
  labs(x = "Ano/Mês", y = "Novos Casos (x 10³)", title = "Novos Casos por Dia no Estado de São Paulo") +
  scale_x_datetime(date_labels = "%Y-%m", date_breaks = "6 month")


