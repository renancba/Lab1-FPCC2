# -*- coding: UTF-8 -*-
# Carregar as bibliotecas necessárias
library(dplyr)
library(readr)
library(ggplot2)
library(kableExtra)
library(stringr)

# Carregar os dados
cursos <- readr::read_csv("..\\Dados Lab1\\cursos-prouni.csv")
estados <- readr::read_csv("..\\Dados Lab1\\estados.csv")

# Filtrar apenas os cursos com grau "Tecnológico" e que sejam da Paraíba (uf_busca == 'PB')
cursos_paraiba <- cursos %>%
  filter(uf_busca == "PB"& str_detect(grau, "Tecno"))  

cursos %>%
  distinct(grau)
# Calcular o total de bolsas de cada tipo para os cursos filtrados, substituindo NA por 0
cursos_paraiba_bolsas <- cursos_paraiba %>%
  select(nome, universidade_nome, bolsa_integral_ampla, bolsa_parcial_ampla, bolsa_integral_cotas, bolsa_parcial_cotas) %>%
  mutate(
    bolsa_integral_ampla = ifelse(is.na(bolsa_integral_ampla), 0, bolsa_integral_ampla),
    bolsa_parcial_ampla = ifelse(is.na(bolsa_parcial_ampla), 0, bolsa_parcial_ampla),
    bolsa_integral_cotas = ifelse(is.na(bolsa_integral_cotas), 0, bolsa_integral_cotas),
    bolsa_parcial_cotas = ifelse(is.na(bolsa_parcial_cotas), 0, bolsa_parcial_cotas),
    total_bolsas = bolsa_integral_ampla + bolsa_parcial_ampla + bolsa_integral_cotas + bolsa_parcial_cotas
  )

# Classificar os cursos pelo total de bolsas e pegar os 5 primeiros
cursos_paraiba_bolsas <- cursos_paraiba_bolsas %>%
  arrange(desc(total_bolsas)) %>%
  head(5)  # Seleciona os 5 primeiros cursos com maior número de bolsas

# Limpar e truncar nomes
cursos_paraiba_bolsas$nome <- str_replace_all(cursos_paraiba_bolsas$nome, "[\r\n]", " ")
cursos_paraiba_bolsas$nome <- str_squish(cursos_paraiba_bolsas$nome)
cursos_paraiba_bolsas$nome_truncado <- str_trunc(cursos_paraiba_bolsas$nome, 40)

# Exibir a tabela com os 5 cursos que oferecem mais bolsas, incluindo o nome da instituição
cursos_paraiba_bolsas %>%
  select(nome, universidade_nome, total_bolsas) %>%
  kable(
    format = "html", 
    caption = "Top 5 Cursos Tecnologicos da Paraiba com Maior Quantidade de Bolsas de Estudo"
  ) %>%
  kable_styling(
    full_width = FALSE, 
    position = "center", 
    font_size = 16, 
    bootstrap_options = c("striped", "hover", "condensed")
  )

# Gráfico 
ggplot(cursos_paraiba_bolsas, aes(x = reorder(nome_truncado, total_bolsas), y = total_bolsas, fill = nome_truncado)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Top 5 Cursos Tecnologicos com Mais Bolsas na Paraiba",
    x = "Curso",
    y = "Total de Bolsas"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  scale_fill_brewer(palette = "Paired")
