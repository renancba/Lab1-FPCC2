# Carregar as bibliotecas necessárias
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(kableExtra)
library(knitr)

# Carregar os dados
cursos <- readr::read_csv(normalizePath("..\\Dados Lab1\\cursos-prouni.csv"))
estados <- readr::read_csv(normalizePath("..\\Dados Lab1\\estados.csv"))

# Combinar as tabelas
dados_combinados <- left_join(cursos, estados, by = c("uf_busca" = "subdivision"))
cursos_interesse_tec <- dados_combinados %>%
  filter(nome %in% c("Ciência da Computação", "Engenharia da Computação", "Sistemas de Informação")) %>%
  filter(str_detect(grau, "Tecnológico", negate = FALSE))

# Filtrar os cursos de interesse
dados_filtros <- dados_combinados %>%
  filter(nome %in% c("Ciência da Computação", "Engenharia da Computação", "Sistemas de Informação"))

# Calcular o total de bolsas por curso e turno
dados_bolsas_turno <- dados_filtros %>%
  mutate(total_bolsas = rowSums(across(c(bolsa_integral_cotas, bolsa_integral_ampla, 
                                         bolsa_parcial_cotas, bolsa_parcial_ampla)), na.rm = TRUE)) %>%
  group_by(nome, turno) %>%
  summarise(total_bolsas_turno = sum(total_bolsas, na.rm = TRUE), .groups = "drop")

# Calcular o total de bolsas por curso (para calcular o percentual)
total_por_curso <- dados_bolsas_turno %>%
  group_by(nome) %>%
  summarise(total_bolsas_curso = sum(total_bolsas_turno), .groups = "drop")

# Calcular o percentual por turno/modalidade
dados_percentuais <- dados_bolsas_turno %>%
  left_join(total_por_curso, by = "nome") %>%
  mutate(percentual = (total_bolsas_turno / total_bolsas_curso) * 100)

# Exibir a tabela
dados_percentuais %>%
  select(Curso = nome, Modalidade = turno, `Percentual de Bolsas (%)` = percentual) %>%
  arrange(Curso, desc(`Percentual de Bolsas (%)`)) %>%
  kable(format = "html", caption = "Percentual de Bolsas por Turno/Modalidade para Cada Curso") %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 14,
    bootstrap_options = c("striped", "hover", "condensed")
  )

# Gráfico de barras
ggplot(dados_percentuais, aes(x = turno, y = percentual, fill = turno)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ nome) +
  labs(
    title = "Percentual de Bolsas por Turno para Cada Curso",
    x = "Turno (Modalidade)",
    y = "Percentual de Bolsas (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")