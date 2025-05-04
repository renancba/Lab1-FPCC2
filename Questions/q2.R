# Carregar as bibliotecas necessárias
library(dplyr)
library(readr)
library(kableExtra)
library(ggplot2)

# Carregar os dados
cursos <- readr::read_csv("..\\Dados Lab1\\cursos-prouni.csv")
estados <- readr::read_csv("..\\Dados Lab1\\estados.csv")

# Combinar as tabelas
dados_combinados <- left_join(cursos, estados, by = c("uf_busca" = "subdivision"))

# Calcular o total de bolsas para cada curso
dados_combinados <- dados_combinados %>%
  mutate(total_bolsas = bolsa_integral_cotas + bolsa_integral_ampla + bolsa_parcial_cotas + bolsa_parcial_ampla)

# Calcular a média de mensalidade por curso
dados_combinados <- dados_combinados %>%
  group_by(nome) %>%
  mutate(media_mensalidade = mean(mensalidade, na.rm = TRUE))

# Calcular a razão média mensalidade por bolsa
dados_combinados <- dados_combinados %>%
  mutate(mensalidade_por_bolsa = media_mensalidade / total_bolsas)

# Ordenar os cursos pela razão média mensalidade por bolsa de forma decrescente
top10_cursos_caro_por_bolsa <- dados_combinados %>%
  arrange(desc(mensalidade_por_bolsa)) %>%  # Ordenar em ordem decrescente
  distinct(nome, .keep_all = TRUE) %>%
  head(10) %>%
  select(nome, media_mensalidade, total_bolsas, mensalidade_por_bolsa)

# Visualizar os resultados com uma tabela formatada
top10_cursos_caro_por_bolsa %>%
  kable(format = "html", caption = "Top 10 Cursos Mais Caros em Relação à Média de Mensalidade por Bolsa") %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 16,
    bootstrap_options = c("striped", "hover", "condensed")
  ) %>%
  column_spec(1, bold = TRUE, color = "white", background = "#0073e6") %>%
  column_spec(2:4, bold = TRUE, color = "black", background = "#f2f2f2") %>%
  row_spec(0, bold = TRUE, font_size = 18, color = "black", background = "#e6e6e6") %>%
  row_spec(1:nrow(top10_cursos_caro_por_bolsa), extra_css = "padding: 10px;")

# Gerar gráfico de barras para visualizar a razão de mensalidade por bolsa
ggplot(top10_cursos_caro_por_bolsa, aes(x = reorder(nome, mensalidade_por_bolsa), y = mensalidade_por_bolsa, fill = nome)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +  # Inverte os eixos para tornar os nomes dos cursos mais legíveis
  labs(title = "Top 10 Cursos Mais Caros em Relação à Média de Mensalidade por Bolsa",
       x = "Curso",
       y = "Razão de Mensalidade por Bolsa") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), axis.title = element_text(size = 12))
