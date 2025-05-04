library(readr)
library(dplyr)
library(kableExtra)

# Carregar os dados
cursos <- readr::read_csv("..\\Dados Lab1\\cursos-prouni.csv")
estados <- readr::read_csv("..\\Dados Lab1\\estados.csv")

# Combinar as tabelas
dados_combinados <- left_join(cursos, estados, by = c("uf_busca" = "subdivision"))

# Filtrar cursos de Ciência da Computação nas regiões Nordeste (NE) e Sudeste (SE)
dados_computacao <- dados_combinados %>%
  filter(region %in% c("NE", "SE") & 
           (grepl("Ciência da Computação", nome) | 
              grepl("Ciencias da Computacao", nome)))

# Calcular as mensalidades médias e medianas nas duas regiões
media_nordeste <- mean(dados_computacao$mensalidade[dados_computacao$region == "NE"], na.rm = TRUE)
mediana_nordeste <- median(dados_computacao$mensalidade[dados_computacao$region == "NE"], na.rm = TRUE)

media_sudeste <- mean(dados_computacao$mensalidade[dados_computacao$region == "SE"], na.rm = TRUE)
mediana_sudeste <- median(dados_computacao$mensalidade[dados_computacao$region == "SE"], na.rm = TRUE)

# Criar tabela com os resultados
tabela_resultados <- data.frame(
  Regiao = c("Nordeste", "Sudeste"),
  Media = c(media_nordeste, media_sudeste),
  Mediana = c(mediana_nordeste, mediana_sudeste)
)

# Criar a tabela de resultados com kableExtra
tabela_resultados %>%
  kable(format = "html", caption = "Mensalidades Médias e Medianadas nas Regiões Nordeste (NE) e Sudeste (SE)") %>%
  kable_styling(
    full_width = FALSE,   # Ajusta a tabela para não ocupar toda a largura da página
    position = "center",  # Centraliza a tabela
    font_size = 16,       # Aumenta o tamanho da fonte para melhorar a legibilidade
    bootstrap_options = c("striped", "hover", "condensed") # Estilos do Bootstrap
  ) %>%
  column_spec(1, bold = TRUE, color = "white", background = "#0073e6") %>%
  column_spec(2:3, bold = TRUE, color = "black", background = "#f2f2f2") %>%
  row_spec(0, bold = TRUE, font_size = 18, color = "black", background = "#e6e6e6") %>%
  row_spec(1:nrow(tabela_resultados), extra_css = "padding: 10px;")
