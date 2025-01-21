# Carregar bibliotecas necessárias
library(httr)
library(dplyr)
library(jsonlite)

# URL da API
url <- "https://blaze.ac/api/roulette_games/history?startDate=2024-08-14T16:42:29.179Z&endDate=2024-09-13T16:42:29.179Z&page=1"

# Fazer a requisição GET
resposta <- GET(url)

# Converter o conteúdo da resposta para texto JSON
dados <- content(resposta, as = "text")

# Converter o texto JSON em um data frame R
dados_json <- fromJSON(dados)

# Verificar os dados
dados_json$records$color  # Acessar as cores dos registros

# Criar a tabela de frequência das cores
table(dados_json$records$color)


# Requisição para obter dados de múltiplas páginas
url_base = "https://blaze.ac/api/roulette_games/history?startDate=2022-09-15T16:42:29.179Z&endDate=2022-10-13T16:42:29.179Z&page="

resultados = c()

for(i in 1:50) {
  url = paste0(url_base, i)
  
  dados <- content(resposta, as = "text")
  dados_json <- fromJSON(dados)
  resultados = c(resultados, dados_json$records$color)
}

# Exibir as proporções das cores
resultados
prop.table(table(resultados))


# ================================================================
# ------------------------- Raspando o letras ---------------------
# ================================================================

library(rvest)
library(dplyr)
library(ggplot2)
library(tidytext)

# URL da página de letras da música
url = "https://www.letras.mus.br/grelo-da-seresta/so-fe/"

# Ler conteúdo HTML da página
html = read_html(url)

# Extrair o título da música
html |>
  html_elements("h1") |>
  html_text2()

# Extrair a letra da música contida na classe .lyric-original
musica <- html_elements(html, ".lyric-original p")

# Limpar o texto extraído dos elementos <p>
musica <- html_text2(musica)

# Concatenar o texto da letra em uma única string
musica = paste(collapse = " ", musica)

# Criar dataframe com a letra da música
musica = data.frame(musica)

# Análise de palavras frequentes na letra da música
musica |> unnest_tokens(output = word, input = musica) |>
  count(word, sort = TRUE) |>
  head(n = 10) |>
  ggplot(aes(y = word, x = n)) +
  geom_col()
