# Análise de sentimentos
# O script analisa as 10 palavras mais frequentes de um artigo e um livro,
# removendo as palavras de parada ("stop words"). Em seguida, realiza uma Análise de sentimentos.

# Carregar as bibliotecas necessárias
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rvest)
library(stopwords)

# URL do artigo da BBC
url = "https://www.bbc.com/portuguese/articles/c5ywpr8kjvko"

# Ler o conteúdo HTML da página
html = read_html(url)

# Extrair o texto dos parágrafos com a classe 'bbc-hhl7in'
texto = html |> html_elements('p.bbc-hhl7in') |> html_text2() |> paste()

# Concatenar o texto em uma única string
texto = paste(texto, collapse = " ")

# Criar um data frame com o texto
conjunto = data.frame(texto) 

# Tokenizar o texto e mostrar as 10 palavras mais frequentes
conjunto |> unnest_tokens(output = word, input = texto) |>
  count(word, sort = TRUE) |> top_n(10)

# Carregar palavras de parada em português
palavras = data.frame(word = stopwords(language = 'pt'))

# Tokenizar o texto, remover as palavras de parada e mostrar as 10 palavras mais frequentes
conjunto |> unnest_tokens(output = word, input = texto) |>
  anti_join(palavras) |>
  count(word, sort = TRUE) |> top_n(10)

# Visualizar as 10 palavras mais frequentes sem as palavras de parada
conjunto |> unnest_tokens(output = word,input = texto) |> anti_join(palavras) |> 
  count(word, sort=TRUE) |> top_n(10) |>
  mutate(word=reorder(word,n)) |> 
  ggplot(aes(y = word, x = n)) + 
  geom_col(fill='orange') + theme_minimal()

# Carregar o pacote para análise do livro
library(janeaustenr)

# Acessar o texto do livro "Orgulho e Preconceito"
livro = prideprejudice
livro = data.frame(texto = livro)

# Tokenizar o texto do livro e mostrar as 10 palavras mais frequentes
livro |> unnest_tokens(output = word,input = texto) |>
  count(word, sort=TRUE) |> top_n(10)

# Carregar palavras de parada em inglês
palavras2 = data.frame(word = stopwords(language = 'en'))

# Tokenizar o texto do livro, remover as palavras de parada e mostrar as 10 palavras mais frequentes
livro |> unnest_tokens(output = word,input = texto) |>
  anti_join(palavras2) |>
  count(word, sort=TRUE) |>
  top_n(10) |>
  mutate(word=reorder(word,n)) |> 
  ggplot(aes(y = word, x = n)) + 
  geom_col(fill='red') + theme_minimal()

# Análise de sentimentos "BING"
sentimentos = get_sentiments("bing")

# Detectar capítulos no livro
library(stringr)
capitulos = str_detect(livro$texto, "^Chapter \\d+")
capitulos = cumsum(capitulos)
capitulos

# Analisar sentimentos por capítulo
livro |> mutate(capitulo = capitulos) |>
  unnest_tokens(word, texto) |>
  inner_join(sentimentos) |>
  count(capitulo, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulo, y = total)) + 
  geom_col() + theme_minimal()

