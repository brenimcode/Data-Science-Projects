# Descrição: Análise de sentimentos no livro "Emma" de Jane Austen, removendo stop words e 
# calculando o sentimento por capítulo. O script também realiza essa análise para todos os livros de Jane Austen.

# Carregar as bibliotecas necessárias
library(tidytext)  # Para processamento de texto
library(dplyr)     # Para manipulação de data frames
library(ggplot2)   # Para visualização de dados
library(tidyr)     # Para reorganização de dados
library(rvest)     # Para web scraping
library(stopwords) # Para lista de stopwords
library(janeaustenr) # Para acessar os livros de Jane Austen
library(stringr)   # Para manipulação de strings

# Obtém todos os livros de Jane Austen em um data frame
livros = austen_books()

# Exibe os títulos únicos dos livros disponíveis
unique(livros$book)

# Cria data frame a partir do vetor de stopwords em inglês para junções
stopwords_en = data.frame(word = stopwords('en'))

# Filtra os dados para o livro "Emma", transforma o texto em palavras,
# remove as stopwords, conta as palavras e retorna as 10 mais frequentes
resultados = livros |> 
  filter(book == "Emma") |>
  unnest_tokens(output = word, input = text) |>  # Divide o texto em palavras
  anti_join(stopwords_en, by = "word") |> # Remove stopwords
  count(word, sort = TRUE) |>
  top_n(10)                           

print(resultados)

# Filtra os livros para obter apenas os capítulos do livro "Emma"
emma = livros |> filter(book == "Emma") 

# Detecta a presença de capítulos no texto usando uma expressão regular, identifica todas as possíveis variações de algarismos romanos
capitulos = str_detect(emma$text, "CHAPTER [IVXLCDM]+")

# Calcula a soma acumulada dos capítulos detectados
capitulos = cumsum(capitulos)

# Adiciona uma nova coluna 'capitulos' ao data frame 'emma' conforme a soma acumulada
emma$capitulos = capitulos 

# Exibe as primeiras 20 linhas do data frame 'emma'
print(emma, n = 20)

# Exibe a estrutura do data frame 'emma' para verificar suas colunas e tipos
str(emma)

# Análise de sentimentos por capítulo usando o dicionário "bing"
emma |>
  unnest_tokens(output = word, input = text) |>
  anti_join(stopwords_en) |>
  inner_join(get_sentiments("bing"), relationship = "many-to-many") |>
  count(capitulos, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulos, y = total)) + geom_col()

# Realiza a análise de sentimentos para todos os livros de Jane Austen
livros |> group_by(book) |>
  mutate(capitulos = cumsum(str_detect(text, regex("^chapter (\\d|[IVXLCDM])+", ignore_case = TRUE)))) |>
  ungroup() |>
  unnest_tokens(output = word, input = text) |>
  anti_join(stopwords_en) |>
  inner_join(get_sentiments("bing"), relationship = "many-to-many") |>
  count(book, capitulos, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulos, y = total)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book, scales = "free_x")
