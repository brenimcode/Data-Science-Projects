# aula11.R

# Web Scraping - Processo de extrair informações de páginas web

# Carregar bibliotecas necessárias
library(dplyr)
library(stringr)
library(rvest)  # Pacote para web scraping

# Definir a URL da página web que será raspada
url = "https://www.timeout.com/film/best-horror-films"

# Ler o conteúdo HTML da página
html = read_html(url)

# Selecionar os elementos com a classe "_zoneItems_882m9_1 zoneItems"
# Use o "." antes da classe para indicar que é uma classe CSS
zone_items = html_elements(html, "h3")

# Dentro dos elementos selecionados, buscar os elementos <h3>
nomes = html_text2(zone_items)

# Pegando os números de posição dos filmes
posicao = str_extract_all(string = nomes, pattern = "^\\d+")
posicao = as.numeric(posicao)

# Extraindo o nome dos filmes
# Remover números no início (por exemplo, "1." ou "2.") e o ano entre parênteses no final
title = str_remove_all(nomes, pattern = "^\\d+\\.\\s+")  # Remove a posição numérica no começo
title = str_remove_all(title, pattern = "\\s+\\(\\d+\\)$")  # Remove o ano entre parênteses no final
title

# Extraindo o ano do filme que está entre parênteses
anos = unlist(str_extract_all(nomes, pattern = "\\(\\d+\\)$"))
anos = unlist(str_extract_all(anos, "\\d+"))  # Extrai apenas o número do ano

# Criando o dataframe final
filmes_horror = data.frame(posicao, title, anos)

# Salvar o dataframe como um arquivo CSV
write.csv(filmes_horror, file = "filmes_horror.csv", row.names = FALSE)

