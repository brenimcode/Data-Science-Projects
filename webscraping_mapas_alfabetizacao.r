# Web Scraping e Visualização de Mapas
# Descrição: Este script realiza web scraping para extrair dados de alfabetização de unidades federativas do Brasil e gera mapas interativos.

# Carregar as bibliotecas necessárias
library(rvest)    # Pacote para web scraping
library(geobr)    # Pacote para leitura de dados geográficos do Brasil
library(dplyr)    # Manipulação de dados
library(stringr)  # Manipulação de strings
library(ggplot2)  # Visualização de dados

# Definir a URL da página web que será raspada
url = "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetiza%C3%A7%C3%A3o"

# Ler o conteúdo HTML da página web
html = read_html(url)

# Selecionar todos os elementos <h1> da página HTML
html_elements = html_elements(html, "h1")

# Extrair e exibir o texto dos elementos <h1>
text_elements = html_text2(html_elements)
print(text_elements)

# Selecionar todas as tabelas da página HTML
tabelas = html |>                
  html_elements("table") |>        
  html_table()                     

# Acessar a terceira tabela extraída (taxa de alfabetização por estado)
alfabetizacao = tabelas[[3]]

# Selecionar as colunas de interesse e renomeá-las
alfabetizacao <- alfabetizacao[,c(2,3)]
names(alfabetizacao) <- c("estado", "taxa")

# Limpar os dados da coluna 'taxa'
parte1 <- str_replace_all(alfabetizacao$taxa, pattern = ",", replacement = ".")
parte2 <- str_replace_all(parte1, pattern = "%", replacement = "")
parte_final <- as.numeric(parte2)
parte_final = parte_final / 100

# Atualizar a coluna 'taxa' com os valores limpos
alfabetizacao$taxa = parte_final

# Carregar dados geográficos de Minas Gerais para visualização
minas = read_state(code_stat = "MG") 
ggplot(data = minas) + 
  geom_sf(fill = "red") +
  labs(title = "Mapa de Minas Gerais")

# Carregar dados dos municípios da Bahia
muni <- read_municipality(code_muni = "BA")

# Adicionar destaque para múltiplas cidades
cidades_destaque <- c("Xique-Xique", "Iuiú", "Salvador")  # Lista de cidades a destacar
muni$tipo <- ifelse(muni$name_muni %in% cidades_destaque, "destaque", "normal")

# Plotar o mapa com destaque
ggplot(data = muni) +
  geom_sf(aes(fill = tipo), color = "white") +
  scale_fill_manual(values = c("destaque" = "green", "normal" = "white")) +
  theme_dark() +
  labs(fill = "Destaque", title = "Cidades da Bahia em Destaque")

# Carregar e filtrar os estados do Brasil com taxa de alfabetização
estados <- read_state()

# Organizar os dados por nome do estado
estados <- estados[order(estados$name_state),]
alfabetizacao <- alfabetizacao[order(alfabetizacao$estado),]

# Mesclar os dados de alfabetização com os dados geográficos dos estados
estados$taxa <- alfabetizacao$taxa

# Plotar o mapa do Brasil com a taxa de alfabetização
ggplot(data = estados, aes(fill = taxa)) +
  geom_sf() +
  scale_fill_gradient(high = "#132B39", low = "#0910f9") +
  theme_void() +
  labs(title = "Taxa de Alfabetização por Estado no Brasil", fill = "Taxa de Alfabetização")
