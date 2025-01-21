# Análise dos dados sobre Harold Shipman, conhecido como "Doutor Morte". 
# Gráficos gerados para explorar as distribuições de gênero, idade, local e ano das mortes das vítimas. 
# As visualizações incluem gráficos de barras, pizza, histograma e boxplot.

library(ggplot2)

# Lendo os dados
dados <- read.csv("dados.txt", sep = ";")
dados$Genero <- as.factor(dados$Genero)

# Gráfico de barras para visualização da frequência por gênero
ggplot(data = dados, aes(x = Genero)) +
  geom_bar(fill = "blue") +
  labs(title = "Frequência por Gênero", x = "Gênero", y = "Frequência") +
  theme_minimal()

# Gráfico de pizza para visualização da distribuição de gênero
ggplot(data = dados, aes(x = "", fill = Genero)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribuição de Gêneros") +
  theme_void() +
  theme(legend.position = "right")

# Histograma da idade, separando por gênero
ggplot(dados, aes(x = Idade, fill = Genero)) +
  geom_histogram(bins = 8, position = "dodge", color = "black") +
  labs(title = "Distribuição de Idade por Gênero", x = "Idade", y = "Frequência") +
  theme_minimal()

# Boxplot para a distribuição da idade
ggplot(dados, aes(x = "", y = Idade)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot da Idade", x = "", y = "Idade") +
  theme_minimal()

# Analisando o local da morte
ggplot(dados, aes(x = LocalDaMorte, fill = LocalDaMorte)) +
  geom_bar(color = "black") +
  labs(title = "Distribuição dos Locais de Morte", x = "Local", y = "Frequência") +
  theme_minimal()

# Gráfico de barras para o ano de morte
ggplot(dados, aes(x = AnoDaMorte, fill = AnoDaMorte)) +
  geom_bar(color = "black") +
  labs(title = "Distribuição do Ano de Morte", x = "Ano da Morte", y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
