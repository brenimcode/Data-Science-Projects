# Projeto: Classificação do Conjunto de Dados Iris
# Descrição: Este script realiza uma análise exploratória e classificação do conjunto de dados Iris. O dataset contém medidas de flores de três espécies de Iris: Setosa, Versicolor e Virginica.
# O código divide o conjunto de dados em treino e teste, realiza algumas visualizações e implementa uma abordagem de classificação com base nas características das pétalas.

# Carregar o conjunto de dados Iris
data(iris)

# Exibir o conjunto de dados em uma janela de visualização (ideal para ambientes como RStudio)
View(iris)

# Passo 1 - Dividir em conjunto de treino e teste
# Definir uma semente para garantir que a amostragem aleatória seja reprodutível
set.seed(1711)

# Obter o número total de linhas no conjunto de dados
qtd <- nrow(iris)

# Embaralhar aleatoriamente as linhas do conjunto de dados
iris <- iris[sample(qtd), ]

# Definir o número de amostras para o conjunto de treino (80% do total)
n <- round(0.8 * qtd)

# Criar o conjunto de treino com as primeiras 'n' linhas
treino <- iris[1:n, ]
teste <- iris[-(1:n), ]

# Visualizações:

# Gráfico de barras para a distribuição de espécies no conjunto de treino
ggplot(data = treino, mapping = aes(x = Species)) +
  geom_bar() + 
  theme_classic()

# Histograma da variável Petal.Length no conjunto de treino
ggplot(data = treino, mapping = aes(x = Petal.Length)) +
  geom_histogram(bins = 100, fill = "red") + 
  theme_classic()

# Boxplot da variável Petal.Length para cada espécie
ggplot(data = treino, mapping = aes(y = Petal.Length)) +
  geom_boxplot(fill = "yellow") + 
  facet_wrap(~Species)

# Dispersão entre Petal.Length e Petal.Width para cada espécie
ggplot(data = treino, mapping = aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +  # Ajusta o tamanho e transparência dos pontos
  facet_wrap(~Species) +  # Cria painéis separados por espécie
  labs(
    title = "Distribuição do Comprimento e Largura das Pétalas por Espécie",
    x = "Comprimento da Pétala (cm)",
    y = "Largura da Pétala (cm)",
    color = "Espécie"
  ) + 
  theme_minimal() +  # Tema minimalista para o gráfico
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgrey"),
    strip.background = element_rect(fill = "lightblue"),
    strip.text = element_text(size = 12, face = "bold"),
    plot.background = element_rect(fill = "gray")
  ) +
  scale_color_manual(values = c("setosa" = "red", "versicolor" = "blue", "virginica" = "green"))

# Passo 2 - Classificação (Modelo de Classificação Simples)

# Inicializar um vetor para armazenar os resultados da classificação
resultados <- c()

# Classificação simples com base no comprimento e largura da pétala
for(i in 1:nrow(teste)){
  
  if(teste$Petal.Length[i] < 2.5){
    resultados[i] <- "setosa"
  } else if(teste$Petal.Width[i] < 1.75){
    resultados[i] <- "versicolor"
  } else {
    resultados[i] <- "virginica"
  }
}

# Calcular a acurácia do modelo comparando as previsões com os valores reais
acuracia <- mean(teste$Species == resultados)
print(paste("Acurácia do modelo: ", acuracia))
