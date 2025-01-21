# Projeto: Classificação de Diagnósticos de Câncer com KNN
# Descrição: Este script utiliza o algoritmo KNN (K-Nearest Neighbors) para classificar diagnósticos de câncer (benigno ou maligno)
# baseado no conjunto de dados 'cancer.csv'. O modelo é avaliado utilizando diferentes valores de k e a melhor acurácia é identificada.

# Carregar bibliotecas necessárias
library(class)     # Função para o algoritmo KNN
library(ggplot2)   # Para visualização

# Carregar o conjunto de dados
dados <- read.csv("cancer.csv", header = TRUE, sep = ",")
str(dados)  # Verificar a estrutura dos dados

# Dividir os dados em conjuntos de treino e teste (80% treino, 20% teste)
tam = nrow(dados)  # Número total de amostras
n = round(0.8 * tam)  # 80% para treino
indices_treino = sample(1:tam, size = n, replace = FALSE)  # Índices para treino

# Criar conjuntos de treino e teste
treino = dados[indices_treino,]  # Conjunto de treino
teste = dados[-indices_treino, ] # Conjunto de teste

# Padronizar dados (colocar as variáveis na mesma escala)
treino_padronizado = scale(treino[,-1])  # Exclui a coluna 'diagnosis' (variável alvo) ao padronizar
teste_padronizado = scale(teste[,-1])    # O mesmo para o conjunto de teste

# Variável alvo
classe_treino = treino$diagnosis
classe_teste = teste$diagnosis

# Aplicar o modelo KNN com k=5
modelo1 = knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = 5)

# Avaliar a acurácia do modelo KNN com k=5
acuracia1 = mean(modelo1 == teste$diagnosis)  # Comparar as predições com os valores reais
print(paste("Acurácia com k=5: ", acuracia1))

# Testar diferentes valores de k e avaliar o desempenho do modelo
taxa <- c()  # Vetor para armazenar as taxas de acurácia para diferentes k
for(i in 1:10) {
  modelo <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = i)
  taxa[i] <- mean(modelo == teste$diagnosis)  # Calcular a acurácia para cada k
}

# Criar um data frame com os resultados para plotar
df <- data.frame(k = 1:10, taxa = taxa)

# Visualizar a acurácia para diferentes valores de k
ggplot(data = df, aes(x = k, y = taxa)) + 
  geom_line() +  # Gráfico de linha para mostrar a variação da acurácia com k
  labs(title = "Acurácia do Modelo KNN para Diferentes Valores de k", 
       x = "Número de Vizinhos (k)", 
       y = "Acurácia") +
  theme_minimal()  # Tema limpo e minimalista

# Encontrar o melhor valor de k (k que maximiza a acurácia)
maior = 0          # Inicializa com 0 para encontrar a maior acurácia
indice = 0         # Inicializa o índice do melhor k

# Loop para avaliar a acurácia para diferentes valores de k
for(i in 1:100) {
  acuracia = mean(knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = i) == teste$diagnosis)
  
  # Atualiza se a acurácia atual for maior que a anterior
  if(acuracia > maior) {
    maior = acuracia
    indice = i
  }
}

# Exibir o melhor k e sua acurácia
print("Melhor k:")
print(indice)
print("Valor da maior acurácia:")
print(maior)

# Criar uma tabela de confusão para comparar as predições com os valores reais
table(modelo1, classe_teste)  # Confusion matrix (modelo vs. classe real)
