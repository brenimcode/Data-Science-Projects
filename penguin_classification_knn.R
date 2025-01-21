# Projeto: Classificação de Espécies de Pinguins usando KNN
# Descrição: Este script aplica o algoritmo KNN (K-Nearest Neighbors) para classificar espécies de pinguins, utilizando o conjunto de dados 'penguins' do pacote 'palmerpenguins'.
# O script realiza a divisão dos dados em conjuntos de treino e teste, aplica a padronização e avalia o modelo KNN para diferentes valores de 'k'.

# Carregar bibliotecas necessárias
library(ggplot2)          # Visualizações
library(palmerpenguins)   # Conjunto de dados dos pinguins
library(class)            # Funções para classificação, incluindo KNN

# Carregar e preparar os dados
data(penguins)            # Carregar dados dos pinguins
pinguins = penguins       # Renomear para 'pinguins'
str(pinguins)             # Ver estrutura dos dados

# Remover colunas irrelevantes e dados ausentes
pinguins = pinguins[, -c(2,7,8)]  # Remover colunas 'island', 'year', 'bill_depth_mm'
pinguins = na.omit(pinguins)  # Excluir linhas com NA (dados ausentes)

# Dividir dados em treino (80%) e teste (20%)
tam = nrow(pinguins)  # Número total de linhas
n = round(0.8 * tam)  # Definir 80% para o conjunto de treino
indices_treino = sample(1:tam, size = n, replace = FALSE)  # Índices para treino

# Definir conjuntos de treino e teste
treino = pinguins[indices_treino,]  # Conjunto de treino
teste = pinguins[-indices_treino, ] # Conjunto de teste

# Padronizar dados (colocar na mesma escala)
treino_padronizado = scale(treino[,-1])  # Excluir coluna de 'species' do treino para padronizar as variáveis
teste_padronizado = scale(teste[,-1])    # Excluir coluna de 'species' do teste para padronizar as variáveis

# Treinar e testar o modelo KNN com k=1
classe_treino = treino$species  # A variável alvo (espécies) para o treino
modelo1 = knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = 1)

# Avaliar a acurácia do modelo com k=1
acuracia1 = mean(modelo1 == teste$species)  # Calcula a acurácia comparando as predições com os valores reais
print(paste("Acurácia com k=1: ", acuracia1))
modelo1  # Exibir as predições do modelo com k=1

# Treinar e testar o modelo KNN com k=10
modelo2 = knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = 10)

# Avaliar a acurácia do modelo com k=10
acuracia2 = mean(modelo2 == teste$species)  # Calcula a acurácia para k=10
print(paste("Acurácia com k=10: ", acuracia2))

# Encontrar o melhor valor de k (k que maximiza a acurácia)
maior = 0          # Inicializa com 0 para encontrar a maior acurácia
indice = 0         # Inicializa o índice do melhor k

for(i in 1:274) {
  acuracia = mean(knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = i) == teste$species)
  
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

# Analisar a correlação entre as variáveis do conjunto de treino (excluindo a coluna 'species')
cor(treino[,-1])  # Excluir 'species' para verificar a correlação entre as variáveis numéricas

# Calcular e visualizar a acurácia para diferentes valores de k (1 a 10)
taxa <- c()  # Vetor para armazenar as acurácias para cada k

for(i in 1:10){
  modelo <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = i)
  taxa[i] <- mean(modelo == teste$species)  # Calcular a acurácia para cada k
}

# Criar um data frame para armazenar as taxas de acurácia
df <- data.frame(k = 1:10, taxa = taxa)

# Visualizar a acurácia para diferentes valores de k
ggplot(data = df, aes(x = k, y = taxa)) + 
  geom_line() +  # Linha para mostrar a variação da acurácia com k
  labs(title = "Acurácia do Modelo KNN para Diferentes Valores de k", 
       x = "Número de Vizinhos (k)", 
       y = "Acurácia") +
  theme_minimal()  # Tema limpo e minimalista
