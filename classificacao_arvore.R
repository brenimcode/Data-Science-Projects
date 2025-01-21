# Projeto - Classificação com Árvores de Decisão e Florestas Aleatórias
# Descrição: Este script treina modelos de árvore de decisão (rpart) e floresta aleatória (randomForest) 
# para classificar diagnósticos de câncer em benignos ou malignos a partir de um conjunto de dados.

# Carregar as bibliotecas necessárias
library(ggplot2)        # Para visualizações
library(rpart)          # Para o modelo de árvore de decisão
library(rpart.plot)     # Para visualização das árvores
library(randomForest)   # Para o modelo de floresta aleatória

# Converter a variável 'diagnosis' para fator, pois é a variável de classificação
dados$diagnosis = as.factor(dados$diagnosis)

# Definir o tamanho do conjunto de treinamento (80% dos dados)
n = round(0.8 * nrow(dados))  # 80% do total de dados

# Definir a semente para garantir reprodutibilidade dos resultados
set.seed(1)

# Amostrar índices para o conjunto de treinamento
indices_treino = sample(1:nrow(dados), size = n, replace = FALSE)

# Criar os conjuntos de treinamento e teste com base nos índices amostrados
treino = dados[indices_treino,]  # Conjunto de treino
teste = dados[-indices_treino,]  # Conjunto de teste

# Treinar o modelo de árvore de decisão (rpart) para prever 'diagnosis'
arvore = rpart(formula = diagnosis ~ ., data = treino, method = "class")

# Fazer previsões com a árvore de decisão no conjunto de teste
previsao_arvore = predict(arvore, newdata = teste, type = "class")

# Avaliar a acurácia do modelo de árvore de decisão
acuracia_arvore = mean(previsao_arvore == teste$diagnosis)
print(paste("Acurácia da Árvore de Decisão: ", acuracia_arvore))

# Treinar o modelo de floresta aleatória (randomForest) com 100 árvores
floresta = randomForest(formula = diagnosis ~ ., data = treino, ntree = 100)

# Fazer previsões com o modelo de floresta aleatória no conjunto de teste
previsao_floresta = predict(floresta, newdata = teste, type = "class")

# Exibir as previsões feitas pela floresta aleatória
print(previsao_floresta)

# Avaliar a acurácia do modelo de floresta aleatória
acuracia_floresta = mean(previsao_floresta == teste$diagnosis)
print(paste("Acurácia da Floresta Aleatória: ", acuracia_floresta))

