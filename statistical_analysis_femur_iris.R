# Descrição: Análise de correlação entre variáveis do conjunto de dados `iris` e `femur.csv`.
# O código inclui gráficos de dispersão, cálculo de correlação e construção de um modelo de regressão linear.

# Carregar pacotes necessários
library(ggplot2)

# Analisando o conjunto de dados iris
data(iris)

# Criação de gráfico de dispersão entre comprimento e largura das pétalas
ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point()

# Cálculo da correlação entre o comprimento e a largura das pétalas
cor(iris$Petal.Length, iris$Petal.Width)

# Cálculo da matriz de correlação entre todas as variáveis numéricas
cor(iris[,-5])

# Filtrando dados para a espécie 'setosa' e calculando a correlação
setosa = iris[iris$Species == "setosa", ]
cor(setosa$Petal.Length, setosa$Petal.Width)

# Carregar e filtrar o conjunto de dados 'femur.csv'
femur = read.csv("femur.csv")
male = femur[femur$genero == 'Male', ]
female = femur[femur$genero == 'Female', ]

# Cálculo da correlação entre altura e comprimento do fêmur para homens e mulheres
cor(male$altura, male$femur)
mean(male$altura)
cor(female$altura, female$femur)

# Gráfico de dispersão entre comprimento do fêmur e altura para indivíduos masculinos
ggplot(data = male, aes(x = femur, y = altura)) + 
  geom_point()

# Ajuste de um modelo de regressão linear para prever a altura com base no comprimento do fêmur
modelo_linear = lm(data = male, formula = altura ~ femur)

# Exibição do modelo de regressão linear
modelo_linear
summary(male$femur)
