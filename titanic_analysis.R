# Projeto: Análise do Conjunto de Dados Titanic
# Descrição: Este script realiza uma análise exploratória no conjunto de dados Titanic, 
# com foco na sobrevivência dos passageiros com base em características como sexo e classe.
# 
# 1. Leitura e limpeza dos dados
# 2. Análise descritiva e visualização da sobrevivência por sexo
# 3. Análise da quantidade de homens sobreviventes na 3ª classe

# Leitura dos dados
titanic <- read.table(file = "titanic.txt", header = TRUE, sep = ",")
# Remover colunas desnecessárias (1, 9-12) e transformar variáveis em fator
titanic <- titanic[,-c(1,9:12)] 
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)

# Exibir a estrutura dos dados
str(titanic)

# Resumo estatístico dos dados
summary(titanic)

# Análise dos homens
homens <- titanic[titanic$Sex == "male",]   # Subconjunto de homens
summary(homens)                            # Resumo dos dados dos homens
table(homens$Survived)                     # Contagem de sobreviventes e não sobreviventes
barplot(table(homens$Survived), main = "Sobrevivência entre Homens", col = "lightblue")

# Quantidade de homens sobreviventes
print("Homens sobreviventes:")
print(sum(homens$Survived == 1))

# Análise das mulheres
muie <- titanic[titanic$Sex == "female",]  # Subconjunto de mulheres
summary(muie)                             # Resumo dos dados das mulheres
table(muie$Survived)                      # Contagem de sobreviventes e não sobreviventes
barplot(table(muie$Survived), main = "Sobrevivência entre Mulheres", col = "lightgreen")

# Análise de homens da 3ª classe
homem3classe <- homens[homens$Pclass == 3,] # Homens da 3ª classe
print("Homens sobreviventes da 3ª classe:")
print(sum(homem3classe$Survived == 1))

# Visualizações com ggplot2
# Gráfico de barras da sobrevivência
ggplot(data = titanic, aes(x = Survived)) +
  geom_bar(color = "green", fill = "red") +
  theme_classic()

# Gráfico de barras da sobrevivência por sexo
ggplot(data = titanic, aes(x = Survived, fill = Sex)) +
  geom_bar() +
  theme_classic()

# Gráfico com cores personalizadas para o sexo
ggplot(data = titanic, aes(x = Survived, fill = Sex)) +
  geom_bar() +
  scale_fill_manual(values = c("green", "blue")) +  # Cores personalizadas para Sex
  theme_classic()
