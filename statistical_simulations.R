# Projeto: Simulação e Cálculos Estatísticos com R
# Descrição: Este script contém simulações e cálculos aplicados a problemas clássicos de probabilidade e estatísticas, incluindo o problema dos aniversários, cálculo de probabilidade em uma sala com N alunos e simulação de sorteios da Mega-Sena.

# 1. Aniversários: Simulação do problema clássico de probabilidade de coincidência de aniversários
# 2. Função de Cálculo de Probabilidade: Função para calcular a probabilidade de coincidência de aniversários em uma sala com N alunos
# 3. Mega-Sena: Simulação para calcular quantas semanas seriam necessárias para acertar 4 números na Mega-Sena

# Exemplo simples de vetores e gráfico de barras
a <- c(1, 2, 5)
b <- c(10, 12)
d <- c(a, 6)  # Concatenar 'a' com o número 6
print(d)
barplot(d)

# Laços FOR: Exemplo de laços aninhados
vetor <- c(8, 9, 1)
tam <- length(vetor)
vetor[1]

# Laço duplo que não tem operação relevante, mas simula iterações
for(i in 1:tam) {
  for(j in 1:tam) {
    # O loop interno está vazio, pois não há operação aqui
  }
  print(i)
}

# Simulação de coincidência de aniversários
# Gerar 40 aniversários aleatórios e contar as coincidências
aniversarios <- sample(x = 1:365, size = 40, replace = TRUE)
quantidade <- sum(duplicated(aniversarios))  # Contar coincidências de aniversários
print("Quantidade de coincidências: ")
print(quantidade)

# Simulação com 10.000 repetições para calcular a probabilidade de coincidência
resultados <- c()  # Vetor vazio para armazenar resultados

for(j in 1:10000) {
  aniversarios <- sample(x = 1:365, size = 90, replace = TRUE)
  resultados[j] <- any(duplicated(aniversarios))  # Se houver duplicados, armazena TRUE
}
# Plotar a média das ocorrências de coincidência
plot(mean(resultados))

# Função para calcular a probabilidade de coincidência em uma sala com N alunos
calcular_prob <- function(n) {
  resultados <- c()  # Vetor vazio para armazenar resultados
  
  for(j in 1:10000) {
    aniversarios <- sample(x = 1:365, size = n, replace = TRUE)
    resultados[j] <- any(duplicated(aniversarios))  # Verifica se há aniversários duplicados
  }
  return(mean(resultados))  # Retorna a probabilidade média
}

# Calcular a probabilidade para uma sala com 50 alunos
prob_50 <- calcular_prob(n = 50)
print(paste("Probabilidade de coincidência para 50 alunos: ", prob_50))

# Simulação da Mega-Sena: Quantas semanas para acertar 4 números
bilhete <- c(4, 5, 12, 43, 21, 34)  # Números do bilhete

semanas <- 0  # Inicializar contagem de semanas
acertos <- 0  # Inicializar contagem de acertos

# Continuar sorteando até acertar 4 números
while(acertos < 4) {
  sorteio <- sample(x = 1:60, size = 6, replace = FALSE)  # Sorteio da Mega-Sena
  acertos <- sum(bilhete %in% sorteio)  # Verificar quantos números do bilhete estão no sorteio
  semanas <- semanas + 1  # Incrementar o número de semanas
}

# Exibir quantidade de semanas (convertido para anos)
print(paste("Quantidade de anos para acertar 4 números: ", semanas / 52))
pie(semanas / 52)  # Exibir gráfico de pizza com a quantidade de anos
