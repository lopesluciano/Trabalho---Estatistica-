# Passo 1: Carregar os dados
dados <- read.csv("/home/luciano/Documents2/DadosEstatisca/heart_statlog_cleveland_hungary_final.csv")

# Passo 2: Visualizar os dados com um gráfico de dispersão
plot(dados$age, dados$max.heart.rate, main="Scatterplot de Age vs Max Heart Rate",
     xlab="Age", ylab="Max Heart Rate (s)", pch=19)

# Passo 3: Ajustar um modelo de regressão linear
modelo <- lm(max.heart.rate ~ age, data=dados)

# Passo 4: Resumir o modelo de regressão
summary_modelo <- summary(modelo)

# Exibir o resumo do modelo
print(summary_modelo)

# Passo 5: Plotar a linha de regressão
abline(modelo, col="blue")

# Passo 6: Analisar os resíduos do modelo
par(mfrow=c(2, 2))
plot(modelo)

# Extrair os coeficientes alfa e beta
alfa <- summary_modelo$coefficients[1, 1]
beta <- summary_modelo$coefficients[2, 1]

cat("O valor do coeficiente alfa (intercepto) é:", alfa, "\n")
cat("O valor do coeficiente beta (coeficiente angular) é:", beta, "\n")
