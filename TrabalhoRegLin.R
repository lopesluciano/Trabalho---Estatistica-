# Carregar os dados
dados <- read.csv("/home/luciano/Documents2/DadosEstatisca/heart_statlog_cleveland_hungary_final.csv")

# Visualizar os dados com um gráfico de dispersão
plot(dados$age, dados$max.heart.rate, main="Scatterplot de Age vs Max Heart Rate",
     xlab="Age", ylab="Max Heart Rate (s)", pch=19)

# Ajustar um modelo de regressão linear
modelo <- lm(max.heart.rate ~ age, data=dados)

# Resumir o modelo de regressão
summary_modelo <- summary(modelo)

# Exibir o resumo do modelo
print(summary_modelo)

# Plotar a linha de regressão
abline(modelo, col="blue")

# Analisar os resíduos do modelo
par(mfrow=c(2, 2))
#plot(modelo)

# Extrair os coeficientes alfa e beta
alfa <- summary_modelo$coefficients[1, 1]
beta <- summary_modelo$coefficients[2, 1]

