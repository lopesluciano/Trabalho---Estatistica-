# Passo 1: Carregar os dados
dados <- read.csv("/home/luciano/Documents2/DadosEstatisca/heart_statlog_cleveland_hungary_final.csv")

# Supondo que a coluna de dados seja chamada 'age'
idades <- dados$age

# Criar um fator para as regiões
regioes <- factor(rep(c("Cleveland", "Hungarian", "Switzerland", "Long Beach VA", "Irvine"), each = 238))

# Combinar em um dataframe
df <- data.frame(age = idades, regiao = regioes)

# Realizar a ANOVA
anova_resultado <- aov(age ~ regiao, data = df)

# Exibir o resumo da ANOVA
summary(anova_resultado)

# Exibir o resumo da ANOVA usando print
print(summary(anova_resultado))

