# Carregar os dados
dados <- read.csv("/home/luciano/Documents2/DadosEstatisca/heart_statlog_cleveland_hungary_final.csv")

# Verificar as primeiras linhas do dataframe
print(head(dados))

# Criar uma tabela de contingência
tabela_contingencia <- table(dados$resting.ecg, dados$chest.pain.type)

# Exibir a tabela de contingência
print(tabela_contingencia)

# Realizar o teste qui-quadrado
resultado_chi2 <- chisq.test(tabela_contingencia)

# Exibir os resultados do teste
print(resultado_chi2)
