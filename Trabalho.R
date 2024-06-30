# Leitura do CSV
data <- read.csv("/home/luciano/Documents2/DadosEstatisca/heart_statlog_cleveland_hungary_final.csv")
 
# Funcao para calcular medidas resumo
calculate_statistics <- function(data, column_name) {
  # Checar se a coluna existe
  if (column_name %in% colnames(data)) {
    # Extrair dados da coluna
    column_data <- data[[column_name]]
    
    # Checar se os dados da coluna sao numericos
    if (is.numeric(column_data)) {
      # Calcular a media da coluna
      average <- mean(column_data, na.rm = TRUE)
      
      # Calcular a variancia da coluna
      variance <- var(column_data, na.rm = TRUE)
      
      # Calcular outros valores estatisticos da coluna
      standard_deviation <- sd(column_data, na.rm = TRUE)
      median_value <- median(column_data, na.rm = TRUE)
      min_value <- min(column_data, na.rm = TRUE)
      max_value <- max(column_data, na.rm = TRUE)
      
      # Calcular modo
      mode_table <- table(column_data)
      mode_value <- as.numeric(names(mode_table)[which.max(mode_table)])
      
      # Calcular quantis
      quantiles <- quantile(column_data, probs = c(0.25, 0.5, 0.75))
      
      # Imprimir resultados
      cat("Summary Measures of", column_name, "\n")
      cat("Average of", column_name, ":", average, "\n")
      cat("Variance of", column_name, ":", variance, "\n")
      cat("Standard Deviation of", column_name, ":", standard_deviation, "\n")
      cat("Median of", column_name, ":", median_value, "\n")
      cat("Minimum value of", column_name, ":", min_value, "\n")
      cat("Maximum value of", column_name, ":", max_value, "\n")
      cat("Mode of", column_name, ":", mode_value, "\n")
      cat("Quantiles of", column_name, ":", quantiles, "\n")
    } else {
      cat("The specified column is not numeric.\n")
    }
  } else {
    cat("The specified column does not exist.\n")
  }
  cat("\n")
}

# Calculando as medidas resumo
calculate_statistics(data, "age")
calculate_statistics(data, "sex")
calculate_statistics(data, "chest.pain.type")
calculate_statistics(data, "resting.bp.s")
calculate_statistics(data, "cholesterol")
calculate_statistics(data, "fasting.blood.sugar")
calculate_statistics(data, "resting.ecg")
calculate_statistics(data, "max.heart.rate")
calculate_statistics(data, "exercise.angina")
calculate_statistics(data, "oldpeak")
calculate_statistics(data, "ST.slope")
calculate_statistics(data, "target")


# Imprimir os nomes das colunas do arquivo original
cat("Column Names:", colnames(data), "\n")

# Funcao para gerar tabela de frequencias 
generate_frequency_table <- function(data, column_name) {
  # Checar se a coluna existe
  if (column_name %in% colnames(data)) {
    # Extrair dados da coluna
    column_data <- data[[column_name]]
    
    # Gerar tabela de frequencias
    frequency_table <- table(column_data)
    
    # Calcular o valor total das frequencias
    total <- sum(frequency_table)
    
    # Calculate as frequencias relativas
    relative_freq <- prop.table(frequency_table)
    
    # Imprimir as frequencias totais e relativas
    cat("Frequency Table for", column_name, ":\n")
    print(frequency_table)
    cat("\nRelative Frequencies:\n")
    print(relative_freq)
    cat("\nTotal count:", total, "\n")
  } else {
    cat("The specified column does not exist.\n")
  }
  cat("\n")
}

# Dado Qualitativo:
generate_frequency_table(data, "chest.pain.type")

#Dado Quantitativo:
generate_frequency_table(data, "age")

# Funcao para gerar boxplot
generate_boxplot <- function(data, column_name) {
  # Checar se a coluna existe
  if (column_name %in% colnames(data)) {
    # Extrair os dados da coluna
    column_data <- data[[column_name]]

       # Criar pasta de de saida se ainda nao existir
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Criar o "Path" para o arquivo de imagem
    png_file <- file.path(output_folder, paste(column_name, ".png", sep = ""))
    
    # Criar um grafico boxplot e salvar como PNG
    png(png_file)
    boxplot(column_data, main = paste("Boxplot of", column_name), xlab = column_name)
    dev.off()
    
    cat("Boxplots saved as", png_file, "\n")
  } else {
    cat("The specified column does not exist.\n")
  }
}

# Exemplo de uso:
output_folder <- "boxplots_pngs"
generate_boxplot(data, "resting.bp.s")

# Funcao para gerar grafico pizza
generate_pie_chart <- function(data, column_name) {
  # Checar se a coluna existe
  if (column_name %in% colnames(data)) {
    # Extrair dados da coluna
    column_data <- data[[column_name]]
    
   # Criar pasta de saida se ainda nao existir
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Criar "Path" para salvar imagem
    png_file <- file.path(output_folder, paste(column_name, ".png", sep = ""))
    
    # Criar um grafico pizza e salvar como PNG
    png(png_file)
    pie(table(column_data), main = paste("Pie Chart of", column_name, xlab = column_name))
    dev.off()
    
    cat("Pie Chart saved as", png_file, "\n")
  } else {
    cat("The specified column does not exist.\n")
  }
}

# Exemplo de uso:
output_folder <- "pie_charts_pngs"
generate_pie_chart(data, "chest.pain.type")

#Funcao para gerar histograma
generate_histogram <- function(data, column_name, output_folder) {
  # Checar se coluna existe
  if (column_name %in% colnames(data)) {
    # Extrair dados da coluna
    column_data <- data[[column_name]]
    
    # Criar pasta de saida se nao existir
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Criar "path" para salvar a imagem
    png_file <- file.path(output_folder, paste(column_name, "_histogram.png", sep = ""))
    
    # Criar um histograma e salvar como PNG
    png(png_file)
    hist(column_data, main = paste("Histogram of", column_name), xlab = column_name)
    dev.off()
    
    cat("Histogram saved as", png_file, "\n")
  } else {
    cat("The specified column does not exist.\n")
  }
}

# Exemplo de uso:
output_folder <- "histogram_pngs"
generate_histogram(data, "resting.bp.s", output_folder)

# Funcao para gerar grafico de barras
generate_barplot <- function(data, column_name, output_folder) {
  # Checar se coluna existe
  if (column_name %in% colnames(data)) {
    # Extrair dados da coluna
    column_data <- data[[column_name]]
    
    # Criar pasta de saida se ainda nao existir
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Criar "Path" para salvar imagem
    png_file <- file.path(output_folder, paste(column_name, ".png", sep = ""))
    
    # Criar grafico de barras e salvar como PNG
    png(png_file)
    barplot(table(column_data), main = paste("Barplot of", column_name), xlab = column_name)
    dev.off()
    
    cat("Barplot saved as", png_file, "\n")
  } else {
    cat("The specified column does not exist.\n")
  }
}

# Exemplo de uso:
output_folder <- "barplot_pngs"
generate_barplot(data, "chest.pain.type", output_folder)
generate_barplot(data, "age", output_folder)


# Funcao para gerar grafico de dispersao e calcular covariancia e correlacao
generate_scatterplot <- function(data, x_column, y_column, output_folder) {
  # Checar se coluna existe
  if (x_column %in% colnames(data) && y_column %in% colnames(data)) {
    # Extrair dados da coluna
    x_data <- data[[x_column]]
    y_data <- data[[y_column]]
    
    # Criar pasta de saida se ainda nao existir
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Criar "Path" para salvar imagem
    png_file <- file.path(output_folder, paste(x_column, "_vs_", y_column, "_scatterplot.png", sep = ""))
    
    # Gerar o grafico de dispersao e salvar como PNG
    png(png_file)
    plot(x_data, y_data, main = paste("Scatter Plot of", x_column, "vs", y_column), 
         xlab = x_column, ylab = y_column)
    dev.off()
    
    cat("Scatter plot saved as", png_file, "\n")
    
    # Calcular correlacao e covariancia
    cat("\n")
    cat("Relationship Between Variables:\n")
    covariance <- cov(x_data, y_data)
    cat("Covariance between", x_column, "and", y_column, ":", covariance, "\n")  
    correlation <- cor(x_data, y_data)
    cat("Correlation between", x_column, "and", y_column, ":", correlation, "\n")
  } else {
    cat("One or both of the specified columns do not exist.\n")
  }
}

# Uso exemplo:
output_folder <- "scatterplot_pngs"
generate_scatterplot(data, "age", "max.heart.rate", output_folder)

generate_boxplot_age_vs_chestpain <- function(data, output_folder) {
  # Check if the columns exist
  if ("age" %in% colnames(data) && "chest.pain.type" %in% colnames(data)) {
    # Extract the column data
    age_data <- data[["age"]]
    chest_pain_data <- data[["chest.pain.type"]]
    
    # Create the output folder if it doesn't exist
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Set the file path for the PNG file
    png_file <- file.path(output_folder, "age_vs_chest_pain_boxplot.png")
    
    # Create the box plot and save it as PNG
    png(png_file)
    boxplot(age_data ~ chest_pain_data, main = "Boxplot of Age vs Chest Pain Type",
            xlab = "Chest Pain Type", ylab = "Age")
    dev.off()
    
    cat("Box plot saved as", png_file, "\n")
  } else {
    cat("One or both of the specified columns do not exist.\n")
  }
}

# Example usage:
output_folder <- "boxplot_pngs"
generate_boxplot_age_vs_chestpain(data, output_folder)
