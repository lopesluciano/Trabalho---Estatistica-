# Read the CSV file into a data frame
data <- read.csv("/home/luciano/Documents2/DadosEstatisca/heart_statlog_cleveland_hungary_final.csv")
 
#install.packages("DescTools")


calculate_statistics <- function(data, column_name) {
  # Check if the column exists
  if (column_name %in% colnames(data)) {
    # Extract the column data
    column_data <- data[[column_name]]
    
    # Check if the column data is numeric
    if (is.numeric(column_data)) {
      # Calculate the average of the column
      average <- mean(column_data, na.rm = TRUE)
      
      # Calculate the variance of the column
      variance <- var(column_data, na.rm = TRUE)
      
      # Calculate other statistical features of the column
      standard_deviation <- sd(column_data, na.rm = TRUE)
      median_value <- median(column_data, na.rm = TRUE)
      min_value <- min(column_data, na.rm = TRUE)
      max_value <- max(column_data, na.rm = TRUE)
      
      # Calculate mode
      mode_table <- table(column_data)
      mode_value <- as.numeric(names(mode_table)[which.max(mode_table)])
      
      # Calculate quantiles
      quantiles <- quantile(column_data, probs = c(0.25, 0.5, 0.75))
      
      # Print out the results
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


# Test the function with the data
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


# Print out the column names of the original dataset
cat("Column Names:", colnames(data), "\n")


generate_frequency_table <- function(data, column_name) {
  # Check if the column exists
  if (column_name %in% colnames(data)) {
    # Extract the column data
    column_data <- data[[column_name]]
    
    # Generate frequency table
    frequency_table <- table(column_data)
    
    # Calculate total count
    total <- sum(frequency_table)
    
    # Calculate relative frequencies
    relative_freq <- prop.table(frequency_table)
    
    # Print out the frequency table with relative frequencies and total
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

generate_boxplot <- function(data, column_name) {
  # Check if the column exists
  if (column_name %in% colnames(data)) {
    # Extract the column data
    column_data <- data[[column_name]]

       # Create the output folder if it doesn't exist
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Set the file path for the PNG file
    png_file <- file.path(output_folder, paste(column_name, ".png", sep = ""))
    
    # Create a pie chart and save it as PNG
    png(png_file)
    boxplot(column_data, main = paste("Boxplot of", column_name), xlab = column_name)
    dev.off()
    
    cat("Boxplots saved as", png_file, "\n")
  } else {
    cat("The specified column does not exist.\n")
  }
}

# Example usage:
output_folder <- "boxplots_pngs"
generate_boxplot(data, "resting.bp.s")

generate_pie_chart <- function(data, column_name) {
  # Check if the column exists
  if (column_name %in% colnames(data)) {
    # Extract the column data
    column_data <- data[[column_name]]
    
   # Create the output folder if it doesn't exist
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Set the file path for the PNG file
    png_file <- file.path(output_folder, paste(column_name, ".png", sep = ""))
    
    # Create a pie chart and save it as PNG
    png(png_file)
    pie(table(column_data), main = paste("Pie Chart of", column_name, xlab = column_name))
    dev.off()
    
    cat("Pie Chart saved as", png_file, "\n")
  } else {
    cat("The specified column does not exist.\n")
  }
}

# Example usage:
output_folder <- "pie_charts_pngs"
generate_pie_chart(data, "chest.pain.type")

generate_histogram <- function(data, column_name, output_folder) {
  # Check if the column exists
  if (column_name %in% colnames(data)) {
    # Extract the column data
    column_data <- data[[column_name]]
    
    # Create the output folder if it doesn't exist
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Set the file path for the PNG file
    png_file <- file.path(output_folder, paste(column_name, "_histogram.png", sep = ""))
    
    # Create a histogram and save it as PNG
    png(png_file)
    hist(column_data, main = paste("Histogram of", column_name), xlab = column_name)
    dev.off()
    
    cat("Histogram saved as", png_file, "\n")
  } else {
    cat("The specified column does not exist.\n")
  }
}

# Example usage:
output_folder <- "histogram_pngs"
generate_histogram(data, "resting.bp.s", output_folder)


generate_barplot <- function(data, column_name, output_folder) {
  # Check if the column exists
  if (column_name %in% colnames(data)) {
    # Extract the column data
    column_data <- data[[column_name]]
    
    # Create the output folder if it doesn't exist
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Set the file path for the PNG file
    png_file <- file.path(output_folder, paste(column_name, ".png", sep = ""))
    
    # Create a bar plot and save it as PNG
    png(png_file)
    barplot(table(column_data), main = paste("Barplot of", column_name), xlab = column_name)
    dev.off()
    
    cat("Barplot saved as", png_file, "\n")
  } else {
    cat("The specified column does not exist.\n")
  }
}

# Example usage:
output_folder <- "barplot_pngs"
generate_barplot(data, "chest.pain.type", output_folder)
generate_barplot(data, "age", output_folder)


# Function to generate scatter plot and calculate covariance and correlation
generate_scatterplot <- function(data, x_column, y_column, output_folder) {
  # Check if the columns exist
  if (x_column %in% colnames(data) && y_column %in% colnames(data)) {
    # Extract the column data
    x_data <- data[[x_column]]
    y_data <- data[[y_column]]
    
    # Create the output folder if it doesn't exist
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    # Set the file path for the PNG file
    png_file <- file.path(output_folder, paste(x_column, "_vs_", y_column, "_scatterplot.png", sep = ""))
    
    # Create the scatter plot and save it as PNG
    png(png_file)
    plot(x_data, y_data, main = paste("Scatter Plot of", x_column, "vs", y_column), 
         xlab = x_column, ylab = y_column)
    dev.off()
    
    cat("Scatter plot saved as", png_file, "\n")
    
    # Calculate covariance and correlation
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

# Example usage:
output_folder <- "scatterplot_pngs"
generate_scatterplot(data, "age", "resting.bp.s", output_folder)
