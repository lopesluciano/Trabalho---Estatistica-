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



