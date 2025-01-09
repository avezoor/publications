# Load required libraries
library(readxl)  # To read Excel files
library(dplyr)   # For data manipulation

# Define the path to the Excel file
file_path <- "your_file_path"  # Replace with the path to your Excel file
data <- read_excel(file_path, sheet = "dataset")  # Read the sheet named "Dataset" in your Excel file

# Display column names in the dataset
colnames(data)

# List of columns to be analyzed
columnsToAnalyze <- c("column_1", "column_2")

# Function to calculate the mode (most frequently occurring value)
mode_function <- function(x) {
  uniq_x <- unique(x)  # Find unique values
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Value with the highest frequency
}

# Iterate over each column in the list of columns to analyze
for (column_name in columnsToAnalyze) {
  # Check if the column exists in the dataset
  if (!column_name %in% colnames(data)) {
    cat("Column", column_name, "not found in the dataset.\n")
    next  # Skip to the next column if not found
  }
  
  # Extract data for the column
  z <- data[[column_name]]
  
  # Check if the column is numeric
  if (!is.numeric(z)) {
    cat("Column", column_name, "is not numeric.\n")
    next  # Skip to the next column if not numeric
  }
  
  # Display descriptive statistics for the column
  cat("Descriptive Statistics for column", column_name, ":\n")
  cat("  Mean:", mean(z, na.rm = TRUE), "\n")
  cat("  Median:", median(z, na.rm = TRUE), "\n")
  cat("  Mode:", mode_function(z), "\n")
  cat("  Standard Deviation:", sd(z, na.rm = TRUE), "\n")
  cat("  Variance:", var(z, na.rm = TRUE), "\n")
  cat("  Minimum:", min(z, na.rm = TRUE), "\n")
  cat("  Maximum:", max(z, na.rm = TRUE), "\n")
  cat("  Range:", max(z, na.rm = TRUE) - min(z, na.rm = TRUE), "\n")
  cat("  1st Quartile (25%):", quantile(z, probs = 0.25, na.rm = TRUE), "\n")
  cat("  3rd Quartile (75%):", quantile(z, probs = 0.75, na.rm = TRUE), "\n")
  cat("  Decentile :", quantile(z, probs = 0.10, na.rm = TRUE), "\n")
  cat("  Percentile :", quantile(z, probs = 0.01, na.rm = TRUE), "\n")

  
  # Create a histogram for the column
  hist(z, 
       main = paste("Histogram of", column_name),  # Title of the histogram
       xlab = paste("Values of", column_name),    # X-axis label
       col = "lightblue",                         # Bar color of the histogram
       border = "black")                          # Border color of the bars
  
  # Wait for user input before proceeding to the next column
  readline(prompt = "Press [Enter] to continue to the next column...")
}
