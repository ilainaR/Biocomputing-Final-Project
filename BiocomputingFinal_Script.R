#Supporting Functions 
# Function to convert space- or tab-delimited files to CSV
convertToCSV <- function(directory_path) {
  files <- list.files(directory_path, pattern = "\\.txt$", full.names = TRUE)
  
  for (file in files) {
    data <- read.table(file, header = TRUE, sep = c(" ", "\t"), comment.char = "")
    csv_file <- gsub("\\.txt", ".csv", file)
    write.csv(data, csv_file, row.names = FALSE)
  }
}

# Function to compile data from all CSV files in a directory
compileData <- function(directory_path, remove_NAs = TRUE, warn_NAs = TRUE) {
  files <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE)
  all_data <- data.frame()
  
  for (file in files) {
    data <- read.csv(file, header = TRUE)
    all_data <- rbind(all_data, data)
  }
  
  if (remove_NAs) {
    all_data <- na.omit(all_data)
  } else if (warn_NAs && any(is.na(all_data))) {
    warning("There are NA values in the compiled data.")
  }
  
  return(all_data)
}
# Function to summarize the compiled data set
summarizeData <- function(compiled_data) {
# Convert 'Age' column to numeric, replacing non-numeric values with NA
compiled_data$Age <- suppressWarnings(as.numeric(as.character(compiled_data$Age)))
  
# Convert 'Infected' column to logical, treating non-logical values as NA
compiled_data$Infected <- as.logical(compiled_data$Infected)
  
# Remove rows with NA values in 'Age' and 'Infected' columns
compiled_data <- compiled_data[!is.na(compiled_data$Age) & !is.na(compiled_data$Infected), ]
  
summary_data <- data.frame(
  Screens = nrow(compiled_data),
  PercentInfected = mean(compiled_data$Infected, na.rm = TRUE) * 100,
  PercentMale = mean(compiled_data$Gender == "Male", na.rm = TRUE) * 100,
  PercentFemale = mean(compiled_data$Gender == "Female", na.rm = TRUE) * 100
  )
  
# Create custom age ranges
age_bins <- seq(0, max(compiled_data$Age, na.rm = TRUE) + 10, by = 10)
age_labels <- paste(age_bins[-length(age_bins)], "-", age_bins[-1], sep = "")
compiled_data$AgeRange <- cut(compiled_data$Age, breaks = age_bins, labels = age_labels, include.lowest = TRUE)
  
age_distribution <- table(compiled_data$AgeRange)
age
} 

#Analysis Script 
# Load supporting functions
source("supportingFunctions.R")

# Convert space- or tab-delimited files to CSV in the specified directory
convertToCSV("path/to/your/directory")

# Compile data from all CSV files in the directory
compiled_data <- compileData("BiocomputingFinalR", remove_NAs = TRUE, warn_NAs = TRUE)

# Summarize the compiled data set
summary_result <- summarizeData(compiled_data)

# Print summary data
print("Summary:")
print(summary_result$Summary)

# Print age distribution
print("Age Distribution:")
print(summary_result$AgeDistribution)

# Create plots or additional analysis as needed to answer the CDC questions
# ...

# Save compiled data to a CSV file
write.csv(compiled_data, "compiled_data.csv", row.names = FALSE)
