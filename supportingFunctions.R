#SupportingFunctionsR
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
  summary_data <- data.frame(
    Screens = nrow(compiled_data),
    PercentInfected = mean(compiled_data$Infected) * 100,
    PercentMale = mean(compiled_data$Gender == "Male") * 100,
    PercentFemale = mean(compiled_data$Gender == "Female") * 100
  )
  
  age_distribution <- table(cut(compiled_data$Age, breaks = seq(0, max(compiled_data$Age)+10, by = 10)))
  age_distribution <- as.data.frame(age_distribution)
  colnames(age_distribution) <- c("AgeRange", "Count")
  
  return(list(Summary = summary_data, AgeDistribution = age_distribution))
}
