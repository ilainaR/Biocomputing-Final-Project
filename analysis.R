#AnalysisR
# Load supporting functions
source("supportingFunctions.R")

# Convert space- or tab-delimited files to CSV in the specified directory
convertToCSV("BiocomputingFinalR")

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
