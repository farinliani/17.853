# This code aims to get the constituent 
# voting data based on district for each 
# referendum and create a new dataframe 
# with results of these votes

# Get the results per district from the proposition
get_results <- function (file_path){
  data <- read.csv(file_path)
  data$Percent <- ifelse(grepl("Percent", data$X.1), 1, 0)
  data$Result <- ifelse(data$Percent == 1,
                         ifelse(as.integer(gsub("%", "", data[, 3])) > as.integer(gsub("%", "", data[, 4])), 1, 0), NA)
  prop <- data[!is.na(data$Result), ]
  rownames(prop) <- 1:nrow(prop)
  column <- paste0("Dsitrict ", rownames(prop))
  prop[, 1] <- column
  prop <- prop[, -c(2, 5)]
  colnames(prop) <- c("District Name", "Yes", "No", "Result")
  return(prop)
}

# Specify the folder path where your CSV files are located
folder_path <- "raw_votes"

# Get a list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

dictionary_of_dfs <- list()
# Iterate through each CSV file and apply the function
for (file in csv_files) {
  file_name <- as.character(tools::file_path_sans_ext(basename(file)))
  ref_num <- paste0("R", substr(file_name,
                                nchar(file_name) - 6,
                                nchar(file_name)))
  result <- get_results(file)
  dictionary_of_dfs[[ref_num]] = result
}

# 2000 data for props 30 and 31
ref2000 <- read.csv("raw_votes/exceptions/referendum ca - Ref2000.csv")
ref2000 <- ref2000[, c(1, 2)]
dummy_df <- data.frame(matrix(ncol = 0, nrow = 80))
dummy_df$Result <- ref2000[, 1]
dictionary_of_dfs[["R30_2000"]] = dummy_df
dummy_df <- data.frame(matrix(ncol = 0, nrow = 80))
dummy_df$Result <- ref2000[, 2]
dictionary_of_dfs[["R31_2000"]] = dummy_df

# Create an empty data frame
new_data_s <- data.frame(matrix(ncol = 0, nrow = 80))
column <- rownames(new_data_s)
new_data_s[["Districts"]] <- column # just so same as reps df
# Loop to populate the columns
for (ref in names(dictionary_of_dfs)) {
  res <- dictionary_of_dfs[[as.character(ref)]]
  new_data_s[[as.character(ref)]] <- res$Result
}

write.csv(new_data_s, "final_votes/ca_constituent_votes.csv", row.names = FALSE)
