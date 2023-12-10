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
folder_path <- "raw_votes/ca"

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
ref2000 <- read.csv("raw_votes/ca/exceptions/referendum ca - Ref2000.csv")
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


# GETTING DATA FOR WA
# all have different formatting
# due to time constrained just will do each year seperately

dictionary_of_dfs <- list()

wa_ref2020 <- read.csv("raw_votes/wa/referendum wa - Ref90_2020.csv")
Approved <- wa_ref2020$Referendum.Measure.No..90[2:50]
Rejected <- wa_ref2020$X.3[2:50]

dummy_df <- data.frame(matrix(ncol = 0, nrow = 49))
dummy_df$Result <- as.numeric(Approved > Rejected)
dictionary_of_dfs[["R90_2020"]] = dummy_df

wa_ref2019 <- read.csv("raw_votes/wa/referendum wa - Ref88_2019.csv")
Approved <- wa_ref2019$RaceOrder[6:54]
Rejected <- wa_ref2019$RaceName[6:54]

dummy_df <- data.frame(matrix(ncol = 0, nrow = 49))
dummy_df$Result <- as.numeric(Approved > Rejected)
dictionary_of_dfs[["R88_2019"]] = dummy_df


data <- read.csv("raw_votes/wa/referendum wa - Ref74_2012.csv")
data$Total <- ifelse((grepl("Total", data$X) & !grepl("State Total", data$X)), 1, 0)
data$Result <- ifelse(data$Total == 1,
                        ifelse(as.integer(data$R.74) > as.integer(data$X.5), 1, 0), NA)
prop <- data[!is.na(data$Result), ]
rownames(prop) <- 1:nrow(prop)
column <- paste0("Dsitrict ", rownames(prop))
prop[, 1] <- column
prop <- prop[, c("X", "R.74", "X.5", "Result")]
colnames(prop) <- c("District Name", "Yes", "No", "Result")
dummy_df <- data.frame(matrix(ncol = 0, nrow = 49))
dummy_df$Result <- prop$Result
dictionary_of_dfs[["R74_2012"]] = dummy_df

# Create an empty data frame
new_data_s <- data.frame(matrix(ncol = 0, nrow = 49))
column <- rownames(new_data_s)
new_data_s[["Districts"]] <- column # just so same as reps df
# Loop to populate the columns
for (ref in names(dictionary_of_dfs)) {
  res <- dictionary_of_dfs[[as.character(ref)]]
  new_data_s[[as.character(ref)]] <- res$Result
}

write.csv(new_data_s, "final_votes/wa_constituent_votes.csv", row.names = FALSE)