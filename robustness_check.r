# This code aims to get the constituent 
# voting data based on district for each 
# referendum and create a new dataframe 
# with results of these votes
conversion <- function(data, integer){
  number <- gsub("%", "", data[, integer])
  number <- as.double(gsub(",", ".", number))
  return(number)
}
# Get the results per district from the proposition
get_results <- function (file_path){
  data <- read.csv(file_path)
  data$Percent <- ifelse(grepl("Percent", data$X.1), 1, 0)
  yes <- conversion(data, 3)
  no <- conversion(data, 4)
  data$Result <- ifelse(data$Percent == 1,
                         yes/100, NA)
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

# Create an empty data frame
new_data_s <- data.frame(matrix(ncol = 0, nrow = 80))
column <- rownames(new_data_s)
new_data_s[["Districts"]] <- column # just so same as reps df
# Loop to populate the columns
for (ref in names(dictionary_of_dfs)) {
  res <- dictionary_of_dfs[[as.character(ref)]]
  new_data_s[[as.character(ref)]] <- res$Result
}

write.csv(new_data_s, "final_votes/rcheck_ca_constituent_votes.csv", row.names = FALSE)

library("dplyr")

data_reps <- read.csv("final_votes/ca_representative_votes.csv")
data_consts <- read.csv("final_votes/rcheck_ca_constituent_votes.csv")

convert_values <- function(x) {
  ifelse(x == "yes", 1, ifelse(x == "no", 0,
                               ifelse(x %in% c("Absent", "NV"), -1, NA)))
}
## if data for consts got from the getting_results_for_constituents file in this folder
## and data for reps is of the same format as washington can use this to combine
merge_const_rep <- function(data_reps, data_consts){
  # not really binary lol
  binary_reps <- data_reps %>%
    mutate_at(vars(-Districts), convert_values)

  # a bit messy merge to get idx - Thanks ChatGPT)
  # Merge dataframes on the 'Districts' column
  merged_df <- merge(binary_reps,
                    data_consts,
                    by = "Districts",
                    suffixes = c('_rep', '_const'))

  # Initialize an empty 'Match' column
  merged_df$Match <- 1

  # Iterate over columns and update 'Match' column based on equality
  for (col in colnames(data_consts)[-1]) {
    match_col <- paste0('Match_', col)

    merged_df[[match_col]] <- ifelse(merged_df[[paste0(col, '_rep')]] ==
                                      round(merged_df[[paste0(col, '_const')]]),
                                    ifelse(
                                      merged_df[[paste0(col, '_const')]] > 0.5,
                                      merged_df[[paste0(col, '_const')]],
                                      1-merged_df[[paste0(col, '_const')]]
                                    ),
                                    ifelse(merged_df[[paste0(col, '_rep')]] == -1,
                                    NA, 
                                    ifelse(
                                      merged_df[[paste0(col, '_const')]] > 0.5,
                                      1 - merged_df[[paste0(col, '_const')]],
                                      merged_df[[paste0(col, '_const')]]
                                    )))
    merged_df$Match <- merged_df$Match * merged_df[[match_col]]
  }

  # Drop the redundant columns if needed
  merged_df <- merged_df[c("Districts",
                          grep("Match", colnames(merged_df), value = TRUE))]

  # remove the Match
  merged_df <- merged_df[, -2]
  match_columns <- grep("^Match_", colnames(merged_df), value = TRUE)
  new_column_names <- sub("^Match_", "", match_columns)
  # Rename the columns
  colnames(merged_df) <- c("District", new_column_names)
  return(merged_df)  
}

merged_df <- merge_const_rep(data_reps, data_consts)
write.csv(merged_df, "rcheck_indexed_votes.csv", row.names = FALSE)


library(sf)
library(ggplot2)

# if indexed data produced using the combining_reps_and_consts file
# can use to get average, create barplot, and get disaligned districts
analyze_indexed_data <- function(indexed_data, state) {
  new_data <- data.frame(indexed_data)
  just_refs <- indexed_data[, -1]
  averages <- colMeans(just_refs, na.rm = TRUE)
  new_data$Average_Ref <- rowMeans(just_refs, na.rm = TRUE)

  # Create a new dataframe with averages
  averages_df <- data.frame(
    Variable = names(averages),
    Average = averages
  )

  # Extract the last 4 letters of each value in the first row
  last_four_letters <- substr(averages_df[, 1],
                              nchar(averages_df[, 1]) - 3,
                              nchar(averages_df[, 1]))

  # Order the columns based on the last 4 letters
  averages_df <- averages_df[order(last_four_letters), ]
  averages_df <- na.omit(averages_df)

  file_name <- file.path("figures", paste0(state, "rcheck_average_alignment.png"))
  png(filename = file_name)

  barplot(averages_df$Average,
          names.arg = averages_df$Variable,
          col = "white",
          ylab = "Average Alignment",
          cex.names = 0.8,
          las = 2,
          ylim = c(0, 1))
  abline(h = 0.5, col = "red", lty = 2, lwd = 1.7)
  # abline(h = 0.8, col = "#228B22", lty = 2, lwd = 1.7)
  dev.off()

  punished_df <- list()
  for (ref in 2:(ncol(indexed_data) - 1)){
    array <- c()
    for (i in 1:80){
      if (!is.na(indexed_data[i, ref]) && indexed_data[i, ref] == 0){
        array <- c(array, i)
      }
    }
    punished_df[[colnames(indexed_data)[ref]]] <- array
  }
  return(list("average_per_ref" = averages_df,
              "average_per_district" = new_data,
              "punished_df" = punished_df))
}

indexed_data <- read.csv("rcheck_indexed_votes.csv")
results <- analyze_indexed_data(indexed_data, "ca")
print(results[["punished_df"]])
# don't really know how to save
just_averages <- results[["average_per_district"]]
write.csv(just_averages, "rcheck_indexed_average_votes_ca.csv", row.names = FALSE)
