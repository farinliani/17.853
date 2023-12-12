library("dplyr")

data_reps <- read.csv("final_votes/ca_representative_votes.csv")
data_consts <- read.csv("final_votes/ca_constituent_votes.csv")

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
                     suffixes = c("_rep", "_const"))

  # Initialize an empty 'Match' column
  merged_df$Match <- 1

  # Iterate over columns and update 'Match' column based on equality
  for (col in colnames(data_consts)[-1]) {
    match_col <- paste0('Match_', col)
    merged_df[[match_col]] <- ifelse(merged_df[[paste0(col, '_rep')]] ==
                                       merged_df[[paste0(col, '_const')]],
                                     1,
                                     ifelse(merged_df[[paste0(col, '_rep')]] == -1,
                                            NA, 0))
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
write.csv(merged_df, "indexed_votes.csv", row.names = FALSE)

merge_const_rep_wa <- function(data_reps, data_consts){
  # not really binary lol
  binary_reps <- data_reps %>%
    mutate_at(vars(-Districts), convert_values)
  merged_df <- data.frame("District" = data_consts$Districts)
  # Initialize an empty 'Match' column
  merged_df$Match <- 1

  # Iterate over columns and update 'Match' column based on equality
  for (col in colnames(data_consts)[-1]) {
    ai <- c()
    for (i in 1:49){
      rep1 <- ifelse(data_consts[i, col] == binary_reps[2 * i - 1, col],
                     1,
                     ifelse(binary_reps[2 * i - 1, col] == -1,
                            NA,
                            0))
      rep2 <- ifelse(data_consts[i, col] == binary_reps[2 * i, col],
                     1,
                     ifelse(binary_reps[2 * i, col] == -1,
                            NA,
                            0))
      ai <- c(ai, (rep1 + rep2) / 2)
    }
    merged_df[[col]] <- ai
  }
  merged_df <- merged_df[, -2]
  return(merged_df)
}

data_reps <- read.csv("final_votes/wa_representative_votes.csv")
data_av_reps <- subset(data_reps, select = -c(R71_2009, R67_2007))
data_consts <- read.csv("final_votes/wa_constituent_votes.csv")

merged_df <- merge_const_rep_wa(data_av_reps, data_consts)
write.csv(merged_df, "indexed_votes_wa.csv", row.names = FALSE)