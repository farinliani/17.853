library("dplyr")

data_reps <- read.csv("final_votes/ca_representative_votes.csv")
data_consts <- read.csv("final_votes/ca_constituent_votes.csv")

convert_values <- function(x) {
  ifelse(x == "yes", 1, ifelse(x == "no", 0,
                               ifelse(x %in% c("Absent", "NV"), -1, NA)))
}
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
colnames(merged_df) <- c("Districts", new_column_names) 

write.csv(merged_df, "indexed_votes.csv", row.names = FALSE)