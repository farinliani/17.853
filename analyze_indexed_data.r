library(sf)
library(ggplot2)
indexed_data <- read.csv("indexed_votes.csv")

just_refs <- indexed_data[, -1]
averages <- colMeans(just_refs, na.rm = TRUE)
just_refs$Average_Ref <- rowMeans(just_refs, na.rm = TRUE)

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

barplot(averages_df$Average,
        names.arg = averages_df$Variable,
        col = "#FDFD96",
        ylab = "Representation Score",
        cex.names = 0.8,
        las = 2,
        ylim = c(0, 1))
abline(h = 0.5, col = "red", lty = 2)