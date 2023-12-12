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

  file_name <- file.path("figures", paste0(state, "_average_alignment.png"))
  png(filename = file_name)

  barplot(averages_df$Average,
          names.arg = averages_df$Variable,
          col = "white",
          ylab = "Average Alignment",
          cex.names = 0.8,
          las = 2,
          ylim = c(0, 1))
  abline(h = 0.5, col = "red", lty = 2, lwd = 1.7)
  abline(h = 0.8, col = "#228B22", lty = 2, lwd = 1.7)
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

indexed_data <- read.csv("indexed_votes.csv")
results <- analyze_indexed_data(indexed_data, "ca")
print(results[["punished_df"]])
# don't really know how to save

indexed_data <- read.csv("indexed_votes_wa.csv")
results <- analyze_indexed_data(indexed_data, "wa")
print(results[["punished_df"]])

const_data <- read.csv("final_votes/wa_constituent_votes.csv")
ref_average <- results[["average_per_district"]]
write.csv(ref_average, "indexed_average_votes_wa.csv", row.names = FALSE)