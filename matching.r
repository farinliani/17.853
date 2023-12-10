install.packages("cobalt")
library(MatchIt)
library("cobalt")
library(dplyr)
update.packages("love.plot")

run_lm_for_ref <- function(vars, ref, df) {
  fit <- lm(as.formula(paste(ref, " ~ ", vars)), data = df, weights = weights)
  return(summary(fit))
}

lm_for_ref <- function(vars, ref, df) {
  fit <- glm(as.formula(paste(ref, " ~ ", vars)), data = df, family = "binomial", weights = weights)
  return(fit)
}

add_income_col <- function(df, income_col, rich){
  # richness treshhold
  quantiles <- quantile(df[[income_col]], probs = c(.25, .5, .75))
  treatment_income <- quantiles[rich][1]
  df$income <- as.numeric(df[[income_col]] > treatment_income)
  return(df)
}

get_correct_ref <- function(df, indexed_data){
  year <- df$Year[1]
  ref <- which(grepl(year, colnames(indexed_data)))
  if (length(ref) > 1){
    # get and average refs
    just_refs <- indexed_data[, ref]
    ref_data <- data.frame("District" = indexed_data[, "District"])
    col_name <- paste0("Average_Ref_", year)
    print(ref_data[["District"]])
    print(length(rowMeans(just_refs, na.rm = TRUE)))
    ref_data[[col_name]] <- rowMeans(just_refs, na.rm = TRUE)
    # ref names and merge
    ref_name <- col_name
  } else {
    ref_data <- select(indexed_data, "District", colnames(indexed_data)[ref])
    ref_name <- colnames(ref_data)[2]
  }  
  merged_df <- merge(df, ref_data, by = "District")
  return(list("merged_df" = merged_df, "ref_name" = ref_name))
}

save_bal_table <- function(ref_name, mout){
  graph <- love.plot(mout, binary = "std", main = file_path)
  filename <- paste0(ref_name, "_bt.pdf")
  ggsave(filename, graph, path = "figures") 
}


# matching for specific year returns a lot of information
# "indexed_data" has the alignment for each ref
# "df" is the district characteristics
# "rich" is which quantile of income we match for
matching_year <- function(indexed_data, df, rich) {
  df <- add_income_col(df, "Median.Income.ACS", 3)
  # corect ref and combined df
  ref_comb <- get_correct_ref(df, indexed_data)

  ref_name <- ref_comb[["ref_name"]]
  merged_df <- ref_comb[["merged_df"]]

  # full matching
  mout <- matchit(income ~ Population + `Median.Age` + White + Black + Asian,
                  data = merged_df,
                  method = "nearest",
                  distance = "mahalanobis",
                  replace = TRUE)
  mdata <- match.data(mout)

  save_bal_table(ref_name, mout)

  fit <- lm_for_ref("income + Population + `Median.Age` + White + Black + Asian",
                    ref_name,
                    mdata)
  plot(fit)
  att <- fit$coefficients["income"]

  # bal_tab_simple <- bal.tab(mout, un = TRUE, stats = c("m", "v", "ks"))

  #Nearest neighbor (NN) matching on the PS
  # mout2 <- matchit(income ~ Population + `Median.Age` + White + Black + Asian,
                  #  data = merged_df)
  #Balance on covariates after full and NN matching
  # bal_tab_complex <- bal.tab(income ~ Population + `Median.Age` +
  #                              White + Black + Asian,
  #                            data = merged_df,
  #                            un = TRUE, weights = list(full = mout, nn = mout2))
  return(list("merged" = merged_df,
              "matched" = mdata,
              "fit" = fit,
              "att" = att))
              # "simple balance table" = bal_tab_simple,
              # "complex balance table" = bal_tab_complex))
}

indexed_data <- read.csv("indexed_votes.csv")
years_of_interest <- c(2022, 2020, 2016, 2014)
output <- c()
left <- "district_characteristics"
right <- "_district_characteristics.csv"
for (ref in years_of_interest){
  file_path <- file.path(left, paste0(ref, right))
  df <- read.csv(toString(file_path))
  result <- matching_year(indexed_data, df, 3)
  output <- c(output, result[["att"]])
}
output
png(filename = "figures/plotting_effect.png")  
plot(years_of_interest, output,
      xlab = "Year",
      ylab = "Average Treatment Effect",
      main = " Effect of Income on allignment over 2014-22")
lines(years_of_interest, output, type = "l", col = "red")
dev.off()

# full matching
mout <- matchit(income ~ Population + `Median.Age` + White + Black + Asian,
                data = merged_df,
                method = "nearest",
                distance = "mahalanobis",
                replace = TRUE)

#Nearest neighbor (NN) matching on the PS
mout2 <- matchit(income ~ Population + `Median.Age` + White + Black + Asian,
                 data = merged_df)

#Balance on covariates after full and NN matching
bal.tab(income ~ Population + `Median.Age` + White + Black + Asian,
        data = merged_df,
        un = TRUE, weights = list(full = mout, nn = mout2))

