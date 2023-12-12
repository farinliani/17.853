install.packages("cobalt")
install.packages("marginaleffects")
library(MatchIt)
library("cobalt")
library(dplyr)
library(ggplot2)
library(marginaleffects)
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

get_correct_ref <- function(df, indexed_data) {
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

save_bal_table <- function(ref_name, mout) {
  graph <- love.plot(mout, binary = "std")
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
  s <- summary(mout)
  ss <- s$nn
  control_to_treated <- ss[3, 1] / ss[3, 2]
  save_bal_table(ref_name, mout)

  fit <- lm_for_ref("income + Population + `Median.Age` + White + Black + Asian",
                    ref_name,
                    mdata)
  s <- summary(fit)
  coef <- s$coefficients
  std_error <- coef[, "Std. Error"][2]

  att <- fit$coefficients["income"]
  avg_fit <- avg_comparisons(fit,
                             variables = "income",
                             type = "response",
                             wts = "weights")

  return(list("merged" = merged_df,
              "matched" = mdata,
              "fit" = fit,
              "att" = att,
              "avg_fit" = avg_fit$estimate,
              "error" = std_error,
              "c_to_t" = control_to_treated))
}

indexed_data <- read.csv("indexed_votes.csv")

# matching then logit for each year
years_of_interest <- c("2014", "2016", "2020", "2022")
output <- c()
standard_error <- c()
control_to_treated <- c()
left <- "district_characteristics"
right <- "_district_characteristics.csv"
for (ref in years_of_interest){
  file_path <- file.path(left, paste0(ref, right))
  df <- read.csv(toString(file_path))
  result <- matching_year(indexed_data, df, 3)
  output <- c(output, result[["avg_fit"]])
  standard_error <- c(standard_error, result[["error"]])
  control_to_treated <- c(control_to_treated, result[["c_to_t"]])
}

png(filename = "figures/plotting_estimate.png")
plot(years_of_interest, output,
     xlab = "Year",
     ylab = "Estimate",
     main = "Estimated Effect of Income on Allignment Over 2014-22")
lines(years_of_interest, output, type = "l", col = "green")
dev.off()

png(filename = "figures/plotting_error.png")
plot(years_of_interest, standard_error,
     xlab = "Year",
     ylab = "Error",
     main = "Standard Error of Effect of Income on Allignment Over 2014-22")
lines(years_of_interest, standard_error, type = "l", col = "red")
dev.off()

png(filename = "figures/ctrl_to_treated.png")
plot(years_of_interest, control_to_treated,
     xlab = "Year",
     ylab = "Control to Treated Ratio",
     main = "Control to Treated Ratio over 2014-22 After Matching")
lines(years_of_interest, control_to_treated, type = "l", col = "green")
dev.off()

# all dsitricts
years_of_interest <- c("R31_2022", "R25_2020", "R67_2016", "R48_2014")
ref <- c()
for (referendum in years_of_interest){
  for (dist in 1:80){
    ref <- c(ref, indexed_data[dist, referendum])
  }
}
all_districts <- read.csv("district_characteristics/all_district_characteristics_2008_2022.csv")
all_districts$Ref <- ref
all_districts <- add_income_col(all_districts, "Median.Income.ACS", 3)

# just logit
fit <- glm(Ref ~ income + Population + `Median.Age` + White + Black + Asian,
           data = all_districts, family = "binomial")

summary(fit)

avg_fit <- avg_comparisons(fit,
                           variables = "income",
                           type = "response")

summary(avg_fit)

# matching and doing logit
mout <- matchit(income ~ Population + `Median.Age` + White + Black + Asian,
                data = all_districts,
                method = "nearest",
                distance = "mahalanobis",
                replace = TRUE)
mdata <- match.data(mout)

summary(mout)

fit_t <- glm(Ref ~ income + Population + `Median.Age` + White + Black + Asian,
             data = mdata, family = "binomial", weights = weights)

summary(fit_t)

avg_fit <- avg_comparisons(fit_t,
                           variables = "income",
                           type = "response",
                           wts = "weights")

summary(avg_fit)