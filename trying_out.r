install.packages("cobalt")
library(MatchIt)
library("cobalt")
library(dplyr)

df_2020 <-
  read.csv("district_characteristics/2020_district_characteristics.csv")
df_2020
mahal <- mahalanobis(as.matrix(df_2020)[2, 3:8],
                     as.matrix(df_2020)[1, 3:8],
                     cov(as.matrix(df_2020[, 3:8])))
mahal
quantiles <- quantile(df_2020$`Median.Income.ACS`, probs = c(.25, .5, .75))
print(quantiles[3][1] + 1)
# 66224.00 76026.50 94692.25
treatment_income <- 94692.25 #rich
df_2020$income <- as.numeric(df_2020$`Median.Income.ACS` > treatment_income)

indexed_data <- read.csv("indexed_votes.csv")
indexed_data <- indexed_data[, 1:2]
indexed_data
merged_df <- merge(df_2020, indexed_data, by = "District")

# full matching
mout <- matchit(income ~ Population + `Median.Age` + White + Black + Asian,
                data = merged_df,
                method = "nearest",
                distance = "mahalanobis",
                replace = TRUE)
msum <- summary(mout)
plot(msum, var.order = "unmatched", abs = FALSE)
love.plot(mout, binary = "std")
png(filename = "figures/bal_table.png")
love.plot(mout, binary = "std")
dev.off()
mdata <- match.data(mout)
fit <- lm(R25_2020 ~ income + Population + `Median.Age` + White + Black + Asian,
          data = mdata,
          weights = weights)
summary(fit)
plot(fit)
att <- fit$coefficients["income"]
att

run_lm_for_ref <- function(vars, ref, df) {
  fit <- lm(as.formula(paste(ref, " ~ ", vars)), data = df, weights = weights)
  return(summary(fit))
}

lm_for_ref <- function(vars, ref, df) {
  fit <- lm(as.formula(paste(ref, " ~ ", vars)), data = df, weights = weights)
  return(fit)
}

run_lm_for_ref("income + Population + `Median.Age` + White + Black + Asian",
               "R25_2020",
               mdata)
bal <- bal.tab(mout, un = TRUE, stats = c("m", "v", "ks"))

#Nearest neighbor (NN) matching on the PS
mout2 <- matchit(income ~ Population + `Median.Age` + White + Black + Asian,
                 data = merged_df)

#Balance on covariates after full and NN matching
bal.tab(income ~ Population + `Median.Age` + White + Black + Asian,
        data = merged_df,
        un = TRUE, weights = list(full = mout, nn = mout2))

# matching for specific year returns a lot of information
# "indexed_data" has the alignment for each ref
# "df" is the district characteristics
# "rich" is which quantile of income we match for
matching_year <- function(indexed_data, df, rich) {
  # richness treshhold
  quantiles <- quantile(df$`Median.Income.ACS`, probs = c(.25, .5, .75))
  treatment_income <- quantiles[rich][1]
  df$income <- as.numeric(df$`Median.Income.ACS` > treatment_income)

  # corect ref
  ref <- which(grepl(df$Year[1], colnames(indexed_data)))
  indexed_data <- select(indexed_data, "District", colnames(indexed_data)[ref])

  #combine
  merged_df <- merge(df, indexed_data, by = "District")

  # full matching
  mout <- matchit(income ~ Population + `Median.Age` + White + Black + Asian,
                  data = merged_df,
                  method = "nearest",
                  distance = "mahalanobis",
                  replace = TRUE)
  mdata <- match.data(mout)

  fit <- lm_for_ref("income + Population + `Median.Age` + White + Black + Asian",
                    colnames(indexed_data)[2],
                    mdata)
  att <- fit$coefficients["income"]

  bal_tab_simple <- bal.tab(mout, un = TRUE, stats = c("m", "v", "ks"))

  #Nearest neighbor (NN) matching on the PS
  mout2 <- matchit(income ~ Population + `Median.Age` + White + Black + Asian,
                   data = merged_df)
  #Balance on covariates after full and NN matching
  bal_tab_complex <- bal.tab(income ~ Population + `Median.Age` +
                               White + Black + Asian,
                             data = merged_df,
                             un = TRUE, weights = list(full = mout, nn = mout2))
  return(list("merged" = merged_df,
              "matched" = mdata,
              "fit" = fit,
              "att" = att,
              "simple balance table" = bal_tab_simple,
              "complex balance table" = bal_tab_complex))
}

indexed_data <- read.csv("indexed_votes.csv")
df <-
  read.csv("district_characteristics/2020_district_characteristics.csv")
result <- matching_year(indexed_data, df, 3)
print((result[["att"]]))

df <-
  read.csv("district_characteristics/2022_district_characteristics.csv")
df
result <- matching_year(indexed_data, df, 3)
print((result[["att"]]))

df <-
  read.csv("district_characteristics/2016_district_characteristics.csv")
df
result <- matching_year(indexed_data, df, 3)
print((result[["att"]]))
