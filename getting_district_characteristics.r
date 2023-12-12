library(tidycensus)
library(tidyverse)
census_api_key("4cb22034cde4e2d7c6ec343a85b2c8309b6688bc",
               install = TRUE,
               overwrite = TRUE)
##########################
# Variables of interest  #
##########################
income_var <- c("B19013_001") # acs
income_var_sf3 <- "P053001" # census
income_var_sf4 <- "PCT089001" # census
race_vars <- c("B02001_002", "B02001_003", "B02001_005")
matching_vars <- c("B01002_001", "B01003_001", race_vars)
matching_vars_2000 <- c("P013001", "P001001", "P007002", "P007003", "P007005")
all_vars <- c(income_var, matching_vars)

#####################################################
# Corresponding names for the variables of interest #
#####################################################
col_to_var <- list(
  "B19013_001" = "Median Income ACS",
  "P053001" = "Median Income sf3",
  "PCT089001" = "Median Income sf4",
  "B02001_002" = "White",
  "B02001_003" = "Black",
  "B02001_005" = "Asian",
  "B01002_001" = "Median Age",
  "B01003_001" = "Population",
  # for decennial data
  "P013001" = "Median Age",
  "P001001" = "Population",
  "P007002" = "White",
  "P007003" = "Black",
  "P007005" = "Asian"
)
# only acs
col_to_var_short <- list(
  "B19013_001" = "Median Income ACS",
  "B01003_001" = "Population",
  "B01002_001" = "Median Age",
  "B02001_002" = "White",
  "B02001_003" = "Black",
  "B02001_005" = "Asian"
)
# only decennial
col_to_var_decennial <- list(
  "P013001" = "Median Age",
  "P001001" = "Population",
  "P007002" = "White",
  "P007003" = "Black",
  "P007005" = "Asian"
)

# getting census data for different years
# need to provide year and variables of interest
get_data <- function(vars, year) {
  if (year > 2010) {
    # acs has geometry data
    var_map <- get_acs(geography = "state legislative district (lower chamber)",
                       variables = vars,
                       state = "CA",
                       year = year,
                       survey = "acs5",
                       geometry = FALSE) # can make true to get map
  } else if (year > 2005) {
    # acs and decennial does not have decennial data
    if ((year %% 10) == 0) {
       # decennial is better
      var_map <- get_decennial(geography =
                                 "state legislative district (lower chamber)",
                               variables = vars,
                               state = "CA",
                               year = year)
    } else {
      # no decennial so use acs
      var_map <- get_acs(geography =
                           "state legislative district (lower chamber)",
                         variables = vars,
                         state = "CA",
                         year = year,
                         survey = "acs5")
    }
  } else {
    # no acs so have to use decennial
    var_map <- get_decennial(geography = "tract",
                             variables = vars,
                             state = "CA",
                             year = year)
  }
  return(data.frame(var_map))
}

# getting for each year of interest using the get_data function
# apparently 2022 will be available in december
var_map_2022 <- get_data(all_vars, 2022)
# seems to work
var_map_2020 <- get_data(all_vars, 2020)
var_map_2016 <- get_data(all_vars, 2016)
var_map_2014 <- get_data(all_vars, 2014)
# 2009 actually gives 2006, can get 2010
# SIDE NOTE: could also try average of the two
var_2008 <- get_data(matching_vars_2000, 2010) # need to add incom
var_2000 <- get_data(matching_vars_2000, 2000)
income_2000_sf3 <- get_decennial(geography = "tract",
                                 variables = income_var_sf3,
                                 state = "CA",
                                 year = 2000,
                                 sumfile = "sf3")
income_2000_sf4 <- get_decennial(geography = "tract",
                                 variables = income_var_sf4,
                                 state = "CA",
                                 year = 2000,
                                 sumfile = "sf4")

# data only with 2008 and above
data <- list(m2022 = var_map_2022,
             m2020 = var_map_2020,
             m2016 = var_map_2016,
             m2014 = var_map_2014)
#m2008 = var_2008)

# helper function to create dataframe where each column
# is a characteristic for each district
# SIDE NOTE: should also try getting max and min since
# census is aggregated
make_df <- function(db) {
  df <- data.frame()
  GEOIDs <- unique(db$GEOID)
  col_names_c <- col_to_var_short
  # loop through all districts
  for (district in GEOIDs){
    values <- db[db$GEOID == district, ]

    # get the name of the district and year
    # looks smth like Assembly District _ (_)

    unedited_district_name <- values$NAME[1]
    numbers <- (gsub("\\D", "", unedited_district_name))

    district_number <- as.numeric(substr(numbers, 1, (nchar(numbers) - 4)))
    year <- as.numeric(substr(numbers, (nchar(numbers) - 3), nchar(numbers)))
    if (year == 2018){
      year <- 2020 # use 2018 data for 2020
    }
    # add name and year to the array
    district_year <- c(district_number, year)

    # add the values to the array
    if (year > 2010) {
      col_names_c <- col_to_var_short
      for (var in names(col_names_c)){
        val <- values[values$variable == var, ]$estimate
        district_year <- c(district_year, as.numeric(val))
      }
    } else {
      col_names_c <- col_to_var_decennial
      for (var in names(col_names_c)){
        val <- values[values$variable == var, ]$value
        district_year <- c(district_year, as.numeric(val))
      }
    }
    # add the array as a row to the df
    df <- rbind(df, district_year)
  }
  # convert variable name to column name
  col_names <- c("District", "Year")
  for (var in names(col_names_c)){
    col_names <- c(col_names, col_names_c[[var]])
  }
  colnames(df) <- col_names
  return(df)
}

# loop through all data not including 2000-4
# combine into one big dataframe
df <- data.frame()
for (db in names(data)) {
  db <- data[[db]]
  print(df)
  df <- rbind(df, make_df(db))
}

# please excuse the excessive coppying
write.csv(df,
          "district_characteristics/all_district_characteristics_2008_2022.csv",
          row.names = FALSE)

df_2020 <- make_df(var_map_2020)
df_2020$Year[df_2020$Year == 2018] <- 2020
sorted_df_2020 <- df_2020[order(df_2020$District), ]
write.csv(sorted_df_2020,
          "district_characteristics/2020_district_characteristics.csv",
          row.names = FALSE)

df_2022 <- make_df(var_map_2022)
sorted_df_2022 <- df_2022[order(df_2022$District), ]
write.csv(sorted_df_2022,
          "district_characteristics/2022_district_characteristics.csv",
          row.names = FALSE)

df_2016 <- make_df(var_map_2016)
sorted_df_2016 <- df_2016[order(df_2016$District), ]
write.csv(sorted_df_2016,
          "district_characteristics/2016_district_characteristics.csv",
          row.names = FALSE)

df_2014 <- make_df(var_map_2014)
sorted_df_2014 <- df_2014[order(df_2014$District), ]
write.csv(sorted_df_2014,
          "district_characteristics/2014_district_characteristics.csv",
          row.names = FALSE)

df_2008 <- make_df(var_2008)
df_2008$Year[df_2008$Year == 2010] <- 2008
sorted_df_2008 <- df_2008[order(df_2008$District), ]
write.csv(sorted_df_2008,
          "district_characteristics/2008_district_characteristics.csv",
          row.names = FALSE)
