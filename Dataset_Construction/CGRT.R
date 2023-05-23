############## Create CGRT Variables X1, X2, X3 ##############
# In this section, I import the datasets for the variables X1, X2, and X3.
# The data for these variables comes from the Oxford University COVID-19 Government Response Tracker.
# From this point onward, these three variables will be referred to as the CGRT variables.
# Every regression in my study will use these variables.
# X1 is the Stringency index.
# X2 is the Economic Support Index.
# X3 is the number of Confirmed Cases.

# These datasets include the daily indices for every U.S. State.
# The state and regional entities that I build will
# pull the data columns from these datasets.
# I start by importing the datasets from the
# official GitHub repository for Oxford University's
# COVID-19 Government Response tracker.

library(plyr)
library(tidyverse)
library(readxl)
library(janitor)
library(tibble)
library(openxlsx)

if (!exists("filename")) {
  url <- "https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/timeseries/OxCGRT_timeseries_all.xlsx"
  filename <- tempfile() # creates a temporary file name
  download.file(url, destfile = filename, mode = "wb")
}

############################ X1 ############################

# Read in data from Excel file
CGRT.stringency <- read_excel(filename, sheet = 1) %>%
  # Filter for USA
  filter(country_code == "USA") %>%
  # Remove unnecessary identifier columns
  select(-c(1, 2, 4, 5))
CGRT.stringency[1, 1] <- "US"
# Replace missing values in the beginning with 0.00
CGRT.stringency[, 2:1097][is.na(CGRT.stringency[, 2:1097])] <- 0.00
# Remove rows where all values are missing or empty
CGRT.stringency <- CGRT.stringency %>%
  discard(~all(is.na(.) | . == "")) %>%
  # Transpose the data frame
  t() %>%
  row_to_names(row_number = 1) %>%
  # Convert to tibble and name the first column "Date"
  as_tibble(rownames = "Date")
# Convert the first column (Date column) to Date object, then parse the date column
CGRT.stringency$Date <- dmy(CGRT.stringency$Date) %>%
  as_date()
CGRT.stringency <- CGRT.stringency %>%
  #Remove US_ prefix from every column name besides "Date" and "US"
  rename_with(~gsub("US_", "", .x)) %>%
  #Create rows equal to zero for days between 2019-10-06 and 2019-12-31
  add_row(Date = seq(as.Date("2019-10-06"), as.Date("2019-12-31"), by = "day"), .before = 1)
CGRT.stringency[1:87, 2:53] <- "0"
# Change all case numbers in columns 2 to 53 from character to numeric
CGRT.stringency[, 2:53] <- sapply(CGRT.stringency[, 2:53], as.numeric)

############################ X2 ############################
CGRT.economic_support <- read_excel(filename, sheet = 4) %>%
  filter(country_code == "USA") %>%
  select(-c(1, 2, 4, 5))
CGRT.economic_support[1, 1] <- "US"
# Replace missing values in the beginning with 0.00
CGRT.economic_support[, 2:1097][is.na(CGRT.economic_support[, 2:1097])] <- 0.00
# Remove rows where all values are missing or empty
CGRT.economic_support <- CGRT.economic_support %>%
  discard(~all(is.na(.) | . == ""))
CGRT.economic_support <- t(CGRT.economic_support) %>%
  row_to_names(row_number = 1) %>%
  as_tibble(rownames = "Date")
# Convert the first column (Date column) to Date object, then parse the Date column
CGRT.economic_support$Date <- dmy(CGRT.economic_support$Date, quiet = TRUE) %>%
  as_date()
#Remove US_ prefix from every column name besides "Date" and "US"
CGRT.economic_support <- CGRT.economic_support %>%
  rename_with(~gsub("US_", "", .x)) %>%
  #Create rows equal to zero for days between 2019-10-06 and 2019-12-31
  add_row(Date = seq(as.Date("2019-10-06"), as.Date("2019-12-31"), by = "day"), .before = 1)
CGRT.economic_support[1:87, 2:53] <- "0"
# Change all case numbers in columns 2 to 53 from character to numeric
CGRT.economic_support[, 2:53] <- sapply(CGRT.economic_support[, 2:53], as.numeric)

############################ X3 ############################
CGRT.cases <- read_excel(filename, sheet = 33) %>%
  filter(country_code == "USA") %>%
  select(-c(1, 2, 4, 5))
CGRT.cases[1,1] <- "US"
CGRT.cases[, 2:1097][is.na(CGRT.cases[, 2:1097])] <- 0
CGRT.cases <- CGRT.cases %>%
  discard(~all(is.na(.) | . == ""))
CGRT.cases <- t(CGRT.cases) %>%
  row_to_names(row_number = 1) %>%
  as_tibble(rownames = "Date")
# Convert the first column (Date column) to Date object
CGRT.cases$Date <- dmy(CGRT.cases$Date, quiet = TRUE)
# Parse the Date column
CGRT.cases$Date <- as_date(CGRT.cases$Date)
# Remove US_ prefix from every column name besides "Date" and "US"
CGRT.cases <- CGRT.cases %>%
  rename_with(~gsub("US_", "", .x)) %>%
  #Create rows equal to zero for days between 2019-10-06 and 2019-12-31
  add_row(Date = seq(as.Date("2019-10-06"), as.Date("2019-12-31"), by = "day"), .before = 1)
# Change NA to "0"
CGRT.cases[1:87, 2:53] <- "0"
# Change all case numbers in columns 2 to 53 from character to numeric
CGRT.cases[, 2:53] <- sapply(CGRT.cases[, 2:53], as.numeric)

file.remove(filename) # removes the temporary file
rm(filename, url) # removes the filename and url variables

# Tidy the datasets
CGRT.stringency <- gather(CGRT.stringency, key = "Region", value = "Stringency", -Date)
CGRT.economic_support <- gather(CGRT.economic_support, key = "Region", value = "Economic_Support", -Date)
CGRT.cases <- gather(CGRT.cases, key = "Region", value = "Cases", -Date)

# Join CGRT
CGRT <- left_join(CGRT.stringency, CGRT.economic_support, by = c("Region" = "Region", "Date" = "Date"))
CGRT <- left_join(CGRT, CGRT.cases, by = c("Region" = "Region", "Date" = "Date"))
rm(CGRT.stringency, CGRT.economic_support, CGRT.cases)

# Backup in daily format
CGRT2 <- CGRT

CGRT <- CGRT %>%
  mutate(Date = lubridate::floor_date(Date, "week")) %>% 
  group_by(Region, Date) %>% 
  summarise_at(vars(Stringency, Economic_Support, Cases), ~ mean(., na.rm = TRUE))

# Create 4-week, 8-week, and 12-week lagged variables for Stringency, Economic_Support, and Cases
CGRT <- CGRT %>%
  mutate(Stringency_4wklag = dplyr::lag(Stringency, 4, default = 0),
         Stringency_8wklag = dplyr::lag(Stringency, 8, default = 0),
         Stringency_12wklag = dplyr::lag(Stringency, 12, default = 0),
         Economic_Support_4wklag = dplyr::lag(Economic_Support, 4, default = 0),
         Economic_Support_8wklag = dplyr::lag(Economic_Support, 8, default = 0),
         Economic_Support_12wklag = dplyr::lag(Economic_Support, 12, default = 0),
         Cases_4wklag = dplyr::lag(Cases, 4, default = 0),
         Cases_8wklag = dplyr::lag(Cases, 8, default = 0),
         Cases_12wklag = dplyr::lag(Cases, 12, default = 0)) %>%
  # If date is before 2020-01-01, set all lagged variables to 0
  mutate(Stringency_4wklag = if_else(Date < as.Date("2020-01-01"), 0, Stringency_4wklag),
         Stringency_8wklag = if_else(Date < as.Date("2020-01-01"), 0, Stringency_8wklag),
         Stringency_12wklag = if_else(Date < as.Date("2020-01-01"), 0, Stringency_12wklag),
         Economic_Support_4wklag = if_else(Date < as.Date("2020-01-01"), 0, Economic_Support_4wklag),
         Economic_Support_8wklag = if_else(Date < as.Date("2020-01-01"), 0, Economic_Support_8wklag),
         Economic_Support_12wklag = if_else(Date < as.Date("2020-01-01"), 0, Economic_Support_12wklag),
         Cases_4wklag = if_else(Date < as.Date("2020-01-01"), 0, Cases_4wklag),
         Cases_8wklag = if_else(Date < as.Date("2020-01-01"), 0, Cases_8wklag),
         Cases_12wklag = if_else(Date < as.Date("2020-01-01"), 0, Cases_12wklag))

# count NA values in each column, to assure NA values are only in lag columns
colSums(is.na(CGRT))
# For NA values of lags, replace with 0
CGRT[is.na(CGRT)] <- 0
# I want to categorize each region as having high or low stringency and high or low economic support, with equal size of each category.
# I will do this by first finding each region's mean stringency and economic support

Stringency_mean <- aggregate(Stringency ~ Region, data = CGRT, FUN = mean)
Economic_Support_mean <- aggregate(Economic_Support ~ Region, data = CGRT, FUN = mean)
# Change colnames to be Stringency_mean and Economic_Support_mean
colnames(Stringency_mean) <- c("Region", "Stringency_mean")
colnames(Economic_Support_mean) <- c("Region", "Economic_Support_mean")


# Then I will merge the mean values with the CGRT dataset
CGRT <- merge(CGRT, Stringency_mean, by = "Region")
CGRT <- merge(CGRT, Economic_Support_mean, by = "Region")
CGRT <- CGRT %>%
  filter(Region == "AL" | Region == "AZ" | Region == "AR" | Region == "CA" | Region == "CO" | Region == "CT" | Region == "FL" | Region == "GA" | Region == "IL" | Region == "IN" | Region == "IA" | Region == "KS" | Region == "KY" | Region == "LA" | Region == "ME" | Region == "MD" | Region == "MA" | Region == "MI" | Region == "MN" | Region == "MS" | Region == "MO" | Region == "NE" | Region == "NV" | Region == "NH" | Region == "NM" | Region == "NY" | Region == "NC" | Region == "OH" | Region == "OK" | Region == "OR" | Region == "PA" | Region == "RI" | Region == "SC" | Region == "SD" | Region == "TN" | Region == "TX" | Region == "UT" | Region == "VT" | Region == "VA" | Region == "WA" | Region == "WV" | Region == "WI" | Region == "WY" | Region == "US")
# Find median of mean stringency and mean economic support
Stringency_mean_median <- median(CGRT$Stringency_mean)
Economic_Support_mean_median <- median(CGRT$Economic_Support_mean)
# Count the number of regions with mean stringency above and below the median of mean stringency
sum(CGRT$Stringency_mean > Stringency_mean_median)
sum(CGRT$Stringency_mean < Stringency_mean_median)
sum(CGRT$Economic_Support_mean > Economic_Support_mean_median)
sum(CGRT$Economic_Support_mean < Economic_Support_mean_median)
# Create high and low stringency and economic support categories, EXCEPT FOR US
CGRT <- CGRT %>%
  mutate(Stringency_high = ifelse(Stringency_mean > Stringency_mean_median & Region != "US", 1, 0),
         Stringency_low = ifelse(Stringency_mean < Stringency_mean_median & Region != "US", 1, 0),
         Economic_Support_high = ifelse(Economic_Support_mean > Economic_Support_mean_median & Region != "US", 1, 0),
         Economic_Support_low = ifelse(Economic_Support_mean < Economic_Support_mean_median & Region != "US", 1, 0))
# Check for balance
sum(CGRT$Stringency_high)
sum(CGRT$Stringency_low)
sum(CGRT$Economic_Support_high)
sum(CGRT$Economic_Support_low)
# NOTE: These indicators are not perfectly balanced for ALL states, but they are for the states that are in both the CGRT and Retail datasets
#  Must be "AL" "AZ" "AR" "CA" "CO" "CT" "FL" "GA" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "NE" "NV" "NH" "NM" "NY" "NC" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY"

# CHANGE CGRT TO CGRT2 TO GET THE DAILY DATA. MOVE THIS BEFORE FILTERING FOR STATES TO GET ALL STATES WITH WEEKLY DATA
# Save RData file
save(CGRT, file = "Data/CGRT.RData")
# Create CSV file
write.csv(CGRT, "Data/CGRT.csv", row.names = FALSE)
# Create XLSX file
write.xlsx(CGRT, "Data/CGRT.xlsx", overwrite = TRUE)

# Clear environment
rm(list = ls())

# CITATION
# Thomas Hale, Noam Angrist, Rafael Goldszmidt, Beatriz Kira, Anna Petherick, Toby Phillips, Samuel Webster, Emily Cameron-Blake, Laura Hallas, Saptarshi Majumdar, and Helen Tatlow. (2021). “A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker).” Nature Human Behaviour. https://doi.org/10.1038/s41562-021-01079-8

# All credit for the initial data goes to the authors of the paper and project above. I am not affiliated with the authors of the paper or project in any way.
# The data is licensed under a Creative Commons Attribution 4.0 International License (CC BY 4.0). https://creativecommons.org/licenses/by/4.0/

# My specific contribution was to automatiate the tidying the data in a weekly format using this R script. This was done as part of my Honors Thesis, which may be found on my GitHub page.
# I make this R script publicly available to make the data easier for others to use. Best of luck to you!
# You are free to use this script for any purposes allowed by the original license, as long as you cite the authors of the paper and project above.
# Please cite Eric Trueswell as the author of this script, with a link to this GitHub repository.

# This script is provided as is, with no warranty or guarantee of any kind. I am not responsible for any errors or omissions in the data. Please contact the authors of the paper or project if you have any questions or concerns.

