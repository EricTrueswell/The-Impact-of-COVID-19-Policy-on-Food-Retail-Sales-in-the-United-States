############## Environment Setup ##############
library(dplyr)
library(purrr)
library(ggplot2)
library(readxl)
library(lubridate)
library(janitor)
library(tibble)
library(tidyr)
library(plyr)
library(openxlsx)
source("API.R")

############################ Y ############################
# Setup of Y: https://www.ers.usda.gov/data-products/weekly-retail-food-sales/
# https://www.ers.usda.gov/webdocs/DataFiles/100189/NationalTotalAndSubcategory.csv?v=7375.7 CSV IF EXCEL DOESNT WORK
url <- "https://www.ers.usda.gov/webdocs/DataFiles/100189/NationalTotalAndSubcategory.xlsx?v=7375.7"
filename <- tempfile() # creates a temporary file name
download.file(url, destfile = filename, mode = "wb")

us.retail.allfood <- read_excel(filename, sheet = 1) %>%
  row_to_names(row_number = 1)
us.retail.allfood <- us.retail.allfood[-c(169:172), ]
# Convert the first column (Date column) to Date object
us.retail.allfood$Date <- ymd(us.retail.allfood$Date, quiet = TRUE)
# Parse the Date column
us.retail.allfood$Date <- as_date(us.retail.allfood$Date)
# Add Region code column (US). Set as as the column after "Date"
us.retail.allfood <- us.retail.allfood %>%
  mutate(Region = "US") %>%
  select(Region, everything())
  # Rename colnames to use _ instead of spaces
  colnames(us.retail.allfood) <- gsub(" ", "_", colnames(us.retail.allfood))
us.retail.allfood <- us.retail.allfood %>%
  select(-contains("3_years")) %>%
  # Change variables from character to numeric
  mutate_at(vars(3:11), as.numeric)
# Remove rows that are entirely NA
us.retail.allfood <- us.retail.allfood[complete.cases(us.retail.allfood), ]

us.retail.foodsubcategory <- read_excel(filename, sheet = 2) %>%
  row_to_names(row_number = 1)
us.retail.foodsubcategory <- us.retail.foodsubcategory[-c(9073:9076), ]
# Convert the first column (Date column) to Date object
us.retail.foodsubcategory$Date <- ymd(us.retail.foodsubcategory$Date, quiet = TRUE)
# Parse the Date column
us.retail.foodsubcategory$Date <- as_date(us.retail.foodsubcategory$Date)
# Add Region code column (US). Set as as column 2
us.retail.foodsubcategory <- us.retail.foodsubcategory %>%
  mutate(Region = "US") %>%
  select(Region, everything())
# Rename colnames to use _ instead of spaces
colnames(us.retail.foodsubcategory) <- gsub(" ", "_", colnames(us.retail.foodsubcategory))
us.retail.foodsubcategory <- us.retail.foodsubcategory %>%
  # Drop cols with colnames containing "3_years"
  select(-contains("3_years")) %>%
  # Change variables from character to numeric
  mutate_at(vars(5:16), as.numeric)

file.remove(filename) # removes the temporary file
rm(filename, url) # removes the filename and url variables

# https://www.ers.usda.gov/webdocs/DataFiles/100189/StateAndCategory.csv?v=7375.7 CSV IF EXCEL DOESNT WORK
# Load the excel file from the USDA website

url <- "https://www.ers.usda.gov/webdocs/DataFiles/100189/StateAndCategory.xlsx?v=7375.7"
filename <- tempfile() # creates a temporary file name
download.file(url, destfile = filename, mode = "wb")
state.retail.foodsubcategory <- tryCatch({
  read_excel(filename, sheet = 1) %>%
    row_to_names(row_number = 1)
}, error = function(e) {
  file.remove(filename) # removes the temporary file
  rm(filename, url) # removes the filename and url variables
  stop("Failed to download and read the file from the USDA website.")
}) %>%
  # Convert the first column (Date column) to Date object
  mutate(Date = ymd(Date, quiet = TRUE)) %>%
  # Parse the Date column
  mutate(Date = as_date(Date))
# Convert state names in the State column to state abbreviations (ex. Alabama to AL)
state.retail.foodsubcategory$State <- state.abb[match(state.retail.foodsubcategory$State, state.name)]
# Rename State column to Region
colnames(state.retail.foodsubcategory)[colnames(state.retail.foodsubcategory) == 'State'] <- 'Region'
state.retail.foodsubcategory <- state.retail.foodsubcategory %>%
  select(Region, Date, everything())
# Rename colnames to use _ instead of spaces
colnames(state.retail.foodsubcategory) <- gsub(" ", "_", colnames(state.retail.foodsubcategory))
state.retail.foodsubcategory <- state.retail.foodsubcategory %>%
    # Drop cols with colnames containing "3_years"
  select(-contains("3_years")) %>%
  # Change variables from character to numeric
  mutate_at(vars(4:12), as.numeric)
# Remove rows that are entirely NA
state.retail.foodsubcategory <- state.retail.foodsubcategory[complete.cases(state.retail.foodsubcategory), ]

file.remove(filename) # removes the temporary file
rm(filename, url) # removes the filename and url variables


# rbind us.retail.foodsubcategory and state.retail.foodsubcategory
retail.foodsubcategory <- rbind.fill(us.retail.foodsubcategory, state.retail.foodsubcategory)


# For a combination of state and national-level retail data by food subcategory:
    # retail.foodsubcategory

# For state-level retail data by food subcategory:
    # state.retail.foodsubcategory

# For national-level retail data by food subcategory:
    # us.retail.foodsubcategory

# For national-level retail data on aggregate:
    # us.retail.allfood

# Save RData file
save(retail.foodsubcategory, file = "Data/retail.RData")
# Create CSV file
write.csv(retail.foodsubcategory, "Data/retail.csv", row.names = FALSE)
# Create XLSX file
write.xlsx(retail.foodsubcategory, "Data/retail.xlsx", overwrite = TRUE)


# Clear environment
rm(list = ls())
