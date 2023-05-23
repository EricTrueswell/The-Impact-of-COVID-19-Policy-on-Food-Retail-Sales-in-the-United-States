library(plyr)
library(tidyverse)
library(readxl)
library(janitor)
library(tibble) # Load in CGRT.RData and retail.R
load("Data/CGRT.RData")
load("Data/retail.RData")
load("Data/POP.RData")
load("Data/USPOP.RData")

CGRT_retail <- join(retail.foodsubcategory, CGRT, by = c("Region" = "Region", "Date" = "Date"))

save(CGRT_retail, file = "Data/CGRT_retail.RData")

CGRT_retail_state <- CGRT_retail %>%
  filter(Region != "US") %>%
  # Join POP data, where population "year" is the year of the "Date" column
  mutate(year = year(Date)) %>%
  left_join(POP, by = c("Region" = "Region", "year" = "year")) %>%
  # Remove year column
  select(-year)
CGRT_retail_state <- CGRT_retail_state %>%
  # Group by Region and Date
  group_by(Region,Date) %>%
  # Create Cases_per_100000 column
  mutate(Cases_per_100000 = Cases / population * 100000) %>%
  # Create New_Cases column
  mutate(New_Cases = (Cases - dplyr::lag(Cases, 1, default = 0))) %>%
  # If New_Cases is before 2020-01-01, set to 0
  mutate(New_Cases = ifelse(Date < as.Date("2020-01-01"), 0, New_Cases)) %>%
  # If New_Cases is negative, set to 0
  mutate(New_Cases = ifelse(New_Cases < 0, 0, New_Cases)) %>%
  # Create New_Cases_per_100000 column
  mutate(New_Cases_per_100000 = New_Cases / population * 100000) %>%
  # If New_Cases_per_100000 is before 2020-01-01, set to 0
  mutate(New_Cases_per_100000 = ifelse(Date < as.Date("2020-01-01"), 0, New_Cases_per_100000)) %>%
  # If New_Cases_per_100000 is negative, set to 0
  mutate(New_Cases_per_100000 = ifelse(New_Cases_per_100000 < 0, 0, New_Cases_per_100000)) %>%
  # Create Dollars_per_100000 column
  mutate(Dollars_per_100000 = Dollars / population * 100000) %>%
  # Create Unit_sales_per_100000 column
  mutate(Unit_sales_per_100000 = Unit_sales / population * 100000) %>%
  # Create Volume_sales_per_100000 column
  mutate(Volume_sales_per_100000 = Volume_sales / population * 100000) %>%
  # Create Dollars_per_100000 last year column
  mutate(Dollars_per_100000_last_year = (Dollars_last_year / population_last_year) * 100000) %>%
  # Create Unit_sales_per_100000 last year column
  mutate(Unit_sales_per_100000_last_year = Unit_sales_last_year / population_last_year * 100000) %>%
  # Create Volume_sales_per_100000 last year column
  mutate(Volume_sales_per_100000_last_year = Volume_sales_last_year / population_last_year * 100000) %>%
  #ungroup
  ungroup()

# Change name of "Region" column to "State"
names(CGRT_retail_state)[names(CGRT_retail_state) == "Region"] <- "State"

# Create total sales variable for each state, equal to sum of sales of Alcohol; Beverages; Dairy; Fats and Oils; Fruits; Grains; Meats, eggs, and nuts; Other; Sugar and sweeteners; and Vegetables for that given state and week
CGRT_retail_state_total <- CGRT_retail_state %>%
  dplyr::group_by(State, Date) %>%
  dplyr::summarise(Dollars_total = sum(Dollars), Unit_sales_total = sum(Unit_sales), Volume_sales_total = sum(Volume_sales), Dollars_total_last_year = sum(Dollars_last_year), Unit_sales_total_last_year = sum(Unit_sales_last_year), Volume_sales_total_last_year = sum(Volume_sales_last_year)) %>%
  ungroup()

# Join to CGRT_retail_state
CGRT_retail_state <- CGRT_retail_state %>%
  left_join(CGRT_retail_state_total, by = c("State" = "State", "Date" = "Date")) %>%
  # Create Dollars_proportion_total column
  mutate(Dollars_proportion_total = Dollars / Dollars_total) %>%
  # Create Unit_sales_proportion_total column
  mutate(Unit_sales_proportion_total = Unit_sales / Unit_sales_total) %>%
  # Create Volume_sales_proportion_total column
  mutate(Volume_sales_proportion_total = Volume_sales / Volume_sales_total) %>%
  # Create Dollars_proportion_total_last_year column
  mutate(Dollars_proportion_total_last_year = Dollars_last_year / Dollars_total_last_year) %>%
  # Create Unit_sales_proportion_total_last_year column
  mutate(Unit_sales_proportion_total_last_year = Unit_sales_last_year / Unit_sales_total_last_year) %>%
  # Create Volume_sales_proportion_total_last_year column
  mutate(Volume_sales_proportion_total_last_year = Volume_sales_last_year / Volume_sales_total_last_year) 


save(CGRT_retail_state, file = "Data/CGRT_retail_state.RData")

CGRT_retail_US <- CGRT_retail %>%
  filter(Region == "US") %>%
  # Join USPOP data, where population "year" is the year of the "Date" column and "month" is the month of the "Date" column
  mutate(year = year(Date)) %>%
  mutate(month = month(Date)) %>%
  left_join(USPOP, by = c("year" = "year", "month" = "month")) %>%
  # Remove year and month columns
  select(-year, -month) %>%
  # Create Cases_per_100000 column
  mutate(Cases_per_100000 = Cases / population * 100000) %>%
  # Create New_Cases column
  mutate(New_Cases = (Cases - dplyr::lag(Cases, 1, default = 0))) %>%
  # If New_Cases is before 2020-01-01, set to 0
  mutate(New_Cases = ifelse(Date < as.Date("2020-01-01"), 0, New_Cases)) %>%
  # If New_Cases is negative, set to 0
  mutate(New_Cases = ifelse(New_Cases < 0, 0, New_Cases)) %>%
  # Create New_Cases_per_100000 column
  mutate(New_Cases_per_100000 = (Cases - dplyr::lag(Cases, 1, default = 0)) / population * 100000) %>%
  # If New_Cases_per_100000 is before 2020-01-01, set to 0
  mutate(New_Cases_per_100000 = ifelse(Date < as.Date("2020-01-01"), 0, New_Cases_per_100000)) %>%
  # If New_Cases_per_100000 is negative, set to 0
  mutate(New_Cases_per_100000 = ifelse(New_Cases_per_100000 < 0, 0, New_Cases_per_100000)) %>%
  # Create Dollars_per_100000 column
  mutate(Dollars_per_100000 = Dollars / population * 100000) %>%
  # Create Unit_sales_per_100000 column
  mutate(Unit_sales_per_100000 = Unit_sales / population * 100000) %>%
  # Create Volume_sales_per_100000 column
  mutate(Volume_sales_per_100000 = Volume_sales / population * 100000)
save(CGRT_retail_US, file = "Data/CGRT_retail_US.RData")