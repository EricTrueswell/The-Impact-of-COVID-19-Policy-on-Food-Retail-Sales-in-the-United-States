library(tidyverse)
library(lubridate)
library(openxlsx)
library(fredr)
source("API.R")

# Alabama
ALPOP <- fredr(series_id = "ALPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    #Create Region ID by cutting "ALPOP" in series_id to "AL"
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    # create a new column "year" from "date"
    mutate(year = year(date))

# Alaska
AKPOP <- fredr(series_id = "AKPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Arizona
AZPOP <- fredr(series_id = "AZPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Arkansas
ARPOP <- fredr(series_id = "ARPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# California
CAPOP <- fredr(series_id = "CAPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Colorado
COPOP <- fredr(series_id = "COPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Connecticut
CTPOP <- fredr(series_id = "CTPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Delaware
DEPOP <- fredr(series_id = "DEPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Florida
FLPOP <- fredr(series_id = "FLPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Georgia
GAPOP <- fredr(series_id = "GAPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Hawaii
HIPOP <- fredr(series_id = "HIPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Idaho
IDPOP <- fredr(series_id = "IDPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Illinois
ILPOP <- fredr(series_id = "ILPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Indiana
INPOP <- fredr(series_id = "INPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Iowa
IAPOP <- fredr(series_id = "IAPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Kansas
KSPOP <- fredr(series_id = "KSPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Kentucky
KYPOP <- fredr(series_id = "KYPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Louisiana
LAPOP <- fredr(series_id = "LAPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Maine
MEPOP <- fredr(series_id = "MEPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Maryland
MDPOP <- fredr(series_id = "MDPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Massachusetts
MAPOP <- fredr(series_id = "MAPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Michigan
MIPOP <- fredr(series_id = "MIPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Minnesota
MNPOP <- fredr(series_id = "MNPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Mississippi
MSPOP <- fredr(series_id = "MSPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Missouri
MOPOP <- fredr(series_id = "MOPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Montana
MTPOP <- fredr(series_id = "MTPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Nebraska
NEPOP <- fredr(series_id = "NEPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Nevada
NVPOP <- fredr(series_id = "NVPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# New Hampshire
NHPOP <- fredr(series_id = "NHPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# New Jersey
NJPOP <- fredr(series_id = "NJPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# New Mexico
NMPOP <- fredr(series_id = "NMPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# New York
NYPOP <- fredr(series_id = "NYPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# North Carolina
NCPOP <- fredr(series_id = "NCPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# North Dakota
NDPOP <- fredr(series_id = "NDPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Ohio
OHPOP <- fredr(series_id = "OHPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Oklahoma
OKPOP <- fredr(series_id = "OKPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Oregon
ORPOP <- fredr(series_id = "ORPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Pennsylvania
PAPOP <- fredr(series_id = "PAPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Rhode Island
RIPOP <- fredr(series_id = "RIPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# South Carolina
SCPOP <- fredr(series_id = "SCPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# South Dakota
SDPOP <- fredr(series_id = "SDPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Tennessee
TNPOP <- fredr(series_id = "TNPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Texas
TXPOP <- fredr(series_id = "TXPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Washington DC
DCPOP <- fredr(series_id = "DCPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Utah
UTPOP <- fredr(series_id = "UTPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Vermont
VTPOP <- fredr(series_id = "VTPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Virginia
VAPOP <- fredr(series_id = "VAPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Washington
WAPOP <- fredr(series_id = "WAPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# West Virginia
WVPOP <- fredr(series_id = "WVPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Wisconsin
WIPOP <- fredr(series_id = "WIPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# Wyoming
WYPOP <- fredr(series_id = "WYPOP", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = str_sub(series_id, 1, 2)) %>%
    mutate(year = year(date))

# All US
USPOP <- fredr(series_id = "POPTHM", observation_start = as.Date("2018-01-01"), observation_end = as.Date("2022-12-31")) %>%
    mutate(Region = "US") %>%
    mutate(year = year(date)) %>%
    mutate(month = month(date))

# Combine all state population data into one data frame
POP <- bind_rows(ALPOP, AKPOP, AZPOP, ARPOP, CAPOP, COPOP, CTPOP, DEPOP, FLPOP, GAPOP, HIPOP, IDPOP, ILPOP, INPOP, IAPOP, KSPOP, KYPOP, LAPOP, MEPOP, MDPOP, MAPOP, MIPOP, MNPOP, MSPOP, MOPOP, MTPOP, NEPOP, NVPOP, NHPOP, NJPOP, NMPOP, NYPOP, NCPOP, NDPOP, OHPOP, OKPOP, ORPOP, PAPOP, RIPOP, SCPOP, SDPOP, TNPOP, TXPOP, DCPOP, UTPOP, VTPOP, VAPOP, WAPOP, WVPOP, WIPOP, WYPOP)

POP <- POP %>%
    mutate(population = value) %>%
    # Reorder columns
    select(Region, year, population) %>%
    # population is in thousands. Convert to regular units
    mutate(population = population * 1000) %>%
    group_by(Region, year) %>%
    # Create population_last_year column
    mutate(population_last_year = dplyr::lag(population, 1)) %>%
    # if year is 2018, set population_last_year to population
    mutate(population_last_year = ifelse(year == 2018, population, population_last_year)) %>%
    # if population_last_year is NA, set it to population
    mutate(population_last_year = ifelse(is.na(population_last_year), population, population_last_year)) %>%
    ungroup()
# Explanation: If the year is 2018, then there is no previous year population data. So, the best approximation is to set the previous year population to the current year population.

USPOP <- USPOP %>%
    mutate(population = value) %>%
    select(Region, year, month, population) %>%
    group_by(Region, year, month) %>%
    # population is in thousands. Convert to regular units
    mutate(population = population * 1000) %>%
    # Create population_last_month column
    mutate(population_last_month = dplyr::lag(population, 1)) %>%
    # Create population_last_year column
    mutate(population_last_year = dplyr::lag(population, 12)) %>%
    # if population_last_month is NA, set it to population
    mutate(population_last_month = ifelse(is.na(population_last_month), population, population_last_month)) %>%
    ungroup()

# Remove individual state data frames
rm(ALPOP, AKPOP, AZPOP, ARPOP, CAPOP, COPOP, CTPOP, DEPOP, FLPOP, GAPOP, HIPOP, IDPOP, ILPOP, INPOP, IAPOP, KSPOP, KYPOP, LAPOP, MEPOP, MDPOP, MAPOP, MIPOP, MNPOP, MSPOP, MOPOP, MTPOP, NEPOP, NVPOP, NHPOP, NJPOP, NMPOP, NYPOP, NCPOP, NDPOP, OHPOP, OKPOP, ORPOP, PAPOP, RIPOP, SCPOP, SDPOP, TNPOP, TXPOP, DCPOP, UTPOP, VTPOP, VAPOP, WAPOP, WVPOP, WIPOP, WYPOP)

# Save to CSV
write.csv(POP, file = "Data/POP.csv", row.names = FALSE)
write.csv(USPOP, file = "Data/USPOP.csv", row.names = FALSE)
# Save to RData
save(POP, file = "Data/POP.RData")
save(USPOP, file = "Data/USPOP.RData")
# Save to Excel
write.xlsx(POP, file ="Data/POP.xlsx")
write.xlsx(USPOP, file ="Data/USPOP.xlsx")