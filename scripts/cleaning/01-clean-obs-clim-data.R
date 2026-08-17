# Purpose: Clean and Validate Algonquin East Gate Climate Dataset.
# Last Updated: Aug 16, 2026
# Author: Benedict Cummins-Mburu.
# Contact: b.cumminsmburu@mail.utoronto.ca.
# Note:
#    - Resulting dataset tracks climate info for most spring days (Mar/Apr/May) from 2005 - 2024 inclusive

# ------ Setup -------
library(tidyverse)
AlgonquinEastGateClimateData <- read_csv(
  "data/climate_data/raw_data/Algonquin East Gate Climate Data.csv"
)

# ----- Constants ------

FOCAL_COLUMNS <- c(
  "LOCAL_DATE", # date
  "LOCAL_DAY", # day
  "LOCAL_MONTH", # month
  "LOCAL_YEAR", # year
  "MIN_TEMPERATURE", # tasmin
  "MEAN_TEMPERATURE", # tas
  "TOTAL_PRECIPITATION" # pr
)

START_DATE <- as.Date("2005-03-01")
END_DATE <- as.Date("2024-05-31")
EXPECTED_LENGTH <- 1840

# ----- Cleaning ------

# 1. Filter and rename columns.
cleaned_01 <- AlgonquinEastGateClimateData %>%
  select(all_of(FOCAL_COLUMNS)) %>%
  filter(!is.na(LOCAL_DATE)) %>%
  mutate(
    date = as.Date(LOCAL_DATE),
    day = LOCAL_DAY,
    month = case_when(
      LOCAL_MONTH == 3 ~ "Mar",
      LOCAL_MONTH == 4 ~ "Apr",
      LOCAL_MONTH == 5 ~ "May"
    ),
    year = LOCAL_YEAR,
    tasmin = MIN_TEMPERATURE,
    tas = MEAN_TEMPERATURE,
    pr = TOTAL_PRECIPITATION
  )

# 2. Validate composed vs. decomposed dates.
c0 <- all(day(cleaned_01$date) == cleaned_01$day)
c1 <- all(month(cleaned_01$date) == cleaned_01$LOCAL_MONTH)
c2 <- all(year(cleaned_01$date) == cleaned_01$year)
if (c0 & c1 & c2) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: The date vs. the day, month, year columns do not agree."
  )
}

# 3. Filter rows where month is NA.
cleaned_03 <- cleaned_01 %>%
  filter(!is.na(month)) %>%
  select(date, day, month, year, tas, tasmin, pr)

# 4. Add in rows for dates missing from the dataset.
all_dates <- data.frame(
  date = seq(START_DATE, END_DATE, by = "day")
) %>%
  filter(month(date) %in% 3:5)
cleaned_04 <- all_dates %>%
  left_join(cleaned_03, by = "date") %>%
  mutate(
    day = as.numeric(mday(date)),
    month = month.abb[month(date)],
    year = as.numeric(year(date))
  ) %>%
  arrange(date) %>%
  mutate(complete = !if_any(c(tas, tasmin, pr), is.na)) %>%
  mutate(empty = if_all(c(tas, tasmin, pr), is.na))

# 4. Post-validation.
if (
  nrow(cleaned_04) == length(unique(cleaned_04$date)) &&
    EXPECTED_LENGTH == nrow(cleaned_04)
) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: dates are not unique.")
}

# ------ Save Data ------
AEG_historic_climate <- cleaned_04
write_csv(
  AEG_historic_climate,
  "data/climate_data/clean_data/AEG_historic_climate.csv"
)
