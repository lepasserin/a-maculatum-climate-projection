# Purpose: Clean and Validate BLISS Study Datasets.
# Last Updated: Aug 27, 2026
# Author: Benedict Cummins-Mburu.
# Contact: b.cumminsmburu@mail.utoronto.ca.

# ------ Setup -------
library(tidyverse)
library(readxl)
BLISS_study_climate_indices <- read_csv(
  "data/raw_BLISS_data/Unstandardized_INB.T_OUT.P.csv"
)
BLISS_study_climate_indices <- BLISS_study_climate_indices[
  complete.cases(BLISS_study_climate_indices),
]
BLISS_study_other_variables <- read_excel(
  "data/raw_BLISS_data/BLISS_2008_2022_study_variables.xlsx"
)

# ----- Cleaning ------

# 0. Verify `Year` is a valid key in both datasets.
if (all(BLISS_study_climate_indices$Year == 2008:2022)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}
if (all(BLISS_study_other_variables$Year == 2008:2022)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}

# 1. Standardize Structure
BLISS_study_data <- BLISS_study_climate_indices %>%
  left_join(BLISS_study_other_variables, by = "Year") %>%
  mutate(
    year = Year,
    breedingSeasonStart = `Ice.Out.Ordinal`,
    breedingSeasonEnd = `Peak.Egg.Mass.Ordinal`,
    breedingSeasonLength = breedingSeasonEnd - breedingSeasonStart + 1,
    tempIndex = T.INB,
    precipIndex = P.OUT,
  ) %>%
  select(
    year,
    breedingSeasonStart,
    breedingSeasonEnd,
    breedingSeasonLength,
    tempIndex,
    precipIndex,
  )

# ------ Save -------
write_csv(BLISS_study_data, "data/BLISS_data.csv")
