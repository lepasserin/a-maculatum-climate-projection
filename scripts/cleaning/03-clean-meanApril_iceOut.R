# Purpose: Create Dataset Allowing for the Visualization of the Correlation between Bat Lake Ice Out Date and Mean April Temperature.
# Last Updated: Sep 9, 2026
# Author: Benedict Cummins-Mburu.
# Contact: b.cumminsmburu@mail.utoronto.ca.
# Note: THIS FILE IS CURRENTLY UNUSED IN THE PIPELINE.

# -------- Setup ---------
library(tidyverse)
AEG_historic_climate <- read_csv(
  "data/climate_data/clean_data/AEG_historic_climate.csv"
)
BLISS_data <- read_csv("data/BLISS_data.csv")


thing <- AEG_historic_climate %>%
  filter(year %in% 2008:2022) %>%
  filter(month == "Apr") %>%
  group_by(year) %>%
  summarise(meanAprilTemp = mean(tas, na.rm = TRUE)) %>%
  left_join(BLISS_data, by = "year") %>%
  select(meanAprilTemp, breedingSeasonStart)

thing2 <- lm(breedingSeasonStart ~ meanAprilTemp, data = thing)

cor(thing$meanAprilTemp, thing$breedingSeasonStart)
