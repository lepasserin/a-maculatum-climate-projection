# Purpose: Forecast breeding interval, climate indices, and survival based on climate forecasts.
# Last Updated: Aug 24, 2026
# Author: Benedict Cummins-Mburu.
# Contact: b.cumminsmburu@mail.utoronto.ca.

# ------- Setup -------
library(tidyverse)
library(readxl)
climate_projections <- read_csv(
  "data/climate_data/clean_data/ECCC_climate_projections_BC.csv"
)
BLISS_study_data <- read_csv("data/BLISS_data.csv")

# ----- Constants -----

# calculation constants
PRECIP_INTERVAL_RADIUS <- 10
MEAN_LENGTH <- round(mean(
  BLISS_study_data$breedingSeasonLength,
  na.rm = TRUE
))
OBS_T_MEAN <- mean(BLISS_study_data$tempIndex)
OBS_T_SD <- sd(BLISS_study_data$tempIndex)
OBS_P_MEAN <- mean(BLISS_study_data$precipIndex)
OBS_P_SD <- sd(BLISS_study_data$precipIndex)

# regression coefficients
# SOURCE: Dylan personal regression and model fitting
B0_APRIL <- 125.5190
B1_APRIL <- -3.1373
B0_SURVIVAL <- 0.9656941
B1_SURVIVAL <- -0.762042
B2_SURVIVAL <- 0.4460179

# ------ Helpers ------

# 1. fetch mean April temperature for a given `yr`, `scen`
get_meanAprilTemp <- function(yr, scen, dataType = "ADJ") {
  focalData <- climate_projections %>%
    filter(scenario == scen, year == yr, month == "Apr")
  if (dataType == "ADJ") {
    return(mean(focalData$tasBC))
  }
  if (dataType == "SIM") {
    return(mean(focalData$tas))
  }
  stop("Error: {get_meanAprilTemp_from_EC}: invalid dataType entered.")
}

# 2. estimate breeding start (i.e. Bat Lake ice out) date from mean April temperature.
get_iceOut_from_meanAprilTemp <- function(meanAprilTemp) {
  return(round((B1_APRIL * meanAprilTemp) + B0_APRIL))
}

# 3. calculate breeding end (i.e. peak egg mass) date from breeding start date.
# ASSUMPTION: peak egg mass always occurs 22 days after ice out
get_peakEggMass_from_iceOut <- function(iceOut) {
  return(iceOut + mean_breedingSeasonLength)
}

# 4. calculate climate indices for a given `yr`, `scen`,
get_climateIndices <- function(yr, scen, iceOut, eggMass, dataType = "ADJ") {
  tempInterval <- climate_projections %>%
    filter(
      year == yr,
      scenario == scen,
      ordinalDay >= iceOut,
      ordinalDay <= eggMass
    )
  precipInterval <- climate_projections %>%
    filter(
      year == yr,
      scenario == scen,
      ordinalDay >= eggMass - PRECIP_INTERVAL_RADIUS,
      ordinalDay <= eggMass + PRECIP_INTERVAL_RADIUS
    )
  # in filtered window, calculate indicators: ADJ
  if (dataType == "ADJ") {
    temp <- mean(tempInterval$tasminBC)
    wetDays <- precipInterval %>% filter(prBC > 0.0)
    if (nrow(wetDays) == 0) {
      precip <- 0
      warning(
        "Warning: {get_climateIndices}: a season was found with no wet days."
      )
    } else {
      precip <- sum(precipInterval$prBC) /
        (nrow(wetDays) * nrow(precipInterval))
    }
    return(c(temp, precip))
  }
  # in filtered window, calculate indicators: SIM
  if (dataType == "SIM") {
    temp <- mean(tempInterval$tasmin)
    wetDays <- precipInterval %>% filter(pr > 0.0)
    if (nrow(wetDays) == 0) {
      precip <- 0
      warning(
        "{get_climateIndices}: WARNING: a season was found with no wet days."
      )
    } else {
      precip <- sum(precipInterval$pr) / (nrow(wetDays) * nrow(precipInterval))
    }
    return(c(temp, precip))
  }
  stop("Error: {get_climateIndices}: invalid dataType entered.")
}

# 5. XX

# ----- Execution -----
