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
# SOURCE: I regressed this
B0_APRIL <- 125.5190
B1_APRIL <- -3.1373
# SOURCE: fitted post-hoc breeding migration model
B0_SURVIVAL <- 0.9656941
B1_SURVIVAL <- -0.762042
B2_SURVIVAL <- 0.4460179

# ------ Helpers ------

# 1. fetch mean April temperature for a given `yr`, `scen`
get_meanAprilTemp <- function(yr, scen, dataType = "ADJ") {
  # input validation
  if (!(scen %in% c("SSP126", "SSP245", "SSP585"))) {
    stop("Error: {get_meanAprilTemp}: invalid scenario specified.")
  }
  if (!(yr %in% 2000:2100)) {
    stop("Error: {get_meanAprilTemp}: invalid year specified.")
  }
  # calcultion
  focalData <- climate_projections %>%
    filter(scenario == scen, year == yr, month == "Apr")
  if (dataType == "ADJ") {
    return(mean(focalData$tasBC))
  }
  if (dataType == "SIM") {
    return(mean(focalData$tas))
  }
  stop("Error: {get_meanAprilTemp}: invalid dataType entered.")
}

# 2. estimate breeding start (i.e. Bat Lake ice out) date from mean April temperature.
get_iceOut_from_meanAprilTemp <- function(meanAprilTemp) {
  return(round((B1_APRIL * meanAprilTemp) + B0_APRIL))
}

# 3. calculate breeding end (i.e. peak egg mass) date from breeding start date.
# ASSUMPTION: peak egg mass always occurs 22 days after ice out
get_peakEggMass_from_iceOut <- function(iceOut) {
  return(iceOut + MEAN_LENGTH)
}

# 4. calculate climate indices for a given `yr`, `scen`, `iceOut`, `eggMass`
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

# 5. Estimate survivorship from `temp` and `precip`, with the option to `supress` one variable.
get_survival_from_indices <- function(temp, precip, supress = "none") {
  # standardize temp and precip indices
  standardizedT <- (temp - OBS_T_MEAN) / OBS_T_SD
  standardizedP <- (precip - OBS_P_MEAN) / OBS_P_SD
  # calculate survivorship
  if (supress == "none") {
    logitS <- B0_SURVIVAL +
      B1_SURVIVAL * standardizedT +
      B2_SURVIVAL * standardizedP
  } else if (supress == "precip") {
    logitS <- B0_SURVIVAL + B1_SURVIVAL * standardizedT
  } else if (supress == "temp") {
    logitS <- B0_SURVIVAL + B2_SURVIVAL * standardizedP
  } else {
    stop(
      "Error: {get_survival_from_indices}: invalid supression string entered."
    )
  }
  # revert logit and return
  S <- 1 / (1 + exp(-logitS))
  return(S)
}

# ------ Orchestrator ------

calculate_indices <- function(yr, scen, dataType = "ADJ") {
  # estimate breeding season start date
  iceOut <- get_iceOut_from_meanAprilTemp(get_meanAprilTemp(yr, scen, dataType))
  # estimate breeding season end date
  peakEggMass <- get_peakEggMass_from_iceOut(iceOut)
  # derive climate indices from interval
  climateIndices <- get_climateIndices(yr, scen, iceOut, peakEggMass, dataType)
  tempIndex <- climateIndices[1]
  precipIndex <- climateIndices[2]
  # calculat survivorship from indices
  survivorship <- get_survival_from_indices(tempIndex, precipIndex)
  survivorshipT <- get_survival_from_indices(tempIndex, precipIndex, "precip")
  survivorshipP <- get_survival_from_indices(tempIndex, precipIndex, "temp")
  # return single-row data frame
  df <- data.frame(
    year = yr,
    scenario = scen,
    breedingSeasonStart = iceOut,
    breedingSeasonEnd = peakEggMass,
    tempIndex = tempIndex,
    precipIndex = precipIndex,
    survivorship = survivorship,
    survivorshipT = survivorshipT,
    survivorshipP = survivorshipP
  )
  return(df)
}

# ----- Execution -----

final_df <- data.frame(
  year = integer(),
  scenario = character(),
  breedingSeasonStart = integer(),
  breedingSeasonEnd = integer(),
  tempIndex = numeric(),
  precipIndex = numeric(),
  survivorship = numeric(),
  survivorshipT = numeric(),
  survivorshipP = numeric()
)
for (scen in c("SSP126", "SSP245", "SSP585")) {
  for (yr in 2000:2100) {
    new_row <- calculate_indices(yr, scen)
    final_df <- rbind(final_df, new_row)
  }
}

# ------ Save Data ------
write_csv(final_df, "data/forecast_data/index_forecasts.csv")
