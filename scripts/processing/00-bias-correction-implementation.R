# Purpose: Implement two bias correction algorithms to derive a table of correction coefficients.
# Last Updated: Aug 16, 2026
# Author: Benedict Cummins-Mburu.
# Contact: b.cumminsmburu@mail.utoronto.ca.

# The two bias correction algorithms implemented in this script are:
#     - Additive linear scaling (for temperature, folllowing Hempel et al., 2013)
#     - Local intensity scaling (for precipitation, following Smitha et al., 2018)

# ------- Setup --------
library(tidyverse)
library(itsmr)
clean_AEG <- read_csv("data/climate_data/clean_data/AEG_historic_climate.csv")
clean_ECCC <- read_csv(
  "data/climate_data/clean_data/ECCC_climate_projections.csv"
) %>%
  filter(year %in% 2005:2024) %>%
  filter(scenario == "SSP585") %>% # Note: only using SSP585 to determine coefficients.
  filter(date %in% as.Date(clean_AEG[!clean_AEG$empty, ]$date))

options(dplyr.summarise.inform = FALSE) # stops wall of text from being outputted when run

# ------ Helpers -------

calculate_temp_offsets <- function(ECCC_data, AEG_data, var) {
  # setup
  if (var == "meanTas") {
    use <- "useTas"
  } else if (var == "meanTasmin") {
    use <- "useTasmin"
  } else {
    stop("Error: `var` is not 'meanTas' or 'meanTasmin'.")
  }
  returnVector <- numeric(3)
  # filter datasets
  ECCC_data_Mar <- ECCC_data %>% filter(month == "Mar") %>% filter(.data[[use]])
  ECCC_data_Apr <- ECCC_data %>% filter(month == "Apr") %>% filter(.data[[use]])
  ECCC_data_May <- ECCC_data %>% filter(month == "May") %>% filter(.data[[use]])
  AEG_data_Mar <- AEG_data %>% filter(month == "Mar") %>% filter(.data[[use]])
  AEG_data_Apr <- AEG_data %>% filter(month == "Apr") %>% filter(.data[[use]])
  AEG_data_May <- AEG_data %>% filter(month == "May") %>% filter(.data[[use]])
  # cacluate Tas offsets
  returnVector[1] <- mean(AEG_data_Mar[[var]] - ECCC_data_Mar[[var]])
  returnVector[2] <- mean(AEG_data_Apr[[var]] - ECCC_data_Apr[[var]])
  returnVector[3] <- mean(AEG_data_May[[var]] - ECCC_data_May[[var]])
  # return
  return(returnVector)
}

get_wet_day_freq_SIM <- function(t) {
  # first, get the necessary monthly data summary
  thresholds <- data.frame(month = c("Mar", "Apr", "May"), t_val = t)
  monthly_ECCC_p <- clean_ECCC %>%
    left_join(thresholds, by = "month") %>%
    group_by(year, month) %>%
    summarise(
      days = n(),
      wetDays = sum(pr > t_val),
      wetDaysTotalPr = sum(ifelse(pr > t_val, pr, 0))
    ) %>%
    left_join(
      monthly_AEG_p %>%
        select(year, month, usePr),
      by = c("year", "month")
    )
  # then, cacluate wet day frequencies for each month
  WDFs <- numeric()
  for (m in c("Mar", "Apr", "May")) {
    filteredData <- monthly_ECCC_p %>%
      filter(month == m) %>%
      filter(usePr) %>%
      mutate(target = (wetDays / days))
    WDFs <- c(WDFs, mean(filteredData$target))
  }
  return(WDFs)
}


# ----- Temperature Bias Correction ------

# 1. Prepare Datasets
monthly_ECCC_t <- clean_ECCC %>%
  group_by(year, month) %>%
  summarise(
    days = n(),
    meanTas = mean(tas),
    meanTasmin = mean(tasmin)
  ) %>%
  arrange(year, month)
monthly_AEG_t <- clean_AEG %>% # Note: flagging months with over 20% missing data to DISCARD later
  group_by(year, month) %>%
  summarise(
    days = n(),
    propNATas = sum(is.na(tas)) / days,
    propNATasmin = sum(is.na(tasmin)) / days,
    useTas = ifelse(propNATas > 0.2, FALSE, TRUE),
    useTasmin = ifelse(propNATasmin > 0.2, FALSE, TRUE),
    meanTas = mean(tas, na.rm = TRUE),
    meanTasmin = mean(tasmin, na.rm = TRUE)
  ) %>%
  arrange(year, month)
monthly_ECCC_t <- monthly_ECCC_t %>% # tells ECCC data which months to DISCARD based on AEG flags
  left_join(
    monthly_AEG_t %>%
      select(year, month, useTas, useTasmin),
    by = c("year", "month")
  )

# 2. Calculate Temperature Offsets
monthlyTasOffsets <- calculate_temp_offsets(
  monthly_ECCC_t,
  monthly_AEG_t,
  "meanTas"
)
monthlyTasminOffsets <- calculate_temp_offsets(
  monthly_ECCC_t,
  monthly_AEG_t,
  "meanTasmin"
)

# ----- Precipitation Bias Correction ------

# 1. Prepare observed dataset
monthly_AEG_p <- clean_AEG %>%
  group_by(year, month) %>%
  summarise(
    days = n(),
    NAPrdays = sum(is.na(pr)),
    wetDays = sum(pr > 0, na.rm = TRUE),
    wetDaysTotalPr = sum(
      ifelse(pr > 0, pr, 0),
      na.rm = TRUE
    ),
    usePr = ifelse(NAPrdays / days > 0.2, FALSE, TRUE)
  ) %>%
  arrange(year, month)

# 2. Find the observed wet day frequency
wet_day_freq_OBS <- numeric()
for (m in c("Mar", "Apr", "May")) {
  filteredData <- monthly_AEG_p %>%
    filter(month == m) %>%
    filter(usePr)
  filteredData <- filteredData %>%
    mutate(target = (wetDays / (days - NAPrdays)))
  wet_day_freq_OBS <- c(wet_day_freq_OBS, mean(filteredData$target))
}

# 3. Find t that minimizes MSE between OBS wet day frequencies and SIM wet day frequencies
squared_error <- function(t) {
  candidate <- get_wet_day_freq_SIM(t)
  return(sum((candidate - wet_day_freq_OBS)**2))
}
monthlyPrThresholds <- optim(par = c(2, 2, 2), fn = squared_error)$par
# Note: t is a vector in R^3, one entry per month. So optimises via gradient descent.

# 4. Prepare SIM dataset with fitted t
fitted_t <- data.frame(
  month = c("Mar", "Apr", "May"),
  t_val = monthlyPrThresholds
)
monthly_ECCC_p <- clean_ECCC %>%
  left_join(fitted_t, by = "month") %>%
  group_by(year, month) %>%
  summarise(
    days = n(),
    wetDays = sum(pr > t_val),
    wetDaysTotalPr = sum(ifelse(pr > t_val, pr, 0))
  ) %>%
  left_join(
    monthly_AEG_p %>%
      select(year, month, usePr),
    by = c("year", "month")
  )

# 5. Calculate scaling factors
monthlyPrScalingFactors <- numeric()
for (i in 1:3) {
  m <- c("Mar", "Apr", "May")[i]
  filteredDataEC <- monthly_ECCC_p %>% filter(month == m) %>% filter(usePr)
  filteredDataAlg <- monthly_AEG_p %>% filter(month == m) %>% filter(usePr)
  num <- mean(filteredDataAlg$wetDaysTotalPr / filteredDataAlg$wetDays)
  den <- mean(filteredDataEC$wetDaysTotalPr / filteredDataEC$wetDays) -
    monthlyPrThresholds[i]
  monthlyPrScalingFactors <- c(monthlyPrScalingFactors, num / den)
}

# ------ Save to File -------

pretty_coefficients <- tribble(
  ~`Correction Factor`                         , ~Value                     ,
  "March mean temperature offset (\u00B0C)"    , monthlyTasOffsets[1]       ,
  "April mean temperature offset (\u00B0C)"    , monthlyTasOffsets[2]       ,
  "May mean temperature offset (\u00B0C)"      , monthlyTasOffsets[3]       ,
  "March minimum temperature offset (\u00B0C)" , monthlyTasminOffsets[1]    ,
  "April minimum temperature offset (\u00B0C)" , monthlyTasminOffsets[2]    ,
  "May minimum temperature offset (\u00B0C)"   , monthlyTasminOffsets[3]    ,
  "March dry day threshold (mm)"               , monthlyPrThresholds[1]     ,
  "April dry day threshold (mm)"               , monthlyPrThresholds[2]     ,
  "May dry day threshold (mm)"                 , monthlyPrThresholds[3]     ,
  "March intensity coefficient (unitless)"     , monthlyPrScalingFactors[1] ,
  "April intensity coefficient (unitless)"     , monthlyPrScalingFactors[2] ,
  "May intensity coefficient (unitless)"       , monthlyPrScalingFactors[3]
)
write_csv(pretty_coefficients, "data/bias_correction_coefficients.csv")
