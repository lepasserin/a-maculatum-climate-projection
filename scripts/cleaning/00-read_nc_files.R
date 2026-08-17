# Purpose: Parses and aggregates 3x3x3=27 .nc files together into a single dataset of environmental projection data. Resulting dataset is written to file.
# Last Updated: Jun 23, 2026.
# Author: Benedict Cummins-Mburu.
# Contact: b.cumminsmburu@mail.utoronto.ca.
# Notes:
#    - Outputted gridpoint: (45.54167, -78.29166)

# ------- Setup --------
library(tidyverse)
library(ncdf4)
GENERIC_FILEPATH <- "data/climate_data/raw_data/NC_data/NC_"
FILES_PER_DESIGN <- 3
VARIABLE_NAME <- c("tas", "tasmin", "pr")
SCENARIO_NAME <- c("SSP585", "SSP245", "SSP126")

# ------ Helpers -------

#' @param ncConnection an open NetCDF connection object.
#' @param scenarioName string in SCENARIO_NAME.
#' @param varName string in VARIABLE_NAME
#' @returns a dataframe containing the extracted NetCDF data structured for analysis.
convert_nc_to_df <- function(ncConnection, scenarioName, varName) {
  # extract nc data, then close connection
  time <- ncvar_get(ncConnection, "time")
  currVar <- ncvar_get(ncConnection, varName)
  lat <- ncvar_get(ncConnection, "lat")
  lon <- ncvar_get(ncConnection, "lon")
  if (length(c(lat, lon)) != 2) {
    stop("Somehow, more than one grid point was accessed.")
  }
  nc_close(ncConnection)
  dates <- as.Date(time, origin = "1850-01-01")
  # put into dataframe, then return
  df <- data.frame(
    date = dates,
    year = year(dates),
    month = month(dates, label = TRUE),
    day = day(dates),
    ordinalDay = yday(dates),
    varName = varName,
    varValue = currVar,
    scenario = scenarioName
  )
  message(paste("Successfully parsed a NetCDF connection"))
  return(df)
}

load_all_nc_files <- function(folder_path) {
  # get list of all nc files (full system paths)
  file_list <- list.files(
    path = folder_path,
    pattern = "\\.nc$",
    full.names = TRUE
  )
  # open them all, and name them based on their filenames
  nc_objects <- lapply(file_list, nc_open)
  names(nc_objects) <- basename(file_list)
  if (length(nc_objects) != FILES_PER_DESIGN) {
    stop(
      "Error in `load_all_nc_files`: there are a wrong number of files in this folder."
    )
  }
  # return a list of these
  message(paste(
    "Successfully loaded",
    length(nc_objects),
    "NetCDF connections."
  ))
  return(nc_objects)
}

# ----- Execution ------

ECCclimateData <- data.frame(
  date = c(), # Date object
  year = integer(),
  month = character(),
  day = integer(),
  ordinalDay = numeric(),
  varName = character(),
  varValue = numeric(),
  scenario = character()
)

for (scenario in SCENARIO_NAME) {
  for (var in VARIABLE_NAME) {
    curr_path <- paste0(GENERIC_FILEPATH, var, scenario)
    curr_allNC <- load_all_nc_files(curr_path)
    currDataList <- lapply(
      curr_allNC,
      convert_nc_to_df,
      scenarioName = scenario,
      varName = var
    )
    for (df in currDataList) {
      ECCclimateData <- rbind(ECCclimateData, df)
    }
  }
}

# ----- Write to CSV -----
savedData <- pivot_wider(
  ECCclimateData,
  names_from = varName,
  values_from = varValue
)
write.csv(
  savedData,
  "data/climate_data/clean_data/ECCC_climate_projections.csv",
  row.names = FALSE
)
