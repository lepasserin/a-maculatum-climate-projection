# Purpose: Use fitted correction coefficients toderive bias-corrected  climate projection estimates.
# Last Updated: Aug 24, 2026
# Author: Benedict Cummins-Mburu.
# Contact: b.cumminsmburu@mail.utoronto.ca.

# ------- Setup -------
ECCC_raw_projections_only <- read_csv(
  "data/climate_data/clean_data/ECCC_climate_projections.csv"
)
BC <- read_csv("data/bias_correction_coefficients.csv")

# ----- Execution -----

# 0. Get abbreviations for `pr` thresolds to improve code readability
t_Mar <- as.numeric(BC[
  BC$`Correction Factor` == "March dry day threshold (mm)",
  2
])
t_Apr <- as.numeric(BC[
  BC$`Correction Factor` == "April dry day threshold (mm)",
  2
])
t_May <- as.numeric(BC[
  BC$`Correction Factor` == "May dry day threshold (mm)",
  2
])
D_Mar <- as.numeric(BC[
  BC$`Correction Factor` == "March intensity coefficient (unitless)",
  2
])
D_Apr <- as.numeric(BC[
  BC$`Correction Factor` == "April intensity coefficient (unitless)",
  2
])
D_May <- as.numeric(BC[
  BC$`Correction Factor` == "May intensity coefficient (unitless)",
  2
])


ECCC_projections_full <- ECCC_raw_projections_only %>%
  # 1. Apply additive linear scaling on `tas`
  mutate(
    tasBC = case_when(
      month == "Mar" ~ as.numeric(BC[
        BC$`Correction Factor` == "March mean temperature offset (°C)",
        2
      ]) +
        tas,
      month == "Apr" ~ as.numeric(BC[
        BC$`Correction Factor` == "April mean temperature offset (°C)",
        2
      ]) +
        tas,
      month == "May" ~ as.numeric(BC[
        BC$`Correction Factor` == "May mean temperature offset (°C)",
        2
      ]) +
        tas,
    )
  ) %>%
  # 2. Apply additive linear scaling on `tasmin`
  mutate(
    tasminBC = case_when(
      month == "Mar" ~ as.numeric(BC[
        BC$`Correction Factor` == "March minimum temperature offset (°C)",
        2
      ]) +
        tasmin,
      month == "Apr" ~ as.numeric(BC[
        BC$`Correction Factor` == "April minimum temperature offset (°C)",
        2
      ]) +
        tasmin,
      month == "May" ~ as.numeric(BC[
        BC$`Correction Factor` == "May minimum temperature offset (°C)",
        2
      ]) +
        tasmin,
    )
  ) %>%
  # 3. Apply local intensity scaling on `pr`
  mutate(
    prBC = case_when(
      month == "Mar" ~ ifelse(pr < t_Mar, 0, ((pr - t_Mar) * D_Mar)),
      month == "Apr" ~ ifelse(pr < t_Apr, 0, ((pr - t_Apr) * D_Apr)),
      month == "May" ~ ifelse(pr < t_May, 0, ((pr - t_May) * D_May))
    )
  )

# ------ Write to File ------
write_csv(
  ECCC_projections_full,
  "data/climate_data/clean_data/ECCC_climate_projections_BC.csv"
)
