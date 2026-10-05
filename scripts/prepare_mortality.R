library(tidyverse)

# Convert mortality data from wide (XYYYY columns) to long format
prepare_mortality <- function(input_path, value_name) {
  read.csv(input_path, header = TRUE) |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = value_name
    ) |>
    mutate(year = as.numeric(year)) |>
    select(iso, year, all_of(value_name))
}

# Apply function to each mortality outcome
maternal_mortality_long <- prepare_mortality(
  "./data/raw/maternal_mortality.csv",
  "maternal_mortality"
)

infant_mortality_long <- prepare_mortality(
  "./data/raw/infant_mortality.csv",
  "infant_mortality"
)

neonatal_mortality_long <- prepare_mortality(
  "./data/raw/neonatal_mortality.csv",
  "neonatal_mortality"
)

under5_mortality_long <- prepare_mortality(
  "./data/raw/under5_mortality.csv",
  "under5_mortality"
)
