library(tidyverse)
library(janitor)

raw_dir <- "data/raw"
processed_dir <- "data/processed"

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

prepare_wb_mortality <- function(path, value_name) {
  dat <- read_csv(path, show_col_types = FALSE)

  dat |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = value_name
    ) |>
    mutate(
      year = as.integer(year),
      iso = as.character(iso)
    ) |>
    select(iso, year, all_of(value_name))
}

maternal <- prepare_wb_mortality(
  file.path(raw_dir, "maternal_mortality.csv"),
  "maternal_mortality"
)

infant <- prepare_wb_mortality(
  file.path(raw_dir, "infant_mortality.csv"),
  "infant_mortality"
)

neonatal <- prepare_wb_mortality(
  file.path(raw_dir, "neonatal_mortality.csv"),
  "neonatal_mortality"
)

under5 <- prepare_wb_mortality(
  file.path(raw_dir, "under5_mortality.csv"),
  "under5_mortality"
)

mortality <- maternal |>
  left_join(infant, by = join_by(iso, year)) |>
  left_join(neonatal, by = join_by(iso, year)) |>
  left_join(under5, by = join_by(iso, year))

disaster_path <- file.path(raw_dir, "disaster.csv")

if (file.exists(disaster_path) && file.size(disaster_path) > 0) {
  disasters <- read_csv(disaster_path, show_col_types = FALSE) |>
    clean_names() |>
    transmute(
      iso = as.character(iso),
      year = as.integer(year),
      disaster_type = as.character(disaster_type)
    ) |>
    filter(disaster_type %in% c("Earthquake", "Drought")) |>
    mutate(any_disaster = 1L) |>
    distinct(iso, year, disaster_type, any_disaster) |>
    pivot_wider(
      names_from = disaster_type,
      values_from = any_disaster,
      values_fill = 0L,
      names_prefix = "disaster_"
    ) |>
    rename(
      disaster_earthquake = disaster_Earthquake,
      disaster_drought = disaster_Drought
    ) |>
    mutate(
      disaster_earthquake = as.integer(disaster_earthquake),
      disaster_drought = as.integer(disaster_drought)
    )
} else {
  disasters <- tibble(
    iso = character(),
    year = integer(),
    disaster_earthquake = integer(),
    disaster_drought = integer()
  )
}

conflict <- read_csv(file.path(raw_dir, "conflict.csv"), show_col_types = FALSE) |>
  transmute(
    iso = as.character(iso),
    year = as.integer(year),
    best = as.numeric(best)
  ) |>
  summarize(
    battle_deaths = sum(best, na.rm = TRUE),
    .by = c(iso, year)
  ) |>
  mutate(
    armed_conflict = as.integer(battle_deaths >= 25)
  ) |>
  arrange(iso, year) |>
  mutate(
    armed_conflict_lag1 = lag(armed_conflict, default = 0L),
    .by = iso
  ) |>
  select(iso, year, armed_conflict, armed_conflict_lag1)

covariates <- read_csv(file.path(raw_dir, "covariates.csv"), show_col_types = FALSE) |>
  mutate(
    iso = as.character(iso),
    year = as.integer(year)
  ) |>
  select(-any_of(c(
    "maternal_mortality",
    "infant_mortality",
    "neonatal_mortality",
    "under5_mortality"
  )))

countries_path <- file.path(raw_dir, "countries.txt")
if (file.exists(countries_path) && file.size(countries_path) > 0) {
  countries <- read_lines(countries_path, skip = 1) |>
    str_extract("[A-Z]{3}(?=\"?\\s*$)") |>
    na.omit() |>
    as.character()
} else {
  countries <- character(0)
}

final_data <- mortality |>
  left_join(conflict, by = join_by(iso, year)) |>
  left_join(disasters, by = join_by(iso, year)) |>
  left_join(covariates, by = join_by(iso, year)) |>
  mutate(
    armed_conflict = coalesce(armed_conflict, 0L),
    armed_conflict_lag1 = coalesce(armed_conflict_lag1, 0L),
    disaster_earthquake = coalesce(disaster_earthquake, 0L),
    disaster_drought = coalesce(disaster_drought, 0L)
  ) |>
  arrange(iso, year)

if (length(countries) > 0) {
  final_data <- final_data |>
    filter(iso %in% countries)
}

write_csv(final_data, file.path(processed_dir, "final_data_copilot.csv"))

final_data