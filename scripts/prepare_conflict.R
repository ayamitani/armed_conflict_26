library(tidyverse)

# Read in conflict data
conflict <- read.csv("./data/raw/conflict.csv", header = TRUE)

confdata <- conflict |>
  # Outcome 1: Binary indicator of armed conflict (0 if <25, 1 if >= 25 battle related deaths) for each country-year
  group_by(iso, year) |>
  summarise(total_death = sum(best)) |>
  mutate(armed_conflict = ifelse(total_death < 25, 0, 1)) |>
  ungroup() |>
  # the exposure variable "armed_conflict" is lagged by a year in the model
  mutate(year = year + 1) |>
  dplyr::select(iso, year, armed_conflict) 
