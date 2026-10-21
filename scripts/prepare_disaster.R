library(janitor)
library(tidyverse)

disaster <- read.csv("./data/raw/disaster.csv", header = TRUE)

disaster <- clean_names(disaster)

### select rows for earthquake and drought and columns for year, country, iso, disaster type
disaster |>
  dplyr::filter(year >= 2000 & year <= 2019) |>
  dplyr::filter(disaster_type %in% c("Earthquake", "Drought")) |>
  dplyr::select(year, iso, disaster_type) |>
  rename(year = year, iso = iso) |> 
  group_by(year, iso) |>
  mutate(drought0 = ifelse(disaster_type == "Drought", 1, 0),
         earthquake0 = ifelse(disaster_type == "Earthquake", 1, 0)) |>
  summarize(drought = max(drought0),
            earthquake = max(earthquake0)) |> 
  ungroup() |>
  arrange(iso, year) -> disasters 
