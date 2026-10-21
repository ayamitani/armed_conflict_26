source("scripts/prepare_mortality.R")
source("scripts/prepare_conflict.R")
source("scripts/prepare_disaster.R")

covariates <- read.csv("./data/raw/covariates.csv", header = TRUE)

## Merge all datasets into one final dataset
final_data <- maternal_mortality_long |>
  left_join(infant_mortality_long, by = c("iso", "year")) |>
  left_join(neonatal_mortality_long, by = c("iso", "year")) |>
  left_join(under5_mortality_long, by = c("iso", "year")) |>
  left_join(confdata, by = c("iso", "year")) |>
  left_join(disasters, by = c("iso", "year")) |>
  left_join(covariates, by = c("iso", "year")) |>
  mutate(
    drought = coalesce(drought, 0),
    earthquake = coalesce(earthquake, 0),
    armed_conflict = coalesce(armed_conflict, 0)
  )

write.csv(final_data, "./data/processed/final_data.csv", row.names = FALSE)
