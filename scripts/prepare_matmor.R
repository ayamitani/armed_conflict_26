library(tidyverse)

# Read in maternal mortality data
matmor <- read.csv("./data/raw/maternal_mortality.csv", header = TRUE)

# Change wide to long format
matmor_long <- matmor |>
  pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = "maternal_mortality") |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, maternal_mortality)
