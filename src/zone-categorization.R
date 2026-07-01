library(dplyr)
library(arcgislayers)
library(readr)


ptb <- read_csv("data/hawaii_zone_dist2.csv") |>
  select(!("...1"))


# I made a CSV of each specific type. 
