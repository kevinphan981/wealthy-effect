library(tidycensus)
library(tidyverse)
library(priceR)
library(gt)
library(mapview)
library(tigris)
options(tigris_use_cache = TRUE)


zips <- read.csv("data/simplemaps_uszips_basicv1.94/uszips.csv") |>
  rename(zipcode = zip)

hawaii_zips <- zips |> filter(state_id == "HI") |> select(zipcode)

hawaii_zctas <- zctas(
  cb = TRUE, 
  starts_with = c("967", "968", "999"),
  # state = "HI",
  year = 2020
) |>
  mutate(zipcode = as.integer(NAME20))

usa_zctas <- zctas(
  cb = TRUE,
  year = 2020
) |>   mutate(zipcode = as.integer(NAME20))


hawaii_zctas <- hawaii_zctas |>
  inner_join(hawaii_zips, by = "zipcode")
# mapview(hawaii_zctas) # looks fine, we are missing some because of the SOI eligibility

setwd("~/Programming/wealthy-effect")
# reading in data
v24 <- load_variables(2024, "acs5", cache = TRUE)
v_profile_23 <- load_variables(2023, "acs5/profile", cache = T)
# write.csv(v24, "v24-data_name")
# view to see what's going on

# list of variables
# income, population, # of households, gross median rent, unemployment, 
# gini

# B25003_001 tenure for both, 002 for owners, 003 for renters

vars <- c("B17001_002", "B19083_001", "B19080_005", 
          "B25077_001", "B25070_001", "B23025_002",
          "B25001_001", "B19001_017", "B08536_001",
          "B01003_001", "B19057_001", "B15003_022", 
          "B25034_002", "B19001_001", "B19013_001",
          "B01003_001", "B02001_006", 'DP03_0009P', 'DP02_0065',
          "B19080_005")

# if we did zcta's, it must be ALL, then we filter.

# county_gen <- get_acs(
#   geography = "county",
#   variables = vars,
#   year = 2024,
#   survey = "acs5"
# )

# test <- get_acs(
#   geography = "county",
#   variables = "DP03_0009P",
#   year = 2023,
#   survey = "acs5"
# )

start_year = 2021
end_year = 2024
years = c(seq(start_year, end_year, 1))

gen_data = data.frame()
for (y in years) {
  sprintf(paste0("Reading in ", y))
  haw_gen <- get_acs(
    geography = "county",
    state = "HI",
    variables = vars,
    year = y,
    survey = "acs1"
  ) |>
    filter(grepl("Hawaii", NAME, ignore.case = TRUE)) |>
    mutate(year = y)
  gen_data <- rbind(gen_data, haw_gen) 
  sprintf(paste0("Finished reading in ", y))
}

gen_wide <- gen_data |>
  pivot_wider(
    id_cols = c(GEOID, year), # Keep geographic identifiers
    names_from = variable,       # Column names come from your variable IDs
    values_from = estimate        # Cell values come from your estimates
  )


# --------------- two versions, one for regression, one for plotting ---------
haw_gen <- get_acs(
  geography = "tract",
  state = "HI",
  variables = vars,
  year = 2023,
  survey = "acs5",
  # geometry = TRUE
) |>
  mutate(coeff_var = (moe/(1.645*estimate)) * 100) |>
  filter(coeff_var < 25)

haw_gen_wide <- haw_gen |> 
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  )

# tigris_hi_tract <- tracts(state = "HI", year = 2023)
library(readr)
xwalk <- read_csv("https://lehd.ces.census.gov/data/lodes/LODES8/hi/hi_xwalk.csv.gz") |>
  select(tabblk2020, trct)  # or stplc for places

wac_hi <- read.csv("data/LEHD-data/WAC.csv") |>
  mutate(year = as.character(year))
rac_hi <- read.csv("data/LEHD-data/RAC.csv") |>
  mutate(year = as.character(year))

crosswalk <- function(df, xwalk) {
  df_f <- df |>
    left_join(xwalk, by = c("h_geocode" = "tabblk2020")) |>
    group_by(trct) |>
    select(!h_geocode) |> #not needed
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) |>
    mutate(trct = as.character(trct))
  
  return(df_f)
}

rac_tract <- rac_hi|>
  left_join(xwalk, by = c("h_geocode" = "tabblk2020")) |>
  group_by(trct) |>
  select(!h_geocode) |> #not needed
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) |>
  mutate(trct = as.character(trct))

rac_tract = crosswalk(rac_hi, xwalk)

# i would be relabeling with with an _r if I were concerned with the WAC, but I'm not so 
# i think things will be okay.
# rac_tract[, paste0(names(rac_hi), "_r")] <- rac_tract

haw_gen_complete <- haw_gen_wide |>
  left_join(rac_tract, by = c("GEOID" = 'trct'))


