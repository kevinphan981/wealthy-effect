library(tidycensus)
library(tidyverse)
library(priceR)
library(gt)
library(mapview)
library(readr)
library(tigris)
options(tigris_use_cache = TRUE)

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

# test <- get_acs(
#   geography = "county",
#   variables = "DP03_0009P",
#   year = 2023,
#   survey = "acs5"
# )

# --- general data that was an attempt at iterating through 1 year ACS to create panel ---

# start_year = 2021
# end_year = 2024
# years = c(seq(start_year, end_year, 1))
# 
# gen_data = data.frame()
# for (y in years) {
#   sprintf(paste0("Reading in ", y))
#   haw_gen <- get_acs(
#     geography = "county",
#     state = "HI",
#     variables = vars,
#     year = y,
#     survey = "acs1"
#   ) |>
#     filter(grepl("Hawaii", NAME, ignore.case = TRUE)) |>
#     mutate(year = y)
#   gen_data <- rbind(gen_data, haw_gen) 
#   sprintf(paste0("Finished reading in ", y))
# }
# 
# gen_wide <- gen_data |>
#   pivot_wider(
#     id_cols = c(GEOID, year), # Keep geographic identifiers
#     names_from = variable,       # Column names come from your variable IDs
#     values_from = estimate        # Cell values come from your estimates
#   )


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

# -------- aggregating the hawaii_block_cat.csv to commercial zones ------------
# requires an aggregation (total_shares), a normalization
# we also pivot to force all the different ratios as columns

haw_blocks_cat <- read_csv("data/hawaii_blocks_cat.csv") |>
  group_by(GEOID20) %>%
  mutate(
    # 1. Calculate the total sum of shares inside each census block
    total_block_share = sum(share, na.rm = TRUE),
    
    # 2. Normalize individual shares so they mathematically sum perfectly to 1.0 (or 100%)
    # This adjusts for any rounding errors or slight geographic overlap clipping
    normalized_share = if_else(total_block_share > 0, share / total_block_share, 0),
    
    # 3. Normalize block areas
    # This is our way of doing area weights
  ) %>%
  ungroup() # Always ungroup after group_by + mutate operations

zones_tract_long <- haw_blocks_cat |> 
  mutate(
      # Extract the 11-digit Census Tract ID from the Block ID
      GEOID_TRACT = str_sub(GEOID20, start = 1, end = 11)
    ) %>%
      # Group by the Tract and our simplified zone classifications
      group_by(GEOID_TRACT, county, zone_main_type) %>%
      summarize(
        # Sum the actual physical area (footprint) of this zone across the tract
        total_zone_area = sum(overlap_area, na.rm = TRUE),
        .groups = "drop"
  )

zones_tract_long <- zones_tract_long %>%
  group_by(GEOID_TRACT) %>%
  mutate(
    # Calculate the actual total area of the entire Census Tract
    total_tract_area = sum(total_zone_area, na.rm = TRUE),
    
    # This ratio perfectly preserves the geographic weight of each zone
    tract_share = if_else(total_tract_area > 0, total_zone_area / total_tract_area, 0)
  ) %>%
  ungroup()

# everything should still be 1, perfect
zones_tract_long |>
  group_by(GEOID_TRACT) |>
  summarize(total_shares = sum(tract_share), .groups = 'drop') |>
  summary()

# pivot to wide to join with main file
haw_zones_wide <- zones_tract_long |>
  pivot_wider(
    id_cols = c(GEOID_TRACT, county, total_tract_area), # Included total area for your records
    names_from = zone_main_type,
    values_from = tract_share,
    values_fill = 0
  )

# -------------- LODES8 data and crosswalk for final steps ----------------
# tigris_hi_tract <- tracts(state = "HI", year = 2023)
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

# 2 perform the actual crosswalk

wac_tract = crosswalk(wac_hi, xwalk)

# i would be relabeling with with an _r if I were concerned with the WAC, but I'm not so 
# i think things will be okay.

haw_gen_complete <- haw_gen_wide |>
  left_join(wac_tract, by = c("GEOID" = 'trct')) |>
  left_join(haw_zones_wide, by = c('GEOID' = 'GEOID_TRACT'))


write.csv(haw_gen_complete, "data/clean-data/final-df-spatial")
