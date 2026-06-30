library(sf)
library(tidycensus)
library(arcgislayers)
library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)

# --- county lookup table ---
# each county's zoning is a separate sub-layer in the same MapServer,
# with a different field name for the zone code. Kalawao County
# (the small ex-leper colony jurisdiction on Molokai) has no separate
# zoning layer and is omitted.

county_lookup <- tibble::tribble(
  ~county,     ~layer_id, ~zone_field,
  "hawaii",    2,         "zone", #unavoidable, have to look up codes
  "kauai",     29,        "description",  # short "zoning" code isn't documented in the public legend
  "maui",      33,        "zone_dist",     # full description, not the numeric "zone_code"
  "honolulu",  3,         "zone_class" # 100x bigger than Maui btw
)

base_url <- "https://geodata.hawaii.gov/arcgis/rest/services/ParcelsZoning/MapServer"

# --- function: do the full block <-> zoning crosswalk for one county ---
crosswalk_county <- function(county, layer_id, zone_field) {
  
  message("processing ", county, " ...")
  
  layer <- arc_open(paste0(base_url, "/", layer_id))
  zoning <- arc_select(layer) |>
    rename(zone_raw = all_of(zone_field)) |>
    select(zone_raw)
  
  block_county <- blocks(state = "hi", county = county, year = 2023)
  blocks_sf <- st_transform(block_county, st_crs(zoning))
  
  if (!(inherits(zoning, "sf") && inherits(blocks_sf, "sf"))) {
    message("unable to intersect geometries for ", county, "!")
    return(NULL)
  }
  
  blocks_sf <- st_make_valid(blocks_sf)
  zoning <- st_make_valid(zoning)
  
  # exclude water area from blocks before computing shares (coastline
  # mismatch between TIGER blocks and county zoning shoreline)
  water_county <- area_water(state = "hi", county = county, year = 2023) |>
    st_transform(st_crs(zoning)) |>
    st_make_valid()
  
  blocks_land <- st_difference(blocks_sf, st_union(water_county)) |>
    st_make_valid()
  
  ix <- st_intersection(
    blocks_land |> select(GEOID20),
    zoning |> select(zone_raw)
  ) |>
    mutate(overlap_area = as.numeric(st_area(geometry)))
  
  block_area <- blocks_land |>
    mutate(block_area = as.numeric(st_area(geometry))) |>
    st_drop_geometry() |>
    select(GEOID20, block_area)
  
  block_zone_dist <- ix |>
    st_drop_geometry() |>
    group_by(GEOID20, zone_raw) |>
    summarise(overlap_area = sum(overlap_area), .groups = "drop") |>
    left_join(block_area, by = "GEOID20") |>
    mutate(share = overlap_area / block_area)
  
  # gap-fill: add an explicit "Unzoned" category so shares sum to 1
  zoning_union <- zoning |> st_union() |> st_make_valid()
  
  gaps_summary <- st_difference(blocks_land |> select(GEOID20), zoning_union) |>
    st_make_valid() |>
    mutate(gap_area = as.numeric(st_area(geometry))) |>
    st_drop_geometry() |>
    filter(gap_area > 1) |>
    left_join(block_area, by = "GEOID20") |>
    mutate(gap_share = gap_area / block_area)
  
  block_zone_dist_complete <- gaps_summary |>
    transmute(GEOID20, zone_raw = "Unzoned", overlap_area = gap_area, block_area,
              share = gap_share) |>
    bind_rows(block_zone_dist) |>
    mutate(county = county) |>
    arrange(GEOID20, zone_raw)
  
  block_zone_dist_complete
}

# --- run for all counties and stack into one statewide table ---
hawaii_zone_dist <- purrr::pmap_dfr(
  county_lookup,
  crosswalk_county
)

# write it into a csv for me to deal later
write.csv(hawaii_zone_dist, "data/hawaii_zone_dist.csv")

# --- diagnostics across the whole state ---
block_totals <- hawaii_zone_dist |>
  group_by(GEOID20) |>
  summarise(total_share = sum(share), .groups = "drop")

summary(block_totals$total_share)

block_totals |>
  filter(total_share < 0.999 | total_share > 1.001) |>
  nrow()