library(sf)
library(tidycensus)
library(arcgislayers)
library(dplyr)
library(tigris)
library(stringr)
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
  t0 <- Sys.time()
  
  layer <- arc_open(paste0(base_url, "/", layer_id))
  zoning <- arc_select(layer) |>
    rename(zone_raw = all_of(zone_field)) |>
    select(zone_raw) |>
    st_make_valid() |>
    # Group and dissolve geometries by zone type
    group_by(zone_raw) |>
    summarise(geometry = st_union(geometry), .groups = "drop") |>
    st_make_valid()
  
  block_county <- blocks(state = "hi", county = county, year = 2023)
  blocks_sf <- st_transform(block_county, st_crs(zoning))
  
  if (!(inherits(zoning, "sf") && inherits(blocks_sf, "sf"))) {
    message("unable to intersect geometries for ", county, "!")
    return(NULL)
  }
  
  blocks_sf <- st_make_valid(blocks_sf)
  zoning <- st_make_valid(zoning)
  
  # --- exclude water area from blocks before computing shares ---
  # (coastline mismatch between TIGER blocks and county zoning shoreline)
  #
  # NOTE: previously this unioned the *entire* county water layer in one
  # shot before differencing. For Honolulu's water layer (reefs, Pearl
  # Harbor, harbor inlets) that single union is expensive and scales
  # worse than linearly with feature/vertex count. Instead, only union
  # the water polygons that actually intersect each block, which is
  # almost always a small local subset.
  water_county <- area_water(state = "hi", county = county, year = 2023) |>
    st_transform(st_crs(zoning)) |>
    st_make_valid()
  
  water_idx <- st_intersects(blocks_sf, water_county)
  needs_clip <- lengths(water_idx) > 0
  
  if (any(needs_clip)) {
    clipped <- purrr::map2(
      which(needs_clip),
      water_idx[needs_clip],
      function(i, wi) {
        st_difference(blocks_sf[i, ], st_union(water_county[wi, ]))
      }
    ) |> bind_rows()
    
    blocks_land <- bind_rows(
      blocks_sf[!needs_clip, ],
      clipped
    ) |> st_make_valid()
  } else {
    blocks_land <- blocks_sf
  }
  
  message("  water clip done (", round(difftime(Sys.time(), t0, units = "secs"), 1), "s elapsed)")
  
  ix <- st_intersection(
    blocks_land |> select(GEOID20),
    zoning |> select(zone_raw)
  ) |>
    mutate(overlap_area = as.numeric(st_area(geometry)))
  
  message("  overlay done (", round(difftime(Sys.time(), t0, units = "secs"), 1), "s elapsed)")
  
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
  
  # pull out road/ROW polygons before returning — present in Hawaii County's
  # zoning layer (and possibly others in smaller amounts), and double-counts
  # against the underlying zone since roads are drawn on top rather than cut out
  road_pattern <- regex("road", ignore_case = TRUE)
  # road_coverage_county <- block_zone_dist_complete |> filter(str_detect(zone_raw, road_pattern))
  block_zone_dist <- block_zone_dist |> filter(!str_detect(zone_raw, road_pattern))
  
  
  # --- gap-fill: add an explicit "Unzoned" category so shares sum to 1 ---
  # NOTE: previously this called st_union(zoning) across the *entire*
  # county zoning layer and then st_difference'd every block against
  # it. For Honolulu (~100x Maui's parcel count, much denser urban
  # geometry) that full union was the single most expensive step in the
  # script, with real risk of stalling or exhausting memory.
  #
  # It's also unnecessary: the gap share is just 1 minus the area
  # already accounted for in block_zone_dist, computed arithmetically
  # from the overlap areas already produced by the intersection above,
  # no extra geometry operations required.
  block_zone_dist_complete <- block_zone_dist |>
    group_by(GEOID20) |>
    summarise(covered_area = sum(overlap_area), .groups = "drop") |>
    left_join(block_area, by = "GEOID20") |>
    mutate(
      gap_area = pmax(block_area - covered_area, 0),
      gap_share = gap_area / block_area
    ) |>
    filter(gap_area > 1) |>
    transmute(GEOID20, zone_raw = "Unzoned", overlap_area = gap_area, block_area,
              share = gap_share) |>
    bind_rows(block_zone_dist) |>
    mutate(county = county) |>
    arrange(GEOID20, zone_raw)
  
  

  
  message("  ", county, " complete (", round(difftime(Sys.time(), t0, units = "secs"), 1), "s total)")
  
  block_zone_dist_complete
}

# --- run for all counties and stack into one statewide table ---
hawaii_zone_dist <- purrr::pmap_dfr(
  county_lookup,
  crosswalk_county
)

# write it into a csv for me to deal later
write.csv(hawaii_zone_dist, "data/hawaii_zone_dist3.csv")

# --- diagnostics across the whole state ---
block_totals <- hawaii_zone_dist |>
  group_by(GEOID20) |>
  summarise(total_share = sum(share), .groups = "drop")

summary(block_totals$total_share)

block_totals |>
  filter(total_share < 0.999 | total_share > 1.001) |>
  nrow()

# the shares sum over 1 for some reason, it's all in hawaii county
shares_over <- hawaii_zone_dist |>
  group_by(GEOID20) |>
  mutate(total_share = sum(share)) |>
  ungroup() |>
  filter(total_share > 1.01)

# --- diagnostics for hawaii county ---
zoning_hi <- arc_select(arc_open(paste0(base_url, "/2"))) |>
  rename(zone_raw = zone)

# what are the actual road-like categories?
zoning_hi |> 
  st_drop_geometry() |> 
  count(zone_raw, sort = T)

# for one of your >1.001 blocks, what zonzone_raw# for one of your >1.001 blocks, what zone_raw values does it actually get?
test_hi <- hawaii_zone_dist |>
  filter(GEOID20 == "150010216061001") |>
  arrange(desc(share))

# # --- diagnostics for honolulu county ---
# zoning_hon <- arc_select(arc_open(paste0(base_url, "/3"))) |>
#   rename(zone_raw = zone_class)
# 
# zoning_hon_fix <- zoning_hon |>
#   st_make_valid()
# 
# table(st_is_valid(zoning_hon_fix))
# 
# hon_dups <- zoning_hon_fix %>%
#   distinct(geometry)
