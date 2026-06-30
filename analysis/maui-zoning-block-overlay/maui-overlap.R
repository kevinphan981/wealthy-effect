library(sf)
library(tidycensus)
library(arcgislayers)
library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)


# get in the maui parcel zoning data

url <- "https://geodata.hawaii.gov/arcgis/rest/services/ParcelsZoning/MapServer/33"
layer <- arc_open(url)
zoning <- arc_select(layer)


block_maui <- blocks(state = 'hi', county = "maui", year = 2023)

blocks <- st_transform(block_maui, st_crs(zoning))

if (!(inherits(zoning, "sf") && inherits(blocks, "sf"))) {
  message("unable to intersect geometries!")
}


# there is something off with the ring
summary(st_is_valid(zoning)) # has one bad 
bad_zoning  <- which(!st_is_valid(zoning))

# fixing both
blocks <- st_make_valid(blocks)
zoning <- st_make_valid(zoning)


### --- exclude water area from blocks before computing shares ---
# shares were undershooting almost everywhere, traced to a coastline
# mismatch between TIGER blocks (which extend into nearshore water)
# and the county zoning layer's shoreline (which doesn't). subtracting
# TIGER's water polygons fixes the denominator.

water_maui <- area_water(state = "hi", county = "maui", year = 2023) |>
  st_transform(st_crs(zoning)) |>
  st_make_valid()

blocks_land <- st_difference(blocks, st_union(water_maui)) |>
  st_make_valid()

ix <- st_intersection(
  blocks_land |> select(GEOID20),
  zoning |> select(zone_code)
) |>
  mutate(overlap_area = as.numeric(st_area(geometry)))

# land-only block area (denominator)
block_area <- blocks_land |>
  mutate(block_area = as.numeric(st_area(geometry))) |>
  st_drop_geometry() |>
  select(GEOID20, block_area)

# zoning shares within each block
block_zone_dist <- ix |>
  st_drop_geometry() |>
  group_by(GEOID20, zone_code) |>
  summarise(overlap_area = sum(overlap_area), .groups = "drop") |>
  left_join(block_area, by = "GEOID20") |>
  mutate(share = overlap_area / block_area)

#--------- checking for possible mistakes ----------

# 1. how close do shares now sum to 1 per block?
block_totals <- block_zone_dist |>
  group_by(GEOID20) |>
  summarise(total_share = sum(share), .groups = "drop")

summary(block_totals$total_share)

overshoot <- block_totals |> filter(total_share > 1.000)
undershoot <- block_totals |> filter(total_share < 0.999)
nrow(overshoot); nrow(undershoot)

# 2. check true area overlap within the zoning layer itself
#    (st_intersects() also flags polygons that merely share an edge,
#    so use st_overlaps() to isolate genuine area overlap)
ov <- st_overlaps(zoning)
overlap_idx <- which(lengths(ov) > 0)
length(overlap_idx)

if (length(overlap_idx) > 0) {
  zoning_overlapping <- zoning[overlap_idx, ]
  self_int <- st_intersection(zoning_overlapping |> select(zone_code)) |>
    mutate(overlap_area = as.numeric(st_area(geometry))) |>
    filter(overlap_area > 1)  # drop floating-point slivers
  
  nrow(self_int)
  sum(self_int$overlap_area)
}

# 3. quantify any remaining gaps (land in a block with no matching
#    zoning polygon at all, e.g. true unzoned/conservation land)
zoning_union <- zoning |> st_union() |> st_make_valid()

gaps <- st_difference(blocks_land |> select(GEOID20), zoning_union) |>
  st_make_valid() |>
  mutate(gap_area = as.numeric(st_area(geometry)))

gaps_summary <- gaps |>
  st_drop_geometry() |>
  filter(gap_area > 1) |>
  left_join(block_area, by = "GEOID20") |>
  mutate(gap_share = gap_area / block_area) |>
  arrange(desc(gap_share))

gaps_summary

# 4. add an explicit "Unzoned" category so shares sum to 1 by construction
block_zone_dist_complete <- gaps_summary |>
  transmute(GEOID20, zone_code = "Unzoned", overlap_area = gap_area, block_area,
            share = gap_share) |>
  bind_rows(block_zone_dist) |>
  arrange(GEOID20, zone_code)

# acceptable gaps
# ggplot() + 
#   geom_sf(data = gaps, fill = "pink", color = "grey") +
#   theme_void()
