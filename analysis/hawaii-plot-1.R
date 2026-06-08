library(tmap)
library(sf)
tmap_mode("view")


# we have to isolate it to one singular category
haw_gen_1 |>
  filter(variable == "B25034_002") |>
  tm_shape(haw_gen_1, projection = 26918) + 
  tm_fill(col = "estimate", 
          palette = "Greens", 
          title = "Moved From Another State")

haw_gen_1 |>
  filter(variable == "B17001_001") |>
  filter(!is.na(variable)) |> #nothing is missing???
  tm_shape(haw_gen_1, projection = 26918) + 
  tm_fill(col = "estimate", 
          palette = "brewer.reds", 
          title = "Poverty")

# tm_shape(haw_wide, projection = 26918) + 
#   tm_polygons(
#     col = vars, # Add all variables you want in the dropdown here
#     palette = "Reds"
#   ) +
#   tm_options(by.mode = "layer") # <-- Tells tmap v4 to map variables as selectable layers
#   

tm_shape(haw_wide, projection = 26918) + 
  # 1. Use 'col' for v3 syntax
  tm_polygons(
    col = c("B17001_001", "B19013_001"), 
    palette = "Reds",
    title = c("Poverty", "Median Income")
  ) +
  # 2. Force the layer control menu explicitly
  tm_facets(as.layers = TRUE)
