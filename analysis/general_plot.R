# pure claude on this one... for now

library(leaflet)
library(sf)
library(htmlwidgets)

# ── 1. Validate class, clean geometry, reproject ──────────────────────────────
# FIX #5: Assert sf class before transforming — dplyr joins can silently drop it
stopifnot(inherits(haw_wide, "sf"))

# FIX #6: Use st_is_empty() not is.na() to correctly filter empty geometries
haw_clean <- haw_wide %>%
  filter(!st_is_empty(geometry)) %>%
  st_transform(crs = 4326)

# ── 2. Build color palettes ───────────────────────────────────────────────────
# FIX #2: Use B17001_001 (people BELOW poverty line), not B17001_001 (total pop)
# FIX #3: Pass na.color to handle tracts with missing income estimates
pal_poverty <- colorQuantile("Reds",  haw_clean$B17001_001, n = 5)
pal_income  <- colorQuantile("Blues", haw_clean$B19013_001, n = 5, na.color = "#808080")

# ── 3. Build the map ──────────────────────────────────────────────────────────
map <- leaflet(haw_clean) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # --- LAYER 1: POVERTY ---
  addPolygons(
    fillColor  = ~pal_poverty(B17001_001),
    weight     = 1, opacity = 1, color = "white", fillOpacity = 0.7,
    group      = "Poverty Count",
    # FIX #4: Add labels and hover highlighting for genuine interactivity
    label      = ~paste0("Tract: ", GEOID, "<br>Below poverty: ", B17001_001) %>%
      lapply(htmltools::HTML),
    highlightOptions = highlightOptions(
      weight       = 2,
      color        = "#333",
      fillOpacity  = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  
  # --- LAYER 2: MEDIAN INCOME ---
  addPolygons(
    fillColor  = ~pal_income(B19013_001),
    weight     = 1, opacity = 1, color = "white", fillOpacity = 0.7,
    group      = "Median Income",
    # FIX #4: Labels and hover highlighting
    label      = ~paste0("Tract: ", GEOID, "<br>Median income: $",
                         format(B19013_001, big.mark = ",")) %>%
      lapply(htmltools::HTML),
    highlightOptions = highlightOptions(
      weight       = 2,
      color        = "#333",
      fillOpacity  = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  
  # --- LEGENDS ---
  # We use className to stamp a guaranteed CSS class on each legend container.
  # layerId is unreliable for JS selection across leaflet versions, but
  # className is injected directly into the div Leaflet renders, so
  # document.querySelector() will always find it.
  addLegend(
    pal       = pal_poverty, values = ~B17001_001,
    title     = "Poverty Percentiles", position = "bottomright",
    className = "legend-poverty info legend"   # 'info legend' keeps default leaflet styling
  ) %>%
  addLegend(
    pal       = pal_income, values = ~B19013_001,
    title     = "Income Percentiles", position = "bottomright",
    className = "legend-income info legend"
  ) %>%
  
  # --- LAYER CONTROL ---
  addLayersControl(
    baseGroups = c("Poverty Count", "Median Income"),
    options    = layersControlOptions(collapsed = TRUE),
    position   = "topright"
  ) %>%
  
  # Wire legend visibility to the layer control via baselayerchange
  onRender("
    function(el, x) {
      var map = this;

      function syncLegends(activeGroup) {
        var pov = el.querySelector('.legend-poverty');
        var inc = el.querySelector('.legend-income');
        if (!pov || !inc) return;
        pov.style.display = (activeGroup === 'Poverty Count') ? 'block' : 'none';
        inc.style.display = (activeGroup === 'Median Income') ? 'block' : 'none';
      }

      // Hide income legend on initial load (poverty is the default active layer)
      syncLegends('Poverty Count');

      map.on('baselayerchange', function(e) {
        syncLegends(e.name);
      });
    }
  ")

map