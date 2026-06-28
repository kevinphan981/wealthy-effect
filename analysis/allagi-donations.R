library(tidyverse)
library(tidycensus)
library(priceR)
library(gt)
library(mapview)
library(tigris)
options(tigris_use_cache = TRUE)

hawaii_zctas <- zctas(
  cb = TRUE, 
  starts_with = c("967", "968", "999"),
  # state = "HI",
  year = 2020
) |>
  mutate(zipcode = as.integer(NAME20))
mapview(hawaii_zctas) # looks fine, we are missing some because of the SOI eligibility

# # zip codes don't exist for this
# haw_income <- get_acs(
#   geography = "county", 
#   variables = "B19013_001",
#   state = "HI", 
#   year = 2022,
#   geometry = TRUE
# )
# plot(haw_income["estimate"])

# read online rather than local
df <- read.csv("https://www.irs.gov/pub/irs-soi/22zpallagi.csv")
zips <- read.csv("data/simplemaps_uszips_basicv1.94/uszips.csv") |>
  rename(zipcode = zip)

# make sure to look at the docx in order to see which variables to fetch
# N19700 - total # of returns with charitable donations
# A19700 - total amount in charitable donations !!!
quantiles <- quantile(df$A19700, probs = seq(0.01, 0.99, by = 0.01))

df_perc <- df |>
  filter(A19700 > 0) |>
  mutate(percentile = rank(A19700)/length(A19700) * 100)

df_perc |>
  left_join(zips, by = "zipcode") |>
  filter(STATE == "HI", zipcode != 0) |>
  select(city, county_name, zipcode, agi_stub, A19700, percentile) |>
  group_by(county_name) |>
  arrange(desc(A19700)) |>
  gt() |>
  fmt_percent(percentile, decimals = 1) |>
  fmt_currency(A19700)

# we look at agi_stub group quantiles specifically
df_grouped <- df |>
  group_by(agi_stub) |>
  mutate(percentile = rank(A19700)/length(A19700),
         agi_stub_str = as.character(agi_stub))

(
  ggplot(data = df_grouped, aes(x = percentile, y = A19700, fill = agi_stub_str)) +
    geom_area(position = 'stack') 
  + scale_x_continuous(limits = c(0.5, 1))
  + scale_y_continuous(limits = c(0,500000))
)

df_grouped |>
  left_join(zips, by = "zipcode") |>
  filter(STATE == "HI", zipcode != 0) |>
  select(city, county_name, zipcode, agi_stub, A19700, percentile) |>
  arrange(desc(A19700)) |>
  gt() |>
  fmt_percent(percentile, decimals = 1) |>
  fmt_currency(A19700)

df_hawaii <- df |>
  filter(STATE == "HI") |>
  group_by(agi_stub) |>
  mutate(percentile = rank(A19700)/length(A19700) * 100,
         agi_stub_str = as.character(agi_stub),
         taxes_per_return = A10300/N10300)

(
  ggplot(data = df_hawaii, aes(x = percentile, y = A19700, fill = agi_stub_str)) +
    geom_area(position = 'stack') 
  + scale_x_continuous(limits = c(0.5, 1))
  + scale_y_continuous(limits = c(0,500000))
)


n = length(df_hawaii$A19700)
plot((1:n - 1)/(n - 1), sort(df_hawaii$A19700), type="l",
     main = "Visualizing Percentiles",
     xlab = "Percentile",
     ylab = "Value",
     ylim = c(0,35000))

hawaii_se_data <- hawaii_zctas |>
  left_join(zips, by = "zipcode") |>
  left_join(df_hawaii, by = "zipcode") |>
  filter(agi_stub == 6)

# |>select(city, zipcode, agi_stub, A19700, percentile)

library(tmap)
tmap_mode("view")

tm_shape(hawaii_se_data, projection = 26918) + 
  tm_fill(col = "percentile", 
          palette = "Greens", 
          title = "Charitable Donations by Zip, Percentile Nationally")

