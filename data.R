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
          "B01003_001", "B02001_006")

# if we did zcta's, it must be ALL, then we filter.

county_gen <- get_acs(
  geography = "county",
  variables = vars,
  year = 2024,
  survey = "acs5"
)

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


haw_gen <- get_acs(
  geography = "tract",
  state = "HI",
  variables = vars,
  year = 2023,
  survey = "acs5",
  geometry = TRUE
) |>
  mutate(coeff_var = (moe/(1.645*estimate)) * 100) |>
  filter(coeff_var < 25)

haw_gen_wide <- haw_gen |> 
  pivot_wider(
    id_cols = c(GEOID, NAME, GEOMETRY),
    names_from = variable,
    values_from = estimate
  )

# tigris_hi_tract <- tracts(state = "HI", year = 2023)
library(readr)
xwalk <- read_csv("https://lehd.ces.census.gov/data/lodes/LODES8/hi/hi_xwalk.csv.gz") |>
  select(tabblk2020, trct)  # or stplc for places

rac_hi <- read.csv("data/LEHD-data/RAC.csv") |>
  mutate(year = as.character(year))

rac_tract <- rac_hi|>
  left_join(xwalk, by = c("h_geocode" = "tabblk2020")) |>
  group_by(trct) |>
  select(!h_geocode) |> #not needed
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) |>
  mutate(trct = as.character(trct))

haw_gen_complete <- haw_gen_wide |>
  left_join(rac_tract, by = c("GEOID" = 'trct'))

usa_gen <- get_acs(
  geography = "zcta",
  variables = vars, 
  year = 2024,
  survey = "acs5"
)

usa_gen_1 <- usa_gen |>
  mutate(ZCTA5CE20 = substring(NAME, 7, 11),
         coeff_var = (moe/(1.645*estimate)) * 100) |>
  filter(coeff_var < 25) # 30 is too unreliable, 20 is generally acceptable with caveats

haw_gen_1 <- hawaii_zctas |>
  inner_join(usa_gen_1, by = "ZCTA5CE20")

# plots

# wide test to not do this filter bs

haw_wide <- haw_gen_1 |>
  filter(!is.na(variable)) |>
  pivot_wider(
    id_cols = c(GEOID, geometry), # Keep geographic identifiers
    names_from = variable,       # Column names come from your variable IDs
    values_from = estimate        # Cell values come from your estimates
  )

usa_gen_wide <- usa_gen_1 |>
  filter(!is.na(variable)) |>
  pivot_wider(
    id_cols = c(GEOID), # Keep geographic identifiers
    names_from = variable,       # Column names come from your variable IDs
    values_from = estimate        # Cell values come from your estimates
  )

# --------------- PUMA Microdata for certain characteristics -----------------#

# to understand what I'm fetching
# View(pums_varipables)

# initial "SERIALNO"?
vars_micro <- c("SEX", "AGEP", "HHT", "SCHL", "ADJINC", 
                "FINCP", "HINCP", "INTP", "PUMA20", "REGION",
                "MIGSP", "POWPUMA20", "MIGPUMA20", "ST", 
                "GRPIP", "PINCP", "SVAL", "SRNT",
                "TEN", "VACS", "ESR", "VALP", "RELSHIPP")

states <- c("HI")

micro_hi <- get_pums(
  variables = vars_micro,
  state = states,
  survey = "acs5",
  return_vacant = FALSE,
  year = 2022,
  rep_weights = "housing",
  recode = TRUE
) 

# there has to be a smarter way to do this

# micro_hi_recode <- micro_hi |>
#   mutate(
#     sex = case_when(
#       SEX == "01" ~ "M",
#       SEX == "02" ~ "F",
#       TRUE ~ "Other"
#     ),
#     married = case_when(
#       HHT == "1" ~ "Married",
#       TRUE ~ "Not married"
#     ),
#     school = case_when(
#       SCHL == "023" ~ "Professional Degree Beyond Masters",
#     ),
#   )

# idea is to take the var_labels and then use that if variables categorical
# else we'll have to leave it up to what we know and rename things

micro_hi_rc <- micro_hi |>
  filter(grepl("^2022", SERIALNO)) |>
  mutate(
    sex = SEX_label, 
    married = HHT_label,
    age = AGEP,
    school = case_when(
      SCHL %in% c("16", "17") ~ "High School or Equivalent",
      SCHL %in% c("18", "19") ~ "Some College",
      SCHL == "20" ~ SCHL_label,
      SCHL == "21" ~ SCHL_label,
      SCHL == "22" ~ SCHL_label,
      SCHL == "23" ~ SCHL_label,
      SCHL == "24" ~ SCHL_label,
      TRUE ~ "Less than High School"
    )
  )

micro_hi <- micro_hi |>
  distinct(SERIALNO, .keep_all = TRUE) # TO GET TO HOUSEHOLD WEIGHTS ??

# we can cross tabulate and make tables of our own.
library(survey)
library(srvyr)

hi_survey <- micro_hi %>%
  to_survey(type = "housing", 
            design = "rep_weights")

class(hi_survey)

# simple survey counts:
hi_survey |>
  survey_count(MIGPUMA20)

# how many people left based on their income greater than 200k AGI?


