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


# --------------- second part, getting general data for plots ---------------
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

