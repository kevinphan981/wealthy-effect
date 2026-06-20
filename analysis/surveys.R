library(tidyverse)
library(survey)
library(tidycensus)
library(srvyr)
library(fixest)

# should just read the other data file in order to get the env set up
# source("data.R"

# ------------------- overview -------------------
# the best possible method here is likely matching (if we extend this to the rest of the country), or IV
# i think regardless, we will have to use pums or acs data. 

# 1. basic linear regressions to find relationships
# Travel time as outcome—does longer commute correlate with lower income?
model1 <- lm("B17001_002 ~ B19013_001 + B25077_001", data = usa_gen_wide)

summary(model1)

# Education's return on income
summary(lm(log(B19013_001) ~ B23025_002 + B19057_001 + B01003_001, data = usa_gen_wide))

# Education's effect on poverty ( Wald/instrumental variable setup possible )
summary(lm(log(B17001_001) ~ B23025_002 + B19013_001, data = usa_gen_wide))


# ------------------- microdata-based models ---------------------------

# ensure that the data is transformed correctly before deploying models

# tenure TEN dependent, want to measure the effect of Household income HINCP
mp_m1 <- svyglm(TEN ~ SEX + SCHL + HINCP + ESR,
  design = hi_survey,
  weights = ~pw
)

summary(mp_m1)



