library(tidyverse)
library(survey)
library(tidycensus)

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
