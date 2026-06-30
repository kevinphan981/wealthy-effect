library(tidyverse)
library(survey)
library(tidycensus)
library(srvyr)
library(fixest)
library(corrplot)

# should just read the other data file in order to get the env set up
# source("data.R"

# ------------------- overview -------------------
# the best possible method here is likely matching (if we extend this to the rest of the country), or IV
# i think regardless, we will have to use pums or acs data. 


# some basic things

haw_gen_complete <- haw_gen_complete |>
  mutate(wealth = B19080_005/B19083_001)

haw_gen_num <- haw_gen_complete |>
  select(-c('GEOID', 'NAME'))

cor(haw_gen_num, method = 'pearson') # waht?? 


# there generally seems to be a relationship between 
haw_gen_complete |>
  ggplot(aes(x = B19083_001, y = C000)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = "Gini Coefficient, 0-1", y = "Resided Workers in Given Area")

summary(lm(formula = "C000 ~ B19083_001", data = haw_gen_complete))

# are wealthier areas necessarily more unequal?
haw_gen_complete |>
  ggplot(aes(x = B19083_001, y = B19013_001)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = "Gini Coefficient, 0-1", y = "Median Household Income")
# quite homogenous areas, which makes sense given the living dynamics...

haw_gen_complete |>
  ggplot(aes(x = B19083_001, y = B19080_005)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = "Gini Coefficient, 0-1", y = "Lower Limit for Top 5%")


haw_gen_complete |>
  ggplot(aes(x = wealth, y = C000)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = "Wealthy People Index", y = "Resided Workers in Given Area")

summary(lm("C000 ~ wealth", data = haw_gen_complete))

# the number of workers that live in an area do not have anything to do with the unemployment rate...
summary(lm("DP03_0009P ~ C000", data = haw_gen_complete))

summary(lm("DP03_0009P ~ wealth + B17001_002 + S1501_C02_015 + B11007_001 + B25056_001", data = haw_gen_complete))

plot(haw_gen_complete$B19083_001, haw_gen_complete$C000)
# 1. basic linear regressions to find relationships

# Travel time as outcome—does longer commute correlate with lower income?
model1 <- lm("B17001_002 ~ B19013_001 + B25077_001", data = usa_gen_wide)

summary(model1)

# Education's return on income
summary(lm(log(B19013_001) ~ B23025_002 + B19057_001 + B01003_001, data = usa_gen_wide))

# Education's effect on poverty ( Wald/instrumental variable setup possible )
summary(lm(log(B17001_001) ~ B23025_002 + B19013_001, data = usa_gen_wide))


# Do wealthier areas impact unemployment?

# ------------------- microdata-based models ---------------------------

# ensure that the data is transformed correctly before deploying models

# tenure TEN dependent, want to measure the effect of Household income HINCP
mp_m1 <- svyglm(TEN ~ SEX + SCHL + HINCP + ESR,
  design = hi_survey,
  weights = ~pw
)

summary(mp_m1)



