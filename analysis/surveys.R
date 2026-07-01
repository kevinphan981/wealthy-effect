library(sf)
library(spdep)
library(spatialreg)
library(tigris)
library(dplyr)
library(rgeoda)
options(tigris_use_cache = TRUE)

# ------------------- overview -------------------
# the best possible method here is likely matching (if we extend this to the rest of the country), or IV
# i think regardless, we will have to use pums or acs data. 


# some basic things
df <- read.csv('data/clean-data/final-df-spatial') |>
  mutate(GEOID = as.character(GEOID))

# ============= TREATMENT/MEASURE OF WEALTH =================
# we need to use the lognormal distribution to get better data on the wealthy
# we do the 95th percentile, but we could change it...
df <- df %>%
  mutate(
    sigma_hat  = sqrt(2) * qnorm((1 + B19083_001) / 2),
    wealth_p95 = B19013_001 * exp(sigma_hat * qnorm(0.95))
  )

# ================= GEOMETRY ===================
# will have to reintegrate geometry if needed for the model...
hi_tracts <- tracts(state = 'hi', cb = T, year = 2023) |>
  select(GEOID, geometry) |>
  mutate(GEOID = as.character(GEOID))

model_sf <- hi_tracts %>%
  inner_join(df, by = "GEOID") %>%
  st_as_sf()

cat("Tracts with geometry + data:", nrow(model_sf), "\n")
stopifnot(nrow(model_sf) == nrow(df))  # confirm no join loss



# =================== QUEEN CONTIGUITY WEIGHTS ======================

# rgeoda's queen_weights() is a one-line equivalent of spdep::poly2nb() +
# nb2listw(), and has_isolates() tells you upfront whether any tract has zero
# neighbors (expected here, since Hawaii is an archipelago and islands don't
# share a border). Per your call, we do NOT manually bridge islands -- we
# just let isolated tracts carry a zero-weight row and use zero.policy=TRUE
# everywhere downstream so spdep/spatialreg permit that instead of erroring.
# Trade-off: an isolated tract's spatial-lag term is 0, i.e. it behaves like
# a non-spatial observation for the lag/error structure.
qw <- queen_weights(model_sf)
cat("Any isolated tracts (no queen neighbors)?", has_isolates(qw), "\n")

#TRUE, we must check the neighbor lists to see if anything is wrong...
# Pull the full weights matrix and find rows that sum to zero -- no loop needed
# W <- as.matrix(qw)
# isolate_idx <- which(rowSums(W) == 0)
# cat("Number of isolated tracts:", length(isolate_idx), "\n")
# 
# # Inspect WHICH tracts these are
# model_sf[isolate_idx, c("GEOID", "NAME", "county")]
# 
# plot(st_geometry(model_sf), col = ifelse(seq_len(nrow(model_sf)) %in% isolate_idx,
#                                          "red", "grey90"),
#      main = "Isolated tracts (no queen-contiguous neighbors)")

#It's Lanai, no one cares.

# Convert rgeoda's weight object into an spdep "nb" list so we can still use
# spdep::lm.LMtests / spatialreg::lagsarlm / errorsarlm downstream, since
# rgeoda itself doesn't implement LM tests or SAR/SEM estimation.
# rgeoda_to_nb <- function(gda_w, n) {
#   nb <- vector("list", n)
#   for (i in seq_len(n)) {
#     nbrs <- get_neighbors(gda_w, idx = i)
#     nb[[i]] <- if (length(nbrs) == 0) 0L else as.integer(nbrs)
#   }
#   class(nb) <- "nb"
#   attr(nb, "region.id") <- as.character(seq_len(n))
#   nb
# }
# 
# nb <- rgeoda_to_nb(qw, nrow(model_sf))

nb <- poly2nb(model_sf, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ============== Model Variables ====================

model_sf <- model_sf %>%
  mutate(
    log_econ_power    = log(CE03/C000),
    log_services      = log((CNS09 + CNS010 + CNS012 + CNS013)/C000),
    log_C000          = log(C000),
    log_wealth_p95    = log(wealth_p95),
    log_median_income = log(B19013_001),
    log_pop           = log(B01003_001),
    log_area          = log(total_tract_area),
    log_home_value    = log(B25077_001),
    labor_force_rate  = B23025_002 / B01003_001,
    pct_bachelors     = B15003_022 / B01003_001,
    gini              = B19083_001
  )

# ==================Baseline OLS + Moran's I on residuals============

# i will have to specify more models in order to answer those specific types of questions
# involving the labor market and what jobs are out there.
form <- log_services ~  log_wealth_p95 + 
  labor_force_rate + pct_bachelors + log_home_value +
  Commercial + Residential + Agricultural

# log_wealth_p95 and log_median_income share two ultimate source columns
# (median income, Gini), and are correlated (~0.78 in this data) without
# being collinear to the point of non-identification. Check VIF before
# trusting individual coefficients:

# Restrict to complete cases so the weights matrix and design matrix align
# zero.policy stays TRUE throughout, since isolated (island) tracts from step
# 5 can persist in this subsample and would otherwise make nb2listw() error.
complete_idx <- complete.cases(model.frame(form, data = model_sf, na.action = na.pass))
model_sf_cc  <- model_sf[complete_idx, ]
nb_cc        <- subset(nb, complete_idx, zero.policy = TRUE)
lw_cc        <- nb2listw(nb_cc, style = "W", zero.policy = TRUE)
cat("Estimation sample after complete-case restriction:", nrow(model_sf_cc), "\n")
cat("Isolated tracts in estimation sample:",
    sum(card(nb_cc) == 0), "\n")

ols <- lm(form, data = model_sf_cc)
summary(ols)
car::vif(ols) # for multicolinearity


cat("\n--- Moran's I on the outcome ---\n")
print(moran.test(model_sf_cc$log_econ_power, lw_cc, zero.policy = TRUE))

cat("\n--- Moran's I on OLS residuals ---\n")
print(lm.morantest(ols, lw_cc, zero.policy = TRUE))


# ============== Lagrange Multiplier tests: lag vs. error vs. both ==============
lm_tests <- lm.RStests(ols, lw_cc, zero.policy = TRUE,
                       test = c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
print(lm_tests)
# Decision rule:
#   - If LMerr sig but LMlag not -> spatial error model (SEM)
#   - If LMlag sig but LMerr not -> spatial lag model (SAR)
#   - If both sig, check robust versions (RLMerr/RLMlag): whichever robust
#     test remains significant identifies the correct model
#   - If both robust tests are significant -> consider a SARAR/SAC model

# based on this, since adjrslag insignificant when adjrserr is significant, we do SEM.



# ====================== TRUE MODEL =========================
# -- Spatial lag (SAR): use if LMlag/RLMlag wins
sar_model <- lagsarlm(form, data = model_sf_cc, listw = lw_cc, zero.policy = TRUE)
summary(sar_model, Nagelkerke = TRUE)

# -- Spatial error (SEM): use if LMerr/RLMerr wins
sem_model <- errorsarlm(form, data = model_sf_cc, listw = lw_cc, zero.policy = TRUE)
summary(sem_model, Nagelkerke = TRUE)

# -- SARAR/SAC: use if both robust LM tests are significant
sarar_model <- sacsarlm(form, data = model_sf_cc, listw = lw_cc, zero.policy = TRUE)
summary(sarar_model, Nagelkerke = TRUE)

# Compare fit (pick the model your LM test in step 8 pointed to; AIC as a
# secondary check across the fitted candidates)
AIC(ols, sar_model, sem_model, sarar_model)


# SDEM Model with County Fixed Effects
sdem_model <- errorsarlm(
  log_econ_power ~ log_wealth_p95 +
    labor_force_rate + pct_bachelors + log_home_value +
    Commercial + Residential + Agricultural + factor(county), # Added County Fixed Effects
  data = model_sf_cc, 
  listw = lw_cc, 
  etype = "emix", # Turning SEM into an SDEM (adds lags of X variables)
  zero.policy = TRUE
)

summary(sdem_model)

# ============== PLOTS =================
# there generally seems to be a relationship between 

df |>
  ggplot(aes(x = B19083_001, y = C000)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = "Gini Coefficient, 0-1", y = "Resided Workers in Given Area")

summary(lm(formula = "C000 ~ B19083_001", data = df))

# are wealthier areas necessarily more unequal?
df |>
  ggplot(aes(x = B19083_001, y = B19013_001)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = "Gini Coefficient, 0-1", y = "Median Household Income")
# quite homogenous areas, which makes sense given the living dynamics...

# most of B19080_005 is actually missing, good to look at though
df |>
  ggplot(aes(x = B19083_001, y = B19080_005)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = "Gini Coefficient, 0-1", y = "Lower Limit for Top 5%")


# the number of workers that live in an area do not have anything to do with the unemployment rate...
summary(lm("DP03_0009P ~ C000", data = df))

summary(lm("DP03_0009P ~ B17001_002 + S1501_C02_015 + B11007_001 + B25056_001", data = df))

plot(df$B19083_001, df$C000)



