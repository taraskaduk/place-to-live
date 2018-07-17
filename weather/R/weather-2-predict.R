# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(scales)
library(rsample)
library(caret)
library(recipes)

setwd("weather")

# Import ------------------------------------------------------------------

load("data/1-import.RData")

# Data --------------------------------------------------------------------
stations_join <- stations_us %>% 
  select(usaf, wban, lat, lon)

weather <- weather_data %>% 
  inner_join(stations_join, by = c('stn' = 'usaf', 'wban'))

w_mutated <- weather %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0),
         
         year = year(date),
         month = month(date),
         day = day(date),
         day = if_else(day == 29 & month == 2, 28, as.numeric(day)),
         date = date(ISOdate(year,month,day)),
         yday = yday(date)
         ) %>% 
  select(-c(lat, lon, stn, wban)) %>% 
  group_by(lat.0, lon.0, date, year, month, day) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  purrr::map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', 'pressure', 'wind', 'gust'), ~ifelse(is.nan(.x), NA, .x)) %>% 
  bind_rows()


coords <- w_mutated %>% 
  select(lat.0, lon.0, year) %>% 
  distinct()

dummy <- tibble(date = seq.Date(date("2012-01-01"), date("2017-12-31"), by = "day")) %>% 
  filter(!(day(date)==29 & month(date)==2)) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  merge(coords, all = TRUE)

w_expanded <- w_mutated %>% 
  full_join(dummy, by = c("date", "lat.0", "lon.0", "year", "month", "day")) %>% 
  arrange(lat.0, lon.0, date)

w_missing <- w_expanded %>% 
  filter(is.na(temp_mean))


# Max’s prediction class --------------------------------------------------

set.seed(4595)
data_temp_mean <- w_mutated %>% select(lat.0, lon.0, yday, temp_mean)
data_split <- initial_split(data_temp_mean, strata = "temp_mean")
data_train <- training(data_split)
data_test  <- testing(data_split)
nrow(data_train)/nrow(w_mutated)


simple_lm <- lm(temp_mean ~ (lat.0 + lon.0) + yday, data = data_train)

summary(simple_lm)


cv_splits <- vfold_cv(data_train, v = 10)
cv_splits


# We'll need to write a function to fit the model to each data set and another to compute performance.

library(yardstick)
lm_fit <- function(data_split, ...) 
  lm(..., data = analysis(data_split))
# A formula is also needed for each model:
form <- as.formula(
  temp_mean ~ .
)

# For performance, the first argument should be the rsplit object contained in cv_splits$splits:
model_perf <- function(data_split, mod_obj) {
  vars <- rsample::form_pred(mod_obj$terms)
  assess_dat <- assessment(data_split) %>%
    select(!!!vars, temp_mean) %>%
    mutate(
      pred = predict(
        mod_obj, 
        newdata = assessment(data_split)
      )
    )
  rmse <- assess_dat %>% 
    rmse(truth = temp_mean, estimate = pred)
  rsq <- assess_dat %>% 
    rsq(truth = temp_mean, estimate = pred)
  data.frame(rmse = rmse, rsq = rsq)
}


# The purrr package will be used to fit the model to each analysis set. These will be saved in a column called lm_mod:
  
cv_splits <- cv_splits %>%
  mutate(lm_mod = purrr::map(splits, lm_fit, formula = form))
cv_splits 

# Now, let's compute the two performance measures:

lm_res <- map2_df(cv_splits$splits, cv_splits$lm_mod, model_perf) %>% 
  dplyr::rename(rmse_simple = rmse, rsq_simple = rsq)
head(lm_res, 3)

## Merge in results:
cv_splits <- cv_splits %>% bind_cols(lm_res)
## Rename the columns and compute the resampling estimates:
cv_splits %>% select(rmse_simple, rsq_simple) %>% colMeans

## Now let's look at diagnostics using the predictions from the assessment sets.

get_assessment <- function(splits, model) 
augment(model, newdata = assessment(splits)) %>%
mutate(.resid = temp_mean - .fitted)
holdout_results <- map2_df(cv_splits$splits, cv_splits$lm_mod, get_assessment)
holdout_results %>% dim()

data_train %>% dim()




# KNN ---------------------------------------------------------------------

## Recipes 

mod_rec <- recipe(temp_mean ~ lat.0 + lon.0 + yday, data = data_train) %>% 
  step_scale(all_numeric()) %>% 
  step_center(all_numeric())

mod_rec_trained <- prep(mod_rec, training = data_train, retain = TRUE, verbose = TRUE)

data_train_prep <- bake(mod_rec_trained,newdata = data_train)









knn_train_mod <- knnreg(temp_mean ~ lat.0 + lon.0 + yday, 
                        data = data_train_prep,
                        k = 10)

repredict <- data.frame(temp_mean_p = data_train_prep$temp_mean) %>%
  mutate(pred = 
           predict(knn_train_mod, 
                   newdata = data_train_prep)
  )
repredict %>% rsq(truth = "price", estimate = "pred") # <- the ruckus is here
