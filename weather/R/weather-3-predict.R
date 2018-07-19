# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(scales)
library(rsample)
library(caret)
library(recipes)
library(yardstick)

setwd("weather")

# Import ------------------------------------------------------------------

load("data/2-tidy.RData")





# THis is for later -----------------------------------

# data_all <- data_all %>% 
#   mutate(angle = (yday - 1)/365*360,
#          day.x = cos(angle),
#          day.y = sin(angle))
# 
# data_all %>% select(yday, angle, day.x, day.y) %>% distinct()
# 
# data_all %>% select(yday, angle, day.x, day.y) %>% distinct() %>% 
#   ggplot(aes(x=day.x, y=day.y)) + geom_point()
# 
# 
# 
# set.seed(4595)
# data_temp_mean <- data_all %>% select(lat.0, lon.0, yday, day.x, day.y, temp_mean, flag)





# Max’s prediction class --------------------------------------------------

set.seed(4595)

data_select <- data_all %>% select(lat.0, lon.0, flag, yday, temp_mean, temp_max, temp_min, precip, snow)

data_missing <- data_select %>% filter(flag == "missing") %>% select(-flag)
data_exist <- data_select %>% filter(flag == "existing") %>% select(-flag)

data_split <- initial_split(data_exist, strata = "temp_mean")
data_train <- training(data_split)
data_test  <- testing(data_split)
nrow(data_train)/nrow(data_exist)


# KNN ---------------------------------------------------------------------

## Recipes 


mod_rec <- recipe( ~., data = data_train) %>% 
  add_role(temp_mean, temp_max, temp_min, precip, snow,
           new_role = "outcome") %>% 
  add_role(lat.0, lon.0, yday, new_role = "predictor") %>% 
  step_scale(all_predictors()) %>% 
  step_center(all_predictors())

mod_rec_trained <- prep(mod_rec, training = data_train, retain = TRUE, verbose = TRUE)

data_train_prep <- bake(mod_rec_trained,newdata = data_train)
data_test_prep <- bake(mod_rec_trained,newdata = data_test)
data_pred_prep <- bake(mod_rec_trained,newdata = data_missing)

f_knn <- function(col){

col <- quo(col)

knn_train_mod <- knnreg(!!col ~ lat.0 + lon.0 + yday, 
                        data = data_train_prep,
                        k = 5)
}


f_knn(temp_mean)
