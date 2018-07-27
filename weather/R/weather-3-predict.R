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
library(RANN)

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

r <- 365 / (2 * pi) #if 1 year is 365 days, it is a line that is 365 units long. Now make it a circle. That's circumference. Now calc the radius

data_select <- data_all %>% 
  mutate(day.x = cos((yday - 1)/365*360)*r,
         day.y = sin((yday - 1)/365*360)*r) %>% 
  select(lat.0, lon.0, day.x, day.y, year, flag, temp_mean, temp_max, temp_min, precip, snow)

data_missing <- data_select %>% filter(flag == "missing") %>% select(-flag)
data_exist <- data_select %>% filter(flag == "existing") %>% select(-flag)

# data_split <- initial_split(data_exist, strata = "temp_mean")
# data_train <- training(data_split)
# data_test  <- testing(data_split)
# nrow(data_train)/nrow(data_exist)
# 
# data_sample <- data_select %>%
#   select(-flag) %>%
#   arrange(lat.0, lon.0, yday) %>%
#   head(15000)
# 
# 
# 
# 
# 
# # LM… This is shiiiit!!! --------------------------------------------------
# 
# mod_lm <- lm(temp_mean ~ lat.0 + lon.0 + day.x + day.y, data = data_train)
# 
# pred_data <- data_test %>%
#   mutate(pred = predict(mod_lm, newdata = .))
# 
# summary(mod_lm)





# Preprocessing -----------------------------------------------------------

data_train <- data_select %>% 
  filter(!is.na(temp_max)) %>% 
  select(lat.0:year, temp_max)

data_pred <- data_select %>% 
  filter(is.na(temp_max) & !is.na(temp_mean)) %>% 
  select(lat.0:year, temp_max)

knn_data <- data_select %>% 
  mutate(prep_predict = if_else(is.na(temp_max) & !is.na(temp_mean), 1, 0),
         prep_train = if_else(!is.na(temp_max), 1, 0))

nn <- nn2(data = knn_data %>% filter(prep_predict == 1) %>% select(lat.0:year),
          query = knn_data %>% filter(prep_train == 1) %>% select(lat.0:year),
          k = 10)

index_leave <- nn[[1]] %>% as.vector() %>% unique()

knndata$flag <- knn_datachrome[index_leave, ] 

  
#formula <- as.formula(substitute(col ~ lat.0 + lon.0 + day.x + day.y + year, list(col = as.name(col))))
formula <- as.formula(temp_max ~ lat.0 + lon.0 + day.x + day.y + year)


knn_train_mod <- knnreg(formula = formula, 
                        data = data_train,
                        k = 5)

#col <- quo_name(enquo(col))


predict(knn_train_mod, newdata = data_pred)

# pred_data <- df_pred %>% 
#   head(100) %>% 
#   mutate(!!col := predict(knn_train_mod, newdata = .))



data_predicted <- data_pred %>% 
  mutate(temp_max = predict(knn_train_mod, newdata = .))





# KNN ---------------------------------------------------------------------

## Recipes.... NOPE

# data_join <- data_leave %>% 
#   union_all(data_missing)

# mod_rec <- recipe(~., data = data_join) %>% 
#   add_role(temp_mean, temp_max, temp_min, precip, snow,
#            new_role = "outcome") %>% 
#   add_role(lat.0, lon.0, yday, new_role = "predictor") %>% 
  # step_scale(all_predictors()) %>% 
  # step_center(all_predictors()) %>% 
  #step_knnimpute(all_numeric(), -all_predictors())

#mod_prep <- prep(mod_rec, training = data_, retain = TRUE)



f_knn <- function(df_train = data_leave, df_pred = data_missing, col){
  
  formula <- as.formula(substitute(col ~ lat.0 + lon.0 + day.x + day.y + year, list(col = as.name(col))))
  
  knn_train_mod <- knnreg(formula = formula, 
                          data = df_train,
                          k = 5)
  
  col <- quo_name(enquo(col))
  
  
  predict(knn_train_mod, newdata = df_pred)
  
  # pred_data <- df_pred %>% 
  #   head(100) %>% 
  #   mutate(!!col := predict(knn_train_mod, newdata = .))
  
}


data_predicted <- data_missing %>% 
  mutate(temp_mean = f_knn(df_pred = ., col = "temp_mean"),
         temp_min =  f_knn(df_pred = ., col = "temp_min"),
         temp_max =  f_knn(df_pred = ., col = "temp_max"),
         precip =    f_knn(df_pred = ., col = "precip"),
         snow =      f_knn(df_pred = ., col = "snow"))
  

data_predicted2 <- data_all %>% 
  filter(flag == "missing") %>% 
  select(-c(temp_mean:is_tornado)) %>% 
  left_join(data_predicted, by = c("lat.0", "lon.0", "yday", "year"))
  
w_filled <- data_all %>% 
  anti_join(data_predicted2, by = c("lat.0", "lon.0", "yday", "year")) %>% 
  union_all(data_predicted2)

## -------------

save(w_filled, file = "data/3-predict.RData")

