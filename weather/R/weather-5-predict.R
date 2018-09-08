setwd("weather")

library(tidyverse)
library(lubridate)
library(caret)
library(broom)

data <- readRDS("data/4-data-tidied.RDS")
locations <- readRDS("data/2-locations-filtered.RDS")


dummy <- data %>% 
  select(cbsafp) %>% 
  distinct() %>% 
  merge(tibble(date = seq.Date(date("2013-01-01"), date("2017-12-31"), by = "day")), all = TRUE) 


full_data <- dummy %>% 
  left_join(data, by = c("cbsafp", "date")) 

data_gathered <- full_data %>% 
  gather(metric, value, -c(cbsafp, date)) %>% 
  mutate(flag = if_else(is.na(value), "missing", "train"),
         yday = yday(date),
         year = year(date))

data_nested <- data_gathered %>% 
  group_by(cbsafp, metric, flag) %>% 
  nest() %>% 
  spread(flag, data) %>% 
  filter(missing != "NULL" & metric != "gust" & train != "NULL")
  

f_lm <- function(df){
  lm(value ~ yday + year, data = df)
}

f_knnreg <- function(df){
  knnreg(value ~ yday + year, data = df, k = 4)
}


data_modeled <- data_nested %>% 
  mutate(lm = train %>% map(f_lm), 
         knn = train %>% map(f_knnreg)) %>% 
  mutate(pred_knn = map2(.x = knn, .y = missing, .f = predict))


data_unnested <- data_modeled %>% 
  select(-train, -lm, -knn) %>% 
  unnest(missing, pred_knn) %>% 
  mutate(value = pred_knn) %>% 
  select(-pred_knn)
  
data_complete <- data_gathered %>% 
  filter(!is.na(value) & metric != "gust") %>% 
  union_all(data_unnested) %>% 
  spread(metric, value)


saveRDS(data_complete, file = "data/5-data-predicted.RDS")

