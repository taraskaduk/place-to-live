setwd("weather")

library(tidyverse)
library(lubridate)

data <- readRDS("data/data-tidied.RDS")
locations <- readRDS("data/locations-filtered.RDS")


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
  

f_model <- function(df){
  lm(value ~ yday + year, data = df)
}

data_modeled <- data_nested %>% 
  mutate(model = train %>% map(f_model)) %>% 
  mutate(pred = map2(.x = model, .y = missing, .f = predict))


data_unnested <- data_modeled %>% 
  select(-train, -model) %>% 
  unnest(missing, pred) %>% 
  mutate(value = pred) %>% 
  select(-pred)
  
data_complete <- data_gathered %>% 
  filter(!is.na(value) & metric != "gust") %>% 
  union_all(data_unnested) %>% 
  spread(metric, value)


saveRDS(data_complete, file = "data/data-predicted.RDS")

