setwd("weather")

library(RANN)
library(caret)
library(lubridate)
library(tidyverse)

load("data/1-import.RData")

# 2.1 Tidy -----------------------------------------------------------------

# stations_join <- stations_us %>% 
#   select(usaf, wban, lat, lon)

# data_coords <- data_weather %>% 
#   inner_join(stations_join, by = c('stn' = 'usaf', 'wban'))



## Previously, I was trying to get average readings for 3-5 closest stations to each city, and was using RANN's ANN algo to find the nearest neighbors,
## but also to predict the missing values.
## I was getting some weird results, particularly for Philly, where my outcome was ZERO rainy or snowy days in 5 years.
## (I was isolating Philly early - I wasn't getting the same results)

## I am thinking I should do something else now - treat each metric as an observation, and for each day find

data_weather$date <- as.Date(data_weather$date)

data_tidy <- data_weather %>% 
  mutate(precip = if_else(is.na(precip) & is_rain == 0, 0, precip),
         is_element = if_else(is_rain + is_hail + is_snow > 0, 1, 0))

data_gather <- data_tidy %>% 
  select(stn:snow, is_element) %>% 
  gather(metric, value, -c(stn:date)) %>% 
  filter(!is.na(value))


# add st index - we'll need it later for joining
stations_us <- stations_us %>% 
  mutate(index_st = as.integer(rownames(.)))

locations <- locations %>% 
  mutate(index_loc = as.integer(rownames(.)))

# Find the closest neighbors for each city among stations
nn <- nn2(data = stations_us %>% select(lat,lon),
          query = locations %>% select(lat,lon),
          k = 5)

##So, radius var in nn2 foesn't seem to filter out stations far away. PR ends up pulling Miami stations. The next sequence addresses it.
nn1_df <- as_data_frame(nn[[1]]) %>% 
  mutate(index_loc = as.integer(rownames(.))) %>% 
  gather(V, index_st, V1:V5)
  
nn2_df <- as_data_frame(nn[[2]]) %>% 
  mutate(index_loc = as.integer(rownames(.))) %>% 
  gather(V, dist, V1:V5)
  
nn_df <- nn1_df %>% 
  inner_join(nn2_df, by = c("index_loc", "V")) %>% 
  #and this is where I filter out remote locations
  filter(dist <= 2) %>% 
  mutate(rank = str_remove(V, "V") %>% as.integer()) %>% 
  select(-c(V,dist))




## add a new prikey - index - to the data
data_join <- data_gather %>% 
  inner_join(stations_us %>% select(usaf, wban, index_st), by = c('stn' = 'usaf', 'wban')) %>% 
  select(-c(stn, wban))


##rm(nn1_df, nn2_df, nn, data_weather, data_gather)


## Now to the big join.
## First, take locations.
## Then, find 5 closest stations for each
## Then, join the actual data from each station
## Lastly, filter to return the top reading

nn_subset <- nn_df %>% 
  filter(rank == 1)


locations_join <- locations %>% 
  select(index_loc) %>% 
  left_join(nn_subset, by = "index_loc") %>% 
  # join dates - daily grain from 2013 to 2017
  merge(tibble(date = seq.Date(date("2013-01-01"), date("2017-12-31"), by = "day")), all = TRUE) %>% 
  merge(tibble(metric = c("temp_mean", "temp_max", "temp_min", "precip", "snow", "is_element")), all = TRUE)


# ---------
  ## TBD


locs

%>%
  inner_join(data_forjoin, by = c("date", "index_st", "metric"))




## This is a very slow piece of code. I guess I could have written a for loop to bring station observations one by one - 
##... the closest one first, then for NAs - repeat again... etc.


data_filtered <- data_joined %>% 
  select(-index_st) %>% 
  group_by(index_loc, date, metric) %>% 
  arrange(rank) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

data_spread <- data_filtered %>% 
  spread(metric, value)










loc_nn <- locations %>% 
  mutate(i = rownames(.)) %>% 
  inner_join(nn_df_spread, by = "i")





result_dummy <- loc_nn %>% 
  select(geoid, name, lat, lon, V1:V3) %>% 
  distinct() %>% 
  merge(tibble(date = seq.Date(date("2013-01-01"), date("2017-12-31"), by = "day")), all = TRUE)

result_gathered <- result_dummy %>% 
  mutate(year = year(date),
         yday = yday(date)) %>% 
  gather(key = "neighbor", value = "index", c(V1:V3)) %>% 
  mutate(index = as.character(index)) %>% 
  filter(!is.na(index))

data_tojoin <- data_nn %>% 
  mutate(date = as.Date(date)) %>% 
  select(index, date, temp_mean:snow, is_rain, is_snow)

result_joined <- result_gathered %>% 
  left_join(data_tojoin, by=c("index", "date"))



result_summed <- result_joined %>% 
  group_by(geoid, name, lat, lon, date, year, yday) %>% 
  summarise(temp_mean = mean(temp_mean, na.rm = TRUE),
            temp_min = mean(temp_min, na.rm = TRUE),
            temp_max = mean(temp_max, na.rm = TRUE),
            precip = max(precip, na.rm = TRUE),
            snow = max(snow, na.rm = TRUE),
            is_rain = max(is_rain, na.rm = TRUE),
            is_snow = max(is_snow, na.rm = TRUE)) %>% 
  ungroup() %>% 
  purrr::map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', "is_rain", "is_snow"), ~ifelse(is.nan(.x) | is.infinite(.x), NA, .x)) %>% 
  bind_rows() %>% 
  mutate(is_na = if_else(rowSums(is.na(.)) > 0, 1, 0))






knn_train <- data_nn %>% 
  select(lat, lon, date:snow, is_rain, is_snow) %>% 
  mutate(year = year(date),
         yday = yday(date))



f_knn <- function(df_train = knn_train, 
                  df_pred, col){
  
  nn <- nn2(data = df_train %>% select(lat,lon,yday,year),
            query = df_pred %>% select(lat,lon,yday,year),
            k = 5)
  i_nn <- nn[[1]] %>% as.vector() %>% unique()
  
  df_train_nn <- df_train[i_nn, ]
  
  formula <- as.formula(substitute(col ~ lat + lon + yday + year, list(col = as.name(col))))
  knn_model <- knnreg(formula = formula, 
                      data = df_train_nn,
                      k = 5)
  col <- quo_name(enquo(col))
  
  predict(knn_model, newdata = df_pred)
  
}


## This is where it will probably choke...

result_predicted <- result_summed %>%
  filter(is_na == 1) %>% 
  ##I just can't work this into the function... I give up
  mutate(temp_mean = if_else(is.na(temp_mean), f_knn(df_pred = ., col = "temp_mean"), temp_mean),
         temp_max = if_else(is.na(temp_max), f_knn(df_pred = ., col = "temp_max"), temp_max),
         temp_min = if_else(is.na(temp_min), f_knn(df_pred = ., col = "temp_min"), temp_min),
         precip = if_else(is.na(precip), f_knn(df_pred = ., col = "precip"), precip),
         snow = if_else(is.na(snow), f_knn(df_pred = ., col = "snow"), snow),
         is_rain = if_else(is.na(is_rain), f_knn(df_pred = ., col = "is_rain"), is_rain),
         is_snow = if_else(is.na(is_snow), f_knn(df_pred = ., col = "is_snow"), is_snow)) %>% 
  union_all(result_summed %>% filter(is_na == 0)) %>% 
  select(-is_na)


data <- result_predicted

## Checkpoint
save(data, locations, file = "data/2-tidy.RData")

