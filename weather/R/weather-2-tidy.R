# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)

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
         flag = "existing",
         year = year(date),
         month = month(date),
         day = day(date),
         day = if_else(day == 29 & month == 2, 28, as.numeric(day)),
         date = date(ISOdate(year,month,day)),
         yday = yday(date)
         ) %>% 
  select(-c(lat, lon, stn, wban)) %>% 
  group_by(lat.0, lon.0, flag, date, year, month, day, yday) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  purrr::map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', 'pressure', 'wind', 'gust'), ~ifelse(is.nan(.x), NA, .x)) %>% 
  bind_rows()


coords <- w_mutated %>% 
  select(lat.0, lon.0, year) %>% 
  distinct()

dummy <- tibble(date = seq.Date(date("2012-01-01"), date("2017-12-31"), by = "day")) %>% 
  filter(!(day(date)==29 & month(date)==2)) %>% 
  mutate(flag = "missing",
         year = year(date),
         month = month(date),
         day = day(date),
         yday = yday(date)) %>% 
  merge(coords, all = TRUE)

data_missing <- dummy %>% 
  anti_join(w_mutated, by = c("lat.0","lon.0","date"))

data_all <- w_mutated %>% 
  union_all(data_missing)

data_all %>% group_by(flag) %>% count()

save(data_all, file = "data/2-tidy.RData")
