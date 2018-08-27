setwd("weather")

library(lubridate)
library(tidyverse)
library(sf)
library(maps)

# Load locations of interest ----------------------------------------------
load("data/locations-import.RData")

locs_join <- locations %>%  select(csafp, cbsafp)
# test <- locs_join %>% filter(cbsafp == 42660)
# test_scale <-  test$geometry * 1.5
locs_buffer <- st_buffer(locs_join, dist = 0.3, nQuadSegs = 1)
# ggplot() +  geom_sf(data = locs_buffer) + geom_sf(data = test)

# Import stations ---------------------------------------------------------------

# path_stations <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt"
path_stations <- "data/0-raw/isd-history.txt"

stations_import <- read_table(path_stations,
                              col_types = cols(BEGIN = col_date(format = "%Y%m%d"), 
                                               CALL = col_character(), 
                                               CTRY = col_character(), 
                                               `ELEV(M)` = col_double(), 
                                               END = col_date(format = "%Y%m%d"), 
                                               LAT = col_double(), 
                                               LON = col_double(), 
                                               ST = col_character(), 
                                               `STATION NAME` = col_character(), 
                                               USAF = col_character(), 
                                               WBAN = col_character()),
                              skip = 20,
                              na = c('-999.0', ''))

colnames(stations_import) <- tolower(colnames(stations_import))


stations_all <- stations_import %>% 
  rename(station = `station name`,
         elev = `elev(m)`) %>% 
  filter(!is.na(usaf) & !(is.na(lat) | is.na(lon)))

stations_land <- stations_all %>% 
  mutate(country = map.where('world', lon, lat),
         lakes = map.where('lakes', lon, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)

stations_sf <- stations_land %>% 
  select(usaf, wban, lat, lon) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4269)



stations_join <- st_join(locs_join, stations_sf) 
st_geometry(stations_join) <- NULL


## Wider net
stations_buffer <- st_join(locs_buffer, stations_sf) 
st_geometry(stations_buffer) <- NULL


stations <- stations_land %>% 
  inner_join(stations_buffer, by = c("usaf", "wban"))

missing_data <- anti_join(stations_buffer, stations_all, by = c("usaf", "wban")) %>% select(cbsafp)

locations <- locations %>% anti_join(missing_data, by = "cbsafp")
st_geometry(locations) <- NULL

ggplot(stations, aes(x=lon, y=lat)) + geom_point(size = 0.1, alpha = 0.1)

# 1.4 Save necessary data -----------------------------------------------------
save(stations, file = "data/stations.RData")
save(locations, file = "data/locations.RData")

