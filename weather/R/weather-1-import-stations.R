setwd("weather")

library(lubridate)
library(tidyverse)
library(sf)


# Load locations of interest ----------------------------------------------
load("data/locations.RData")

locs_join <- locations %>%  select(csafp, cbsafp)

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

stations_sf <- stations_all %>% 
  select(usaf, wban, lat, lon) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4269)


stations_join <- st_join(locs_join, stations_sf) 
st_geometry(stations_join) <- NULL

stations <- stations_all %>% 
  inner_join(stations_join, by = c("usaf", "wban"))

missing_data <- anti_join(stations_join, stations_all, by = c("usaf", "wban")) %>% select(cbsafp)

locations <- locations %>% anti_join(missing_data, by = "cbsafp")

ggplot(stations, aes(x=lon, y=lat)) + geom_point(size = 0.1, alpha = 0.1)

# 1.4 Save necessary data -----------------------------------------------------
save(stations, file = "data/stations.RData")
save(locations, file = "data/locations.RData")

