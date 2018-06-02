# Libraries ---------------------------------------------------------
library(tidyverse)
library(readr)
library(rvest)
library(tools)
library(stringr)
library(maps)
library(jsonlite)
library(geosphere)
library(rnoaa)


options("noaakey" = Sys.getenv("noaakey"))

# Cities ------------------------------------------------------------------

url_cities <- 'https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv'

cities <- read_csv(url_cities) %>% 
  rename(latitude = lat,
         longitude = lng)

ggplot(cities, aes(x = longitude, y = latitude)) + 
  geom_point(alpha = 0.2, size = 0.1) +
  theme_light()

# Stations ----------------------------------------------------------------

# #Scrape all this!!!
# stations <- read_csv("stations_final.csv")
# stations %>% filter(city == 'Jacksonville' & state == 'Florida')
# 
# stations_usa <- stations %>% 
#   semi_join(cities, by = c('city', 'state'))
# 
# n_distinct(stations_usa$city)


# this file looks like a tab separated file, however readr doesn't take the \t. 
# Whitespace does the job, but oblviously breaks things on the city column. 
# I therefore skip everything after the altitude column, as that data is not important to me
stations_import <- read_table2("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 
                              col_names = c("station", "lat", "lon", "alt"), 
                              col_types = cols(X5 = col_skip(), 
                                               X6 = col_skip(), 
                                               X7 = col_skip(), 
                                               X8 = col_skip(), 
                                               X9 = col_skip()
                              ))

# Back up
write_csv(stations_import, 'stations_import.csv')

ggplot() + 
  geom_point(data = stations_import, aes(x = lon, y = lat), alpha = 0.1, size = 0.1, col = 'red') +
  geom_point(data = cities, aes(x = longitude, y = latitude), alpha = 0.2, size = 0.1, col = 'blue') +
  theme_light()


# Get stations data -------------------------------------------------------


## Initial download - uncomment
 url_gsoy <- "https://www.ncei.noaa.gov/data/gsoy/archive/gsoy-latest.tar.gz"
 destfile <- "gsoy.tar.gz"
 curl::curl_download(url_gsoy, destfile)
 untar(destfile, exdir = "gsoy")


files_all <- list.files(path = "gsoy")

# Subset leaving only stations for which we have the data
stations <- stations_import %>% 
  mutate(file = paste0(station,".csv")) %>% 
  filter(file %in% files_all)

remove(stations_import)

# Test before creating a map of purrr -------------------------------------

radius <- 0.3

cities_test <- cities %>% 
  filter(id == "1840022312")

cities_merge_test <- cities_test %>% 
  select(id, latitude, longitude)

stations_needed_test <- cities_merge_test %>% 
  merge(stations %>% select(-alt) %>% 
          filter(lat <= cities_test$latitude + radius &
                   lat >= cities_test$latitude - radius &
                   lon <= cities_test$longitude + radius &
                   lon >= cities_test$longitude - radius), all = TRUE) %>% 
  mutate(distance = distHaversine(cbind(longitude, latitude), cbind(lon, lat))) %>% 
  arrange(distance) %>% 
  head(1)



stations_needed <- stations_needed_test[1,]

cities_merge <- cities %>% 
  select(id, latitude, longitude)

for (i in 1:nrow(cities_merge)) {
  stations_needed[i, ] <- cities_merge[i, ] %>% 
    select(id, latitude, longitude) %>% 
    merge(stations %>% select(-alt) %>% 
            filter(lat <= cities_merge[i,]$latitude + radius &
                     lat >= cities_merge[i,]$latitude - radius &
                     lon <= cities_merge[i,]$longitude + radius &
                     lon >= cities_merge[i, ]$longitude - radius), all = TRUE) %>% 
    mutate(distance = distHaversine(cbind(longitude, latitude), cbind(lon, lat))) %>% 
    arrange(distance) %>% 
    head(1)
}






## Far far away
for (i in 1:nrow(cities_merge)) {
  for (radius in seq(0.1, 3.1, by = 0.5)) {
    stations_merge <- stations %>% select(-alt) %>% 
      filter(lat <= cities_merge[i,]$latitude + radius &
               lat >= cities_merge[i,]$latitude - radius &
               lon <= cities_merge[i,]$longitude + radius &
               lon >= cities_merge[i, ]$longitude - radius)
    if(nrow(stations_merge) != 0) break
  }

  
  stations_needed[i, ] <- cities_merge[i, ] %>% 
    merge(stations_merge, all = TRUE) %>% 
    mutate(distance = distHaversine(cbind(longitude, latitude), cbind(lon, lat))) %>% 
    arrange(distance) %>% 
    head(1)
}






ggplot(cities_stations) + 
  geom_point(aes(x = lon, y = lat), alpha = 0.1, col = 'red', size = 3) +
  geom_point(aes(x = longitude, y = latitude), alpha = 0.2, col = 'blue') +
  theme_light()


stations_needed_dist <- stations_needed %>% 
  select(station) %>% 
  mutate(station = paste0(station,".csv")) %>% 
  distinct() %>% .[ , "station"]


files_remove <- subset(files_all, !(files_all %in% stations_needed_dist))
files_missing <- subset(stations_needed_dist, !(stations_needed_dist %in% files_all))

setwd("gsoy")
file.remove(files_remove)
setwd("..")

file_left <-  list.files(path = "gsoy")

file.remove(destfile)























# Get the weather data ----------------------------------------------------
## SO, this isn't working... Bummer

year <- "2016"

stations_neeed <- cities_stations %>% 
  select(usaf, wban, station) %>% 
  distinct() %>% 
  mutate(ftp = paste0("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/", year, "/", usaf, "-", wban, "-", year, ".op.gz")) %>% 
  head(10) %>% 
  mutate(data = purrr::map(ftp, safely(read_table)))

map(stations_neeed$ftp, read_table)


year <- 2017
file <- paste0(station,'-',year)
station <- '722066-03853'
# ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
ftp_st <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file2,'.op.gz')
destfile <- paste0(file2,'.op.gz')

table <- read_table(ftp_st)




# New attempt -------------------------------------------------------------





# https://simplemaps.com/data/us-cities
# https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv



ncdc(datasetid = "GSOY",
     locationid = "FIPS:02", 
     startdate = "2012-01-01", 
     enddate = "2013-01-01")
























# Initial import. run and comment out - why tease the ftp with multiple requests?
year <- 1987
file <- paste0('gsod_',year)
file2 <- paste0(station,'-',year)
station <- '722066-03853'
# ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
ftp_st <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file2,'.op.gz')
destfile <- paste0(file2,'.op.gz')

table <- read_table(ftp_st)

curl::curl_download(ftp_st, destfile)
unzip(destfile, exdir = file2)

# data_all <- map_df(list.files(file, full.names = TRUE), read_table, col_types = c('cccccccccccccccccccccc'))
# write_csv(data_all, paste0(file, '.csv'))

data_all <- read_csv("gsod_2017.csv", 
                     col_types = cols(FRSHTT = col_character(), 
                                      WBAN = col_character(),
                                      YEARMODA = col_date(format = "%Y%m%d")),
                     na = c("9999.9", '99999', '99.99', '999.9', '0.00I'))

colnames(data_all) <- tolower(colnames(data_all))

data_clean <- data_all %>% 
  # rename columns
  select(stn = `stn---`, wban, date = yearmoda, temp_mean = temp, temp_min = min, temp_max = max, prcp, sndp) %>% 
  # clean up asterisks and flags
  # (see data description for details)
  map_df(~ str_replace_all(.,'A|B|C|D|E|F|G|H|I|\\*', '')) %>% 
  # convert some columns to numeric after cleaning up
  map_at(c('temp_mean', 'temp_min', 'temp_max', 'prcp', 'sndp'), as.numeric) %>% 
  dplyr::bind_rows() %>% 
  # precipitation and snowfall NAs can be converted to 0 for this project 
  # (see data description for details)
  replace_na(replace = list(prcp = 0, sndp = 0))

stations <- read_table("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt", 
                       skip = 20,
                       na = c('999999', '99999', '-999.0', ''))

colnames(stations) <- tolower(colnames(stations))

stations_us <- stations %>% 
  select(usaf, state = st, lat, lon) %>% 
  filter(!is.na(lat) & !is.na(lon)) %>% 
  mutate(usa = map.where('usa', lon, lat),
         lakes = map.where('lakes', lon, lat)) %>% 
  filter(!is.na(usa) & is.na(lakes)) %>% 
  select(-c(usa,lakes)) %>% 
  distinct() %>% 
  group_by(usaf) %>% 
  summarise(state = max(state, na.rm = TRUE),
            lat = mean(lat),
            lon = mean(lon))

zip <- read_csv('zip.csv', col_types = c('cdd'))



ggplot(stations_us, aes(x=lon, y =lat)) + geom_point(size = 1)


data_us <- data_clean %>% 
  inner_join(stations_us, by = c('stn' = 'usaf'))

data_us %>% 
  filter(stn == '747820') %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(x = date)) +
  geom_point(aes(y = temp_mean), col = 'grey', alpha = 0.5) +
  geom_point(aes(y = temp_min), col = 'blue', alpha = 0.5) +
  geom_point(aes(y = temp_max), col = 'red', alpha = 0.5)
