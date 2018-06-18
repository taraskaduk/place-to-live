# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)


# Import ------------------------------------------------------------------

stations <- read_csv("data/stations.csv")

data_us <- map_df(list.files("data", full.names = TRUE), 
                  read_csv,
                  col_types = cols(date = col_date(format = "%Y-%m-%d"), 
                                   state = col_character(), stn = col_character(), 
                                   wban = col_character()))

cities <- read_csv("data/cities.csv")


# Transform ---------------------------------------------------------------

stations_us <- stations %>% 
  select(usaf, country = ctry, state = st, lat, lon) %>% 
  filter(!is.na(lat) & !is.na(lon)) %>% 
  filter(country %in% c("US", "RQ")) %>%  #USA + territories
  distinct() %>% 
  group_by(usaf) %>% 
  summarise(state = max(state, na.rm = TRUE),
            lat = mean(lat),
            lon = mean(lon)) %>% 
  filter(!is.na(state) & lat > 10 & lon < 0)

ggplot(stations_us, aes(x=lon, y =lat)) + geom_point(size = 1)


# Data --------------------------------------------------------------------

weather_daily <- data_us %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0),
         year = year(date),
         month = month(date),
         day = day(date),
         yday = if_else(year %% 4 == 0 & yday(date) > 60,
                        yday(date) - 1,
                        yday(date))) %>% 
  select(-c(state, lat, lon, stn, wban, date)) %>% 
  group_by(lat.0, lon.0, month, day, yday) %>% 
  summarise_all(mean, na.rm=TRUE) %>% 
  filter(!(day == 29 & month == 2)) %>% 
  ungroup()

weather_daily %>%   
  group_by(lat.0, lon.0) %>% 
  summarise(n = n()) %>% 
  mutate(complete = if_else(n==365, 1, 0)) %>% 
  ggplot(aes(x=lon.0, y=lat.0, col=complete)) + geom_point()

weather_daily %>%   
  group_by(lat.0, lon.0) %>% 
  summarise(n = n()) %>% 
  mutate(complete = if_else(n==365, 1, 0)) %>% 
  ggplot(aes(x=n)) + geom_histogram()


weather_daily %>%   
  group_by(lat.0, lon.0) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(n) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


weather_daily %>%   
  ggplot(aes(x=prcp)) + geom_histogram()

weather_daily_pleasant <- weather_daily %>% 
  mutate(pleasant = if_else(temp_min >= 40 & #was 45
                              temp_max <= 80 &  #was 85
                              (temp_mean >= 50 | temp_mean <=75) & #was 55
                              sndp < 5 & # how to determine this?
                              prcp < 0.2,
                            1,
                            0))


# Visualize ---------------------------------------------------------------

weather_daily_pleasant %>% 
  filter(lat.0 == 48 & lon.0 == -122) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()

weather_daily_pleasant %>% 
  filter(lat.0 == 30 & lon.0 == -81) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()


weather_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ggplot(aes(x=lon.0, y=lat.0, col=pleasant)) + 
  geom_point(size = 3) + 
  scale_color_continuous(low='white', high='red') +
  theme_void()


weather_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  mutate(pleasant_group = as.factor(case_when(pleasant > 250 ~ 3,
                                              pleasant > 150 ~ 2,
                                              TRUE ~ 1))) %>% 
  ggplot(aes(x=lon.0, y=lat.0, col=pleasant_group)) + 
  geom_point(size = 4) + 
  theme_void() +
  coord_map(projection = "albers", lat_0=45, lon_0=-100)


transposed <- weather_daily_pleasant %>% 
  mutate(lat = if_else(lon.0 < -125 & lat.0 >48, lat.0 -50, lat.0),
         lon = if_else(lon.0 < -125 & lat.0 >48, lon.0 + 50, lon.0))

transposed %>% 
  group_by(lon, lat) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ggplot(aes(x=lon, y=lat, col=pleasant)) + 
  geom_point(size = 3) + 
  scale_color_continuous(low='white', high='darkblue') +
  theme_void() +
  coord_map(projection = "albers", lat_0=45, lon_0=-100)

