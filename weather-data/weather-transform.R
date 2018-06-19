# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)


# Import ------------------------------------------------------------------

load("data/data.RData")

# Data --------------------------------------------------------------------
stations_join <- stations_us %>% 
  select(usaf, wban, lat, lon)

weather <- weather_data %>% 
  inner_join(stations_join, by = c('stn' = 'usaf', 'wban'))

weather_daily <- weather %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0),
         year = year(date),
         month = month(date),
         day = day(date),
         yday = if_else(year %% 4 == 0 & yday(date) >= 60,
                        yday(date) - 1,
                        yday(date)),
         day = if_else(day == 29 & month == 2, 28, as.numeric(day))) %>% 
  select(-c(lat, lon, stn, wban, date)) %>% 
  group_by(lat.0, lon.0, month, day, yday) %>% 
  summarise_all(mean, na.rm=TRUE) %>% 
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

w_daily_pleasant <- weather_daily %>% 
  mutate(pleasant = if_else(temp_min >= 45 & #was 45
                              temp_max <= 85 &  #was 85
                              (temp_mean >= 55 | temp_mean <=75) & #was 55
                              sndp < 3 & # how to determine this?
                              prcp < 0.2,
                            1,
                            0))

weather_summary <- w_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  semi_join(dots, by = c("lat.0", "lon.0"))

# Visualize ---------------------------------------------------------------

w_daily_pleasant %>% 
  filter(lat.0 == 48 & lon.0 == -122) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()

w_daily_pleasant %>% 
  filter(lat.0 == 48 & lon.0 == -122) %>% 
  ggplot(aes(x=yday, y = prcp, col = as.factor(pleasant))) + 
  geom_point()


w_daily_pleasant %>% 
  filter(lat.0 == 30 & lon.0 == -81) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()

w_daily_pleasant %>% 
  filter(lat.0 == 30 & lon.0 == -81) %>% 
  ggplot(aes(x=yday, y = prcp, col = as.factor(pleasant))) + 
  geom_point()

w_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ggplot(aes(x=lon.0, y=lat.0, col=pleasant)) + 
  geom_point(size = 3) + 
  scale_color_continuous(low='white', high='red') +
  theme_void()


w_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  mutate(pleasant_group = as.factor(case_when(pleasant > 250 ~ 3,
                                              pleasant > 150 ~ 2,
                                              TRUE ~ 1))) %>% 
  ggplot(aes(x=lon.0, y=lat.0, col=pleasant_group)) + 
  geom_point(size = 4) + 
  theme_void() +
  coord_map(projection = "albers", lat_0=45, lon_0=-100)



# Generate a data frame with all dots -----------------------------------------------

lat <- data_frame(lat.0 = seq(-90, 90, by = 1))
lon <- data_frame(lon.0 = seq(-180, 180, by = 1))
dots <- lat %>% 
  merge(lon, all = TRUE)

## Only include dots that are within borders. Also, exclude lakes.
dots <- dots %>% 
  mutate(country = map.where('state', lon.0, lat.0),
         lakes = map.where('lakes', lon.0, lat.0)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)



weather_summary <- weather_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  semi_join(dots, by = c("lat.0", "lon.0"))

ggplot() + 
  geom_point(data = dots, aes(x=lon.0, y = lat.0), col = "grey90", size = 6) +
  geom_point(data = weather_summary, aes(x=lon.0, y=lat.0, col=pleasant), size = 6) +
  
  scale_colour_gradient2(low = "red", mid = "yellow",
                         high = "darkblue", midpoint = 150, space = "Lab",
                         na.value = "grey95") +
  theme_fivethirtyeight() +
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(title = "Places with most and least pleasant days in a year",
       caption = "Blue - most pleasant days, yellow - midpoint at 182 pleasant days, red - least pleasant days, grey - no data available\n
       A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2014 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")
