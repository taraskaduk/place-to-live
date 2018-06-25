# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(scales)

setwd("weather")

# Import ------------------------------------------------------------------

load("data/2-tidy.RData")
load("data/1-locations.RData")

# Transform --------------------------------------------------------------------


# https://www.coynecollege.edu/news-events/ideal-temperatures-heat-cool
# https://weather.com/news/news/how-hot-is-too-hot-survey
# https://en.wikipedia.org/wiki/Rain#Intensity


p_temp_max <- 85
p_temp_min <- 50 #lowest for only one extra layer of clothing
p_temp_mean_low <- 65
p_temp_mean_high <- 75
p_prcp <- 0.1
p_sndp <- 1


w_pleasant <- w_filled %>% 
  mutate(pleasant = if_else(temp_min >= p_temp_min &
                              temp_max <= p_temp_max &
                              (temp_mean >= p_temp_mean_low | temp_mean <= p_temp_mean_high) & 
                              sndp < p_sndp & # how to determine this?
                              prcp < p_prcp,
                            1,
                            0),
         rainy = if_else(prcp > 0.1, 1, 0),
         hot = if_else(temp_max > 80, 1, 0),
         xhot = if_else(temp_max > 90, 1, 0)) %>% 
  group_by(lat.0, lon.0, year) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(lat.0, lon.0) %>% 
  summarise(pleasant = mean(pleasant)) %>% 
  mutate(pleasant_cat = case_when(pleasant >=300 ~ "Over 300",
                                  pleasant >=200 ~ "200 - 299",
                                  pleasant >=100 ~ "100 - 199",
                                  pleasant >=50 ~ "50 - 99",
                                  TRUE ~ "Less than 50"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 50", "50 - 99", "100 - 199", "200 - 299", "Over 300")))
    



ggplot() + 
  geom_point(data = w_pleasant %>% filter(pleasant_cat == "Over 300"), aes(x=lon.0, y=lat.0), col = "red", size = 9, alpha = 0.4) +
  geom_point(data = w_pleasant %>% filter(pleasant_cat == "200 - 299"), aes(x=lon.0, y=lat.0), col = "red", size = 8, alpha = 0.3) +
  geom_point(data = w_pleasant, aes(x=lon.0, y=lat.0, col=pleasant_cat), size = 6) +
#  scale_color_brewer(type = "seq", name = "Pleasant days") +
  scale_colour_manual(values = c("grey90", "#c6dbef", "#4292c6", "#2171b5", "#08306b"), name = "Pleasant days") +
  theme_fivethirtyeight() +
  theme(
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")

lat <- data_frame(lat.0 = seq(-90, 90, by = 1))
lon <- data_frame(lon.0 = seq(-180, 180, by = 1))
dots <- lat %>% 
  merge(lon, all = TRUE)

dots <- dots %>% 
  mutate(country = map.where('state', lon.0, lat.0),
         lakes = map.where('lakes', lon.0, lat.0)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)

ggplot() + 
  geom_point(data = dots, aes(x=lon.0, y = lat.0), col = "grey90", size = 6) +
  geom_point(data = w_pleasant, aes(x=lon.0, y=lat.0, col=pleasant), size = 6, alpha = 0.4) +
  geom_point(data = w_pleasant, aes(x=lon.0, y=lat.0, col=pleasant, alpha = pleasant), size = 6) + 
  
  scale_colour_gradient2(low = "grey85",
                         high = "darkblue",
                         
                         na.value = "grey85") +
  theme_fivethirtyeight() +
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")










w_msa <- loc_msa %>% 
  #filter(!(state %in% c("AK", "HI", "PR", "VI"))) %>% 
  # select(id, city = city_ascii, state = state_id, lat.0, lon.0, lat, lon) %>% 
  left_join(w_pleasant, by = c("lat.0", "lon.0")) %>% 
  mutate(pleasant_cat = case_when(pleasant >=200 ~ "Over 200",
                                  pleasant >=150 ~ "150 - 199",
                                  pleasant >=100 ~ "100 - 149",
                                  TRUE ~ "Less than 100"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 100", "100 - 149", "150 - 199", "Over 200")))

top_msa <- w_msa %>% 
  group_by(lat.0, lon.0, pleasant) %>% 
  summarise(loc = paste(name, collapse = ", "))

top_cities$data


ggplot() + 
  geom_point(data = cities_w, aes(x=lon, y=lat, col=pleasant_cat), size = 0.1) +
  scale_colour_manual(values = c("grey90", "#c6dbef", "#4292c6", "#084594"), name = "Pleasant days") +
  theme_fivethirtyeight() +
  theme(
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")






# Visualize ---------------------------------------------------------------

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












weather_summary <- w_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  semi_join(dots, by = c("lat.0", "lon.0")) %>% 
  mutate(pleasant200 = if_else(pleasant >= 200, 1, 0),
         pleasant150 = if_else(pleasant >= 150, 1, 0),
         pleasant_cat = case_when(pleasant >=200 ~ "Over 200",
                                  pleasant >=150 ~ "150 - 199",
                                  pleasant >=100 ~ "100 - 149",
                                  TRUE ~ "Less than 100"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 100", "100 - 149", "150 - 199", "Over 200")))


ggplot() + 
  geom_point(data = dots, aes(x=lon.0, y = lat.0), col = "grey90", size = 6) +
  geom_point(data = weather_summary, aes(x=lon.0, y=lat.0, col=pleasant), size = 6, alpha = 0.4) +
  geom_point(data = weather_summary, aes(x=lon.0, y=lat.0, col=pleasant, alpha = pleasant), size = 6) + 
  
  scale_colour_gradient2(low = "grey90",
                         high = "darkblue",

                         na.value = "grey95") +
  theme_fivethirtyeight() +
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")


ggplot() + 
  geom_point(data = dots, aes(x=lon.0, y = lat.0), col = "grey95", size = 6) +
  geom_point(data = weather_summary, aes(x=lon.0, y=lat.0, col=pleasant_cat), size = 6) +
  
  scale_colour_manual(values = c("grey90", "#c6dbef", "#4292c6", "#084594"), name = "Pleasant days") +

  theme_fivethirtyeight() +
  theme(
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")




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






