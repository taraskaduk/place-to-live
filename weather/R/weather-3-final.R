library(ggthemes)
library(scales)
library(maps)
library(lubridate)
library(tidyverse)

setwd("weather")
load("data/2-tidy.RData")


## Notes
# https://www.coynecollege.edu/news-events/ideal-temperatures-heat-cool
# https://weather.com/news/news/how-hot-is-too-hot-survey
# https://en.wikipedia.org/wiki/Rain#Intensity


p_temp_max <- 85
p_temp_min <- 40 #lowest for only one extra layer of clothing
p_temp_mean_low <- 50
p_temp_mean_high <- 75
p_precip <- 0.1


pleasant <- data %>% 
  left_join(locations %>% select(geoid, type), by = "geoid") %>% 
  mutate(day = day(date),
         month = month(date),
         lat0 = round(lat,0),
         lon0 = round(lon,0),
         lat05 = round(2*lat,0)/2,
         lon05 = round(2*lon,0)/2) %>% 
  mutate(pleasant = if_else(temp_min >= p_temp_min &
                              temp_max <= p_temp_max &
                              temp_mean >= p_temp_mean_low & temp_mean <= p_temp_mean_high & 
                              precip <= p_precip &
                              is_rain < 0.5 &
                              is_snow < 0.5,
                            1,
                            0),
         hot = if_else(temp_max > p_temp_max |
                         temp_mean > p_temp_mean_high,
                       1,
                       0),
         cold = if_else(temp_min < p_temp_min |
                          temp_mean < p_temp_mean_low,
                        1,
                        0),
         elements = if_else(is_rain >= .5 |
                              is_snow >= 0.5 |
                              precip > 0.3,
                            1, 0),
         distinct_class = case_when(pleasant == 1 ~ "pleasant",
                                    hot == 1      ~ "hot",
                                    elements == 1 ~ "elements",
                                    cold == 1     ~ "cold",
                                    TRUE          ~ NA_character_),
         double_class =   case_when(pleasant == 1 ~ "pleasant",
                                    hot == 1 & elements == 0  ~ "hot",
                                    hot == 1 & elements == 1  ~ "hot & elements",
                                    cold == 1 & elements == 0    ~ "cold",
                                    cold == 1 & elements == 1    ~ "cold & elements",
                                    elements == 1    ~ "elements",
                                    TRUE          ~ NA_character_))



# pleasant %>% 
#   filter(year == 2017 &
#            name %in% c("Seattle-Tacoma-Bellevue, WA", 
#                        "Jacksonville, FL",
#                        "San Diego-Carlsbad, CA",
#                        "New York-Newark-Jersey City, NY-NJ-PA",
#                        "Los Angeles-Long Beach-Anaheim, CA",
#                        "Chicago-Naperville-Elgin, IL-IN-WI",
#                        "San Francisco-Oakland-Hayward, CA",
#                        "Washington-Arlington-Alexandria, DC-VA-MD-WV",
#                        "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD")) %>% 
#   mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
#   ggplot(aes(x=day, y = month, fill = distinct_class)) +
#   geom_tile(col = "black") +
#   facet_wrap(~name) +
#   theme_fivethirtyeight() + 
#   scale_fill_manual(values = c(pleasant = "#1a9641", 
#                                hot = "#d7191c", 
#                                cold = "#0571b0", 
#                                elements = "#b3cde3"), 
#                     name = "Distinct classification")+
#   theme(panel.grid.major = element_blank()) +
#   coord_equal()



# MAP ---------------------------------------------------------------------

pleasant_summary <- pleasant %>% 
  group_by(geoid, lat05, lon05, year) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(lat05, lon05) %>% 
  summarise(pleasant = mean(pleasant)) %>% 
  mutate(pleasant_cat = case_when(pleasant >=300 ~ "Over 300",
                                  pleasant >=200 ~ "200 - 299",
                                  pleasant >=100 ~ "100 - 199",
                                  pleasant >=50 ~ "50 - 99",
                                  TRUE ~ "Less than 50"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 50", "50 - 99", "100 - 199", "200 - 299", "Over 300")))


lat <- data_frame(lat05 = seq(-90, 90, by = .5))
lon <- data_frame(lon05 = seq(-180, 180, by = .5))
dots <- lat %>% 
  merge(lon, all = TRUE)

dots <- dots %>% 
  mutate(country = maps::map.where('state', lon05, lat05),
         lakes = maps::map.where('lakes', lon05, lat05)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)



ggplot() + 
  geom_point(data = dots, aes(x=lon05, y = lat05), col = "grey90", size = 3) +
  #geom_point(data = pleasant_summary %>% filter(pleasant_cat == "Over 300"), aes(x=lon0, y=lat0), col = "red", size = 9, alpha = 0.4) +
  #geom_point(data = pleasant_summary %>% filter(pleasant_cat == "200 - 299"), aes(x=lon0, y=lat0), col = "red", size = 8, alpha = 0.3) +
  geom_point(data = pleasant_summary, aes(x=lon05, y=lat05, col=pleasant_cat), size = 3) +
  # scale_color_brewer(type = "seq", name = "Pleasant days") +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  #scale_color_brewer(type = "div", palette = "RdBu")+
  scale_colour_manual(values = c("#f4a582",
                                 "#fddbc7", 
                                 "#92c5de", 
                                 "#4393c3", 
                                 "#2166ac"), 
                      name = "Pleasant days") +
  theme_fivethirtyeight() +
  theme(
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")



ggplot() +
  geom_point(data = dots, aes(x=lon05, y = lat05), col = "grey90", fill = "grey95", size = 2, pch=21) +
  geom_point(data = pleasant_summary, aes(x=lon05, y=lat05, col=pleasant), size = 2) +
  #geom_point(data = pleasant_summary, aes(x=lon0, y=lat0, col=pleasant, alpha = pleasant), size = 6) +

  scale_color_gradient(low = "#d1e5f0",
                         high = "#2166ac",
                         na.value = "grey90") +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(24, 50)) +
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
  geom_point(data = dots, aes(x=lon05, y = lat05), col = "grey90", fill = "grey95", size = 2, pch=21) +
  geom_point(data = pleasant_summary, aes(x=lon05, y=lat05, col=pleasant), size = 2) +
  #geom_point(data = pleasant_summary, aes(x=lon0, y=lat0, col=pleasant, alpha = pleasant), size = 6) +
  
  scale_color_gradient2(low = "#99000d",
                        mid = "#fcbba1",
                       high = "#fee0d2",
                       midpoint = 110,
                       na.value = "grey90") +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(24, 50)) +
  theme_fivethirtyeight() +
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")



summary_metro <- pleasant %>% 
  filter(type == "Metro Area") %>% 
  group_by(geoid, name, year) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(geoid, name) %>% 
  summarise(pleasant = mean(pleasant)) %>% 
  ungroup() %>% 
  mutate(rank = row_number(desc(pleasant)),
         name2 = reorder(name, rank))











# Top 10? -----------------------------------------------------------------



top25_metro <- summary_metro %>% 
  filter(rank <= 25) %>% 
  mutate(name2 = reorder(name, rank))

pleasant %>% 
  inner_join(summary_metro %>% filter(rank <= 25), by = "geoid") %>% 
  filter(year == 2017) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = distinct_class)) +
  geom_tile(col = "black") +
  facet_wrap(~name2) +
  theme_fivethirtyeight() + 
  labs(title = "25 U.S. metro areas with the best weather",
       subtitle = "Weather visualized for 2017, ranking is based on a 6-year average",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk") +
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d7191c", 
                               cold = "#0571b0", 
                               elements = "#b3cde3"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) 

ggsave("top25.png",
       width = 440,
       height = 280,
       units = "mm",
       dpi = 250)


pleasant %>% 
  inner_join(summary_metro %>% filter(rank <= 25), by = "geoid") %>% 
  filter(year == 2017) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = double_class)) +
  geom_tile(col = "black") +
  facet_wrap(~name2) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d7191c", 
                               cold = "#0571b0", 
                               elements = "#756bb1",
                               `hot & elements` = "#e6550d",
                               `cold & elements` = "#9ecae1"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  coord_equal()


pleasant %>% 
  inner_join(summary_metro %>% filter(rank >= nrow(summary_metro) - 10), by = "geoid") %>% 
  filter(year == 2017) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = double_class)) +
  geom_tile(col = "black") +
  facet_wrap(~name2) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d7191c", 
                               cold = "#0571b0", 
                               elements = "#756bb1",
                               `hot & elements` = "#e6550d",
                               `cold & elements` = "#9ecae1"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  coord_equal()



pleasant %>% 
  filter(name == "Seattle-Tacoma-Bellevue, WA") %>% 
  ggplot(aes(x=day, y = year, fill = as.factor(elements)), col = grey) +
  geom_tile() +
  facet_wrap( ~ month) +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  scale_fill_fivethirtyeight()+
  coord_equal()

pleasant %>% 
  filter(name == "Seattle-Tacoma-Bellevue, WA") %>% 
  ggplot(aes(x=day, y = year, fill = precip), col = grey) +
  geom_tile() +
  facet_wrap( ~ month) +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  coord_equal()

pleasant %>% 
  filter(name == "Seattle-Tacoma-Bellevue, WA") %>% 
  group_by(year, month) %>% 
  summarize(precip = sum(precip)) %>% 
  ggplot(aes(x=month, y = precip, col = as.factor(year), group = year, alpha = year, size = year)) +
  geom_line() +
  theme_bw() + 
  theme(panel.grid.major = element_blank())











metro <- locations %>% 
  #filter(!(state %in% c("AK", "HI", "PR", "VI"))) %>% 
  # select(id, city = city_ascii, state = state_id, lat0, lon0, lat, lon) %>% 
  left_join(pleasant_summary, by = c("lat0", "lon0")) %>% 
  mutate(pleasant_cat = case_when(pleasant >=200 ~ "Over 200",
                                  pleasant >=150 ~ "150 - 199",
                                  pleasant >=100 ~ "100 - 149",
                                  TRUE ~ "Less than 100"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 100", "100 - 149", "150 - 199", "Over 200")))

top_metro <- metro %>% 
  group_by(lat0, lon0, pleasant) %>% 
  summarise(loc = paste(name, collapse = ", "))



weather_summary <- pleasant %>% 
  group_by(lon0, lat0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  semi_join(dots, by = c("lat0", "lon0")) %>% 
  mutate(pleasant200 = if_else(pleasant >= 200, 1, 0),
         pleasant150 = if_else(pleasant >= 150, 1, 0),
         pleasant_cat = case_when(pleasant >=200 ~ "Over 200",
                                  pleasant >=150 ~ "150 - 199",
                                  pleasant >=100 ~ "100 - 149",
                                  TRUE ~ "Less than 100"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 100", "100 - 149", "150 - 199", "Over 200")))


ggplot() + 
  geom_point(data = dots, aes(x=lon0, y = lat0), col = "grey90", size = 6) +
  geom_point(data = weather_summary, aes(x=lon0, y=lat0, col=pleasant), size = 6, alpha = 0.4) +
  geom_point(data = weather_summary, aes(x=lon0, y=lat0, col=pleasant, alpha = pleasant), size = 6) + 
  
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
  geom_point(data = dots, aes(x=lon0, y = lat0), col = "grey95", size = 6) +
  geom_point(data = weather_summary, aes(x=lon0, y=lat0, col=pleasant_cat), size = 6) +
  
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




pleasant %>% 
  filter(lat0 == 48 & lon0 == -122) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()

pleasant %>% 
  filter(lat0 == 48 & lon0 == -122) %>% 
  ggplot(aes(x=yday, y = precip, col = as.factor(pleasant))) + 
  geom_point()


pleasant %>% 
  filter(lat0 == 30 & lon0 == -82) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()

pleasant %>% 
  filter(lat0 == 30 & lon0 == -82) %>% 
  ggplot(aes(x=yday, y = precip, col = as.factor(pleasant))) + 
  geom_point()

pleasant %>% 
  group_by(lon0, lat0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ggplot(aes(x=lon0, y=lat0, col=pleasant)) + 
  geom_point(size = 3) + 
  scale_color_continuous(low='white', high='red') +
  theme_void()




