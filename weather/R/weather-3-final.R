setwd("weather")

library(ggthemes)
library(scales)
library(maps)
library(lubridate)
library(tidyverse)

load("data/2-tidy.RData")


## Notes
# https://www.coynecollege.edu/news-events/ideal-temperatures-heat-cool
# https://weather.com/news/news/how-hot-is-too-hot-survey
# https://en.wikipedia.org/wiki/Rain#Intensity


p_temp_max <- c(65, 85) #65 - "if it didn't get up to 65F in the warmest hour..."
p_temp_min <- c(40, 70) #lowest would be night + sunrise temp. Let's rule out near freezing temps.
                        #the upper limit is "when even the lowest night temp is too hot..."
p_temp_mean <- c(55, 75)
p_precip <- 0.15* 24
p_snow <- 1


pleasant <- data %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         lat0 = round(lat,0),
         lon0 = round(lon,0),
         lat05 = round(2*lat,0)/2,
         lon05 = round(2*lon,0)/2) %>% 
  mutate(pls_temp = case_when(temp_min > p_temp_min[2] ~ 'hot',
                              temp_min < p_temp_min[1] ~ 'cold',
                              temp_max > p_temp_max[2] ~ 'hot',
                              temp_max < p_temp_max[1] ~ 'cold',
                              temp_mean > p_temp_mean[2] ~ 'hot',
                              temp_mean < p_temp_mean[1] ~ 'cold',
                              TRUE ~ "pleasant"),
         pls_elements = if_else(precip <= p_precip &
                                snow <= p_snow &
                                is_element == 1, 
                                "pleasant",
                                "elements"),
         pleasant = if_else(pls_temp == 'pleasant' & pls_elements == "pleasant",
                            1,
                            0),
         distinct_class = case_when(pleasant == 1 ~ "pleasant",
                                    pls_temp == "hot" ~ "hot",
                                    pls_elements == "elements" ~ "elements",
                                    pls_temp == "cold" ~ "cold",
                                    TRUE          ~ NA_character_),
         double_class =   case_when(pleasant == 1 ~ "pleasant",
                                    pls_temp == "hot" & pls_elements != "elements"  ~ "hot",
                                    pls_temp == "hot" & pls_elements == "elements"  ~ "hot & elements",
                                    pls_temp == "cold" & pls_elements != "elements"  ~ "cold",
                                    pls_temp == "cold" & pls_elements == "elements"  ~ "cold & elements",
                                    pls_elements == "elements"    ~ "elements",
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
  scale_colour_manual(values = c("#bdd7e7",
                                 "#6baed6", 
                                 "#2171b5", 
                                 "#08306b"), 
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
  geom_point(data = dots, aes(x=lon05, y = lat05), col = "grey85", fill = "grey95", size = 2, pch=21) +
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
  group_by(geoid, name, year, pop17) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(geoid, name, pop17) %>% 
  summarise(pleasant = mean(pleasant)) %>% 
  ungroup() %>% 
  mutate(rank = row_number(desc(pleasant)),
         name2 = reorder(name, rank),
         pop17 = as.integer(pop17))











# Top 10? -----------------------------------------------------------------


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






pleasant %>% 
  inner_join(summary_metro %>% filter(rank <= 25), by = "geoid") %>% 
  filter(year == 2017) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = double_class)) +
  geom_tile(col = "black") +
  facet_wrap(~name2) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d6604d", 
                               cold = "#4393c3", 
                               elements = "#bebada",
                               `hot & elements` = "#ca0020",
                               `cold & elements` = "#0571b0"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  coord_equal()


pleasant %>% 
  inner_join(summary_metro %>% filter(rank >= nrow(summary_metro) - 25), by = "geoid") %>% 
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
  inner_join(summary_metro %>% filter(pop17 > 1000000), by = "geoid") %>% 
  filter(year == 2017) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = double_class)) +
  geom_tile(col = "black") +
  facet_wrap(~name2) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d6604d", 
                               cold = "#4393c3", 
                               elements = "#bebada",
                               `hot & elements` = "#ca0020",
                               `cold & elements` = "#0571b0"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  coord_equal()





pleasant %>% 
  inner_join(summary_metro %>% filter(str_detect(name, "Philadelphia") == TRUE), by = "geoid") %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = double_class)) +
  geom_tile(col = "black") +
  facet_wrap(~year) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d6604d", 
                               cold = "#4393c3", 
                               elements = "#bebada",
                               `hot & elements` = "#ca0020",
                               `cold & elements` = "#0571b0"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  coord_equal()
