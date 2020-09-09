library(tidyverse)
library(gganimate)
library(lubridate)
library(ggthemes)
#library(ggnewscale)
library(latex2exp)
source("R/functions.R")



theme_set(theme_fivethirtyeight()+
            theme(rect = element_blank(),
                  #panel.border = element_blank(),
                  strip.background = element_blank(),
                  #panel.grid.major = element_blank(),
                  axis.title=element_blank()))





get_edd(8,30)

fun_cos <- function(min, max, hour = 0) {
  a <- (max - min) / 2 #amplitude
  period <- 24
  b <- 2 * pi / period
  d <- min + a
  return(-a * cos(b * hour) + d)
}




data <- readRDS("data/weather-data-daily.RDS")
locations <- read_csv("data/locations.csv") %>% 
  mutate(cbsafp = as.character(cbsafp))

data_sample <- locations %>% 
  filter(name_short %in% c("Miami", "Los Angeles", "Minneapolis")) %>% 
  left_join(data, by = "cbsafp") %>% 
  filter(year(date) == 2018) %>% 
  select(name_short, date, temp_min, temp_max)


data_sample_chart <- data_sample %>% 
  merge(tibble(hour = seq(from = 0, to = 24, by = 1)), all = TRUE) %>% 
  mutate(temp_real = fun_cos(temp_min, temp_max, hour),
         temp = temp_real - 18,
         Area = if_else(temp > 0, "hot", "cold"))

caption <- ("taraskaduk.com | @taraskaduk")
colors <- c(hot = "#ca0020", 
            cold = "#0571b0")

# colors <- c(pleasant = "#1a9641", 
#             hot = "#d6604d", 
#             cold = "#4393c3", 
#             elements = "#bebada",
#             `hot & elements` = "#ca0020",
#             `cold & elements` = "#0571b0",
#             IDK = '#000000')



data_sample_chart %>%
  filter(date == "2018-02-01" & name_short == "Los Angeles") %>% 
  ggplot(aes(x = hour, y = temp)) +
  geom_col(aes(fill = Area)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  annotate("text", label = TeX("$\\theta_1$"), x = 6, y = 1, size = 5)+
  annotate("text", label = TeX("$\\theta_2$"), x = 18, y = 1, size = 5)+
  annotate("text", label = "y=18°C", x = -1, y = -1, size = 5)+
  annotate("text", label = TeX("$f(\\theta)$"), x = -1, y = -8, size = 5)+
  annotate("text", label = "0", x = 0, y = 1, size = 5)+
  annotate("text", label = "24", x = 24, y = 1, size = 5)+
  scale_fill_manual(values = colors) +
  theme(axis.text = element_blank(),
        legend.position = "none")+
  labs(title = "Calculation of area between the temperature curve and a baseline",
       subtitle = "With a hypothetical ideal temperature baseline at 18°C",
       caption = caption)

ggsave("integral.png")



data_sample_chart %>%
  filter(date == "2018-02-01" | date == "2018-07-01") %>%
  mutate(date = format.Date(date, "%B %d, %Y")) %>% 
  ggplot(aes(x = hour, y = temp_real)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 18) +
    scale_x_continuous(breaks = c(12),
                       label = c("12:00")) +
    scale_y_continuous(breaks = c(-10, 0, 10, 20, 30)) +
    facet_grid(date ~ name_short) +
  labs(title = "Simplified model of daily temperature change \ngiven daily min and max values",
       subtitle = "With a hypothetical ideal temperature baseline at 18°C",
       caption = caption)
ggsave("1.png")



data_sample_chart %>%
  filter(date == "2018-02-01" | date == "2018-07-01") %>%
  mutate(date = format.Date(date, "%B %d, %Y")) %>% 
  
  ggplot(aes(x = hour, y = temp)) +
  geom_col(aes(fill = Area), width = 1) +
  #geom_line(size=2) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = c(12),
                     label = c("12:00")) +
  scale_y_continuous(breaks = c(-10, 0, 10, 20, 30)-18,
                     labels = c(-10, 0, 10, 20, 30)) +
  facet_grid(date ~ name_short) +
  labs(title = "Hot and cold areas under the curve \nof daily temperature",
       #subtitle = "With a hypothetical ideal temperature baseline at 18°C",
       caption = caption)

ggsave("2.png")



p <- data_sample_chart %>%
  mutate(date_formatted = format.Date(date, "%B %d, %Y")) %>% 

  ggplot(aes(x = hour, y = temp)) +
  geom_col(aes(fill = Area), width = 1, alpha = 0.1) +
  #geom_line(size=2) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = c(12),
                     label = c("12:00")) +
  scale_y_continuous(breaks = 0,
                     labels = "18°C") +
  facet_wrap(~name_short) +
  labs(title = "Visualization of area under curve \nfor every day in 2018",
       subtitle = 'Date: {frame_time}',
       caption = caption)+
  transition_time(date) +
  shadow_trail()

animate(p, end_pause = 25)
anim_save("3.gif")





# EDD ---------------------------------------------------------------------

summary_locations_edd <- data %>%
  mutate(year = year(date)) %>% 
  filter(year < year(today())) %>% 
  left_join(locations, by = "cbsafp") %>% 
  rename(city = name) %>% 
  group_by(cbsafp, name_short, lat, lon, year) %>% 
  summarise(edd_total = sum(edd_total),
            edd_cold = sum(edd_cold),
            edd_hot = sum(edd_hot),
            days = n(),
            pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  ## This if_else accounts for cases of leap year with all known days.
  ## It makes sure we don't have negative unknown days
  ## But also levels out leap year for the next step of averaging
  mutate(unknown = if_else(days >= 365, 0, 365 - days)) %>% 
  filter(unknown < 365 * 0.1) %>% 
  #filter(population >= 250000) %>% 
  group_by(cbsafp, name_short, lat, lon) %>% 
  summarise(edd_total = mean(edd_total),
            edd_cold = mean(edd_cold),
            edd_hot = mean(edd_hot),
            pleasant = mean(pleasant)) %>% 
  ungroup() %>% 
  mutate(rank = row_number(edd_total),
         rank_rev = row_number(desc(edd_total)),
         points = edd_total / max(edd_total) * 100,
         city = reorder(name_short, rank))

ggplot(summary_locations_edd, aes(x = pleasant, y = edd_total))+
  geom_point(alpha = 0.4) +
  geom_smooth(span = 0.9) +
  labs(subtitle = "Reverse correlation between the annual count of pleasant days \nand the average annual Weather Unpleasantness Index",
       caption = caption,
       x = "Average amount of pleasant days in a year",
       y = "Average annual Weather Unpleasantness Index") +
  theme(axis.title=element_text())
ggsave("7.png")

summary_locations_edd %>% 
  mutate(top = if_else(rank <= 25, "Best 25", if_else(rank_rev <= 25, "Worst 25", NA_character_)),
         city = paste0(rank, ". ", city, ": ", round(edd_total,0), "/", round(edd_cold,0), "/", round(edd_hot,0)),
         city = reorder(city, rank_rev)) %>%
  filter(!is.na(top)) %>% 
  ggplot(aes(x=city)) +
  geom_col(aes(y = edd_hot), fill = "#ca0020")+
  geom_col(aes(y = -edd_cold), fill = "#0571b0")+
 
  # geom_point(aes(y = 1, alpha = edd_hot, size = edd_hot), col = "#ca0020")+
  # geom_point(aes(y = 1, alpha = edd_cold,size = edd_cold), col = "#0571b0")+
  coord_flip() +
  facet_wrap(~top, scales = "free_y")+
  theme(strip.text = element_text(face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = "Weather Unpleasantness Index",
       subtitle = "For cities with over 1M people in population",
       caption = paste0("Labels: #Rank. City, Country: edd (area under curve) cold / edd hot / edd total",
                        "\n", caption))
ggsave("4.png")



summary_locations_edd %>% 
  mutate(top = if_else(rank <= 25, "Best 25", if_else(rank_rev <= 25, "Worst 25", NA_character_)),
         city = paste0(rank, ". ", city, ", ", country, ": ", round(edd_cold,0), "/", round(edd_hot,0), "/", round(edd_total,0)),
         city = reorder(city, rank_rev)) %>%
  filter(!is.na(top)) %>% 
  ggplot(aes(x=city)) +
  geom_col(aes(y = edd_hot), fill = "#ca0020")+
  geom_col(aes(y = -edd_cold), fill = "#0571b0")+
  scale_y_continuous(breaks = c(-500, -250, 0, 250, 500)) +
  # geom_point(aes(y = 1, alpha = edd_hot, size = edd_hot), col = "#ca0020")+
  # geom_point(aes(y = 1, alpha = edd_cold,size = edd_cold), col = "#0571b0")+
  coord_flip() +
  facet_grid(top~., scales = "free_y")+
  theme(strip.text = element_text(face = "bold"),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(title = "Weather Unpleasantness Index",
       subtitle = "For cities with over 1M people in population",
       caption = paste0("Labels: #Rank. City, Country: edd (area under curve) cold / edd hot / edd total",
                        "\n", caption))
ggsave("6.png")



summary_locations_edd %>% 
  mutate(top = if_else(rank <= 25, "Best 25", if_else(rank_rev <= 25, "Worst 25", NA_character_)),
         city = paste0(rank, ". ", city, ": ", round(edd_cold,0), "/", round(edd_hot,0), "/", round(edd_total,0)),
         city = reorder(city, rank_rev)) %>%
  ggplot(aes(x=city)) +
  geom_col(aes(y = edd_hot), fill = "#ca0020")+
  geom_col(aes(y = -edd_cold), fill = "#0571b0")+
  scale_y_continuous(breaks = c(-500, -250, 0, 250, 500)) +
  # geom_point(aes(y = 1, alpha = edd_hot, size = edd_hot), col = "#ca0020")+
  # geom_point(aes(y = 1, alpha = edd_cold,size = edd_cold), col = "#0571b0")+
  coord_flip() +
  theme(strip.text = element_text(face = "bold"),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(title = "Weather Unpleasantness Index",
       subtitle = "For U.S. Combined Statistical Areas with over 1,000,000 people in population",
       caption = paste0("Labels: #Rank. Combined Statistical Area: edd (area under curve) cold / edd hot / edd total",
                        "\n", caption))
ggsave("6.png", width = 11, height = 9, units = "in")








data_edd %>% 
  select(location_id, date, year, yday, edd_hot, edd_cold, edd_total) %>% 
  left_join(summary_locations_edd %>% select(location_id, city, rank), by = "location_id") %>% 
  filter(rank <= 25) %>% 
  ggplot() +
  geom_tile(aes(x=yday, y=year, fill = edd_hot), alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "#ca0020") +
  new_scale_fill() +
  geom_tile(aes(x=yday, y=year, fill = edd_cold), alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "#0571b0") +
  facet_wrap(~city, ncol = 5) +
  scale_x_continuous(
    breaks = c(1, 182),
    label = c("January", "July")
  ) +
  coord_polar() +
  expand_limits(y = 2009) +
  theme(strip.text = element_text(face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank()) +
  labs(title = "Your title",
       subtitle = "Your subtitle",
       caption = "Your caption")






