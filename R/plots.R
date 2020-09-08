library(tidyverse)
library(ggthemes)
library(lubridate)
library(scales)
library(sf)

years <-  c((year(today())-11) : (year(today())-1))

caption <-  ("Source: NOAA Global Summary of the Day \n taraskaduk.com | @taraskaduk")
# colors <-  c(pleasant = "#1a9641", 
#              hot = "#d6604d", 
#              cold = "#4393c3", 
#              elements = "#bebada",
#              # wind = '#e6e6e6',
#              `hot & elements` = "#ca0020",
#              `cold & elements` = "#0571b0")

colors <-  c(pleasant = "#1a9641", 
             hot = "#ca0020", 
             cold = "#0571b0", 
             elements = "#bebada",
             unknown = "grey70")

theme_set(theme_fivethirtyeight()+
            theme(rect = element_blank(),
                  panel.border = element_blank(),
                  strip.background = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title=element_blank())
)


summary_locations <- readRDS("data/summary_locations.RDS")
summary_locations_tall <- readRDS("data/summary_locations_tall.RDS")
climates <- read_csv("data/climates.csv")


# What I need:
#   - Top 100 bar plot: all, 1M population, USA all, USA 1M = 4
#   - Top 100 map = 4
#   - Top 1 in every country, sorted by country name? = 1
#   - Top 5 in every region/subregion, bar faceted by region = 1-3
#   - Top 5 in every region map = 1-3
#   - Donut chart, top 25: all, 1M, world and USA. = 4
# Total 15-19 charts...
  



plot_bars <- function(n = 100, column = "rank", facet = NULL) {
  column <- sym(column)
  # facet <- sym(facet)
  summary_locations_tall %>% 
    filter(!!column <= n) %>% 
    mutate(label = paste0(!!column, ". ", name_short, ", ", country)) %>%
    ggplot(aes(y = days, x = reorder(label, points), fill = class)) +
    geom_col() +
    scale_fill_manual(values = colors) +
    facet_wrap(~ NULL, scales = "free", ncol = 1) +
    coord_flip()
  
  ggsave(paste0("bar_", column, "_", n, ".png"), width = 7, height = 16, units = "in")
}
plot_bars(n = 5, column = "rank_cont1")

plot_bars(column = "rank_1", facet = "subregion")
plot_bars(n = 1, column = "rank_country")

summary_locations %>% 
  filter(rank_1 <= 100) %>% 
  ggplot() +
  geom_sf() 



ggplot() +
  geom_polygon(data = map_data("world"), aes(x=long, y = lat, group = group), fill = "grey95", col = "grey30", size = 0.1) +
  geom_sf(data = summary_locations %>% filter(rank <= 100), aes(col = rank, size = population, alpha = 0.6)) +
  scale_color_gradient(low = "#1a9641", high = "grey70")
  # scale_color_manual(values = climate_colors) +
  # scale_fill_manual(values = climate_colors)

ggplot() +
  geom_polygon(data = map_data("world"), aes(x=long, y = lat, group = group), fill = "grey95", col = "grey30", size = 0.1) +
  geom_sf(data = summary_locations %>% 
            filter(rank <= 100),  aes(col = zone)) +
  scale_color_manual(values = climates$color)

summary_locations2 <- summary_locations
st_geometry(summary_locations2) <- NULL

summary_locations2 %>% 
  group_by(zone) %>% 
  summarise(pleasant = mean(pleasant)) %>% 
  ungroup() %>% 
  ggplot(aes(x=reorder(zone, pleasant), y = pleasant, fill = zone)) +
  geom_col()+
  coord_flip() +
  scale_fill_manual(values = climates$color)










# Daily grain circles -----------------------------------------------------

data_joined <- summary_locations %>% 
  rename(total_pleasant = pleasant,
         total_hot = hot,
         total_cold = cold,
         total_elements = elements,
         # total_wind = wind,
         total_unknown = unknown) %>% 
  inner_join(data_daily, by = "location_id") %>% 
  # fuck leap years
  filter(yday != 366) %>% 
  mutate(year = year(date),
         yday = yday(date))

baseplot <- function(df, title = "XXX", subtitle = "YYY") {
  ggplot(df) +
    geom_tile(aes(x=yday, y=year, col = double_class, fill = double_class)) +
    scale_fill_manual(values = colors,
                      name = "Distinct classification",
                      aesthetics = c("colour", "fill")) +
    labs(title = title,
         caption = caption,
         subtitle = subtitle) +
    scale_x_continuous(
      # breaks = c(1, 91, 182, 275),
      # label = c("Jan", "Apr", "Jul", "Oct")
      breaks = c(1, 182),
      label = c("January", "July")
    ) +
    expand_limits(y = min(years)-length(years)) +
    theme(strip.text = element_text(face = "bold", size = 10),
          axis.text.y = element_blank()) +
    coord_polar()
}


data_joined %>% filter(name == "Kiev") %>% 
  baseplot()


plot_data <- function(df,
                      pop = 1000000, 
                      n = 25, 
                      dir = c("most", "least"), 
                      scope = "world",
                      ncol = 5) 
{
  
  for (dir in dir) {
    for (pop in pop) {
      
      if(scope != "world") {
        df <- df %>% filter(country == scope)
      }
      data <- df %>% 
        filter(population > pop) %>% 
        arrange(rank)
      
      if(dir == "most") { 
        top <- data %>% 
          select(location_id, total_pleasant) %>% 
          distinct() %>% 
          mutate(rank = row_number(desc(total_pleasant))) %>% 
          arrange(rank) %>% 
          head(n)
        
      } else { 
        top <- data %>% 
          select(location_id, total_pleasant) %>% 
          distinct() %>% 
          mutate(rank = row_number(total_pleasant)) %>% 
          arrange(rank) %>% 
          head(n)
      }
      
      data <- data %>% 
        select(-rank) %>% 
        inner_join(top, by = "location_id") %>% 
        mutate(label = case_when(scope == "United States" ~ paste0(rank, ". ", name),
                                 scope == "world" ~ paste0(rank, ". ", name_short, ", \n", country),
                                 TRUE ~ paste0(rank, ". ", name_short)),
               label = reorder(label, rank),
               label = str_replace_all(label, "-", "- "),
               label = str_replace_all(label, "- - ", "-- "),
               label = fct_reorder(label, rank))
      
      file <- tolower(paste0(paste(n, dir, pop/1000, scope, sep = " "), ".png"))
      sub <- paste0("With population over ", comma(pop), " people.\nYears ", min(years), "-", max(years),
                    "\nRanked based on years with over 90% of daily data available.",
                    "\nVisualizing all data, including incomplete years")
      
      baseplot(data,
               title = paste("Top", n, scope, "cities with", dir, "pleasant days in a year", sep = " "),
               subtitle = sub) + 
        facet_wrap(~label, ncol = ncol, labeller = labeller(label = label_wrap_gen(15))) 
      
      ggsave(file, width = 10, height = 16, units = "in")
    }
  }
}


plot_data(df = data_joined,
          pop = 1000000,
          n = 25,
          dir = c("most"),
          scope = "world",
          ncol = 5
          )

plot_data(df = data_joined,
          pop = 1000000,
          n = 25,
          dir = c("most"),
          scope = "United States",
          ncol = 5
)
