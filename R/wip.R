library(tmap)
library(spData)
library(grid)

loadd(all_locations_sf)

# https://geocompr.github.io/geocompkg/articles/us-map.html

locations_filtered <- all_locations_sf %>%
  filter(!is.na(weather) & !is.na(zhvi)) %>% 
  mutate(zhvia= zhvi*365/weather)

cbsas48 <- locations_filtered %>% 
  st_crop(xmin=-125.0011, ymin=24.9493, xmax=-66.9326,	ymax=49.5904)
cbsa_hawaii <- locations_filtered %>% 
  filter(lat < 24.9493 & lon < -125.0011)
cbsa_alaska <- locations_filtered %>% 
  filter(lat > 49.5904)
cbsa_48_2163 = st_transform(cbsas48, 2163) %>% 
  st_simplify(dTolerance = 500)



us_states2163 = st_transform(us_states, 2163)
us_states_range = st_bbox(us_states2163)[4] - st_bbox(us_states2163)[2]
hawaii_range = st_bbox(hawaii)[4] - st_bbox(hawaii)[2]
alaska_range = st_bbox(alaska)[4] - st_bbox(alaska)[2]

us_states_hawaii_ratio = hawaii_range / us_states_range
us_states_alaska_ratio = alaska_range / us_states_range

graph <- function(metric) {
  if (metric == "weather") {
    breaks <- c(0, 50, 100, 150, 200, 250, 300, 365)
    palette <- "-plasma"
    legend <- "Pleasant days"
    col <- "weather"
    title <-
      "Average count of pleasant days in a year, by metro and micro areas"
    file <- paste0(metric, ".png")
  } else if (metric == "zhvi") {
    title <- "Zillow House Value Index, by metro and micro areas"
    palette <- "-magma"
    legend <- "ZHVI"
    col <- "zhvi"
    breaks <- c(0, 25000, 50000, 100000, 250000, 500000, 1000000, 1500000)
    file <- paste0(metric, ".png")
  } else if (metric == "zhvia") {
    breaks <- c(0, 100000, 250000, 500000, 1000000, 2500000, 5000000, 10000000)
    title <- "Adjusted House Value, by metro and micro areas"
    palette <- "-magma"
    legend <- "ZHVI Adjusted"
    col <- "zhvia"
    file <- paste0(metric, ".png")
  }
  
  us_states_map = tm_shape(us_states2163) +
    tm_borders() +
    tm_shape(cbsa_48_2163) +
    tm_polygons(col = col, title=legend, breaks = breaks, palette = palette, n = length(breaks)-1) +
    tm_layout(frame = FALSE,
              legend.position=c(0.86, 0.2),
              main.title = title)+
    tm_compass(type = "arrow", position = c("RIGHT", "TOP")) +
    tm_scale_bar(position = c("RIGHT", "BOTTOM"))
  
  hawaii_map = tm_shape(hawaii) +
    tm_borders() +
    tm_shape(cbsa_hawaii) +
    tm_polygons(col = col,  breaks = breaks, palette = palette) +
    tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA,
              title.position = c("left", "BOTTOM"),
              legend.show = FALSE)
  
  alaska_map =  tm_shape(alaska) +
    tm_borders() +
    tm_shape(cbsa_alaska) +
    tm_polygons(col = col, breaks = breaks,palette = palette) +
    tm_layout(title = "Alaska", frame = FALSE, bg.color = NA,
              title.position = c("left", "TOP"),
              legend.show = FALSE)
  
  grid.newpage()
  # pushViewport(
  #   viewport(
  #     layout = grid.layout(2, 1,
  #                          heights = unit(c(us_states_alaska_ratio, 1), "null"))))
  print(us_states_map, vp = viewport(layout.pos.row = 2))
  print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))
  print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))
  
}


graph("zhvia")

us_states_map = tm_shape(us_states2163) +
  tm_borders() +
  tm_shape(cbsa_48_2163) +
  tm_polygons(col = col, title=legend, breaks = breaks, palette = palette, n = length(breaks)-1) +
  tm_layout(frame = FALSE,
            legend.position=c(0.86, 0.2),
            main.title = title)+
  tm_compass(type = "arrow", position = c("RIGHT", "TOP")) +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"))

hawaii_map = tm_shape(hawaii) +
  tm_borders() +
  tm_shape(cbsa_hawaii) +
  tm_polygons(col = col,  breaks = breaks, palette = palette) +
  tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA,
            title.position = c("left", "BOTTOM"),
            legend.show = FALSE)

alaska_map =  tm_shape(alaska) +
  tm_borders() +
  tm_shape(cbsa_alaska) +
  tm_polygons(col = col, breaks = breaks,palette = palette) +
  tm_layout(title = "Alaska", frame = FALSE, bg.color = NA,
            title.position = c("left", "TOP"),
            legend.show = FALSE)

grid.newpage()
# pushViewport(
#   viewport(
#     layout = grid.layout(2, 1,
#                          heights = unit(c(us_states_alaska_ratio, 1), "null"))))
print(us_states_map, vp = viewport(layout.pos.row = 2))
print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))
ggsave("zhvi.png")
