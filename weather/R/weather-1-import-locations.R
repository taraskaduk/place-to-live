setwd("weather")

library(maps)
library(lubridate)
library(tidyverse)
library(sf)
library(tigris)
library(readxl)




msa <- core_based_statistical_areas()
msa_sf <- msa %>% st_as_sf()







# Principal cities --------------------------------------------------------

cbsa_principal_cities <- read_excel("data/0-raw/more data from census/cbsa-principal-cities.xls", 
                                    col_types = c("text", "text", "text", 
                                                  "text", "text", "text",
                                                  "skip", "skip", "skip",
                                                  "skip"),
                                    col_names = c("cbsa_code", "cbsa_title", "cbsa_flag", "principal_city", "fips_st", "fips_place"),
                                    skip = 2
                                    )



















# 1.3 Import locations ------------------------------------------------------------

path_raw <- "data/0-raw/"
path_msa <- paste0(path_raw,"2015_Gaz_cbsa_national.txt")

locations <- read_delim(path_msa, 
                        "\t", escape_double = FALSE, 
                        locale = locale(encoding = "LATIN1", 
                                        asciify = TRUE),
                        col_types = cols(ALAND = col_skip(), 
                                         ALAND_SQMI = col_skip(), 
                                         AWATER = col_skip(), 
                                         AWATER_SQMI = col_skip(),
                                         GEOID = col_character()), 
                        trim_ws = TRUE) %>% 
  rename(csafp = CSAFP,
         geoid = GEOID,
         name = NAME,
         lat = INTPTLAT,
         lon = INTPTLONG,
         type = CBSA_TYPE) %>% 
  select(-type) %>% 
  separate(name, c("name", "type"), sep = -10) %>% 
  mutate(name = trimws(name)) 


path_msa_pop <- paste0(path_raw,"PEP_2017_PEPANNRES_with_ann.csv")
locations_pop <- read_csv(path_msa_pop, 
                          col_types = cols(GEO.id2 = col_character()),
                          locale = locale(encoding = "LATIN1", 
                                          asciify = TRUE),
                          trim_ws = TRUE) %>% 
  filter(GEO.id != "Id") %>% 
  select(geoid = GEO.id2,
         pop10 = rescen42010,
         pop17 = respop72017)

locations <- left_join(locations, locations_pop, by = "geoid") %>% 
  mutate(lat0 = round(lat,0),
         lon0 = round(lon,0),
         lat05 = round(2*lat,0)/2,
         lon05 = round(2*lon,0)/2)



ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = locations, aes(x=lon, y = lat, col = type),alpha = 0.3, size = 1)


ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = locations, aes(x=lon0, y = lat0), col = 'blue',alpha = 0.3)

ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = locations, aes(x=lon05, y = lat05), col = 'blue',alpha = 0.3)


















# 1.4 Save necessary data -----------------------------------------------------
save(data_weather, stations_us, locations, file = "data/1-import.RData")

