






# NE cities - no longer used ----------------------------------------------
# ne_cities_import <-
#   read_sf("data/ne_10m_populated_places", "ne_10m_populated_places") %>%
#   rename_all(tolower)
# ne_cities_filtered <- cities_import %>%
#   filter(featurecla != "Scientific station") %>%
#   filter(scalerank <= 5 & pop_max > 5e+05) %>%
#   dplyr::select(
#     cbsafp = ne_id,
#     scalerank,
#     name = nameascii,
#     capital = adm0cap,
#     worldcity,
#     megacity,
#     country = sov0name,
#     adm0name,
#     state = adm1name,
#     country_iso = iso_a2,
#     lat = latitude,
#     lon = longitude,
#     population = pop_max,
#     pop_min,
#     geonameid,
#     wikidataid,
#     wof_id,
#     ne_id
#   )
# ne_cities_null <- null_geometry(cities_filtered)
# ne_locations <- ne_cities_null

# CBSAs -------------------------------------------------------------------

locations_sf <- core_based_statistical_areas() %>%
  st_as_sf() %>%
  rename_all(tolower) %>%
  mutate(name_short = str_sub(name,
                              start = 1,
                              end = if_else(is.na(str_locate(name,"-")[, 1]),
                                            str_locate(name, ",")[, 1],
                                            str_locate(name,"-")[, 1]) - 1),
         lat=as.numeric(intptlat),
         lon=as.numeric(intptlon)
         ) %>% 
  select(-c(namelsad,mtfcc, intptlat,intptlon,memi,aland, awater))

locations <- null_geometry(locations_sf)

# Zillow ------------------------------------------------------------------

# Raw data 

# https://www.zillow.com/research/data/

# zillow_data_page <- "https://www.zillow.com/research/data/"
# zillow_url <- "http://files.zillowstatic.com/"
# zillow_xml <- xml2::as_xml_document(zillow_url)
# str(zillow_xml)
# zillow_xml$node
# 
# #curl::curl_download("http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv", destfile = "locations/data/crosswalk.csv")
# 
# zips <- c(
#   "Zip.zip",
#   "Neighborhood.zip",
#   "City.zip",
#   "County.zip",
#   "Metro.zip",
#   "State.zip"
# )
# 
# 
# f <- function(x) {
#   curl::curl_download(paste0('http://files.zillowstatic.com/research/public/',x), 
#                                       destfile = paste0("data/", x))
#   }
# 
# purrr::map(zips, f)

# ZHVI ---------------------------------------------------------------------

crosswalk <- read_csv("http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv",
                      col_types = cols(CBSACode = col_character(), 
                                       CountyFIPS = col_character(), CountyRegionID_Zillow = col_character(), 
                                       FIPS = col_character(), MetroRegionID_Zillow = col_character(), 
                                       StateFIPS = col_character())) %>% 
  clean_names()

crosswalk_cbsa <- crosswalk %>% 
  select(cbsa_name, region_id=metro_region_id_zillow, cbsafp=cbsa_code) %>% 
  distinct()

csv_all <-  "http://files.zillowstatic.com/research/public/Metro/Metro_Zhvi_AllHomes.csv"
csv_toptier <- "http://files.zillowstatic.com/research/public/Metro/Metro_Zhvi_TopTier.csv"
csv_3bedroom <- "http://files.zillowstatic.com/research/public/Metro/Metro_Zhvi_3bedroom.csv"

csvs <- c(csv_all, csv_toptier, csv_3bedroom)

zhvi_all <- map_dfr(csvs, read_csv)

zhvi <- zhvi_all %>% 
  mutate(RegionID = as.character(RegionID)) %>% 
  gather(key = "date", value = "zhvi", -c(RegionID:StateName)) %>% 
  clean_names() %>% 
  group_by_at(vars(-c(zhvi))) %>% 
  summarise(zhvi = mean(zhvi, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(region_name != "United States") %>% 
  filter(date >= floor_date(today(), unit="months") - years(10)) %>% 
  mutate(date = floor_date(as.Date(date), unit = "months"),
         weight = 0.9 ^  time_length(
           interval(
             date,
             floor_date(today(),
                        unit="months")), 
           "month"))

zhvi_summary <- zhvi %>% 
  group_by(region_id) %>% 
  summarize(zhvi = weighted.mean(zhvi, w=weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(crosswalk_cbsa, by="region_id")




# ZILLOW NEIGHBORHOOD DATA ------------------------------------------------

# csv_neigh <- "http://files.zillowstatic.com/research/public/Neighborhood/Neighborhood_Zhvi_3bedroom.csv"
# neighborhood_data <- read_csv(csv_neigh)
# 
# zhvi_n <- neighborhood_data %>% 
#   mutate(RegionID = as.character(RegionID)) %>% 
#   gather(key = "date", value = "zhvi", -c(RegionID:CountyName)) %>% 
#   clean_names() %>% 
#   group_by_at(vars(-zhvi)) %>% 
#   summarise(zhvi = mean(zhvi, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   filter(region_name != "United States") %>% 
#   filter(date >= floor_date(today(), unit="months") - years(10)) %>% 
#   mutate(date = floor_date(as.Date(date), unit = "months"),
#          weight = 0.9 ^  time_length(
#            interval(
#              date,
#              floor_date(today(),
#                         unit="months")), 
#            "month"))
# 
# zhvi_n_summary <- zhvi_n %>% 
#   group_by_at(vars(-c(zhvi,date,weight))) %>% 
#   summarize(zhvi = weighted.mean(zhvi, w=weight, na.rm = TRUE)) %>% 
#   ungroup()
# 
# jax <- zhvi_n_summary %>% 
#   filter(city == "Jacksonville" & state == "FL")

# AIRPORTS ----------------------------------------------------------------

## THIS SEEMS TO BE UPDATED RECENTLY. THE DUDE UPDATES IT BY HAND? OH MY GOD
# http://arm.64hosts.com/
# http://arm.64hosts.com/AirlineRouteMapper.zip



# NEW LINK! CHECK IT OUT

# https://www.transtats.bts.gov/Tables.asp?DB_ID=111
# https://www.transportation.gov/policy/aviation-policy/domestic-airline-consumer-airfare-report
# https://www.bts.gov/topics/airlines-and-airports/world-airport-codes
# https://www.bts.gov/explore-topics-and-geography/topics/air-traffic-data
# https://console.cloud.google.com/projectselector2/bigquery?sq=1089836875804:8ee9623e65944bbfaa377b5ec047d8ed&supportedpurview=project


## Old links:
# https://openflights.org/data.html
# https://github.com/GeographicaGS/flightroutes/tree/master/datatools/airportdatatools
# https://www.kaggle.com/open-flights/flight-route-database
# https://github.com/jpatokal/openflights/tree/master/data


routes_raw <- read_csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat",
                       col_names = c("airline",
                                     "airline_id",
                                     "origin",
                                     "origin_id",
                                     "destination",
                                     "destination_id",
                                     "codeshare",
                                     "stops",
                                     "equipment"
                       ),
                       na = '\\N')


airports_raw <- read_csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                         col_names = c('id',
                                       'name',
                                       'city',
                                       'country',
                                       'iata',
                                       'icao',
                                       'lat',
                                       'lon',
                                       'alt',
                                       'timezone',
                                       'dst',
                                       'tz',
                                       'type',
                                       'source'
                         ),
                         na = '\\N')

airports_wanted <- read_csv("data/airports_wanted.csv")

airports_join <- airports_raw %>% 
  select(iata, airport = name, lat, lon) %>% 
  filter(!is.na(iata)) %>% 
  left_join(airports_wanted %>% 
              select(iata) %>% 
              mutate(wanted=1), by="iata")

airports_sf <- airports_join %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 


airports_cbsa <- locations_sf %>% 
  st_set_crs(4326) %>% 
  st_join(airports_sf) %>% 
  select(cbsafp, name, iata, wanted) %>% 
  null_geometry()

flight_tally <- routes_raw %>% 
  filter(stops == 0) %>% 
  group_by(origin, destination) %>% 
  tally() %>% 
  ungroup() %>% 
  inner_join(airports_cbsa, by = c("origin"="iata")) %>% 
  group_by(cbsafp, name) %>% 
  summarise(destinations = n(),
            total_routes = sum(n),
            wanted_dests = sum(wanted, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(dest_index = 0.5 * wanted_dests +
                      0.3 * destinations +
                      0.2 * total_routes)


# POPULATION --------------------------------------------------------------
# https://walkerke.github.io/tidycensus/

population_import <- get_estimates(
  geography = "metropolitan statistical area/micropolitan statistical area",
  product = "population",
  year = "2018") 

population <- population_import %>% 
  filter(variable == "POP") %>% 
  clean_names() %>% 
  select(-variable) %>% 
  rename(population = value)

# WALKSCORE ---------------------------------------------------------------
# https://www.walkscore.com/professional/api.php
# Here is your API key that allows you to make 5,000 daily calls to the Walk Score API: 1db4acececb742c3a593b1259e096095
# Please keep your API key secret by using sample code (http://www.walkscore.com/professional/api-sample-code.php) 
# and follow branding and linking requirements (http://www.walkscore.com/professional/branding-requirements.php).

# walkscore_api <- "1db4acececb742c3a593b1259e096095"
# walkscore_url_test <- paste0("http://api.walkscore.com/score?format=json&address=1119%8th%20Avenue%20Seattle%20WA%2098101&lat=47.6085&lon=-122.3295&transit=1&bike=1&wsapikey=", walkscore_api)

walkscore_import <- read_csv("data/walkscore.csv")

walkscore <- walkscore_import %>% 
  select(cbsafp, walkscore, transitscore, bikescore) %>% 
  mutate(walkscore = 0.2 * walkscore, 
         transitscore = 0.5 * transitscore, 
         bikescore = 0.3 * bikescore) %>% 
  gather(metric, score, -cbsafp) %>% 
  group_by(cbsafp) %>% 
  summarise(walkscore = sum(score, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(cbsafp = as.character(cbsafp))


# CRIME RATE --------------------------------------------------------------
# https://github.com/mpjashby/crimedata

crime_lookup <- read_csv("data/crime_msa_lookup.csv")

crime_url <- "https://ucr.fbi.gov/crime-in-the-u.s/2017/crime-in-the-u.s.-2017/tables/table-6/table-6/output.xls"
crime_destfile <- "data/crime_data.xls"
download_crime <- curl::curl_download(crime_url, crime_destfile)
crime_import <- read_excel(crime_destfile, skip = 3)

crime <- crime_import %>%
  clean_names() %>% 
  rename(msa=metropolitan_statistical_area) %>% 
  fill(msa, .direction="down") %>% 
  filter(counties_principal_cities=="Rate per 100,000 inhabitants" & 
         str_detect(msa, "M.S.A.")) %>% 
  mutate(msa = str_remove(msa, " M.S.A.*")) %>% 
  select(-c(population, counties_principal_cities)) %>% 
  inner_join(crime_lookup, by="msa") %>% 
  mutate(total_crime = violent_crime + property_crime,
         total_crime_weighted = 3*violent_crime + property_crime,
         cbsafp = as.character(cbsafp))

# AIR QUALITY -------------------------------------------------------------

# https://github.com/maurosc3ner/uspm25_2000_2018

pm_import <- read_csv("https://raw.githubusercontent.com/maurosc3ner/uspm25_2000_2018/master/data/pm2.5byCounty.csv")

cbsa_county_fips_crosswalk_import <- read_csv("http://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv")
cbsa_county_fips_crosswalk <- cbsa_county_fips_crosswalk_import %>% 
  filter(!is.na(cbsacode)) %>% 
  mutate(fips = paste0(fipsstatecode, fipscountycode)) %>% 
  dplyr::select(cbsafp = cbsacode, name = cbsatitle, fips, county=countycountyequivalent)

pm <- pm_import %>% 
  mutate(fips=as.character(fips),
         fips=paste0("00", fips),
         fips= str_sub(fips, start=str_length(fips)-4,end=-1L)) %>% 
  inner_join(cbsa_county_fips_crosswalk, by="fips") %>% 
  dplyr::select(-c(county,fips)) %>% 
  group_by(cbsafp,name) %>% 
  summarise_all(.funs=mean, na.rm=TRUE) %>% 
  ungroup() %>% 
  gather(year, pm25, -c(cbsafp, name)) %>% 
  group_by(cbsafp,name) %>% 
  summarise(pm25=mean(pm25)) %>% 
  ungroup() %>% 
  mutate(cbsafp = as.character(cbsafp))

# WEATHER -----------------------------------------------------------------


# Climate (inactive) ------------------------------------------------------

# climate_reference <- read_csv("data/climates.csv")
# climate_zones <- read_table2("data/Koeppen-Geiger-ASCII.txt") %>%
#   dplyr::rename(lon = Lon,
#                 lat = Lat, zone = Cls) %>%
#   mutate(ind = row_number())
# 
# climate_zones_sf <-
#   climate_zones %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
# 
# continents <- world %>%
#   group_by(continent) %>%
#   summarise(pop = sum(pop, na.rm = TRUE),
#             country_n = n()) %>%
#   select(continent)
# 
# subregions <- world %>%
#   group_by(subregion) %>%
#   summarise(pop = sum(pop, na.rm = TRUE),
#             country_n = n()) %>%
#   select(subregion)

# Get stations ------------------------------------------------------------

years <- seq(year(today()) - 11, year(today()) - 1)

isd_stations <- isd_stations() %>% 
  filter(year(ymd(begin)) <= min(years)) %>% 
  filter(!is.na(lat) & !is.na(lon)) %>% 
  mutate(
    stnid = paste(usaf, wban, sep = "-"),
    stnid2 = paste0(usaf,wban)
  )

isd_stations_sf <- isd_stations %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4269)


# Find stations necessary for analysis ------------------------------------

locations_stations_sf <- st_join(locations_sf %>% select(cbsafp), 
                              isd_stations_sf %>% select(stnid, stnid2))
locations_stations <- null_geometry(locations_stations_sf) %>% 
  select(cbsafp, stnid, stnid2) %>%
  distinct()

stations <- locations_stations %>% 
  select(stnid2) %>%
  distinct()
stations_v <- as_vector(locations_stations)


# Get weather data --------------------------------------------------------

weather_import <- get_weather(yrs = years, stns = stations_v)
lower_colnames <- colnames_tolower(weather_import)
weather <- lower_colnames %>%
  rename(lat = latitude, lon = longitude) %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  mutate(yday = yday(yearmoda))

data <- locations_stations %>%
  select(cbsafp, stnid) %>%
  inner_join(weather, by = "stnid") %>%
  select(
    cbsafp,
    date = yearmoda,
    # distance,
    temp_max = max,
    temp_min = min,
    temp_mean = temp,
    wdsp,
    prcp,
    sndp,
    i_rain_drizzle,
    i_snow_ice,
    rh
  ) %>%
  group_by(cbsafp, date) %>%
  summarise_at(vars(temp_max:rh),
               funs(
                 # weighted.mean(., w = 1 / distance, na.rm = TRUE),
                 mean(., na.rm = TRUE))
               ) %>%
  ungroup() %>%
  mutate_all( ~ case_when(!is.nan(.x) ~ .x)) %>%
  mutate(
    temp_max = if_else(
      is.na(temp_max) & !is.na(temp_min) &
        !is.na(temp_mean),
      2 * temp_mean - temp_min,
      temp_max
    ),
    temp_min = if_else(
      is.na(temp_min) & !is.na(temp_max) &
        !is.na(temp_mean),
      2 * temp_mean - temp_max,
      temp_min
    )
  )


# Plug the holes in the data via a simple linear regression ---------------

min_date <- min(data$date)
max_date <- max(data$date)
dummy <- data %>%
  select(cbsafp) %>%
  distinct() %>%
  merge(tibble(date = seq.Date(min_date,
                               max_date,
                               by = "day")), all = TRUE)
full_data <-
  dummy %>% left_join(data, by = c("cbsafp", "date"))
data_check <- full_data %>%
  mutate(year = year(date)) %>%
  group_by(cbsafp,
           year) %>%
  summarise(total = n(), exist = sum(!is.na(temp_min))) %>%
  ungroup() %>%
  mutate(ratio = exist / total) %>%
  group_by(cbsafp) %>%
  summarise(avg_ratio = mean(ratio)) %>%
  ungroup() %>%
  filter(avg_ratio >=
           0.85) %>%
  select(cbsafp)

locations_filtered <- locations %>% 
  semi_join(data_check, by = "cbsafp")

full_data_filtered <- full_data %>% 
  semi_join(data_check, by = "cbsafp")

data_filtered <- data %>% 
  semi_join(data_check, by = "cbsafp") %>% 
  filter(!is.na(temp_min) & !is.na(temp_max))

save_locations <- write_csv(locations_filtered, "data/locations.csv")

# data_completed <- full_data_filtered %>%
#   select(cbsafp, date, temp_min, temp_max,
#          temp_mean, rh, wdsp, prcp) %>%
#   gather(metric, value,-c(cbsafp,date)) %>%
#   mutate(
#     flag = if_else(is.na(value) | is.null(value), "missing",
#                    "train"),
#     year = year(date),
#     yday = yday(date)
#   ) %>%
#   group_by(cbsafp, metric, flag) %>% 
#   nest() %>% 
#   pivot_wider(names_from = flag,
#               values_from = data) %>%
#   filter(!is.na(missing) & !is.na(train) & 
#            !is.null(missing) & !is.null(train) & 
#            missing != "NULL" & train != "NULL") %>%
#   mutate(lm = train %>% purrr::map(f_lm)) %>%
#   mutate(pred_lm = map2(.x = lm,
#                         .y = missing, .f = predict)) %>%
#   select(-c(train, lm)) %>%
#   unnest(c(missing, pred_lm)) %>%
#   mutate(value = pred_lm, flag = "predicted") %>%
#   select(cbsafp, date, metric, value, flag, yday, year) %>%
#   select(-flag) %>%
#   spread(metric, value) %>%
#   bind_rows(data_filtered)
# 
# 
# data_collapsed <- data_completed %>%
#   mutate(year = year(date), yday = yday(date)) %>%
#   group_by(cbsafp, date, year, date) %>%
#   summarise_all(max,
#                 na.rm = TRUE) %>%
#   ungroup() %>%
#   mutate_at(vars(rh:i_snow_ice),
#             ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)) %>%
#   mutate_at(vars(rh), round, digits = 2) %>%
#   mutate(
#     temp_mean_feel = feels_like(temp_mean,
#                                 rh, wdsp),
#     temp_min_feel = feels_like(temp_min, rh, wdsp),
#     temp_max_feel = feels_like(temp_max, rh, wdsp)
#   )


# Classify days into pleasant/hot/cold etc --------------------------------

params <- list(
  temp_hot_night = 18,
  temp_hot_day = 28,
  temp_cold_night = 0,
  temp_cold_day = 12,
  prcp = 5,
  sndp = 50
)

data_daily <- data_filtered %>%
  mutate(
    temp_mean_feel = feels_like(temp_mean,
                                rh, wdsp),
    temp_min_feel = feels_like(temp_min, rh, wdsp),
    temp_max_feel = feels_like(temp_max, rh, wdsp),
    hot = if_else(temp_min_feel > params$temp_hot_night |
                  temp_max_feel > params$temp_hot_day,
                  1, 0),
    cold = if_else(hot==0 &
                   (temp_min_feel < params$temp_cold_night | 
                    temp_max_feel < params$temp_cold_day),
                   1,0),
    elements = if_else(hot == 0 &
                       cold == 0 &
                       (prcp > params$prcp | 
                        sndp > params$sndp | 
                        i_rain_drizzle >= 0.5 |
                        i_snow_ice >= 0.5),
                       1, 0)) %>%
  replace_na(list(hot = 0,
                  cold = 0,
                  elements = 0)) %>%
  mutate(pleasant = if_else(hot + cold + elements > 0, 0, 1),
         class = case_when(pleasant == 1 ~ "pleasant",
                           hot == 1 ~ "hot",
                           cold == 1 ~ "cold",
                           elements == 1 ~ "elements",
                           TRUE ~ NA_character_),
         class = factor(class,
                        levels = c("pleasant", "elements", "cold", "hot")),
         sum = pleasant + elements + hot + cold)

data_edd <- data_daily %>% 
  bind_cols(map2_dfr(.$temp_min_feel, .$temp_max_feel, get_edd)) %>% 
  mutate(year = year(date),
         yday = yday(date))

# Summarize ---------------------------------------------------------------

summary_locations <- data_edd %>%
  group_by(cbsafp, year) %>%
  summarise_at(vars(pleasant,
                    elements, hot, cold), sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(unknown = if_else(
    pleasant + hot + cold + elements >=
      365,
    0,
    365 - pleasant - hot - cold - elements
  )) %>%
  group_by(cbsafp) %>%
  summarise_at(vars(pleasant, hot,
                    cold, elements, unknown), sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(sum = pleasant + elements + hot + cold + unknown) %>%
  mutate_at(vars(pleasant, elements, hot, cold, unknown),
            ~ round(. / sum *
                      365),
            0) %>%
  mutate(
    remainder = pleasant + elements +
      hot + cold + unknown - 365,
    pleasant = if_else(unknown ==
                         0 &
                         remainder > 0, pleasant - remainder, pleasant),
    unknown = if_else(unknown ==
                        0 &
                        remainder > 0, unknown, unknown - remainder),
    sum = pleasant +
      elements + hot + cold + unknown
  ) %>%
  left_join(locations_filtered,
            by = "cbsafp") %>%
  # st_as_sf(coords = c("lon", "lat"),
  #          crs = 4326) %>%
  # mutate(ind = st_nearest_feature(., climate_zones_sf)) %>%
  # left_join(climate_zones %>% dplyr::select(ind, zone), by = "ind") %>%
  # dplyr::select(-ind) %>%
  # st_intersection(subregions) %>%
  # st_intersection(continents) %>%
  mutate(
    points = pleasant / 365 * 100,
    rank = row_number(desc(pleasant)),
    rank_rev = row_number(pleasant)
    )
  

summary_locations_tall <- summary_locations %>%
  gather("class", "days", pleasant:unknown) %>%
  mutate(class = factor(class, levels = rev(
    c("pleasant", "elements",
      "cold", "hot", "unknown")
  )))



# Half-baked anumation ----------------------------------------------------

# anim <- summary_locations %>% 
#   mutate(lat = as.numeric(intptlat),
#          lon = as.numeric(intptlon)) %>% 
#   filter(lsad == "M1") %>% 
#   ggplot() +
#   geom_point(aes(x=lon, y=lat, col = pleasant)) +
#   scale_color_gradient(low = "red",high = "green")+
#   theme(panel.background = element_rect(color = "grey90", fill = "grey95")) +
#   transition_manual(rank, cumulative = TRUE)
# 
# save_anim <- anim_save("plots/anim.gif", animation = animate(anim, fps =3))




# RE-INSPECT THIS CRAP ----------------------------------------------------

# - Climate disaster resilience: https://www.citylab.com/environment/2019/08/climate-impacts-resilient-cities-environmental-justice/596251/
# - [Where Salaries Go Furthest in 2019 - Indeed Hiring Lab](https://www.hiringlab.org/2019/08/27/adjusted-salaries-2019/)
# - [Maps of natural disasters and extreme weather | FlowingData](https://flowingdata.com/2019/04/26/maps-of-natural-disasters-and-extreme-weather/)
# - https://www.washingtonpost.com/graphics/2019/national/mapping-disasters/
# 
#   [City Comparison Results](https://www.areavibes.com/compare-results/?place1=Jacksonville%2C%20FL&place2=New%20York%2C%20NY#)
#   - [Cities of the World Where You Don’t Need AC or Heat, Mapped.](https://medium.com/@mnolangray/cities-of-the-world-where-you-dont-need-ac-or-heat-mapped-2a3d6e018970)
#   - [A 13,235-Mile Road Trip for 70-Degree Weather - CityLab](https://www.citylab.com/environment/2015/10/a-13235-mile-road-trip-for-70-degree-weather-every-day/411406/)
#   - [This Dreariness Index Shows Which U.S. Cities Have the Lousiest Weather - CityLab](https://www.citylab.com/environment/2015/03/wheres-the-dreariest-place-in-america/388366/)
#   - https://www.theguardian.com/cities/ng-interactive/2018/aug/14/which-cities-are-liveable-without-air-conditioning-and-for-how-much-longer
#   - [Room temperature - Wikipedia](https://en.wikipedia.org/wiki/Room_temperature)
#   * [How Much Hotter Is Your Hometown Than When You Were Born?](https://www.nytimes.com/interactive/2018/08/30/climate/how-much-hotter-is-your-hometown.html)
#   * [CIL_Days-over-90_Method.pdf](http://www.impactlab.org/wp-content/uploads/2018/08/CIL_Days-over-90_Method.pdf)
#   [file:5C80F2EC-994B-425F-9578-21152DB4D948-9788-0000747AED1C616C/CIL_Days-over-90_Method.pdf]
#   
#   * [Summer rainfall: opposite extremes split the nation - Washington Post](https://www.washingtonpost.com/graphics/2018/national/summer-rain/?noredirect=on&utm_term=.bcf35b923a06)
#   * [Global Liveability Ranking](https://www.eiu.com/topic/liveability)
#   * [NPR Investigation: Low-Income Urban Areas Are Often Hotter Than Wealthy Ones : NPR](https://www.npr.org/2019/09/03/754044732/as-rising-heat-bakes-u-s-cities-the-poor-often-feel-it-most?utm_source=pocket-newtab)
#   * https://www.citylab.com/environment/2019/09/how-air-pollution-in-megacities-is-poisoning-childrens-brains/598981/
#     * [Walkability Index - Data.gov](https://catalog.data.gov/dataset/walkability-index)
#   * https://weatherspark.com/ 
#     * [What cities have the most nice days in the United States? - The Washington Post](https://www.washingtonpost.com/news/capital-weather-gang/wp/2018/08/07/the-united-states-of-nice-days-heres-where-and-when-to-find-the-nations-most-frequent-ideal-weather/#comments-wrapper) - VERY SIMILAR REPORT???
#   * [For Your Perusal: Presenting Zillow’s Pleasant Days Processes & Procedures - Zillow Research](https://www.zillow.com/research/pleasant-days-methodology-8513/)
# 
# 
# ## Formulas
# * [Heat Index Equation](https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml)
# * [heat.index](https://www.rdocumentation.org/packages/weathermetrics/versions/1.2.2/topics/heat.index)
# * https://www.researchgate.net/publication/245383326_New_algorithm_for_generating_hourly_temperature_values_using_daily_maximum_minimum_and_average_values_from_climate_models
# 
# ## NOAA data
# - Global hourly data: [Index of /data/global-hourly](https://www.ncei.noaa.gov/data/global-hourly/)
# - 6.4G of daily summaries (CSV): [Index of /data/daily-summaries/archive](https://www.ncei.noaa.gov/data/daily-summaries/archive/)
# 
# 
# ## other data
# [Berkeley Earth](http://berkeleyearth.org/data/)
# 
# ## GSODR
# * [GSODR: Global Surface Summary of the Day (GSOD) Weather Data from R — GSODR • GSODR](https://ropensci.github.io/GSODR/reference/GSODR.html)
# * https://cran.r-project.org/web/packages/GSODR/GSODR.pdf
# 
# 
# ## Weather stations attribution further steps links
# - Read this: https://geocompr.robinlovelace.net/
#   - Documentation here: https://cran.r-project.org/web/packages/tigris/tigris.pdf
# - `core_based_statistical_areas()`
# - `geo_join()`
# - [r - How to find which polygon a point belong to via sf - Stack Overflow](https://stackoverflow.com/questions/43456524/how-to-find-which-polygon-a-point-belong-to-via-sf)
# - [r - Check if point is in spatial object which consists of multiple polygons/holes - Stack Overflow](https://stackoverflow.com/questions/21971447/check-if-point-is-in-spatial-object-which-consists-of-multiple-polygons-holes)
# 
#                           
#                           
# - scale [Geocomputation with R](https://geocompr.robinlovelace.net/geometric-operations.html)
# 
# 
# ## Cool maps to emulate
# * [The Opportunity Atlas](https://www.opportunityatlas.org/)
# 
# Skift article on Monocle cities: https://skift.com/2016/06/22/monocles-new-quality-of-life-top-25-cities-survey-tokyo-is-tops-again/
# Video on Monocle cities: https://monocle.com/film/affairs/top-25-cities-2016/
# Night mayors: http://www.citylab.com/cityfixer/2016/01/night-mayor-amsterdam-mirik-milan/433893/
# https://monocle.com/film/affairs/night-mayors/
# Monocle video on Tokyo: https://monocle.com/film/affairs/most-liveable-city-2016-tokyo/
# Copenhagenize Bicycle Friendly Cities index: http://copenhagenize.eu/index/
# Personal account of moving to DC: http://katieaune.com/moving-to-dc/
# Barcelona superblocks: https://www.theguardian.com/cities/2016/may/17/superblocks-rescue-barcelona-spain-plan-give-streets-back-residents
# http://www.vox.com/2016/8/4/12342806/barcelona-superblocks
# Video on racism in “progressive northern states” like Oregon: https://www.youtube.com/watch?v=W4U1ozz7nM8&index=4&list=PLnvZ3PbKApGM-hHuQ9lNc5oSKsusjn0Z6
# Livability rankings - US - 2016: http://www.livability.com/best-places/top-100-best-places-to-live/2016
# Rent control in Paris (working well): http://www.citylab.com/housing/2016/08/paris-rent-control-laws-are-working/494282/?utm_source=nl__link2_080316
# Rent control in Berlin (Doing okay): https://theknowledgeexchangeblog.com/2016/05/06/rent-controls-lessons-from-berlin/
# Portland housing prices: http://money.cnn.com/2016/02/05/pf/oregon-unaffordable-california/



# PREFERENCE --------------------------------------------------------------

preferences <- read_csv("data/preferences.csv", 
                        col_types = "ccd")

  
# FINAL -------------------------------------------------------------------

final <- locations_sf %>% 
  left_join(summary_locations %>% 
               select(cbsafp, weather=pleasant),
             by="cbsafp") %>% 
  left_join(zhvi_summary %>% 
               select(cbsafp, zhvi),by="cbsafp") %>% 
  left_join(flight_tally %>% select(cbsafp,dest_index), 
             by="cbsafp") %>% 
  left_join(walkscore, by = "cbsafp") %>% 
  left_join(population%>% 
              select(geoid, population), by = "geoid") %>% 
  left_join(pm %>%
              select(-name), by = "cbsafp") %>% 
  left_join(preferences %>% select(-name),
            by= "cbsafp") %>% 
  # left_join(crime %>% 
  #             select(cbsafp, crime = total_crime_weighted)) %>% 
  select(-c(csafp, geoid, lsad))

final_filtered <- final %>% 
  filter(dest_index >= 30 &
         weather >= 100 &
         population >= 750000 &
         zhvi <= 1000000 &
         zhvi >= 200000 &
         walkscore >= 25 &
         preference_points > 0) %>% 
  
  mutate(weather_points= rescale(weather, to=c(0,100)),
         #weather_points= weather/365*100,
         zhvi_points = rescale(zhvi, to=c(100,0)),
         dest_points=rescale(dest_index, to=c(0,100)),
         # crime_points = rescale(crime, to=c(100,0)),
         # pm25_points = rescale(pm25, to=c(100,0)),
         walkscore_points = walkscore
         ) %>% 
  replace_na(list(weather_points=0, zhvi_points=0, dest_points=0)) %>% 
  mutate(total_not_weighted= (weather_points +
                              zhvi_points +
                              dest_points +
                              walkscore_points + 
                              # pm25_points+
                              # crime_points+
                              preference_points
                              )/5,
         total_weighted = 0.3  * zhvi_points +
                          0.25 * weather_points+
                          0.2  * dest_points +
                          0.15 * walkscore_points +
                          # 0.15 * crime_points
                          # 0.05 * pm25_points +
                          0.1  * preference_points 
                          ) %>% 
  mutate(rank = row_number(desc(total_weighted))) %>% 
  arrange(desc(total_weighted))

final_filtered_nogeo <- null_geometry(final_filtered)

save_csv <- write_csv(final_filtered_nogeo, "data/final.csv")
