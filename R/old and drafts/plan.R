
# Plan
# - Get geography (CBSAs via tigris)
# - Weather:
#   - Get weather stations:
#     - Get stations within cbsa borders
#     - get stations by proximity (isd_search or something)
#   - Get weather
# - Housing:
#   - Get Zillow data by CBSA
#   - Find the way to reconcile 2 datasets.


plan <- drake_plan(
  years = seq(year(today()) - 11, year(today())-1),
  
  
  
  # Cities - this is no longer used ----------------
  # https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
  ne_cities_import = read_sf("data/ne_10m_populated_places", "ne_10m_populated_places") %>% 
    rename_all(tolower),
  
  ne_cities_filtered = cities_import %>%
    filter(featurecla != "Scientific station") %>% 
    filter(scalerank <= 5 & pop_max > 500000) %>% 
    dplyr::select(
      location_id = ne_id,
      scalerank, 
      name = nameascii,
      capital = adm0cap,
      worldcity,
      megacity,
      country = sov0name,
      adm0name,
      state = adm1name,
      country_iso = iso_a2,
      lat = latitude,
      lon = longitude,
      population = pop_max,
      pop_min,
      geonameid,
      # un_fid,
      # un_country = un_adm0,
      # un_lat,
      # un_long,
      wikidataid,
      wof_id,
      ne_id
      # bbxmin = min_bbxmin,
      # bbxmax = min_bbxmax,
      # bbymin = min_bbymin,
      # bbymax = min_bbymax
    ),
  
  ne_cities_null = null_geometry(cities_filtered),
  ne_locations = cities_null,
  
  

  # CBSAs -------------------------------------------------------------------

  cbsas = core_based_statistical_areas() %>% 
    st_as_sf() %>% 
    rename_all(tolower) %>% 
    mutate(name_short = str_sub(name, 
                                start = 1, 
                                end = if_else(is.na(str_locate(name, "-")[,1]), 
                                              str_locate(name, ",")[,1], 
                                              str_locate(name, "-")[,1]) -1)),
  
  
  # Stations  ----------------------------------------------------
  
  refresh_stations = isd_stations(refresh = TRUE),
  
  stations_df = locations %>% 
    mutate(st = purrr::map2(lat, lon, isd_stations_search, 
                            # bbox = c(bbxmin, bbymin, bbxmin, bbxmax),
                            radius = 20)) %>% 
    select(location_id, st) %>% 
    unnest(cols = c(st)) %>% 
    mutate(stnid = paste(usaf, wban, sep = "-"),
           stnid2 = paste0(usaf, wban),
           country = maps::map.where('world', lon, lat),
           lakes = maps::map.where('lakes', lon, lat)
    ) %>%
    filter(!is.na(country) & is.na(lakes)) %>%
    select(-c(lakes, country)),
  
  locations_stations = locations %>% 
    inner_join(stations_df %>% 
                 select(location_id,
                        distance,
                        stnid, 
                        stnid2), by="location_id"),
  
  # Get unique stations
  stations = locations_stations %>%
    select(stnid2) %>%
    distinct(),
  stations_v = as_vector(stations),
  
  # Weather -------------------------------------
  # weather_import = get_GSOD(years = years),
  weather_import = get_weather(yrs = years, stns = stations_v),
  lower_colnames = colnames_tolower(weather_import),
  weather = lower_colnames %>%
    # New data return??? WTF, need to rename lat and lon
    rename(lat = latitude,
           lon = longitude) %>% 
    filter(!is.na(lat) & !is.na(lon)) %>% 
    # Get rid of stations on water: oceans and lakes
    mutate(yday = yday(yearmoda)), 
  
  # Join city and weather data ---------
  
  data = locations_stations %>%
    select(location_id, stnid, distance) %>% 
    inner_join(weather, by = "stnid") %>% 
    select(location_id, date = yearmoda, 
           distance,
           temp_max = max,
           temp_min = min, 
           temp_mean = temp, 
           # dewp, slp, 
           # stp, 
           # visib, 
           wdsp, 
           # mxspd, 
           # gust, 
           prcp, 
           sndp, 
           # i_fog:i_tornado_funnel,
           i_rain_drizzle,
           i_snow_ice,
           rh) %>% 
    # filter(location_id == "cbsa12420" & date == "2008-01-01") %>% 
    # head(100) %>% 
    group_by(location_id, date) %>% 
    summarise_at(vars(temp_max:rh),
                 # funs(mean(., na.rm = TRUE)),
                 funs(weighted.mean(., w = 1/distance, na.rm = TRUE))
    ) %>% 
    ungroup() %>% 
    mutate_all( ~ case_when(!is.nan(.x) ~ .x)) %>% 
    # A few possible substitutions
    mutate(temp_max = if_else(is.na(temp_max) & !is.na(temp_min) & !is.na(temp_mean), 
                              2*temp_mean - temp_min, temp_max),
           temp_min = if_else(is.na(temp_min) & !is.na(temp_max) & !is.na(temp_mean), 
                              2*temp_mean - temp_max, temp_min)),
  
  
  
  # Predict missing values --------------------------------------------------
  max_date = max(data$date),
  min_date = min(data$date),
  dummy = data %>%
    select(location_id) %>%
    distinct() %>%
    merge(tibble(date = seq.Date(min_date, max_date, by = "day")), all = TRUE),
  
  full_data = dummy %>%
    left_join(data, by = c("location_id", "date")),
  
  
  data_check = full_data %>%
    mutate(year = year(date)) %>% 
    group_by(location_id, year) %>%
    summarise(total = n(),
              exist = sum(!is.na(temp_min))) %>% 
    ungroup() %>%
    mutate(ratio = exist / total) %>% 
    group_by(location_id) %>%
    summarise(avg_ratio = mean(ratio)) %>%
    ungroup() %>%
    filter(avg_ratio >= 0.85) %>% 
    select(location_id),
  
  full_data_filtered = full_data %>% 
    semi_join(data_check, by = "location_id"),
  locations_filtered = locations %>% 
    semi_join(data_check, by = "location_id"),
  
  data_filtered = data %>% 
    semi_join(data_check, by = "location_id"),
  
  
  data_completed = full_data_filtered %>%
    #GATHER
    select(location_id, date, temp_min, temp_max, temp_mean, rh, wdsp, prcp) %>%
    gather(metric, value, -c(location_id, date)) %>%
    mutate(flag = if_else(is.na(value), "missing", "train"),
           year = year(date),
           yday = yday(date)) %>%
    #NEST
    group_by(location_id, metric, flag) %>%
    nest() %>%
    pivot_wider(names_from = flag, values_from = data) %>%
    filter(!is.na(missing) & !is.na(train)) %>%
    #MODEL
    mutate(lm = train %>% purrr::map(f_lm)) %>%
    mutate(pred_lm = map2(.x = lm, .y = missing, .f = predict)) %>%
    #UNNEST
    select(-c(train, lm)) %>%
    unnest(c(missing, pred_lm)) %>%
    mutate(value = pred_lm,
           flag = "predicted") %>%
    select(location_id, date, metric, value, flag, yday, year) %>%
    #BIND
    select(-flag) %>%
    spread(metric, value) %>%
    bind_rows(data_filtered),
  
  # 2020-04-22
  # The step above tried to predict missing values.
  # I am going to disconnect it for now.
  # To reconnect it back, need to pass `data_completed` data frame into
  # the next step below`
  
  data_collapsed = data_completed %>%  
    mutate(year = year(date),
           yday = yday(date)) %>% 
    # This is the old step from the prediction times
    group_by(location_id, date, year, date) %>%
    summarise_all(max, na.rm = TRUE) %>%
    ungroup() %>%
    mutate_at(vars(rh:i_snow_ice), ~if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)) %>% 
    # replace_na(list(sndp = 0, prcp = 0, i_rain_drizzle = 0, i_snow_ice = 0, wdsp = 0, gust = 0)) %>%
    mutate_at(vars(rh), round, digits = 2) %>% 
    mutate(temp_mean_feel = feels_like(temp_mean, rh, wdsp),
           temp_min_feel = feels_like(temp_min, rh, wdsp),
           temp_max_feel = feels_like(temp_max, rh, wdsp)),
  
  climate_reference = read_csv("data/climates.csv"),
  
  # http://koeppen-geiger.vu-wien.ac.at/data/Koeppen-Geiger-ASCII.zip
  climate_zones = read_table2("data/Koeppen-Geiger-ASCII.txt") %>% 
    dplyr::rename(lon = Lon, lat = Lat, zone = Cls) %>% 
    mutate(ind = row_number()),
  
  climate_zones_sf = climate_zones %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326),
  
  params =  list(
    temp_hot_night = 18,
    temp_hot_day = 30,
    temp_cold_night = 0,
    temp_cold_day = 12,
    prcp = 3,
    sndp = 30
  ),
  
  subregions = world %>% 
    group_by(subregion) %>% 
    summarise(pop = sum(pop, na.rm = TRUE), country_n = n()) %>% 
    select(subregion),
  
  continents = world %>% 
    group_by(continent) %>% 
    summarise(pop = sum(pop, na.rm = TRUE), country_n = n()) %>% 
    select(continent),
  
  data_daily = data_collapsed %>% 
    mutate(hot = if_else(temp_min_feel > params$temp_hot_night |
                           temp_max_feel > params$temp_hot_day, 
                         1, 0),
           cold = if_else(hot == 0 & 
                            (temp_min_feel <  params$temp_cold_night |
                               temp_max_feel <  params$temp_cold_day), 
                          1, 0),
           # auc = map2_dbl(temp_min, temp_max, get_auc),
           elements = if_else(hot == 0 & cold == 0 & 
                                (prcp > params$prcp |
                                   sndp > params$sndp |
                                   i_rain_drizzle >= 0.5 |
                                   i_snow_ice >= 0.5), 1, 0)
           # wind = if_else(wdsp > 10, 1, 0)
    ) %>% 
    replace_na(list(hot = 0, cold = 0, elements = 0)) %>% 
    mutate(
      pleasant = if_else(hot + cold + elements > 0 , 0, 1),
      class = case_when(pleasant == 1 ~ "pleasant",
                        hot == 1 ~ "hot",
                        cold == 1 ~ "cold", 
                        elements == 1 ~ "elements",
                        # wind == 1 ~ "wind",
                        TRUE  ~ NA_character_),
      class = factor(class, levels = c("pleasant", "elements", "cold", "hot")),
      sum = pleasant + elements + hot + cold 
    ),
  
  
  summary_locations0 = data_daily %>%
    group_by(location_id, year) %>% 
    summarise_at(vars(pleasant, elements, hot, cold), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    ## This if_else accounts for cases of leap year with all known days.
    ## It makes sure we don't have negative unknown days
    ## But also levels out leap year for the next step of averaging
    mutate(unknown = if_else(pleasant + hot + cold + elements >= 365, 0, 365 - pleasant - hot - cold - elements)) %>% 
    group_by(location_id) %>% 
    summarise_at(vars(pleasant, hot, cold, elements, unknown), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(sum = pleasant + elements + hot + cold + unknown) %>% 
    mutate_at(vars(pleasant, elements, hot, cold, unknown), ~round(./sum*365),0) %>% 
    mutate(remainder = pleasant + elements + hot + cold + unknown - 365,
           pleasant = if_else(unknown == 0 & remainder > 0, pleasant - remainder, pleasant),
           unknown = if_else(unknown == 0 & remainder > 0 , unknown, unknown - remainder),
           sum = pleasant + elements + hot + cold + unknown) %>%
    left_join(locations_filtered, by = "location_id") %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    mutate(ind = st_nearest_feature(., climate_zones_sf)) %>%
    left_join(climate_zones %>% dplyr::select(ind, zone), by = "ind") %>%
    dplyr::select(-ind) %>% 
    st_intersection(subregions) %>% 
    st_intersection(continents) %>% 
    mutate(
      # name_wrap = str_replace_all(name, "-", "- ") %>% str_replace_all("- -", "--"),
      # name_wrap = str_wrap(name_wrap, 17),
      points = pleasant / 365 * 100,
      rank = row_number(desc(pleasant)),
      rank_rev = row_number(pleasant),
      rank_1 = row_number(desc(if_else(population < 1000000, NA_real_, pleasant))),
    ),
  
  summary_locations = summary_locations0 %>% 
    group_by(country) %>% 
    mutate(rank_country = row_number(desc(pleasant)),
           rank_country1 = row_number(desc(if_else(population < 1000000, NA_real_, pleasant)))
    ) %>% 
    ungroup() %>% 
    group_by(zone) %>% 
    mutate(rank_zone = row_number(desc(pleasant)),
           rank_zone1 = row_number(desc(if_else(population < 1000000, NA_real_, pleasant)))
    ) %>% 
    ungroup() %>% 
    group_by(subregion) %>% 
    mutate(rank_region = row_number(desc(pleasant)),
           rank_region1 = row_number(desc(if_else(population < 1000000, NA_real_, pleasant)))
    ) %>% 
    ungroup() %>% 
    group_by(continent) %>% 
    mutate(rank_cont = row_number(desc(pleasant)),
           rank_cont1 = row_number(desc(if_else(population < 1000000, NA_real_, pleasant)))
    ) %>% 
    ungroup(),
  
  summary_locations_tall = summary_locations %>% 
    gather("class", "days", pleasant:unknown) %>% 
    mutate(class  = factor(class, levels = rev(c("pleasant", "elements", "cold", "hot", "unknown")))),
  
  
  save_data = write_csv(data_collapsed, "data/data.csv"),
  save_locations = write_csv(locations_filtered, "data/locations.csv"),
  save_summary = saveRDS(summary_locations, "data/summary_locations.RDS"),
  save_summary_tall = saveRDS(summary_locations_tall, "data/summary_locations_tall.RDS"),
  save_data_daily = saveRDS(data_daily, "data/data_daily.RDS")
  
)
