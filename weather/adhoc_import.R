library(tidyverse)

# Download ------------------------------------------------------------------

load("data/data.RData")

stations_us <- stations %>% 
  filter(ctry == "US")

# Data --------------------------------------------------------------------

weather_data <- NULL
weather_data_raw <- NULL

for (year in 2012:2017) {
  
  file <- paste0('gsod_',year)
  
  #ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
  destfile <- paste0(file,'.op.gz')
  
  #curl::curl_download(ftp, destfile)
  untar(destfile, exdir = file)
  
  data_raw <- map_df(list.files(file, full.names = TRUE),
                     read_table,
                     col_types = cols(.default = "c",
                                      YEARMODA = col_date(format = "%Y%m%d")),
                     na = c("9999.9", '99.99', '999.9', '0.00I')
  )
  colnames(data_raw) <- tolower(colnames(data_raw))
  data_raw <- data_raw %>%
    rename(stn = `stn---`, date = yearmoda, temp_mean = temp, temp_min = min, temp_max = max)
  
  data_raw_us <- data_raw %>%
    semi_join(stations_us, by = c('stn' = 'usaf', 'wban'))
  
  weather_data_raw <- rbind(weather_data_raw, data_raw_us)
  file.remove(destfile)
  unlink(file, recursive = TRUE)
  
}

weather_data <- weather_data_raw %>%
  # rename columns
  select(stn, wban, date, temp_mean, temp_min, temp_max, prcp, sndp) %>%
  # clean up asterisks and flags
  # (see data description for details)
  map_df(~ str_replace_all(.,'A|B|C|D|E|F|G|H|I|\\*', '')) %>%
  # convert some columns to numeric after cleaning up
  map_at(c('temp_mean', 'temp_min', 'temp_max', 'prcp', 'sndp'), as.numeric, na.rm = TRUE) %>%
  dplyr::bind_rows() %>% 
  # precipitation and snowfall NAs can be converted to 0 for this project
  # (see data description for details)
  # or maybe not...
  replace_na(replace = list(prcp = 0, sndp = 0))



save(cities, stations_us, weather_data, file = "data/data.RData")
