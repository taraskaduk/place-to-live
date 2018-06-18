library(tidyverse)

# Cities ------------------------------------------------------------------

url_cities <- 'https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv'

cities <- read_csv(url_cities) %>% 
  rename(latitude = lat,
         longitude = lng)

write_csv(cities, "data/cities.csv")

# Stations -----------------------------------------------------------------

stations <- read_table("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt", 
                       skip = 20,
                       na = c('999999', '99999', '-999.0', ''))

colnames(stations) <- tolower(colnames(stations))

write_csv(stations, "data/stations.csv")


# Data --------------------------------------------------------------------
for (year in 2014:2017) {

  file <- paste0('gsod_',year)

  ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
  destfile <- paste0(file,'.op.gz')

  curl::curl_download(ftp, destfile)
  untar(destfile, exdir = file)

  data_all <- map_df(list.files(file, full.names = TRUE),
                     read_table,
                     col_types = cols(.default = "c",
                                      YEARMODA = col_date(format = "%Y%m%d")),
                     na = c("9999.9", '99999', '99.99', '999.9', '0.00I')
                     )

  colnames(data_all) <- tolower(colnames(data_all))

  data_clean <- data_all %>%
    # rename columns
    select(stn = `stn---`, wban, date = yearmoda, temp_mean = temp, temp_min = min, temp_max = max, prcp, sndp) %>%
    # clean up asterisks and flags
    # (see data description for details)
    map_df(~ str_replace_all(.,'A|B|C|D|E|F|G|H|I|\\*', '')) %>%
    # convert some columns to numeric after cleaning up
    map_at(c('temp_mean', 'temp_min', 'temp_max', 'prcp', 'sndp'), as.numeric) %>%
    dplyr::bind_rows() %>%
    # precipitation and snowfall NAs can be converted to 0 for this project
    # (see data description for details)
    replace_na(replace = list(prcp = 0, sndp = 0))


  data_us <- data_clean %>%
    inner_join(stations_us, by = c('stn' = 'usaf'))

  data_us %>%
    select(lat, lon, stn) %>%
    distinct() %>%
    ggplot(aes(x=lon, y = lat)) + geom_point(size = 0.25, alpha = 0.1)

  data_us %>%
    mutate(latr = round(lat*4,0)/4,
           lonr = round(lon*4,0)/4) %>%
    select(latr, lonr, stn) %>%
    distinct() %>%
    ggplot(aes(x=lonr, y = latr)) + geom_point(size = 0.25, alpha = 0.8)


  write_csv(data_us, paste0("data/data_us_", year, ".csv"))

  file.remove(destfile)
  unlink(file, recursive = TRUE)

}
