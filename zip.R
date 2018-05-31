zip_url <- "https://gist.githubusercontent.com/erichurst/7882666/raw/5bdc46db47d9515269ab12ed6fb2850377fd869e/US%2520Zip%2520Codes%2520from%25202013%2520Government%2520Data"
zip <- read_csv(zip_url, 
                col_types = c('cdd')) %>% 
  rename(zip = ZIP,
         lat = LAT,
         lon = LNG) %>% 
  mutate(lat = round(lat, 4),
         lon = round(lon, 4))
                
write_csv(zip, 'zip.csv') 
