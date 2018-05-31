library(readxl)
library(tidyverse)
library(rvest)
library(jsonlite)

zri <- read_csv('http://files.zillowstatic.com/research/public/Zip/Zip_Zri_AllHomesPlusMultifamily_Summary.csv')


##This data is from https://openflights.org/data.html
airports <- read_delim('https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat', 
                       delim = ',', 
                       col_names = c('airport_id',
                                     'name',
                                     'city',
                                     'country',
                                     'iata',
                                     'icao',
                                     'lat',
                                     'long',
                                     'alt',
                                     'timezone',
                                     'dst',
                                     'tz_timezone',
                                     'type',
                                     'source'))

airlines <- read_delim('https://raw.githubusercontent.com/jpatokal/openflights/master/data/airlines.dat', 
                       delim = ',', 
                       col_names = c('airline_id',
                                     'name',
                                     'alias',
                                     'iata',
                                     'icao',
                                     'callsign',
                                     'country',
                                     'active'))
routes <- read_delim('https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat', 
                     delim = ',', 
                     col_names = c('airline', 
                                   'airline_id', 
                                   'origin_airport', 
                                   'origin_airport_id', 
                                   'dest_airport', 
                                   'dest_airport_id', 
                                   'codeshare', 
                                   'stops', 
                                   'equipment'))

cities <- fromJSON('https://gist.githubusercontent.com/Miserlou/c5cd8364bf9b2420bb29/raw/2bf258763cdddd704f8ffd3ea9a3e81d25e2c6f6/cities.json')


##UNESCO World Heritage
url_unesco <- "http://whc.unesco.org/en/list/xls/?2017"
destfile <- "X_2017.xls"
curl::curl_download(url_unesco, destfile)
unesco <- read_excel(destfile)


url_gunlaws <- 'http://lawcenter.giffords.org/scorecard/'
gunlaws <- read_html(url_gunlaws) %>% html_table() %>% .[[1]]

url_walkscore <- 'https://www.walkscore.com/cities-and-neighborhoods/'
walkscore <- read_html(url_walkscore) %>% html_table() %>% .[[1]]


##Geo data ftp://ftp.ncdc.noaa.gov/pub/data/gsod
##Legend: ftp://ftp.ncdc.noaa.gov/pub/data/gsod/GSOD_DESC.txt
##Data ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2016/gsod_2016.tar
url_weather <- 'ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2016/gsod_2016.tar'

##Weather station IDs: https://data.giss.nasa.gov/gistemp/station_data_v2/v2.temperature.inv.txt
##Another try: ftp://ftp.wmo.int/wmo-ddbs/VolA_New/Pub9volA160415x.flatfile ... from: http://www.wmo.int/pages/prog/www/ois/volume-a/vola-home.htm
##Also, may try this: http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.GLOBAL/.STATION.cuf/

