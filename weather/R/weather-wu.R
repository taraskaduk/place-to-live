library(rvest)
library(tidyverse)

url_test <- "https://www.wunderground.com/history/monthly/us/pa/philadelphia/KPHL/date/2017-8"
url <- url_test

df <- read_html(url) %>% 
  html_node(".observation-table")



df <- read_html(url) %>% 
  html_node("#inner-content .city-body .row.city-history-observation") %>% 
  html_attrs()


