library(jsonlite)
library(tidyverse)
library(httr)
library(RCurl)

# Prereq: You need to get an API key, and then locate your .Renviron file and add a new line
# cfbd_staturdays_key = "mysecretkey"
my_key <- Sys.getenv("cfbd_staturdays_key")

cfbd_api <- function(url, key){

data <- httr::GET(url = url, 
                  content_type_json(),
                  add_headers('Authorization' = paste('Bearer', key)))

data <- content(data, as="text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE) %>% 
  tibble() %>% 
  readr::type_convert()

}
