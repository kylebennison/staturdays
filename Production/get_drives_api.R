# Function to get drives from cfbd api
get_drives <- function(start_week, end_week, start_year, end_year){
  
  library(scales)
  library(tidyverse)
  library(RCurl)
  library(jsonlite)
  library(stringr)
  library(lubridate)
  library(bit64)
  library(data.table)
  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Play%20Types%20and%20Power%20Conference%20Names.R")
  
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

drives.master = tibble()
for (j in start_year:end_year) {
  for (i in start_week:end_week) {
    cat('Loading drives', j, 'Week', i, '\n')
    full_url_drives <- paste0(base_url_drives, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    drives <- cfbd_api(full_url_drives, my_key)
    drives <- as_tibble(drives)
    drives.master = rbind(drives.master, drives)
  }
}

return(drives.master)

}
