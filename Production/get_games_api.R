# Function to get games from cfbd api
get_games <- function(start_week, end_week, start_year, end_year){
  
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
  
base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data

games.master = tibble()
for (j in start_year:end_year) {
  for (i in start_week:end_week) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    games <- cfbd_api(full_url_games, my_key)
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

return(games.master)

}
