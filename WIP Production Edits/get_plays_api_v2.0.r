# Script to get plays data from cfbd api and add success rate and other key stats
get_plays <- function(start_week = 1, end_week = 1, start_year = 2020, end_year = 2020){

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")

base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
start_week <- start_week
end_week <- end_week
start_year <- start_year
end_year <- end_year

plays.master = dplyr::tibble()
for (j in start_year:end_year) {
  for (i in start_week:end_week) {
    cat('Loading Plays', j, 'Week', i, '\n')
    full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
    full_url_plays_encoded <- utils::URLencode(full_url_plays) # If there are spaces in query, formats them correctly
    plays <- cfbd_api(full_url_plays_encoded, my_key)
    plays$week = i
    plays$year = j
    plays.master = rbind(plays.master, plays, make.row.names=TRUE)
  }
}

# Rename columns to match historic
plays.master.temp <- plays.master %>% 
  dplyr::rename(minutes = clock.minutes,
         seconds = clock.seconds) %>% 
  dplyr::select(-wallclock)

plays.master <- plays.master.temp

rm(plays.master.temp)

# Return dataframe

message("Done")
return(plays.master)
}
