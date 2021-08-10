# Script to get plays data from cfbd api and add success rate and other key stats
get_plays <- function(start_week = 1, end_week = 1, start_year = 2020, end_year = 2020){
  
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


base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
start_week <- start_week
end_week <- end_week
start_year <- start_year
end_year <- end_year

plays.master = data.frame()
for (j in start_year:end_year) {
  for (i in start_week:end_week) {
    cat('Loading Plays', j, 'Week', i, '\n')
    full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
    full_url_plays_encoded <- URLencode(full_url_plays) # If there are spaces in query, formats them correctly
    plays <- cfbd_api(full_url_plays_encoded, my_key)
    plays$week = i
    plays$year = j
    plays.master = rbind(plays.master, plays, make.row.names=TRUE)
  }
}

# Start data manipulation
message("Starting success rate calculations")

# Rename columns to match historic
plays.master.temp <- plays.master %>% 
  rename(minutes = clock.minutes,
         seconds = clock.seconds) %>% 
  select(-wallclock)

plays.master <- plays.master.temp

rm(plays.master.temp)

# If there are new plays from this past week of games, run all the below calculations
if(is_empty(plays.master) == F){
  
  ## Mutate Plays with First Downs (Smart), Pass/Rush, Success
  plays.master_temp <- plays.master %>% 
    mutate(play_specifics = play_type) %>% 
    mutate(first_down = 
             case_when(
               play_type %in% scrimmage_plays_all ~
                 ifelse(yards_gained >= distance 
                        & lead(down, n=1, order_by = id) == 1 
                        & lead(offense, n = 1, order_by = id) == offense 
                        & lead(drive_id, n = 1, order_by = id) == drive_id 
                        & (!play_specifics %in% scrimmage_plays_turnover), TRUE, 
                        ifelse(play_specifics %in% c("Passing Touchdown", "Rushing Touchdown") 
                               | ((str_detect(play_text, "TOUCHDOWN") 
                                   | str_detect(play_text, "1ST down")) 
                                  & (!play_specifics %in% scrimmage_plays_turnover)), TRUE, FALSE)),
               TRUE ~ NA
             )
    )
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  # Calculate loss of yards on turnovers
  plays.master_temp <- plays.master %>% 
    mutate(turnover_yards = case_when(play_type %in% scrimmage_plays_turnover ~ 70L - lead(yards_to_goal, order_by = id), #subtract starting field position of next drive from avg. starting field position to find how many yards that turnover cost you
                                      TRUE ~ 0L))
  
  plays.master_temp <- plays.master_temp %>% mutate(yards_gained = case_when(play_type %in% scrimmage_plays_turnover ~ -turnover_yards,
                                                                             TRUE ~ yards_gained))
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  
  # Calculate avg. starting field pos on turnovers vs. normal
  #drives.master %>% summarise(mean(start_yards_to_goal)) # 70 avg.
  
  plays.master %>% mutate(lead_start_yards_to_goal = 
                            case_when(play_type %in% scrimmage_plays_turnover ~ lead(yards_to_goal, order_by = id),
                                      TRUE ~ 0L)) %>% 
    filter(play_type %in% scrimmage_plays_turnover) %>% 
    group_by(play_type) %>% 
    filter(!str_detect(play_type,"Touchdown")) %>% 
    summarise(avg_yds_to_goal = mean(lead_start_yards_to_goal), count = n()) # 54.5 avg, so a turnover is worth -15.5 yds. vs. avg.
  
  # Classify the multiple play types into a simpler just rush or pass
  plays.master$pass_rush[plays.master$play_type %in% scrimmage_plays_pass] <- "Pass"
  plays.master$pass_rush[plays.master$play_type %in% scrimmage_plays_rush] <- "Rush"
  # Rush Fumble Rows
  rush_rows <- plays.master %>% 
    filter(play_specifics %in% c("Fumble Recovery (Own)", 
                                 "Fumble Recovery (Opponent)", 
                                 "Fumble Return Touchdown", 
                                 "Safety"), 
           str_detect(play_text, "run") 
           & !str_detect(play_text, "kick") 
           & !str_detect(play_text, "punt")) %>% 
    mutate(pass_rush = "Rush")
  # Pass Fumble Rows
  pass_rows <- plays.master %>% 
    filter(play_specifics %in% c("Fumble Recovery (Own)", "Fumble Recovery (Opponent)"), str_detect(play_text, "pass") | str_detect(play_text, "sack")) %>% 
    mutate(pass_rush = "Pass")
  # Change fumbles to a pass or rush
  plays.master[which(plays.master$id %in% rush_rows$id), "pass_rush"] <- "Rush"
  plays.master[which(plays.master$id %in% pass_rows$id), "pass_rush"] <- "Pass"
  
  #Add Success Column
  plays.master_temp <- plays.master %>% 
    mutate(success = 
             case_when(
               play_type %in% scrimmage_plays_turnover ~ 0,
               down == 1 & yards_gained >= 0.5 * distance ~ 1,
               down == 2 & yards_gained >= 0.7 * distance ~ 1,
               down == 3 & yards_gained >= 1 * distance ~ 1,
               down == 4 & yards_gained >= 1 * distance ~ 1,
               str_detect(play_text, "1ST down") == TRUE ~ 1,
               str_detect(play_text, "TD") == TRUE ~ 1,
               TRUE ~ 0
             )
    )
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  ##
  
  ## Add Line Yards Stat
  
  plays.master.temp <- plays.master %>% 
    mutate(lineYards = 
             case_when(
               pass_rush == "Rush" & yards_gained < 0 ~ yards_gained * 1.2,
               pass_rush == "Rush" & yards_gained > 0 & yards_gained <= 4 ~ yards_gained * 1,
               pass_rush == "Rush" & yards_gained > 4 & yards_gained <= 10 ~ 4 + yards_gained * 0.5,
               pass_rush == "Rush" & yards_gained > 10 ~ 7,
               TRUE ~ 0
             )
    )
  plays.master <- plays.master.temp
  rm(plays.master.temp)
  ##
  
  ## Garbage Time Indicator WIP
  
  plays.master.temp <- plays.master %>% 
    mutate(garbage_time = 
             case_when(
               period == 2 & abs(offense_score - defense_score) > 38 ~ 1,
               period == 3 & abs(offense_score - defense_score) > 28 ~ 1,
               period == 4 & abs(offense_score - defense_score) > 22 ~ 1,
               period == 4 & minutes < 2 & abs(offense_score - defense_score) > 16 ~ 1,
               TRUE ~ 0
             )
    )
  plays.master <- plays.master.temp
  rm(plays.master.temp)
  
  # Plays by percentile to help guide defining explosive
  percentile_explosiveness <- plays.master %>% group_by(pass_rush) %>% 
    summarise(avg_yds = mean(yards_gained), 
              stand_dev = sd(yards_gained), 
              percentile = quantile(yards_gained, .90))
  
  explosive_pass <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Pass") %>% pull(percentile))
  explosive_rush <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Rush") %>% pull(percentile))
  
  
  # Passing vs. Standard Downs, Explosive Plays
  plays.master.temp <- plays.master %>% 
    mutate(passing_down = case_when(down == 2 & distance >= 7 ~ 1,
                                    down == 3 & distance >= 5 ~ 1,
                                    down == 4 & distance >= 5 ~ 1,
                                    TRUE ~ 0),
           explosive = case_when(play_specifics %in% scrimmage_plays_turnover ~ 0,
                                 yards_gained >= explosive_pass 
                                 & play_specifics %in% scrimmage_plays_non_turnover 
                                 & pass_rush == "Pass" ~ 1,
                                 yards_gained >= explosive_rush
                                 & play_specifics %in% scrimmage_plays_non_turnover 
                                 & pass_rush == "Rush" ~ 1,
                                 TRUE ~ 0))
  
  plays.master <- plays.master.temp
  rm(plays.master.temp)
  
  # Create a master clock completely in seconds
  plays.master.temp <- plays.master %>% 
    mutate(clock_in_seconds = 2700-(900*(period-1)) + minutes*60 + seconds)
  
  plays.master <- plays.master.temp
  rm(plays.master.temp)
  
  # Read in expected success rates
  success_expected <- fread("expected_success_rate.csv")
  
  # Join to plays
  plays.master_temp <- plays.master %>% 
    left_join(success_expected, by = c("down", "distance")) %>% 
    select(-count)
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  
  # Initial write of 2020 plays to csv
  # fwrite(plays.master, file = "C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays/2020_plays_ytd.csv", append = FALSE, col.names = TRUE)
  
  # Convert plays.master ids to characters
  plays.master_temp <- plays.master %>% 
    mutate(id = as.character(id), game_id = as.character(game_id), drive_id = as.character(drive_id))
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
}

# Add QB Names for pass plays
plays_temp_fast <- plays.master %>% 
  mutate(pass_player = if_else(pass_rush == "Pass",
                               str_extract(play_text, "^[A-Z|a-z|\\.|\\-|\\']+\\s[A-Z|a-z|\\.|\\-|\\']+"), # Upper and lowercase characters and -.' before the second space
                               NA_character_))

# Checks to make sure data looks okay
# plays_temp_fast %>% filter(pass_rush == "Pass") %>% count(is.na(pass_player))
# 
# plays_temp_fast %>% filter(pass_rush == "Pass" & !is.na(pass_player)) %>% select(play_text, pass_player) %>% View()
# 
# plays_temp_fast %>% filter(pass_rush == "Pass" & !is.na(pass_player)) %>% select(pass_player) %>% unique() %>% View()

plays.master <- plays_temp_fast

rm(plays_temp_fast)
return(plays.master)
message("Done")
}
