# Script to get plays data from cfbd api and add success rate and other key stats
add_success <- function(plays_df){

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Play%20Types%20and%20Power%20Conference%20Names.R")

  plays.master <- plays_df
  
  if(any(class(plays.master) %in% c("data.frame", "tbl_df", "tbl"))){
  
# Start data manipulation
cat("Starting success rate calculations\n")


rm(plays_df)

# If there are new plays from this past week of games, run all the below calculations
if(purrr::is_empty(plays.master) == F){
  
  ## dplyr::mutate Plays with First Downs (Smart), Pass/Rush, Success
  plays.master_temp <- plays.master %>% 
    dplyr::mutate(play_specifics = play_type) %>% 
    dplyr::mutate(first_down = 
             dplyr::case_when(
               play_type %in% scrimmage_plays_all ~
                 ifelse(yards_gained >= distance 
                        & dplyr::lead(down, n=1, order_by = id) == 1 
                        & dplyr::lead(offense, n = 1, order_by = id) == offense 
                        & dplyr::lead(drive_id, n = 1, order_by = id) == drive_id 
                        & (!play_specifics %in% scrimmage_plays_turnover), TRUE, 
                        ifelse(play_specifics %in% c("Passing Touchdown", "Rushing Touchdown") 
                               | ((stringr::str_detect(play_text, "TOUCHDOWN") 
                                   | stringr::str_detect(play_text, "1ST down")) 
                                  & (!play_specifics %in% scrimmage_plays_turnover)), TRUE, FALSE)),
               TRUE ~ NA
             )
    )
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  # Calculate loss of yards on turnovers
  plays.master_temp <- plays.master %>% 
    dplyr::mutate(turnover_yards = dplyr::case_when(play_type %in% scrimmage_plays_turnover ~ 70L - dplyr::lead(yards_to_goal, order_by = id), #subtract starting field position of next drive from avg. starting field position to find how many yards that turnover cost you
                                      TRUE ~ 0L))
  
  plays.master_temp <- plays.master_temp %>% dplyr::mutate(yards_gained = dplyr::case_when(play_type %in% scrimmage_plays_turnover ~ -turnover_yards,
                                                                             TRUE ~ yards_gained))
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  
  # Calculate avg. starting field pos on turnovers vs. normal
  #drives.master %>% dplyr::summarise(mean(start_yards_to_goal)) # 70 avg.
  
  # plays.master %>% dplyr::mutate(lead_start_yards_to_goal = 
  #                           dplyr::case_when(play_type %in% scrimmage_plays_turnover ~ dplyr::lead(yards_to_goal, order_by = id),
  #                                     TRUE ~ 0L)) %>% 
  #   dplyr::filter(play_type %in% scrimmage_plays_turnover) %>% 
  #   dplyr::group_by(play_type) %>% 
  #   dplyr::filter(!stringr::str_detect(play_type,"Touchdown")) %>% 
  #   dplyr::summarise(avg_yds_to_goal = mean(lead_start_yards_to_goal), count = n()) # 54.5 avg, so a turnover is worth -15.5 yds. vs. avg.

  plays.master <- plays.master %>% 
    dplyr::mutate(pass_rush = NA_character_) # initialize new column to avoid warning
  
  # Classify the multiple play types into a simpler just rush or pass
  plays.master$pass_rush[plays.master$play_type %in% scrimmage_plays_pass] <- "Pass"
  plays.master$pass_rush[plays.master$play_type %in% scrimmage_plays_rush] <- "Rush"

# Directly mutate fumble and safety rows into pass or rush plays -----------------------------------

plays.master <- plays.master %>%
    dplyr::mutate(
      pass_rush = case_when(
        play_specifics %in% c(
          "Fumble Recovery (Own)",
          "Fumble Recovery (Opponent)",
          "Fumble Return Touchdown",
          "Safety"
        ) &
          stringr::str_detect(play_text, "\\srun\\s") ~ "Rush",
        play_specifics %in% c(
          "Fumble Recovery (Own)",
          "Fumble Recovery (Opponent)",
          "Fumble Return Touchdown",
          "Safety"
        ) &
          (
            stringr::str_detect(play_text, "\\spass\\s") |
              stringr::str_detect(play_text, "\\ssacked\\s")
          ) ~ "Pass",
        TRUE ~ pass_rush
      )
    )
  
  # Handle Penalties - TBD decide what to do if you detect "Penalty" in play_text
  # And what to do if you detect "Penalty" and "declined" in the play_text

  #Add Success Column
  plays.master_temp <- plays.master %>% 
    dplyr::mutate(success = 
             dplyr::case_when(
               play_type %in% scrimmage_plays_turnover ~ 0,
               down == 1 & yards_gained >= 0.5 * distance ~ 1,
               down == 2 & yards_gained >= 0.7 * distance ~ 1,
               down == 3 & yards_gained >= 1 * distance ~ 1,
               down == 4 & yards_gained >= 1 * distance ~ 1,
               stringr::str_detect(play_text, "1ST down") == TRUE ~ 1,
               stringr::str_detect(play_text, "TD") == TRUE ~ 1,
               TRUE ~ 0
             )
    )
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  ##
  
  ## Add Line Yards Stat
  
  plays.master.temp <- plays.master %>% 
    dplyr::mutate(lineYards = 
             dplyr::case_when(
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
    dplyr::mutate(garbage_time = 
             dplyr::case_when(
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
  percentile_explosiveness <- plays.master %>% dplyr::group_by(pass_rush) %>% 
    dplyr::summarise(avg_yds = mean(yards_gained), 
              stand_dev = stats::sd(yards_gained), 
              percentile = stats::quantile(yards_gained, .90))
  
  explosive_pass <- as.numeric(percentile_explosiveness %>% dplyr::filter(pass_rush == "Pass") %>% dplyr::pull(percentile))
  explosive_rush <- as.numeric(percentile_explosiveness %>% dplyr::filter(pass_rush == "Rush") %>% dplyr::pull(percentile))
  
  
  # Passing vs. Standard Downs, Explosive Plays
  plays.master.temp <- plays.master %>% 
    dplyr::mutate(passing_down = dplyr::case_when(down == 2 & distance >= 7 ~ 1,
                                    down == 3 & distance >= 5 ~ 1,
                                    down == 4 & distance >= 5 ~ 1,
                                    TRUE ~ 0),
           explosive = dplyr::case_when(play_specifics %in% scrimmage_plays_turnover ~ 0,
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
    dplyr::mutate(clock_in_seconds = 2700-(900*(period-1)) + minutes*60 + seconds)
  
  plays.master <- plays.master.temp
  rm(plays.master.temp)
  
  # Read in expected success rates
  success_expected <- data.table::fread("expected_success_rate.csv")
  
  # Join to plays
  plays.master_temp <- plays.master %>% 
    dplyr::left_join(success_expected, by = c("down", "distance")) %>% 
    dplyr::select(-count)
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  
  # Initial write of 2020 plays to csv
  # fwrite(plays.master, file = "C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays/2020_plays_ytd.csv", append = FALSE, col.names = TRUE)
  
  # Convert plays.master ids to characters
  plays.master_temp <- plays.master %>% 
    dplyr::mutate(id = as.character(id), game_id = as.character(game_id), drive_id = as.character(drive_id))
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
}

# Add QB Names for pass plays
plays_temp_fast <- plays.master %>% 
  dplyr::mutate(pass_player = dplyr::if_else(pass_rush == "Pass",
                               stringr::str_extract(play_text, "^[A-Z|a-z|\\.|\\-|\\']+\\s[A-Z|a-z|\\.|\\-|\\']+"), # Upper and lowercase characters and -.' before the second space
                               NA_character_),
                rush_player = dplyr::if_else(pass_rush == "Rush",
                                             stringr::str_extract(play_text, "^[A-Z|a-z|\\.|\\-|\\']+\\s[A-Z|a-z|\\.|\\-|\\']+"), # Upper and lowercase characters and -.' before the second space
                                             NA_character_))

# Checks to make sure data looks okay
# plays_temp_fast %>% dplyr::filter(pass_rush == "Pass") %>% count(is.na(pass_player))
# 
# plays_temp_fast %>% dplyr::filter(pass_rush == "Pass" & !is.na(pass_player)) %>% select(play_text, pass_player) %>% View()
# 
# plays_temp_fast %>% dplyr::filter(pass_rush == "Pass" & !is.na(pass_player)) %>% select(pass_player) %>% unique() %>% View()

plays.master <- plays_temp_fast

rm(plays_temp_fast)



message("Done")
return(plays.master)

  } else {
    
    message("Data supplied was not of type data.frame or tibble.\n",
            "Please use the plays data provided from the get_plays() function.")
    
  }
  
}
