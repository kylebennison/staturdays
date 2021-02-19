## This script takes the first half of the 2020 Shiny App and grabs the data from
## collegefootballdata.com. The goal is to calculate all of our stats of interest,
## save them in CSV file(s), and read those in to the Shiny app to create a quicker 
## response. Right now the tables are all calculated but none are saved.


### Load necessary packages ###
library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
library(scales)
library(DT)
#library(rjson)
library(jsonlite)
library(htmlwidgets)
library(gt)
library(data.table)
library(RCurl)
library(XML)
library(stringr)
library(ggimage)
library(grid)
library(png)
library(bit64)
library(reactable)



power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC", "FBS Independents")

scrimmage_plays_all <- 
  c(
    "Rush", 
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Rushing Touchdown", 
    "Sack", 
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown", 
    "Fumble Recovery (Own)", 
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown"
  )

scrimmage_plays_non_turnover <-
  c(
    "Rush", 
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Rushing Touchdown", 
    "Sack", 
    "Fumble Recovery (Own)"
  )

scrimmage_plays_turnover <-
  c(
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown", 
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown",
    "Safety"
  )

scrimmage_plays_pass <-
  c(
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Sack",
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown"
  )

scrimmage_plays_rush <-
  c(
    "Rush", 
    "Rushing Touchdown"
  )

no_action_plays <- 
  c(
    "Timeout",
    "End Period",
    "End of Half",
    "End of Game",
    "Kickoff"
  )

scrimmage_plays_kicks <- 
  c(
    "Punt",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Field Goal Good",
    "Blocked Field Goal",
    "Missed Field Goal Return"
  )


# Team Colors and Logos
team_colors <- fromJSON(getURL("https://api.collegefootballdata.com/teams/fbs?year=2020"))

team_colors <- team_colors %>% unnest(cols = logos) %>% 
  mutate(logo_color = if_else(str_detect(logos, "dark"), "dark", "light")) %>% 
  pivot_wider(names_from = logo_color, values_from = logos)


#### get plays data and clean ####
base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of


plays.master = data.frame()
for (j in 2020:2020) {
  for (i in 1:20) {
    cat('Loading Plays', j, 'Week', i, '\n')
    full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
    full_url_plays_encoded <- URLencode(full_url_plays) # If there are spaces in query, formats them correctly
    plays <- fromJSON(getURL(full_url_plays_encoded)) # Pull in API using url
    if(is_empty(plays) == F){
      clockcolumns <- plays %>% unnest_legacy(clock) # Takes clock data out as it's own columns
      plays <- plays %>%
        select(-clock) %>%
        as_tibble() %>%
        mutate(minutes = clockcolumns$minutes, seconds = clockcolumns$seconds) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
        mutate_at(c("minutes", "seconds"), ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
      plays <- tibble(plays)
      plays$week = i
      plays$year = j
      plays.master = rbind(plays.master, plays, make.row.names=TRUE)
    }
  }
}

if(is_empty(plays.master)==F){rm(clockcolumns, plays)}

if(is_empty(plays.master) == F) {
  
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
  
  # plays.master %>% mutate(lead_start_yards_to_goal = 
  #                           case_when(play_type %in% scrimmage_plays_turnover ~ lead(yards_to_goal, order_by = id),
  #                                     TRUE ~ 0L)) %>% 
  #   filter(play_type %in% scrimmage_plays_turnover) %>% 
  #   group_by(play_type) %>% 
  #   filter(!str_detect(play_type,"Touchdown")) %>% 
  #   summarise(avg_yds_to_goal = mean(lead_start_yards_to_goal), count = n()) # 54.5 avg, so a turnover is worth -15.5 yds. vs. avg.
  
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
  ##
  
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
  success_expected <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/expected_success_rate.csv")
  
  # Join to plays
  plays.master_temp <- plays.master %>% 
    left_join(success_expected, by = c("down", "distance")) %>% 
    select(-count)
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  
  # Attempt to clean up turnovers
  plays.master_temp <- plays.master %>% 
    mutate(play_type = if_else(str_detect(play_text, "fumble") == T & !(play_type %in% scrimmage_plays_turnover), "Fumble", play_type)) %>% 
    mutate(play_type = case_when(play_type == "Fumble" & (lead(offense, n = 1L, order_by = play_number) != offense) ~ "Fumble Recovery (Opponent)",
                                 play_type == "Fumble" & (lead(offense, n = 1L, order_by = play_number) == offense) ~ "Fumble Recovery (Own)",
                                 TRUE ~ play_type))
}



#### get drives data and clean ####
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

drives.master = data.frame()
for (j in 2020:2020) {
  cat('Loading Drives', j, '\n')
  full_url_drives <- paste0(base_url_drives, "seasonType=both&", "year=", as.character(j))
  full_url_drives_encoded <- URLencode(full_url_drives)
  drives <- fromJSON(getURL(full_url_drives_encoded))
  start_time_columns <- as_tibble(drives$start_time) # Takes clock data out as it's own columns
  end_time_columns <- as_tibble(drives$end_time)
  elapsed_time_columns <- as_tibble(drives$elapsed)
  drives <- drives %>% 
    select(-start_time, -end_time, -elapsed) %>% 
    as_tibble() %>% 
    mutate(start_minutes = start_time_columns$minutes, start_seconds = start_time_columns$seconds, 
           end_minutes = end_time_columns$minutes, end_seconds = end_time_columns$seconds,
           elapsed_minutes = elapsed_time_columns$minutes, elapsed_seconds = elapsed_time_columns$seconds,) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
    mutate_at(
      c("start_minutes", "start_seconds", "end_minutes", "end_seconds", "elapsed_minutes", "elapsed_seconds"),
      ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
  drives <- as_tibble(drives)
  drives.master = rbind(drives.master, drives)
}





#### Calculate Summary Tables ####
succ_rate_off <- plays.master %>% 
  filter(down != 0) %>% 
  group_by(offense, offense_conference, down) %>% 
  summarise(succ_rate = mean(success), off_play_count = n()) %>% 
  left_join(team_colors, by = c("offense" = "school")) %>% 
  rename(team = offense, team_conference = offense_conference,
         play_count = off_play_count) %>% 
  mutate(stat = "offense_success_rate")


succ_rate_def <- plays.master %>% 
  filter(down != 0) %>% 
  group_by(defense, defense_conference, down) %>% 
  summarise(succ_rate = mean(success), def_play_count = n()) %>% 
  left_join(team_colors, by = c("defense" = "school")) %>% 
  rename(team = defense, team_conference = defense_conference,
         play_count = def_play_count) %>% 
  mutate(stat = "defense_success_rate")


# Explosiveness
explosive_summary <- plays.master %>% 
  filter(!play_specifics %in% no_action_plays, is.na(pass_rush) == F) %>% 
  filter(!play_specifics %in% c("Punt", "Blocked Punt", "Blocked Punt Touchdown")) %>% 
  group_by(offense) %>% 
  mutate(team_explosive_rate = mean(explosive)) %>% 
  group_by(offense, offense_conference, pass_rush) %>% 
  summarise(explosive_rate = mean(explosive), team_explosive_rate = mean(team_explosive_rate), count = n())

# Turnover Yards
####### ****** NEEDS WORK, Turnover counts do not look correct to me - Example - Illinois we're missing 3 fumbles lost - scrimmage_plays_turnover is probably excluding them
turnover_yds <- plays.master %>% 
  filter(play_type %in% scrimmage_plays_turnover) %>% 
  group_by(offense, offense_conference) %>% 
  summarise(avg_turnover_yards = -mean(turnover_yards), count = n()) %>% 
  left_join(team_colors, by = c("offense" = "school"))

# Found them in here - 2 plays designated "Rush" and one "Sack" - could try to classify any plays that we find "fumble" in text as "fumble", but tough to determine who recovered. 
# Could look at next play in play_number and see who's on offense
plays.master %>% filter(offense == "Illinois" | defense == "Illinois") %>% select(offense, defense, play_type, play_text) %>% filter(str_detect(play_text, "fumble")) %>% View()


#### Combined plays data summary table ####
succ_rate <- rbindlist(list(succ_rate_off, succ_rate_def), use.names = TRUE)
explosive_summary <- explosive_summary
turnover_yds <- turnover_yds


# Starting Field Position
off_field_pos <- drives.master %>% 
  group_by(offense, offense_conference) %>% 
  summarise(avg_start_field_pos = mean(start_yards_to_goal))

def_field_pos <- drives.master %>% 
  group_by(defense, defense_conference) %>% 
  summarise(avg_start_field_pos = mean(start_yards_to_goal))

field_pos <- left_join(off_field_pos, def_field_pos, by = c("offense" = "defense"), suffix = c("_off", "_def")) %>% 
  mutate(net_field_pos = avg_start_field_pos_def - avg_start_field_pos_off) %>% 
  group_by(offense) %>% 
  left_join(team_colors, by = c("offense" = "school")) %>% 
  ungroup()

# Pass and Rush Rates by Passing Down over Average
pass_rate_vs_avg_by_down <- plays.master %>% group_by(passing_down) %>% 
  filter(play_type %in% scrimmage_plays_all) %>%
  filter(is.na(pass_rush) == F) %>% 
  mutate(cfb_pass_rate = mean(pass_rush == "Pass")) %>% 
  group_by(offense, offense_conference, passing_down) %>% 
  summarise(pass_rate = mean(pass_rush == "Pass"), cfb_pass_rate = mean(cfb_pass_rate)) %>% 
  mutate(pass_vs_avg = (pass_rate - cfb_pass_rate)/(cfb_pass_rate)) %>% 
  pivot_wider(names_from = c("passing_down"), values_from = c("pass_rate", "cfb_pass_rate", "pass_vs_avg")) %>% 
  select(offense,
         offense_conference,
         pass_rate_standard_downs = pass_rate_0,
         cfb_pass_standard = cfb_pass_rate_0,
         pass_vs_avg_standard = pass_vs_avg_0,
         pass_rate_passing_downs = pass_rate_1,
         cfb_pass_passing = cfb_pass_rate_1,
         pass_vs_avg_passing = pass_vs_avg_1
  )

# Yards per attempt on rush and pass
yards_per_att_off <- plays.master %>% 
  group_by(offense, pass_rush) %>% 
  summarise(yards_per_attempt = mean(yards_gained)) %>% 
  filter(pass_rush != "") %>% 
  pivot_wider(names_from = "pass_rush", values_from = "yards_per_attempt", names_prefix = "ypa_")

yards_per_att_def <- plays.master %>% 
  group_by(defense, pass_rush) %>% 
  summarise(yards_per_attempt = mean(yards_gained)) %>% 
  filter(pass_rush != "") %>% 
  pivot_wider(names_from = "pass_rush", values_from = "yards_per_attempt", names_prefix = "ypa_")

yards_per_att_joined <- left_join(yards_per_att_off, yards_per_att_def, by = c("offense" = "defense"), suffix = c("_off", "_def"))

# Pass Rate by Down
pass_rate_by_down <- plays.master %>% group_by(down) %>% 
  filter(play_type %in% scrimmage_plays_all) %>% 
  filter(is.na(pass_rush) == F) %>% 
  mutate(cfb_pass_rate = mean(pass_rush == "Pass"), cfb_distance = mean(distance)) %>% 
  group_by(offense, offense_conference, down) %>% 
  summarise(pass_rate = mean(pass_rush == "Pass"),
            avg_distance = mean(distance),
            cfb_pass_rate = mean(cfb_pass_rate),
            cfb_distance = mean(cfb_distance)) %>% 
  mutate(pass_vs_avg = (pass_rate - cfb_pass_rate)/(cfb_pass_rate), 
         distance_vs_avg = (avg_distance - cfb_distance)/(cfb_distance)) %>% 
  pivot_wider(names_from = c("down"), values_from = c("pass_rate", "avg_distance", 
                                                      "cfb_pass_rate", "cfb_distance", 
                                                      "pass_vs_avg", "distance_vs_avg")) %>% 
  select(offense,
         offense_conference,
         dplyr::contains("_1"),
         dplyr::contains("_2"),
         dplyr::contains("_3"),
         dplyr::contains("_4"),
         everything()
  )

### Keep only what we need for the app

pass_rate_by_down # Pass Rate by Down
yards_per_att_joined # Yards Per Attempt, Rushing and Passing, on Offense and Defense
pass_rate_vs_avg_by_down # Pass Rate on Passing Downs vs. Standard Downs
field_pos # Average starting field position
turnover_yds # Average Turnover Yards
explosive_summary # Explosive Rate by team on pass and rush
succ_rate # success rate by down on offense and defense

# At this point, all these should be initally write csv to github, and then updated weekly and overwritten since they're season summaries
# Then, the Shiny app will pull the csvs from github at that point

setwd("C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays")

fwrite(pass_rate_by_down, file = "pass_rate_by_down.csv", append = FALSE)
fwrite(yards_per_att_joined, file = "yards_per_att_joined.csv", append = FALSE)
fwrite(pass_rate_vs_avg_by_down, file = "pass_rate_vs_avg_by_down.csv", append = FALSE)
fwrite(field_pos, file = "field_pos.csv", append = FALSE)
fwrite(turnover_yds, file = "turnover_yds.csv", append = FALSE)
fwrite(explosive_summary, file = "explosive_summary.csv", append = FALSE)
fwrite(succ_rate, file = "succ_rate.csv", append = FALSE)

# Now, run git pushes from the command line to push changes weekly
# Also need to run this R script from the command line weekly first to update the csv's