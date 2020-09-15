# Get CFB Data API for Play by Play - Use to generate expected points added (EPA)
rm(list=ls())
library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(gt)
library(lubridate)

#Staturdays Colors

staturdays_col_list <- c(
  lightest_blue = "#5c6272",
  lighter_blue = "#4c5872",
  light_blue = "#394871",
  medium_blue = "#22345a",
  dark_blue = "#041e42",
  orange = "#de703b",
  sign = "#1e1e1e",
  white = "#FFFFFF"
)

staturdays_palette <- c("#041e42", "#22345a", "#394871", "#4c5872", "#5c6272", "#de703b")

staturdays_colors <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (staturdays_col_list)
  
  staturdays_col_list[cols]
}

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")

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
    "Fumble Return Touchdown"
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

base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

plays.master = data.frame()
for (j in 2001:2019) {
  for (i in 1:15) {
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
rm(clockcolumns, pass_rows, plays)

games.master = data.frame()
for (j in 2001:2019) {
  for (i in 1:15) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

drives.master = data.frame()
for (j in 2001:2019) {
  cat('Loading Drives', j, '\n')
  full_url_drives <- paste0(base_url_drives, "seasonType=both&", "year=", as.character(j))
  full_url_drives_encoded <- URLencode(full_url_drives)
  drives <- fromJSON(getURL(full_url_drives_encoded))
  start_time_columns <- as_tibble(unnest(drives$start_time)) # Takes clock data out as it's own columns
  end_time_columns <- as_tibble(unnest(drives$end_time))
  elapsed_time_columns <- as_tibble(unnest(drives$elapsed))
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

# # Join Plays with Games to get additional info
# games.temp <- games.master
# games.temp <- mutate(games.temp, id = as.character(id))
# plays.temp <- plays.master
# plays.temp <- mutate(plays.temp, playid = id, id = substr(playid, 1, 9))
# plays_games_joined.master <- left_join(plays.master, games.temp, by = 'id') # Get regular or post season from games data
# games.temp <- data.frame()

# # Join Plays with Drives to Get Start Yardline
# drives.temp <- drives.master
# drives.temp <- mutate(drives.temp, id = as.character(id))
# plays.temp <- plays.master
# plays.temp <- mutate(plays.temp, playid = id, driveid = substr(playid, 1, 10))
# plays_drives.master <- left_join(plays.temp, drives.temp, by = c("drive_id" = "id"))
# drives.temp <- data.frame()
# plays.temp <- data.frame()

# Adjust yards_gained and start_yardline stats
# plays_drives.master <- plays_drives.master %>% 
#   mutate(start_yardline = if_else(offense.x != home, 100 - start_yardline, as.double(start_yardline)), end_yardline = if_else(offense.x != home, 100 - end_yardline, as.double(end_yardline)))

## Mutate Plays with First Downs (Smart), Pass/Rush, Success
plays.master2 <- plays.master %>% 
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
                             | ((str_detect(play_text, "TOUCHDOWN?") 
                                 | str_detect(play_text, "1ST down?")) 
                                & (!play_specifics %in% scrimmage_plays_turnover)), TRUE, FALSE)),
             TRUE ~ NA
           )
  )
# Classify the multiple play types into a simpler just rush or pass
plays.master2$play_type[plays.master2$play_type %in% scrimmage_plays_pass] <- "Pass"
plays.master2$play_type[plays.master2$play_type %in% scrimmage_plays_rush] <- "Rush"
# Rush Fumble Rows
rush_rows <- plays.master2 %>% 
  filter(play_specifics %in% c("Fumble Recovery (Own)", "Fumble Recovery (Opponent)"), str_detect(play_text, "run?") & !str_detect(play_text, "kick?") & !str_detect(play_text, "punt?")) %>% 
  mutate(play_type = "Rush")
# Pass Fumble Rows
pass_rows <- plays.master2 %>% 
  filter(play_type %in% c("Fumble Recovery (Own)", "Fumble Recovery (Opponent)"), str_detect(play_text, "pass?") | str_detect(play_text, "sack?")) %>% 
  mutate(play_type = "Pass")
# Change fumbles to a pass or rush
plays.master2[which(plays.master2$id %in% rush_rows$id), "play_type"] <- "Rush"
plays.master2[which(plays.master2$id %in% pass_rows$id), "play_type"] <- "Pass"
#Add Success Column
plays.master2 <- plays.master2 %>% 
  mutate(success = 
           case_when(
             down == 1 & yards_gained >= 0.5 * distance ~ 1,
             down == 2 & yards_gained >= 0.75 * distance ~ 1,
             down == 3 & yards_gained >= 1 * distance ~ 1,
             down == 4 & yards_gained >= 1 * distance ~ 1,
             str_detect(play_text, "1ST down?") == TRUE ~ 1,
             str_detect(play_text, "TD?") == TRUE ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master2
rm(plays.master2)
##

## Add Line Yards Stat

plays.master.temp <- plays.master %>% 
  mutate(lineYards = 
           case_when(
             play_type == "Rush" & yards_gained < 0 ~ yards_gained * 1.2,
             play_type == "Rush" & yards_gained > 0 & yards_gained <= 4 ~ yards_gained * 1,
             play_type == "Rush" & yards_gained > 4 & yards_gained <= 10 ~ 4 + yards_gained * 0.5,
             play_type == "Rush" & yards_gained > 10 ~ 7,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

## Garbage Time Indicator WIP

plays.master.temp <- plays.master %>% 
  mutate(garbageTime = 
           case_when(
             period == 4 & minutes < 2 & abs(offense - defense) > 16 ~ 1,
             period == 4 & minutes < 6 & minutes >= 2 & abs(offense - defense) > 24 ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

# Create a master clock completely in seconds
plays.master.temp <- plays.master %>% 
  mutate(clock_in_seconds = 2700-(900*(period-1)) + minutes*60 + seconds)

plays.master <- plays.master.temp
rm(plays.master.temp)


# In-Game Win Probability Based on Score and Elo --------------------------
games.temp <- games.master %>% 
  select(id, home_team, home_points, away_team, away_points)

plays.master.win_prob <- plays.master %>% mutate(home_score = case_when(home == offense ~ offense_score, # Get home lead/deficit
                                               TRUE ~ defense_score),
                        away_score = case_when(away == offense ~ offense_score,
                                               TRUE ~ defense_score),
                        home_score_lead_deficit = home_score - away_score) %>% 
  left_join(games.temp, by = c("home" = "home_team", "away" = "away_team", "game_id" = "id")) # Join games to get final result for each play

# Join Elo Rating as well
elo_ratings <- read_csv(file = "https://raw.githubusercontent.com/kylebennison/staturdays/master/elo_ratings_historic.csv",
                        col_types = list(col_character(), col_character(), col_double(), col_integer(), col_integer(), col_date(format = "%Y-%m-%d"))) %>% 
  select(team, elo_rating, week, season)

# Having an issue here where i end up with more rows than before. Join keys may not be unique i.e. multiple matches on rhs for certain plays on lhs
plays.master.win_prob2 <- plays.master.win_prob %>% left_join(elo_ratings, by = c("home" = "team", "week", "year" = "season")) %>% 
  rename(home_elo = elo_rating) %>% 
  left_join(elo_ratings, by = c("away" = "team", "week", "year" = "season")) %>% 
  rename(away_elo = elo_rating) %>% 
  distinct()

# Add win/loss boolean
plays.master.win_prob3 <- plays.master.win_prob2 %>% mutate(home_outcome = case_when(home_points > away_points ~ 1,
                                                           home_points < away_points ~ 0,
                                                           TRUE ~ 0.5))

# Summarise by home_deficit, time left in game (rounded to 10 seconds), and home_elo
win_prob_in_game <- plays.master.win_prob3 %>% 
  mutate(rnded_time_remaining = round(clock_in_seconds, -1)) %>% 
  mutate(elo_deficit = round(home_elo - away_elo, -1)) %>% 
  group_by(home_score_lead_deficit, rnded_time_remaining, elo_deficit) %>% 
  summarise(home_result = mean(home_outcome), count = n())
# Consider bucketing these further for leads, etc. or rounding elo by maybe 50 instead of 10s right now, seconds by 30 instead of 10, etc.

## EPA WIP
# The thinking behind this model is to look at each drive per offense, and get the min offense score
# at the start of the drive, and compare it to the max offense score of that drive to find out if they scored.
# We will also look at the defense score difference to see if there was a turnover for a TD.
# 
# We will also look at the following drive in the case that it wasn't a scoring drive to see if the offense scored
# on the following drive.
# 
# From there, we will get the average score for each down, distance, and yards to the goalline.

# Get points scored on each drive
plays.points.temp <- plays.master %>% 
  filter(play_specifics != "Uncategorized") %>% 
  group_by(drive_id, offense) %>% # Looks like some drive ids are for both teams drive.. so grouped by offense and drive, but even then offenses and defenses are both scoring on the same exact drive
  mutate(score_offense = max(offense_score) - min(offense_score), score_defense = -(max(defense_score) - min(defense_score))) #%>% # defense score is made negative
  # ungroup() %>% 
  # mutate(next_drive_offense = lead(max(offense_score), n = 1L, order_by = drive_id) - lead(min(offense_score), n = 1L, order_by = drive_id), next_drive_defense = -(lead(max(defense_score), n = 1L, order_by = drive_id) - lead(min(defense_score), n = 1L, order_by = drive_id)))

plays.master %>% 
  group_by(down, distance, yards_to_goal) %>% 
  mutate(epa_offense = )
  summarise(mean(
    case_when(scoring == T ~ case_when) # If it's scoring drive, get the max offense_score - min offense_score OR max defense_score - min defense_score for that drive
  ))
  
## 2020 Plays
  plays.master.2020 = data.frame()
  for (j in 2020) {
    for (i in 1:15) {
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
        plays.master.2020 = rbind(plays.master.2020, plays, make.row.names=TRUE)
      }
    }
  }
  rm(clockcolumns, pass_rows, plays)

# Bring in team abbreviations
team_url <- "https://api.collegefootballdata.com/teams"
team_info <- fromJSON(getURL(team_url))
  
# Top plays in terms of EPA/PPA for the week (absolute value)
wk <- 1

plays.master.2020 <- left_join(plays.master.2020, team_info, by = c("offense" = "school"), suffix = c("", ".offense"))
plays.master.2020 <- plays.master.2020 %>% select(-last_col(c(0:7))) %>% rename(offense_abb = abbreviation)
plays.master.2020 <- left_join(plays.master.2020, team_info, by = c("defense" = "school"), suffix = c("", ".defense"))
plays.master.2020 <- plays.master.2020 %>% select(-last_col(c(0:7))) %>% rename(defense_abb = abbreviation)

top_plays_ppa <- plays.master.2020 %>% 
  filter(week == wk) %>% 
  mutate(ppa = as.double(ppa)) %>% 
  mutate(ppa_abs = abs(ppa)) %>% 
  arrange(desc(ppa_abs)) %>% 
  select(ppa, offense_score, defense_score, ppa_abs, play_text, play_type, home, away, offense, defense, offense_abb, defense_abb, down, distance, yards_to_goal, minutes, seconds, period) %>% 
  mutate(matchup = paste0(away, " @ ", home), 
         situation = as.character(paste0(as.character(down), case_when(down == 1 ~ "st",
                                                                       down == 2 ~ "nd",
                                                                       down == 3 ~ "rd",
                                                                       down == 4 ~ "th",
                                                                       TRUE ~ ""), " and ", as.character(distance), " from the ", as.character(case_when(yards_to_goal > 50 ~ offense,
                                                                             yards_to_goal < 50 ~ defense,
                                                                             yards_to_goal == 50 ~ "",
                                                                             TRUE ~ "")), " ", as.character(case_when(yards_to_goal > 50 ~ 100L - yards_to_goal,
                                                                                                        yards_to_goal < 50 ~ yards_to_goal,
                                                                                                        yards_to_goal == 50 ~ yards_to_goal,
                                                                                                        TRUE ~ yards_to_goal), " Yard Line"))),
         gametime = paste0(minutes, ":", seconds, " ", period, "Q"),
         score = paste0(offense_abb, " ", offense_score, " - ", defense_score, " ", defense_abb))

top_plays_ppa_tbl <- top_plays_ppa %>% 
  select(ppa_abs, ppa, play_text, matchup, situation, gametime, score) %>% 
  slice_max(n = 10, order_by = ppa_abs) %>% 
  select(-ppa_abs) %>% 
  gt() %>% 
  tab_header(title = paste0("2020 Week ", wk, " Top Plays by EPA"),
             subtitle = "These were the biggest plays of the week on offense and defense") %>% 
  cols_label(ppa = "EPA", play_text = "Description", matchup = "Matchup", situation = "Situation", gametime = "Time", score = "Score (Off - Def)") %>% 
  fmt_number(vars(ppa), decimals = 2, use_seps = FALSE) %>% 
  data_color(columns = vars(ppa), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data PPA")

gtsave(data = top_plays_ppa_tbl, 
       filename = paste0("top_plays_ppa_tbl_", wk, "_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")
