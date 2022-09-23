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
    "Fumble Return Touchdown",
    "Blocked Punt Touchdown",
    "Blocked Punt"
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


## 2020 Plays
plays.master.2020 = data.frame()
for (j in 2019) {
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

plays.master2 <- plays.master.2020 %>% 
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
             down == 2 & yards_gained >= 0.70 * distance ~ 1,
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

plays.master$play_type %>% unique()

# 4th and 2 or less conversion rate
two_pt_conv_rate <- plays.master %>% 
  group_by(offense) %>% 
  filter((down == 4 & distance <= 2) | yards_to_goal <= 2) %>% 
  summarise(success_rate = mean(success), count = n())

# Offensive FG/XP inside 2 yd line
make_rate_off <- plays.master %>% 
  filter(!(play_type %in% scrimmage_plays_turnover), !(play_type == "Punt")) %>% 
  filter((str_detect(play_type, "Field Goal?") & yards_to_goal <=2) | (str_detect(play_text, "Kick[)]") | (str_detect(play_text, "MISSED?") & scoring == T))) %>% 
  mutate(fg_made = if_else(str_detect(play_type, "Field Goal?") == T & scoring == T, 1, 0)) %>% 
  mutate(xp_made = if_else(str_detect(play_text, "Kick[)]") == T, 1, 0)) %>% 
  mutate(makes = fg_made + xp_made) %>% 
  group_by(offense) %>% 
  summarise(make_rate = mean(makes), make_sum = sum(makes), total = n())

# Defense XP made
make_rate_def <- plays.master %>% 
  filter((play_type %in% scrimmage_plays_turnover) | (play_type %in% c("Punt", "Field Goal Missed", "Blocked Field"))) %>% 
  filter((str_detect(play_text, "Kick[)]") | (str_detect(play_text, "MISSED?") & scoring == T))) %>% 
  mutate(xp_made = if_else(str_detect(play_text, "Kick[)]") == T, 1, 0)) %>% 
  mutate(makes = xp_made) %>% 
  group_by(offense) %>% 
  summarise(make_rate = mean(makes), make_sum = sum(makes), total = n())

# Combined Off/Def kicking
make_rate <- make_rate_off %>% rbind(make_rate_def) %>% 
  group_by(offense) %>% 
  summarise(make_sum = sum(make_sum), total = sum(total)) %>% 
  mutate(make_rate = make_sum / total)

# Joined table
go_for_two_tbl <- two_pt_conv_rate %>% left_join(make_rate)

# Plot
go_for_two_tbl %>% 
  ggplot(aes(x = make_rate, y = success_rate)) +
  geom_point() +
  geom_abline(intercept = 0, slope = .5)

# CFB-wide rates - note there are NAs rn for some reason for make_rate.
go_for_two_tbl %>% summarise(mean_succ = mean(success_rate), mean_make = mean(make_rate, na.rm = T))
