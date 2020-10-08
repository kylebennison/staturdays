{# Get CFB Data API for Play by Play - Use to generate expected points added (EPA)
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
}

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
rm(clockcolumns, plays)

# Bring in team abbreviations
team_url <- "https://api.collegefootballdata.com/teams"
team_info <- fromJSON(getURL(team_url))

# Top plays in terms of EPA/PPA for the week (absolute value)
wk <- plays.master.2020$week %>% max()

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
