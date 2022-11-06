

#' Goal of script is to get as much CFB data as possible for as far back as possible
#' to give the model ample data to train with

# Betting - Data Prep -----------------------------------------------------

# Libraries and Themes ----------------------------------------------------

library(scales)
library(tidyverse)
library(jsonlite)
library(stringr)
library(lubridate)
library(data.table)
library(statRdaysCFB)

# Data --------------------------------------------------------------------
train_start <- 2001
train_end <- 2021

# Get game results
# games <- get_games(train_start, train_end)
# saveRDS(games, "Data/games_raw_2001-2021")
games <- readRDS("Data/games_raw_2001-2021")

# Get team records
# records <- get_anything(url = "https://api.collegefootballdata.com/records",
#                         start_year = train_start,
#                         end_year = train_end,
#                         key = my_key())
# 
# saveRDS(records, "Data/records_raw_2001-2021")

records <- readRDS("Data/records_raw_2001-2021")

# Plays
# plays <- get_plays(1, 15, train_start, train_end)
# 
# saveRDS(plays, "Data/plays_raw_2001-2021")

plays <- readRDS("Data/plays_raw_2001-2021")


# Get AP rankings
# rankings <- get_anything(url = "https://api.collegefootballdata.com/rankings",
#                         start_year = train_start,
#                         end_year = train_end)
# 
# saveRDS(rankings, "Data/rankings_raw_2001-2021")

rankings <- readRDS("Data/rankings_raw_2001-2021")

# Get recruiting class rankings
# recruiting <- get_anything(url = "https://api.collegefootballdata.com/recruiting/teams",
#                            start_year = train_start,
#                            end_year = train_end,
#                            key = my_key())
# 
# saveRDS(recruiting, "Data/recruiting_raw_2001-2021")

recruiting <- readRDS("Data/recruiting_raw_2001-2021")

# ppa <- get_anything(url = "https://api.collegefootballdata.com/ppa/players/season",
# start_year = train_start,
# end_year = train_end,
# key = my_key)

# Get advanced stats
# stats_advanced <- get_anything(url = "https://api.collegefootballdata.com/stats/season/advanced",
#                                start_year = train_start,
#                                end_year = train_end,
#                                key = my_key())
# 
# saveRDS(recruiting, "Data/stats_advanced_raw_2001-2021")

stats_advanced <- readRDS("Data/stats_advanced_raw_2001-2021")

# Get coaching records
# coaching <- get_anything(url = "https://api.collegefootballdata.com/coaches",
#                         start_year = 1980,
#                         end_year = train_end,
#                         key = my_key())
# 
# saveRDS(coaching, "Data/coaching_raw_1980-2021")

coaching <- readRDS("Data/coaching_raw_1980-2021")

# Clean any data ----------------------------------------------------------

# Unnest columns
rankings <- rankings %>% unnest(cols = polls) %>% unnest(cols = ranks)

# Unnest coaching
coaching <- coaching %>%
  mutate(seasons = map(seasons,
                       ~ mutate(.x,
                                sp_overall = as.double(sp_overall),
                                sp_defense = as.double(sp_defense),
                                sp_offense = as.double(sp_offense)))) %>% 
  unnest(cols = seasons)

# Mutate years with team
coaching <- coaching %>% 
  group_by(first_name, last_name, school) %>% 
  mutate(hire_date = as_datetime(hire_date)) %>% 
  mutate(hire_date = if_else(is.na(hire_date), as_datetime(glue::glue(min(year), "-01-01")), hire_date)) %>% 
  mutate(yrs_with_team = year - lubridate::year(hire_date),
         join_year = year + 1L,
         coach = paste(first_name, last_name)) %>% 
  group_by(coach) %>% 
  mutate(n_total_seasons = year - min(year))

# Prep for joining ---------

records_prep <- records %>% 
  mutate(join_year = year + 1L)

stats_prep <- stats_advanced %>% 
  mutate(join_year = year + 1L)

# Need to confirm that the rankings come out after that week is played.
# i.e. Week 1 rankings should be used to predict Week 2 games.
# If they come out before the week, they should be used to predict the same week
# upon review this data doesn't look reliable so omitting
# rankings_prep <- rankings %>% 
#   filter(poll %in% c("AP Top 25", 
#                      "Playoff Committee Rankings")) %>% 
#   mutate(consensus_rank == mean(rank, na.rm = TRUE),
#          join_week = week + 1L)
# Update: I will know based on how good the fit is with that variable. If it's after,
# there will be a near perfect correlation in the model.

# Build a giant table -----------------------------------------------------

df_joined <- games %>% 
  left_join(stats_prep, by = c("home_team" = "team",
                               "season" = "join_year")) %>% 
  left_join(stats_prep, by = c("away_team" = "team",
                               "season" = "join_year"),
            suffix = c("_home", "_away"))

#### PPA is by player, not team. Omit.
df_joined <- df_joined %>% 
  left_join(records_prep, by = c("home_team" = "team",
                                 "season" = "join_year")) %>% 
  left_join(records_prep, by = c("away_team" = "team",
                                 "season" = "join_year"),
            suffix = c("_home", "_away"))

# Summarise plays into success rate, ppa/game over rolling past 4 games and join
# Note, currently grouping by offense
plays <- plays %>% 
  add_success()

plays <- plays %>%
  mutate(game_id = as.integer(game_id)) %>% 
  left_join(games, by = c("game_id" = "id")) %>%  # Get game start_time info for joining to elo
  mutate(start_date = lubridate::as_datetime(start_date))

### across(everything(), .fns = sum)

plays <- plays %>%
  mutate(offense_total_points = case_when(offense == home_team ~ home_points,
                                          TRUE ~ away_points),
         defense_total_points = case_when(offense == home_team ~ away_points,
                                          TRUE ~ home_points),
         ppa = as.double(ppa),
         defense_elo = case_when(offense == home_team ~ away_pregame_elo,
                                 TRUE ~ home_pregame_elo)) %>% 
  group_by(game_id, offense) %>%
  summarise(
    completions = sum(pass_completion, na.rm = TRUE),
    pass_attempts = sum(pass_attempt, na.rm = TRUE),
    pass_touchdowns = sum(pass_touchdown, na.rm = TRUE),
    interceptions = sum(pass_intercepted, na.rm = TRUE),
    sacks = sum(passer_sacked, na.rm = TRUE),
    success_plays = sum(success, na.rm = TRUE),
    success_rate = sum(success, na.rm = TRUE) / n(),
    rush_attempts = sum(rush_attempt, na.rm = TRUE),
    pass_rate = sum(pass_attempt, na.rm = TRUE) / sum(
      sum(pass_attempt, na.rm = TRUE),
      sum(rush_attempt, na.rm = TRUE),
      na.rm = TRUE
    ),
    rush_touchdowns = sum(rush_touchdown, na.rm = TRUE),
    pass_fumbles = sum(pass_fumbled, na.rm = TRUE),
    rush_fumbles = sum(rush_fumbled, na.rm = TRUE),
    total_touchdown_rate = sum(
      sum(pass_touchdown, na.rm = TRUE),
      sum(rush_touchdown, na.rm = TRUE)
    ) / sum(pass_attempt, rush_attempt, na.rm = TRUE),
    total_ppa = sum(ppa, na.rm = TRUE),
    avg_ppa = mean(ppa, na.rm = TRUE),
    n_plays = n(),
    points_for = max(offense_total_points),
    points_against = max(defense_total_points),
    home_spread = points_for - points_against,
    won_game = if_else(points_for > points_against, 1, 0),
    opponent_elo = max(defense_elo)
  )

# Get moving average of last 2, 4, 6, and 8 previous games (note, this currently takes averages of averages on completion rate and other stats)
#TODO: Confirm this works
for (lookback in c(2,4,6,8)){
  message("Working on lookback ", lookback)
  
  plays <- plays %>% 
    arrange(game_id) %>% 
    group_by(offense) %>% 
    select(game_id, where(is.numeric)) %>%
    mutate(across(.cols = -c(game_id, contains("_ma_")), # make sure not to calculate moving average on top of existing moving averages
                  .fns = ~ zoo::rollapply(.x,
                                          width = list(c(seq(-lookback, -1, 1))),
                                          FUN = mean, # TODO: should I average stats or sum them? Or maybe some should be added while others should be averaged?
                                          na.rm = TRUE,
                                          fill = NA
                  ),
                  .names = "{.col}_ma_{lookback}gms"
    )
    )
  
  # TODO: Copy the code above and sum stats as well
  
}

plays <- plays %>% 
  select(offense, game_id, contains("_ma_"))

# Get points and result of their last matchup between the two teams
# TODO: can expand upon this to get more stats from previous matchups eventually, yards, ppa, etc.
games <- games %>% 
  group_by(id) %>% 
  mutate(matchup = paste(min(home_team, away_team), max(home_team, away_team))) %>% # Team names in alpha-order
  group_by(matchup) %>% 
  mutate(previous_home = lag(home_team, n = 1L, order_by = start_date),
         previous_away = lag(away_team, n = 1L, order_by = start_date),
         previous_home_points = lag(home_points, n = 1L, order_by = start_date),
         previous_away_points = lag(away_points, n = 1L, order_by = start_date),
         home_last_meeting_points = if_else(home_team == previous_home, 
                                            previous_home_points,
                                            previous_away_points),
         away_last_meeting_points = if_else(home_team == previous_home, 
                                            previous_away_points,
                                            previous_home_points),
         home_last_meeting_margin = home_last_meeting_points - away_last_meeting_points,
         home_last_meeting_won = if_else(home_last_meeting_margin > 0, 1, 0),
         last_meeting_total_points = home_last_meeting_points + away_last_meeting_points
         ) %>% 
  select(-c(previous_home_points, previous_away_points, previous_home, previous_away))

# Join everything together

df_joined <- df_joined %>% 
  left_join(plays, by = c("home_team" = "offense",
                       "id" = "game_id")) %>% 
  left_join(plays, by = c("away_team" = "offense",
                       "id" = "game_id"),
            suffix = c("_home", "_away"))

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

df_joined <- df_joined %>% 
  mutate(home_elo_adv = home_pregame_elo + if_else(neutral_site == TRUE, 0, 55) - away_pregame_elo,
         home_elo_wp = calc_expected_score(home_pregame_elo + if_else(neutral_site == TRUE, 0, 55),
                                           away_pregame_elo))

# Join in coaching and coaching_history tables
df_joined <- df_joined %>% 
  left_join(coaching, by = c("season" = "join_year",
                             "home_team" = "school")) %>% 
  left_join(coaching, by = c("season" = "join_year",
                             "away_team" = "school"),
            suffix = c("_home", "_away"))

# Cross-validate xgboost model --------------------------------------------
# Prep data
prepped_table <- df_joined %>% 
  filter(season != train_start) %>% # remove first year since it doesn't have lookback data
  ungroup() %>% 
  mutate(response_home_win = if_else(home_points > away_points, 1, 0),
         response_total_points = home_points + away_points,
         response_home_spread = away_points - home_points)

# NOTE: right now including pre-game line and spread as predictors, but in the
# future may want to omit

saveRDS(prepped_table, "Data/betting_prepped.rds")
