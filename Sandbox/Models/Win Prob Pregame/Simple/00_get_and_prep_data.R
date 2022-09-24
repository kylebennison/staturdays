

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
plays <- get_plays(1, 15, train_start, train_end)

# TODO: Save plays data as an RDS file, add to gitignore if it's too large for github

# Get AP rankings
# rankings <- get_anything(url = "https://api.collegefootballdata.com/rankings",
                         start_year = train_start,
                         end_year = train_end)

# saveRDS(rankings, "Data/rankings_raw_2001-2021")

rankings <- readRDS("Data/rankings_raw_2001-2021")

# Get recruiting class rankings
recruiting <- get_anything(url = "https://api.collegefootballdata.com/recruiting/teams",
                           start_year = train_start,
                           end_year = train_end,
                           key = my_key())

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
#                          start_year = train_start,
#                          end_year = train_end,
#                          key = my_key())

# saveRDS(coaching, "Data/coaching_raw_2001-2021")

coaching <- readRDS("Data/coaching_raw_2001-2021")

# Get coaching data as far back as it goes
# coaching_history <- get_anything(url = "https://api.collegefootballdata.com/coaches",
#                                  start_year = 1980,
#                                  end_year = train_end,
#                                  key = my_key())
# 
# saveRDS(coaching_history, "Data/coaching_history_raw_1980-2021")

coaching_history <- readRDS("Data/coaching_history_raw_1980-2021")

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
  mutate(yrs_with_team = year - lubridate::year(hire_date),
         join_year = year + 1L,
         coach = paste(first_name, last_name))

# Fix coaching datatype discrepency
coaching_history <- coaching_history %>%
  mutate(seasons = map(seasons,
                       ~ mutate(.x,
                                sp_overall = as.double(sp_overall),
                                sp_defense = as.double(sp_defense),
                                sp_offense = as.double(sp_offense)))) %>% 
  unnest(cols = seasons)

# group by coach and get years experience
coaching_history <- coaching_history %>% 
  mutate(coach = paste(first_name, last_name)) %>% 
  group_by(coach) %>% 
  mutate(n_total_seasons = year - min(year)) %>% 
  select(coach, year, n_total_seasons)

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
p2 <- plays %>% 
  add_success()

games <- games %>% 
  mutate(id = as.character(id))

p2_5 <- p2 %>%
  mutate(game_id = as.character(str_sub(game_id, start = 4L))) %>% 
  left_join(games, by = c("game_id" = "id")) %>%  # Get game start_time info for joining to elo
  mutate(start_date = lubridate::as_datetime(start_date))

### across(everything(), .fns = sum)

# TODO: Just sum up stats, don't calculate rates, and then calculate rates from moving-averages 
# in the next step.

p3 <- p2_5 %>%
  group_by(game_id, offense) %>%
  summarise(
    completion_rate = sum(pass_completion, na.rm = TRUE) / sum(pass_attempt, na.rm = TRUE),
    pass_touchdown_rate = sum(pass_touchdown, na.rm = TRUE) /
      sum(pass_attempt, na.rm = TRUE),
    interception_rate = sum(pass_intercepted, na.rm = TRUE) /
      sum(pass_attempt, na.rm = TRUE),
    sack_rate = sum(passer_sacked, na.rm = TRUE) / n(),
    success_rate = sum(success, na.rm = TRUE) / n(),
    pass_rate = sum(pass_attempt, na.rm = TRUE) / sum(
      sum(pass_attempt, na.rm = TRUE),
      sum(rush_attempt, na.rm = TRUE),
      na.rm = TRUE
    ),
    rush_touchdown_rate = sum(rush_touchdown, na.rm = TRUE) /
      sum(rush_attempt, na.rm = TRUE),
    rush_rate = sum(rush_attempt, na.rm = TRUE) / sum(
      sum(pass_attempt, na.rm = TRUE),
      sum(rush_attempt, na.rm = TRUE),
      na.rm = TRUE
    ),
    pass_fumble_rate = sum(pass_fumbled, na.rm = TRUE) /
      sum(
        sum(pass_attempt, na.rm = TRUE),
        sum(passer_sacked, na.rm = TRUE),
        na.rm = TRUE
      ),
    rush_fumble_rate = sum(rush_fumbled, na.rm = TRUE) /
      sum(rush_attempt, na.rm = TRUE),
    total_touchdown_rate = sum(
      sum(pass_touchdown, na.rm = TRUE),
      sum(rush_touchdown, na.rm = TRUE)
    ) / sum(pass_attempt, rush_attempt, na.rm = TRUE),
    avg_ppa = mean(ppa, na.rm = TRUE),
    n = n(),
    points_for = max(offense_score),
    points_against = max(defense_score),
    home_spread = points_for - points_against,
    won_game = if_else(max(offense_score) > max(defense_score), 1, 0),
    opponent_elo = max(defense_elo)
  )

rm("p2", "p2_5")

# Get moving average of last 2, 4, 6, and 8 previous games (note, this currently takes averages of averages on completion rate and other stats)
for (lookback in c(2,4,6,8)){
  p3 <- p3 %>% 
    arrange(game_id) %>% 
    group_by(offense) %>% 
    mutate(across(.cols = -game_id,
                  .fns = ~ zoo::rollapply(.x,
                                          width = list(c(seq(-lookback, -1, 1))),
                                          FUN = mean,
                                          na.rm = TRUE,
                                          fill = NA
                                          ),
                  .names = "{.col}_ma_{lookback}gms"
                  )
           ) %>% 
    select(offense, game_id, contains("_ma_"))
}

p4 <- p3

# TODO Join in result of previous meeting with that team (score margin, win/loss result, total points, yards, etc.)

rm(list = c("p3"))

df_joined <- df_joined %>% 
  mutate(id = as.character(id)) %>% 
  left_join(p4, by = c("home_team" = "offense",
                       "id" = "game_id")) %>% 
  left_join(p4, by = c("away_team" = "offense",
                       "id" = "game_id"),
            suffix = c("_home", "_away"))

# May want to add elo to give some idea of the quality of their opponents in wins/losses
elo <- get_elo(2014,2019)

# Mutate week for joining so it's pre-game elo
elo <- elo %>% 
  group_by(team) %>% 
  mutate(join_date = lead(date, n = 1L, order_by = date)) # get next week's game date.

df_joined <- df_joined %>% 
  mutate(start_date = lubridate::as_datetime(start_date)) %>% 
  left_join(elo, by = c("home_team" = "team",
                        "start_date" = "join_date",
                        "season" = "season")) %>% 
  left_join(elo, by = c("away_team" = "team",
                        "start_date" = "join_date",
                        "season" = "season"),
            suffix = c("_home", "_away"))

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

df_joined <- df_joined %>% 
  mutate(home_elo_adv = elo_rating_home + if_else(neutral_site == TRUE, 0, 55) - elo_rating_away,
         home_elo_wp = calc_expected_score(elo_rating_home + if_else(neutral_site == TRUE, 0, 55),
                                           elo_rating_away))

# Join in coaching and coaching_history tables
df_joined <- df_joined %>% 
  left_join(coaching, by = c("season" = "join_year",
                             "home_team" = "school")) %>% 
  left_join(coaching, by = c("season" = "join_year",
                             "away_team" = "school"),
            suffix = c("_home", "_away")) %>% 
  left_join(coaching_history, by = c("coach_home" = "coach",
                                     "season" = "year")) %>% 
  left_join(coaching_history, by = c("coach_away" = "coach",
                                     "season" = "year"),
            suffix = c("_home", "_away"))

# Cross-validate xgboost model --------------------------------------------
# Prep data
prepped_table <- df_joined %>% 
  filter(season != 2014) %>% # remove 2014 since it doesn't have 2013 data
  ungroup() %>% 
  mutate(response_home_win = if_else(home_points > away_points, 1, 0),
         response_total_points = home_points + away_points,
         response_home_spread = away_points - home_points)

# NOTE: right now including pre-game line and spread as predictors, but in the
# future may want to omit

saveRDS(prepped_table, "Data/betting_prepped.rds")
