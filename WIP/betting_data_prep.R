

# Betting - Data Prep -----------------------------------------------------

# Libraries and Themes ----------------------------------------------------

library(scales)
library(tidyverse)
library(jsonlite)
library(stringr)
library(lubridate)
library(data.table)

# API wrapper
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
# Staturdays themes + colors
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")
# Get everything important
source("Production/source_everything.r")

# Data --------------------------------------------------------------------
train_start <- 2014
train_end <- 2019

# Get game results
games <- get_games(train_start, train_end)

# Get team records
records <- get_anything(url = "https://api.collegefootballdata.com/records",
                        start_year = train_start,
                        end_year = train_end,
                        key = my_key)

# Get play-by-play
plays <- fread("Data/plays_2014_2020.csv")

# Get pre-game lines
lines <- get_anything(url = "https://api.collegefootballdata.com/lines",
                      start_year = train_start,
                      end_year = train_end,
                      key = my_key)

lines <- lines %>% unnest(cols = lines)

# Get talent ratings
talent <- get_anything(url = "https://api.collegefootballdata.com/talent",
                       start_year = train_start,
                       end_year = train_end,
                       key = my_key)

# Get AP rankings
rankings <- get_anything(url = "https://api.collegefootballdata.com/rankings",
                         start_year = train_start,
                         end_year = train_end,
                         key = my_key)

# Get returning starters
returning <- get_anything(url = "https://api.collegefootballdata.com/player/returning",
                          start_year = train_start,
                          end_year = train_end,
                          key = my_key)

# Get recruiting class rankings
recruiting <- get_anything(url = "https://api.collegefootballdata.com/recruiting/teams",
                           start_year = train_start,
                           end_year = train_end,
                           key = my_key)

# ppa <- get_anything(url = "https://api.collegefootballdata.com/ppa/players/season",
# start_year = train_start,
# end_year = train_end,
# key = my_key)

# Get advanced stats
stats_advanced <- get_anything(url = "https://api.collegefootballdata.com/stats/season/advanced",
                               start_year = train_start,
                               end_year = train_end,
                               key = my_key)

# Get coaching records
coaching <- get_anything(url = "https://api.collegefootballdata.com/coaches",
                         start_year = train_start,
                         end_year = train_end,
                         key = my_key)

# Get coaching data as far back as it goes
coaching_history <- get_anything(url = "https://api.collegefootballdata.com/coaches",
                                 start_year = 1980,
                                 end_year = 2019,
                                 key = my_key)

# Clean any data ----------------------------------------------------------

# Unnest columns
# rankings <- rankings %>% unnest(cols = polls) %>% unnest(cols = ranks)

# Unnest coaching
coaching <- coaching %>% 
  unnest(cols = seasons)

# Mutate years with team
coaching <- coaching %>% 
  mutate(yrs_with_team = year - lubridate::year(hire_date),
         join_year = year + 1L,
         coach = paste(first_name, last_name))

# group by coach and get years experience
coaching_history <- coaching_history %>% 
  unnest(cols = seasons) %>% 
  mutate(coach = paste(first_name, last_name)) %>% 
  group_by(coach) %>% 
  mutate(n_total_seasons = year - min(year)) %>% 
  select(coach, year, n_total_seasons)

# Summarise a single line per game
lines_tmp <- lines %>% 
  mutate(spread = as.double(spread),
         overUnder = as.integer(overUnder)) %>% 
  group_by(id, season, seasonType, week, homeTeam, awayTeam,
           homeScore, awayScore) %>% 
  summarise(avg_spread = mean(spread),
            avg_over_under = mean(overUnder, na.rm = T))

lines <- lines_tmp

rm(lines_tmp)


# Prep for joining ---------

# Join last years stats to this year's games for predictions
# ppa_prep <- ppa %>% 
#   mutate(join_year = season + 1L)

records_prep <- records %>% 
  mutate(join_year = year + 1L)

stats_prep <- stats_advanced %>% 
  mutate(join_year = season + 1L)

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

games_lines <- games %>% 
  inner_join(lines, by = 
               c("id", 
                 "home_team" = "homeTeam", 
                 "away_team" = "awayTeam",
                 "home_points" = "homeScore",
                 "away_points" = "awayScore"))

games_lines_stats <- games_lines %>% 
  left_join(stats_prep, by = c("home_team" = "team",
                               "season.x" = "join_year")) %>% 
  left_join(stats_prep, by = c("away_team" = "team",
                               "season.x" = "join_year"),
            suffix = c("_home", "_away"))

#### PPA is by player, not team. Omit.
games_lines_stats_records <- games_lines_stats %>% 
  left_join(records_prep, by = c("home_team" = "team",
                                 "season.x" = "join_year")) %>% 
  left_join(records_prep, by = c("away_team" = "team",
                                 "season.x" = "join_year"),
            suffix = c("_home", "_away"))

# These three can join with the same season as games
df_joined_7_tables <- games_lines_stats_records %>% 
  left_join(recruiting, by = c("home_team" = "team",
                               "season.x" = "year")) %>% 
  left_join(recruiting, by = c("away_team" = "team",
                               "season.x" = "year"),
            suffix = c("_home", "_away")) %>% 
  left_join(returning, by = c("home_team" = "team",
                              "season.x" = "season")) %>% 
  left_join(returning, by = c("away_team" = "team",
                              "season.x" = "season"),
            suffix = c("_home", "_away")) %>% 
  left_join(talent, by = c("home_team" = "school",
                           "season.x" = "year")) %>% 
  left_join(talent, by = c("away_team" = "school",
                           "season.x" = "year"),
            suffix = c("_home", "_away"))

# Summarise plays into success rate, ppa/game over rolling past 4 games and join
# Note, currently grouping by offense
p2 <- plays %>% 
  filter(year != 2020) %>% 
  add_success()

# Add elo so we can get moving average elo of opponent
elo <- get_elo(2014, 2019)

# Mutate week for joining so it's pre-game elo
elo <- elo %>% 
  group_by(team) %>% 
  mutate(join_date = lead(date, n = 1L, order_by = date)) # get next week's game date.

games2 <- games %>% 
  mutate(id = as.character(id))

p2_5 <- p2 %>%
  mutate(game_id = as.character(str_sub(game_id, start = 4L))) %>% 
  left_join(games2, by = c("game_id" = "id")) %>%  # Get game start_time info for joining to elo
  mutate(start_date = lubridate::as_datetime(start_date)) %>%
  left_join(elo,
            by = c(
              "home_team" = "team",
              "start_date" = "join_date",
              "season" = "season"
            )) %>%
  left_join(
    elo,
    by = c(
      "away_team" = "team",
      "start_date" = "join_date",
      "season" = "season"
    ),
    suffix = c("_home", "_away")
  ) %>%
  mutate(
    offense_elo = if_else(offense == home_team, elo_rating_home, elo_rating_away),
    defense_elo = if_else(defense == home_team, elo_rating_home, elo_rating_away)
  )

### across(everything(), .fns = sum)

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

# Get moving average of 4 previous games (note, this currently takes averages of averages on completion rate and other stats)
p4 <- p3 %>% 
  arrange(game_id) %>% 
  group_by(offense) %>% 
  mutate(across(.cols = -game_id, 
                .fns = ~ zoo::rollapply(.x,
                                        width = list(c(-4, -3, -2, -1)),
                                        FUN = mean,
                                        na.rm = TRUE,
                                        fill = NA),
                .names = "{.col}_ma_4")) %>% 
  select(offense, game_id, contains("_ma_4")) # Filter out stats from the current game since that should be hidden from the model

rm(list = c("p2", "p3"))

df_joined_8_tables <- df_joined_7_tables %>% 
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

df_joined_9_tables <- df_joined_8_tables %>% 
  mutate(start_date = lubridate::as_datetime(start_date)) %>% 
  left_join(elo, by = c("home_team" = "team",
                        "start_date" = "join_date",
                        "season.x" = "season")) %>% 
  left_join(elo, by = c("away_team" = "team",
                        "start_date" = "join_date",
                        "season.x" = "season"),
            suffix = c("_home", "_away"))

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

df_joined_9_tables_w_elo <- df_joined_9_tables %>% 
  mutate(home_elo_adv = elo_rating_home + if_else(neutral_site == TRUE, 0, 55) - elo_rating_away,
         home_elo_wp = calc_expected_score(elo_rating_home + if_else(neutral_site == TRUE, 0, 55),
                                           elo_rating_away))

# Join in coaching and coaching_history tables
df_joined <- df_joined_9_tables_w_elo %>% 
  left_join(coaching, by = c("season.x" = "join_year",
                             "home_team" = "school")) %>% 
  left_join(coaching, by = c("season.x" = "join_year",
                             "away_team" = "school"),
            suffix = c("_home", "_away")) %>% 
  left_join(coaching_history, by = c("coach_home" = "coach",
                                     "season.x" = "year")) %>% 
  left_join(coaching_history, by = c("coach_away" = "coach",
                                     "season.x" = "year"),
            suffix = c("_home", "_away"))

# Cross-validate xgboost model --------------------------------------------
# Prep data
prepped_table <- df_joined %>% 
  filter(season.x != 2014) %>% # remove 2014 since it doesn't have 2013 data
  ungroup() %>% 
  mutate(response_home_win = if_else(home_points > away_points, 1, 0),
         response_total_points = home_points + away_points,
         response_home_spread = away_points - home_points)

# NOTE: right now including pre-game line and spread as predictors, but in the
# future may want to omit
