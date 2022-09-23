# Predict upcoming game spreads
source("Production/source_everything.r")

model <- readRDS(file = "Production Models/elo_combo_spread_model_v2.rds")

elo <- get_elo(2021, 2021)

games <- get_games(2021, 2021)

e2 <- elo %>% 
  group_by(team) %>% 
  slice_max(order_by = date, n = 1L)

week_of_games_just_played <- elo %>% 
  slice_max(order_by = date, n = 1L) %>% 
  pull(week) %>% 
  unique()


week_of_upcoming_games <-
  games %>% 
  group_by(week) %>% 
  summarise(any_na = any(is.na(home_points))) %>% 
  filter(any_na == TRUE) %>% 
  pull(week) %>% 
  min()

games_elo <- games %>% 
  filter(week == week_of_upcoming_games) %>% 
  mutate(start_date = lubridate::as_datetime(start_date)) %>% 
  left_join(e2, by = c("home_team" = "team")) %>% 
  left_join(e2, by = c("away_team" = "team"),
            suffix = c("_home", "_away")) %>% 
  mutate(home_elo_adv = elo_rating_home + if_else(neutral_site == TRUE, 0 , 55) - elo_rating_away,
         alt_elo_adv = home_pregame_elo - away_pregame_elo)

betting.master = data.frame()
betting_url <-
  paste0("https://api.collegefootballdata.com/lines?year=", 2021)
full_url_betting <- paste0(betting_url)
full_url_betting_encoded <- URLencode(full_url_betting)
betting <- cfbd_api(full_url_betting_encoded, my_key)
betting <- as_tibble(betting)
betting <- unnest(betting, cols = c(lines))
betting.master = rbind(betting.master, betting)

# Need to summarise lines for teams with multiple lines
betting_consensus <- betting.master %>% 
  mutate(spread = as.double(spread),
         overUnder = as.double(overUnder)) %>%
  group_by(id, season, week, homeTeam, awayTeam,
           homeConference, awayConference, homeScore, awayScore) %>% 
  summarise(consensus_spread = mean(spread, na.rm = TRUE),
            consensus_over_under = mean(overUnder, na.rm = TRUE),
            consensus_home_ml = mean(homeMoneyline, na.rm = TRUE),
            consensus_away_ml = mean(awayMoneyline, na.rm = TRUE))

games_elo_spread <- games_elo %>% 
  left_join(betting_consensus, by = "id")

predictions <- games_elo_spread %>% 
  mutate(predicted = predict(model, newdata = games_elo_spread)) %>% 
  select(id, home_team, away_team, predicted) %>% 
  rename(home = home_team,
         away = away_team)

data.table::fwrite(predictions, file = "Data/predictions.csv")
