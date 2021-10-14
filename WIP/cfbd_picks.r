# Predict upcoming game spreads
source("Production/source_everything.r")

model <- readRDS(file = "Production Models/elo_combo_spread_model.rds")

elo <- get_elo(2021, 2021)

games <- get_games(2021, 2021)

e2 <- elo %>% 
  group_by(team) %>% 
  slice_max(order_by = date, n = 1L)

week_of_games_just_played <- elo %>% 
  slice_max(order_by = date, n = 1L) %>% 
  pull(week) %>% 
  unique()

week_of_upcoming_games <- week_of_games_just_played + 1L

games_elo <- games %>% 
  filter(week == week_of_upcoming_games) %>% 
  mutate(start_date = lubridate::as_datetime(start_date)) %>% 
  left_join(e2, by = c("home_team" = "team")) %>% 
  left_join(e2, by = c("away_team" = "team"),
            suffix = c("_home", "_away")) %>% 
  mutate(home_elo_adv = elo_rating_home + if_else(neutral_site == TRUE, 0 , 55) - elo_rating_away,
         alt_elo_adv = home_pregame_elo - away_pregame_elo)

predictions <- games_elo %>% 
  mutate(predicted = predict(model, newdata = games_elo)) %>% 
  select(id, home_team, away_team, predicted) %>% 
  rename(home = home_team,
         away = away_team)

data.table::fwrite(predictions, file = "Data/predictions.csv")
