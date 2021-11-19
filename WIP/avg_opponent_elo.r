quick_data <- get_games(2021,2021, 1,11)
quick_elo <- get_elo()

quick_elo <- quick_elo %>% 
  group_by(team) %>% 
  mutate(join_date = lead(date, n = 1L, order_by = date))



gt_result <- left_join(quick_data %>% 
  mutate(start_date = as_datetime(start_date),
         home_result = if_else(home_points > away_points, 1, 0)) %>% 
  left_join(quick_elo, by = c("home_team" = "team", "start_date" = "join_date")) %>% 
  left_join(quick_elo, by = c("away_team" = "team", "start_date" = "join_date"),
            suffix = c("_home", "_away")) %>% 
  group_by(home_team) %>% summarise(opp_elo = mean(elo_rating_away),
                                    n = n(),
                                    wins = sum(home_result)) %>% 
  arrange(desc(opp_elo)) %>% 
  mutate(total = opp_elo * n)
,
quick_data %>% 
  mutate(start_date = as_datetime(start_date),
         away_result = if_else(away_points > home_points, 1, 0)) %>% 
  left_join(quick_elo, by = c("home_team" = "team", "start_date" = "join_date")) %>% 
  left_join(quick_elo, by = c("away_team" = "team", "start_date" = "join_date"),
            suffix = c("_home", "_away")) %>% 
  group_by(away_team) %>% summarise(opp_elo = mean(elo_rating_away),
                                    n = n(),
                                    wins = sum(away_result)) %>% 
  arrange(desc(opp_elo)) %>% 
  mutate(total = opp_elo * n)
,
by = c("home_team" = "away_team")) %>% 
  mutate(total = total.x + total.y,
         avg = total / (n.x + n.y),
         wins = wins.x + wins.y) %>% 
  select(total, avg, home_team, wins) %>% 
  arrange(desc(avg)) %>% 
  select(home_team, wins, avg) %>% 
  filter(wins >= 9) %>% 
  gt() %>% 
  gt::data_color(columns = avg,
                 colors = c("red", "white", "blue"),
                 alpha = .5)

gtsave(gt_result, "R Plots/opp_avg_elo.png")
