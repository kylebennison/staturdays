# Evaluate elo vs. moneyline for 2021

# Run elo_weekly_plots_tables_v2.1.r first, then this.

betting_url <- paste0("https://api.collegefootballdata.com/lines?year=", j)
full_url_betting <- paste0(betting_url)

betting.master = data.frame()
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
           homeConference, awayConference, homeScore, awayScore,
           formattedSpread) %>% 
  summarise(consensus_spread = mean(spread, na.rm = TRUE),
            consensus_over_under = mean(overUnder, na.rm = TRUE),
            consensus_home_ml = mean(homeMoneyline, na.rm = TRUE),
            consensus_away_ml = mean(awayMoneyline, na.rm = TRUE)) %>% 
  filter(week < week_of_upcoming_games) %>% 
  rename(spread = consensus_spread,
         overUnder = consensus_over_under,
         homeMoneyline = consensus_home_ml,
         awayMoneyline = consensus_away_ml)

win_probs <- upcoming.games %>% 
  filter(week < week_of_upcoming_games) %>% 
  select(id, game_date, game_outcome_home, home_team,home_elo, home_pred_win_prob, home_conference, away_team, away_elo, away_pred_win_prob, away_conference) %>%
  arrange(desc(home_pred_win_prob))

win_probs_w_lines <- win_probs %>% 
  left_join(betting_consensus, by = "id")

# Join Lines and find any mismatches between Elo and the Lines
win_probs_w_lines <- win_probs_w_lines %>% 
  mutate(home_favorite = case_when(str_detect(win_probs_w_lines$formattedSpread, win_probs_w_lines$home_team) ~ T, # Search if home team is favored
                                   TRUE ~ F)) %>% 
  mutate(elo_different = case_when(is.na(spread) == TRUE ~ F, # Check if Elo agrees or disagrees
                                   (home_favorite == TRUE) & (home_pred_win_prob < 0.5) ~ T,
                                   (home_favorite == FALSE) & (home_pred_win_prob >= 0.5) ~ T,
                                   TRUE ~ F))

# Filter any games already played from win prob this week table (covers cases where teams play twice in one week (e.g. Week 0 and Week 1))
win_probs_w_lines <- win_probs_w_lines %>% 
  filter(game_date <= lubridate::now()) %>% 
  arrange(game_date, home_team) %>% # arrange by date first, then home team
  mutate(game_date = lubridate::with_tz(game_date, "America/New_York")) %>% # convert to Eastern timezone
  mutate(across(.cols = formattedSpread, .fns = ~ replace_na(.x, ""))) %>%  # replace NAs in spread with blanks for the table
  mutate(homeMoneyline = if_else(homeMoneyline < 0, 
                                 as.character(homeMoneyline),
                                 paste0("+", homeMoneyline)),
         awayMoneyline = if_else(awayMoneyline < 0, 
                                 as.character(awayMoneyline),
                                 paste0("+", awayMoneyline)))

win_probs_moneyline_1 <- win_probs_w_lines %>% 
  mutate(home_implied_odds = case_when(str_detect(homeMoneyline, "-") == TRUE ~ abs(as.integer(homeMoneyline))/(abs(as.integer(homeMoneyline)) + 100),
                                       str_detect(homeMoneyline, "-") == FALSE ~ 100/(abs(as.integer(homeMoneyline))+100),
                                       TRUE ~ 0),
         away_implied_odds = case_when(str_detect(awayMoneyline, "-") == TRUE ~ abs(as.integer(awayMoneyline))/(abs(as.integer(awayMoneyline)) + 100),
                                       str_detect(awayMoneyline, "-") == FALSE ~ 100/(abs(as.integer(awayMoneyline))+100),
                                       TRUE ~ 0)) %>% 
  filter(is.na(homeMoneyline) == FALSE)

# Calc expected value on Elo bets

expected_value_tbl <- win_probs_moneyline_1 %>% 
  mutate(home_diff = home_pred_win_prob - home_implied_odds,
         away_diff = away_pred_win_prob - away_implied_odds,
         home_win_10d_bet = if_else(str_detect(homeMoneyline, "-") == TRUE,
                                    (abs(as.double(homeMoneyline) - 100)) / (abs(as.double(homeMoneyline)) / 10),
                                    (as.double(homeMoneyline) + 100) / 10),
         away_win_10d_bet = if_else(str_detect(awayMoneyline, "-") == TRUE,
                                    (abs(as.double(awayMoneyline) - 100)) / (abs(as.double(awayMoneyline)) / 10),
                                    (as.double(awayMoneyline) + 100) / 10),
         home_exp_value = ((home_win_10d_bet - 10) * home_pred_win_prob) - (10 * (1-home_pred_win_prob)),
         away_exp_value = ((away_win_10d_bet - 10) * away_pred_win_prob) - (10 * (1-away_pred_win_prob)))

# Calc profit/loss for the year

expected_value_tbl %>% 
  filter(home_exp_value > 10 | away_exp_value > 10) %>% 
  mutate(profit = case_when(game_outcome_home == 1 & home_exp_value > 0 ~ home_win_10d_bet - 10,
                            game_outcome_home == 0 & home_exp_value > 0 ~ -10,
                            game_outcome_home == 1 & away_exp_value > 0 ~ -10,
                            game_outcome_home == 0 & away_exp_value > 0 ~ away_win_10d_bet - 10,
                            TRUE ~ -Inf)) %>% 
  ungroup() %>% 
  summarise(p_and_l = sum(profit), n = n())
