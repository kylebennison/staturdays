
# Vegas Implied vs. Actual ------------------------------------------------

source("Production/source_everything.r")

betting_url <- paste0("https://api.collegefootballdata.com/lines?year=", j, "&")
full_url_betting <- paste0(betting_url, "week=", as.character(if_else(
  {upcoming.games %>% 
      filter(week == week_of_upcoming_games) %>% 
      pull(season_type) %>% 
      unique()} == "postseason", 
  "1&seasonType=postseason", 
  as.character(week_of_upcoming_games))))

betting.master <- get_anything("https://api.collegefootballdata.com/lines", key = my_key)

betting.master <- as_tibble(betting.master)
betting.master <- unnest(betting.master, cols = c(lines))

# Need to summarise lines for teams with multiple lines
betting_consensus <- betting.master %>% mutate(spread = as.double(spread)) %>%
  group_by(id, homeTeam, awayTeam, homeScore, awayScore) %>% 
  summarise(spread = mean(spread),
            overUnder = mean(as.double(overUnder)),
            homeMoneyline = mean(homeMoneyline, na.rm = TRUE),
            awayMoneyline = mean(awayMoneyline, na.rm = TRUE))

# Add implied odds

implied_odds <- betting_consensus %>% 
mutate(home_implied_odds = case_when(str_detect(homeMoneyline, "-") == TRUE ~ abs(as.integer(homeMoneyline))/(abs(as.integer(homeMoneyline)) + 100),
                                     str_detect(homeMoneyline, "-") == FALSE ~ 100/(abs(as.integer(homeMoneyline))+100),
                                     TRUE ~ 0),
       away_implied_odds = case_when(str_detect(awayMoneyline, "-") == TRUE ~ abs(as.integer(awayMoneyline))/(abs(as.integer(awayMoneyline)) + 100),
                                     str_detect(awayMoneyline, "-") == FALSE ~ 100/(abs(as.integer(awayMoneyline))+100),
                                     TRUE ~ 0)) %>% 
  filter(is.na(homeMoneyline) == FALSE)

# Brier
brier <- implied_odds %>% 
  filter(is.na(homeScore) == FALSE) %>% 
  mutate(home_result = if_else(homeScore > awayScore, 1L, 0L)) %>% 
  mutate(error = (home_result - home_implied_odds)^2) %>% 
  ungroup() %>% 
  summarise(mean(error)) %>% 
  round(digits = 2) %>% as.character()

# Graph

implied_odds %>% 
  filter(is.na(homeScore) == FALSE) %>% 
  mutate(home_result = if_else(homeScore > awayScore, 1L, 0L)) %>% 
  mutate(implied_bucket = round(home_implied_odds, digits = 1)) %>% 
  group_by(implied_bucket) %>% 
  summarise(avg_home_result = mean(home_result), n_games = n()) %>% 
  ggplot(aes(x = avg_home_result, y = implied_bucket)) +
  geom_point(aes(size = n_games), alpha = .5, color = staturdays_colors("light_blue")) +
  geom_abline(linetype = 2, color = staturdays_colors("orange")) + 
  staturdays_theme +
  labs(x = "Actual Win Rate",
       y = "Implied Win Probability",
       title = "Sportsbook Home Moneylines and Actual Outcomes",
       subtitle = paste0("Brier of ", brier),
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data",
       size = "# of Games") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  annotate(geom = "label",
           x = .75, y = .25,
           label = "Vegas Underconfident") +
  annotate(geom = "label",
           x = .25, y = .75,
           label = "Vegas Overconfident")

ggsave(filename = paste0(lubridate::today(),
                         "_",
                         "vegas_vs_actual",
                         ".jpg"),
       path = "R Plots/",
       plot = last_plot(),
       height = 200, width = 400,
       units = "mm",
       dpi = 300)
