source("Production/source_everything.r")

elo <- get_elo(2000, 2021)

elo_ranked <- elo %>% 
  group_by(season, week) %>% 
  mutate(elo_ranking = rank(desc(elo_rating), ties.method = "min"))

elo_ready <- elo_ranked %>% 
  group_by(team) %>% 
  mutate(join_date = lead(date, n = 1L, order_by = date))

ap <- get_anything("https://api.collegefootballdata.com/rankings", start_year = 2000, end_year = 2021, key = my_key)

ap_top_25 <- ap %>% 
  rename(pre_week = week) %>% 
  unnest(polls) %>% 
  unnest(ranks) %>% 
  filter(poll %in% c("AP Top 25"))

cfp_top_25 <- ap %>% 
  rename(pre_week = week) %>% 
  unnest(polls) %>% 
  unnest(ranks) %>% 
  filter(poll %in% c("Playoff Committee Rankings"))

ap_joined <- ap_top_25 %>% 
  full_join(cfp_top_25, by = c("season", "pre_week", "seasonType", "school",
                               "conference"),
            suffix = c("_ap", "_cfp"))

games <- get_games(2000, 2021)

games_result <- games %>% 
  filter(is.na(home_points) == FALSE) %>% 
  mutate(home_result = if_else(home_points > away_points, 1L, 0L)) %>% 
  mutate(start_date = lubridate::as_datetime(start_date))

games_joined <- games_result %>% 
  left_join(elo_ready, by = c("start_date" = "join_date",
                              "home_team" = "team")) %>% 
  left_join(elo_ready, by = c("start_date" = "join_date",
                              "away_team" = "team"),
            suffix = c("_home", "_away")) %>% 
  left_join(ap_joined, by = c("season",
                              "week" = "pre_week",
                              "home_team" = "school")) %>% 
  left_join(ap_joined, by = c("season",
                              "week" = "pre_week",
                              "away_team" = "school"),
            suffix = c("_home", "_away"))

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

games_ready <- games_joined %>%
  filter((rank_ap_home <= 25 &
            rank_ap_away <= 25),
         season_type == "regular"
  ) %>% 
  mutate(home_elo_diff = elo_rating_home - elo_rating_away,
         home_elo_wp = calc_expected_score(elo_rating_home + if_else(neutral_site == TRUE, 0L, 55L), elo_rating_away))

games_ready_cfp <- games_joined %>% 
  filter((rank_cfp_home <= 25 &
            rank_cfp_away <= 25),
         season_type == "regular")
  

model_ap <- glm(formula = home_result ~ rank_ap_home + rank_ap_away, family = "binomial", data = games_ready)
model_elo <- glm(formula = home_result ~ elo_ranking_home + elo_ranking_away, family = "binomial", data = games_ready)
model_elo_rating <- glm(formula = home_result ~ elo_rating_home + elo_rating_away, family = "binomial", data = games_ready)
model_elo_diff <- glm(formula = home_result ~ home_elo_diff, family = "binomial", data = games_ready)
model_elo_wp <- glm(formula = home_result ~ home_elo_wp, family = "binomial", data = games_ready)
model_cfp <- glm(formula = home_result ~ rank_cfp_home + rank_cfp_away, family = "binomial", data = games_ready_cfp)

summary(model_ap) # 1111 AIC
summary(model_elo) # 1117
summary(model_elo_rating) # 1082
summary(model_elo_diff) # 1081
summary(model_elo_wp) # 1078.9
summary(model_cfp) # 135

games_predict <- games_ready %>% 
  mutate(ap_predict = predict(model_ap, newdata = games_ready),
         ap_home_wp = exp(ap_predict) / (1 + exp(ap_predict)),
         ap_sq_error = (home_result - ap_home_wp)^2,
         elo_rank_predict = predict(model_elo, newdata = games_ready),
         elo_rank_home_wp = exp(elo_rank_predict) / (1 + exp(elo_rank_predict)),
         elo_rank_sq_error = (home_result - elo_rank_home_wp)^2,
         elo_rating_predict = predict(model_elo_rating, newdata = games_ready),
         elo_rating_home_wp = exp(elo_rating_predict) / (1 + exp(elo_rating_predict)),
         elo_rating_sq_error = (home_result - elo_rating_home_wp)^2,
         elo_wp_predict = predict(model_elo_wp, newdata = games_ready),
         elo_wp_home_wp = exp(elo_wp_predict) / (1 + exp(elo_wp_predict)),
         elo_wp_sq_error = (home_result - elo_wp_home_wp)^2)

games_predict %>% 
  ggplot(aes(x = home_result, y = ap_home_wp)) +
  geom_point(alpha = .3)

games_predict %>% 
  mutate(ap_wp_bucket = round(ap_home_wp, 1)) %>% 
  group_by(ap_wp_bucket) %>% 
  summarise(avg_result = mean(home_result)) %>% 
  ggplot(aes(x = avg_result, y = ap_wp_bucket)) +
  geom_point(alpha = .3) +
  geom_abline()

# Briers
brier_gt <- games_predict %>% 
  pivot_longer(cols = c(ap_sq_error, elo_rank_sq_error, elo_rating_sq_error, elo_wp_sq_error)) %>% 
  group_by(name) %>% 
  summarise(brier = mean(value)) %>% 
  mutate(name = gsub("ap_sq_error", 
                     "AP Ranking",
                     name),
         name = gsub("elo_rank_sq_error", 
                     "Elo Ranking",
                     name),
         name = gsub("elo_rating_sq_error", 
                     "Elo Ratings",
                     name),
         name = gsub("elo_wp_sq_error", 
                     "Elo Win Probability",
                     name)) %>% 
  gt::gt() %>% 
  gt::tab_header(title = "Brier Scores by Model") %>% 
  gt::fmt_number(columns = brier, decimals = 3) %>% 
  cols_label(brier = "Brier Score",
             name = "Model") %>% 
  gt::data_color(columns = brier, colors = scales::col_numeric(
    palette = c("blue", "white", "red"),
    domain = NULL),
                 alpha = .5) %>% 
  gt::tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.border.bottom.style = "hidden",
    heading.title.font.weight = "lighter"
  ) %>% 
  gtExtras::gt_theme_538()

gtsave(brier_gt, 
       "R Plots/ap_vs_elo_brier_tbl.png")

# Compare models
games_predict %>% 
  pivot_longer(cols = c(ap_home_wp, elo_rank_home_wp, elo_rating_home_wp, elo_wp_home_wp)) %>% 
  mutate(bucket = round(value, 1)) %>% 
  group_by(bucket, name) %>% 
  summarise(avg_result = mean(home_result), n = n()) %>% 
  filter(n >= 5) %>% 
  mutate(name = gsub("ap_home_wp", 
                     "AP Ranking",
                     name),
         name = gsub("elo_rank_home_wp", 
                     "Elo Ranking",
                     name),
         name = gsub("elo_rating_home_wp", 
                     "Elo Ratings",
                     name),
         name = gsub("elo_wp_home_wp", 
                     "Elo Win Probability",
                     name)) %>% 
  ggplot(aes(x = avg_result, y = bucket, color = name)) +
  geom_smooth(se = FALSE) +
  geom_abline() +
  labs(x = "Home Win Rate",
       y = "Predicted Win Probability",
       color = "Model",
       title = "AP Poll vs. Elo in Predicting Top 25 Matchups",
       subtitle = "Data From 2000 - 2021") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  staturdays_theme +
  theme(plot.background = element_rect(fill = "#cfcfcf"),
        panel.background = element_rect(fill = "#cfcfcf"),
        legend.background = element_rect(fill = "#cfcfcf"),
        panel.grid = element_line(color = "#b3b3b3"),
        legend.key = element_blank())

source("Production/plot_save.r")
plot_save(filename = "elo_vs_ap_plot")

games_predict %>% 
  group_by(rank_home) %>% 
  summarise(avg_result = mean(home_result), n = n()) %>% 
  ggplot(aes(x = rank_home, y = avg_result)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  staturdays_theme +
  theme(plot.background = element_rect(fill = "#cfcfcf"),
        panel.background = element_rect(fill = "#cfcfcf"),
        legend.background = element_rect(fill = "#cfcfcf"),
        panel.grid = element_line(color = "#b3b3b3"),
        legend.key = element_blank())

games_predict %>% 
  group_by(elo_ranking_home) %>% 
  summarise(avg_result = mean(home_result), n = n()) %>% 
  filter(elo_ranking_home <= 25) %>% 
  ggplot(aes(x = elo_ranking_home, y = avg_result)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  staturdays_theme +
  theme(plot.background = element_rect(fill = "#cfcfcf"),
        panel.background = element_rect(fill = "#cfcfcf"),
        legend.background = element_rect(fill = "#cfcfcf"),
        panel.grid = element_line(color = "#b3b3b3"),
        legend.key = element_blank())

plot_save()
