source("Production/source_everything.r")

elo <- get_elo(2015, 2021)

elo_ranked <- elo %>% 
  group_by(season, week) %>% 
  mutate(elo_ranking = rank(desc(elo_rating), ties.method = "min"))

elo_ready <- elo_ranked %>% 
  group_by(team) %>% 
  mutate(join_date = lead(date, n = 1L, order_by = date))

ap <- get_anything("https://api.collegefootballdata.com/rankings", start_year = 2015, end_year = 2021, key = my_key)

ap_top_25 <- ap %>% 
  rename(pre_week = week) %>% 
  unnest(polls) %>% 
  unnest(ranks) %>% 
  filter(poll == "AP Top 25") %>% 
  rename(ap_rank = rank)

games <- get_games(2015, 2021)

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
  left_join(ap_top_25, by = c("season",
                              "week" = "pre_week",
                              "home_team" = "school")) %>% 
  left_join(ap_top_25, by = c("season",
                              "week" = "pre_week",
                              "away_team" = "school"),
            suffix = c("_home", "_away"))

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

games_ready <- games_joined %>% 
  filter(ap_rank_home <= 25 & ap_rank_away <= 25,
         season_type == "regular") %>% 
  mutate(home_elo_diff = elo_rating_home - elo_rating_away,
         home_elo_wp = calc_expected_score(elo_rating_home + if_else(neutral_site == TRUE, 0L, 55L), elo_rating_away))

model_ap <- glm(formula = home_result ~ ap_rank_home + ap_rank_away, family = "binomial", data = games_ready)
model_elo <- glm(formula = home_result ~ elo_ranking_home + elo_ranking_away, family = "binomial", data = games_ready)
model_elo_rating <- glm(formula = home_result ~ elo_rating_home + elo_rating_away, family = "binomial", data = games_ready)
model_elo_diff <- glm(formula = home_result ~ home_elo_diff, family = "binomial", data = games_ready)
model_elo_wp <- glm(formula = home_result ~ home_elo_wp, family = "binomial", data = games_ready)

summary(model_ap) # 340 AIC
summary(model_elo) # 337
summary(model_elo_rating) # 313
summary(model_elo_diff) # 315
summary(model_elo_wp) # 314

games_predict <- games_ready %>% 
  mutate(ap_predict = predict(model_ap, newdata = games_ready),
         ap_home_wp = exp(ap_predict) / (1 + exp(ap_predict)),
         ap_sq_error = (home_result - ap_home_wp)^2)

games_predict %>% 
  ggplot(aes(x = home_result, y = ap_home_wp)) +
  geom_point(alpha = .3)
