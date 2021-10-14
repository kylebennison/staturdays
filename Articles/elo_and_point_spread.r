source("Production/source_everything.r")

elo <- get_elo(2000, 2021)

games <- get_games(2000, 2021)

e2 <- elo %>% 
  group_by(team) %>% 
  mutate(join_date = lead(date, n = 1L, order_by = date))

games_elo <- games %>% 
  mutate(start_date = lubridate::as_datetime(start_date)) %>% 
  left_join(e2, by = c("start_date" = "join_date",
                       "home_team" = "team")) %>% 
  left_join(e2, by = c("start_date" = "join_date",
                       "away_team" = "team"),
            suffix = c("_home", "_away"))

ge2 <- games_elo %>%
  mutate(home_elo_adv = elo_rating_home + 55 - elo_rating_away,
         final_home_spread = away_points - home_points)

# Plot relationship between elo and point spread
ge2 %>% 
  ggplot(aes(x = home_elo_adv, y = final_home_spread)) +
  geom_point(alpha = .1, color = staturdays_colors("light_blue")) +
  geom_smooth(color = staturdays_colors("orange")) +
  staturdays_theme +
  theme(panel.grid.major = element_line(color = "lightgrey")) +
  labs(title = "Elo and Spread 2000-2021",
       subtitle = "Elo advantage includes built-in home-field advantage worth around 3 points",
       x = "Home Elo Point Advantage/Disadvantage",
       y = "Home Win/Loss Point Margin")

# Linear model
model <- lm(final_home_spread ~ home_elo_adv, data = ge2)
summary(model)

# Home margin = home_elo_adv * .05 + .77
# So 1 point of home margin equals  20 elo points advantage.
#' Note, if i exclude the 55 elo point margin, it adds it into the
#' intercept anyway as a 3.59 point intercept. So our home-field adv.
#' may actually be too low.
#' 

ge3 <- ge2 %>% mutate(alt_elo_adv = home_pregame_elo - away_pregame_elo)

model_me <- lm(final_home_spread ~ home_elo_adv, data = ge2) # 17.33, 106 sig

model_cfb <- lm(final_home_spread ~ alt_elo_adv, data = ge3) # 16.53, 100 sig

model_combo <- lm(final_home_spread ~ home_elo_adv + alt_elo_adv, ge3) # 16.43, 37 sig to his, 14 to ours, 18 to a 2.8 hfa int.

saveRDS(model_combo, file = "Production Models/elo_combo_spread_model.rds")

# Calculate Confidence Interval

# Calc t score
qt(p = .025, df = model_combo$df.residual, lower.tail = FALSE)
margin_error <- 16.42 * 1.96

# Calc range of outcomes
ge4 <- ge3 %>% 
  mutate(pred_spread = predict(model_combo, newdata = ge3),
         alt_upper_95 = pred_spread + margin_error,
         alt_lower_95 = pred_spread - margin_error)

# Filter only spreads where outcomes of upper and lower confidence are the same
ge4 %>% 
  filter(sign(alt_upper_95) == sign(alt_lower_95))