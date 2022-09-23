### Compare Models
# requires manual intervention

model_2.3.2 <- readRDS("Production Models/in_game_wp_v2.3.2.rds")
data_2.3.2 <- plays.master.win_prob4 %>% 
  select(year, home_score_lead_deficit, clock_in_seconds, down, distance,
         yards_to_goal, home_poss_flag, home_timeouts_new, away_timeouts_new, 
         home_elo_wp, game_over, home_outcome, spread,
         is_kickoff)

model_3.0 <- wp_model
data_3.0 <- model_data

preds1 <- as.data.frame(
  matrix(predict(model_2.3.2, as.matrix(data_2.3.2 %>% filter(year == 2021) %>% select(-home_outcome, -year, -is_kickoff) %>% 
                                          relocate(clock_in_seconds, .before = down))))
) %>%
  dplyr::rename(wp_2_3_2 = V1)

preds2 <- as.data.frame(
  matrix(predict(model_3.0, as.matrix(data_3.0 %>% filter(year == 2021) %>% select(-home_outcome, -year))))
) %>%
  dplyr::rename(wp_3_0 = V1)

full_preds_232 <- cbind(plays.master.win_prob4 %>% filter(year == 2021), preds1)
full_preds_3 <- cbind(plays.master.win_prob4 %>% filter(year == 2021), preds2)

test_games <- full_preds_232$game_id %>% unique()

ids <- sample(test_games, 30)

for(i in 1:length(ids)) {
  
  p1 <- full_preds_232 %>% 
    mutate(clock_in_seconds = if_else(clock_in_seconds == -10000, -.5, clock_in_seconds)) %>% 
    filter(game_id == ids[i]) %>% 
    ggplot(aes(x = -clock_in_seconds, y = wp_2_3_2)) +
    geom_line() +
    ylim(0,1) +
    geom_vline(xintercept=-900, colour="grey") +
    geom_text(aes(x=-900, label="\nEnd Q3", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
    geom_vline(xintercept=-1800, colour="grey") +
    geom_text(aes(x=-1800, label="\nEnd Q2", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
    geom_vline(xintercept=-2700, colour="grey") +
    geom_text(aes(x=-2700, label="\nEnd Q1", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
    geom_text(aes(x = -3300, y = .9, label = home)) +
    geom_text(aes(x = -3300, y = .1, label = away))
  
  p2 <- full_preds_3 %>% 
    filter(game_id == ids[i]) %>% 
    ggplot(aes(x = play_num, y = wp_3_0)) +
    geom_line() +
    ylim(0,1) +
    geom_text(aes(x = 20, y = .9, label = home)) +
    geom_text(aes(x = 20, y = .1, label = away))
  
  p3 <- gridExtra::grid.arrange(p1, p2, ncol = 2)
  
  message("Finished Game ", i)
  
  plot_save(plot = p3, filename = paste0("sample", i))
  
}

quarters <- full_preds_3 %>% filter(game_id == ids[i]) %>% 
  filter(period == 3 & lead(period) == 4 |
           period == 2 & lead(period) == 3 |
           period == 1 & lead(period) == 2) %>% pull(play_num) + .5

mid_quarters <- full_preds_3 %>% filter(game_id == ids[i]) %>% 
  group_by(period) %>% 
  summarise(min = min(play_num),
            max = max(play_num)) %>% 
  mutate(med = (min+max)/2) %>% 
  filter(period != 20) %>% 
  pull(med)

full_preds_3 %>% 
  filter(game_id == ids[i]) %>% 
  ggplot(aes(x = play_num, y = wp_3_0)) +
  geom_line() +
  ylim(0,1) +
  geom_text(aes(x = 20, y = .9, label = home)) +
  geom_text(aes(x = 20, y = .1, label = away)) +
  geom_vline(xintercept = c(quarters)) +
  annotate(geom = "label", x = c(mid_quarters), y = 0,
           label = paste0("Q", 1:4))
