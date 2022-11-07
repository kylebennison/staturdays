library(dplyr)
library(lubridate)
library(ggplot2)
library(statRdaysCFB)

games <- readRDS("Data/games_raw_2001-2021")

betting <- get_betting(2010, 2021, 1, 15)


# Straight WP
clean_kick_times <- function(games_data){
  games <- games_data %>% 
    mutate(start_date = with_tz(as_datetime(start_date)), "America/New_York") %>% # change timezone to system TZ
    mutate(kick_time = lubridate::hour(start_date),
           kick_time = if_else(kick_time == 0L, 24L, kick_time),
           display_time = if_else(kick_time > 12, kick_time - 12L, kick_time),
           display_time = paste(display_time, if_else(kick_time < 24, "PM", "AM"))) %>% 
    mutate(home_win = if_else(home_points > away_points, 1, 0))
  
  return(games)
}

games %>% 
  clean_kick_times() %>% 
  group_by(kick_time, display_time) %>% 
  summarise(home_wp = mean(home_win, na.rm = TRUE), n = n()) %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = reorder(display_time, kick_time), y = home_wp)) +
  geom_col(aes(fill = home_wp)) +
  geom_text(aes(label = scales::percent(round(home_wp, 2)), y = home_wp + .1), size = 5) +
  labs(title = "Home Win Rate by Kick Time",
       subtitle = "Games from 2001-2021",
       x = "Kick Time",
       y = "Home Win Rate",
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data") +
  staturdays_theme +
  scale_fill_continuous(type = "viridis", guide = "none") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.1))

ggsave(filename = paste0("R Plots/", "kick_time_wp", ".png"),
       plot = last_plot(),
       units = "mm",
       height = 200,
       width = 400
       )

# Group by Elo rating advantage
games %>% 
  clean_kick_times() %>% 
  mutate(home_elo_advantage = home_pregame_elo - away_pregame_elo,
         home_elo_advantage = round(home_elo_advantage, -2)) %>% 
  group_by(kick_time, display_time, home_elo_advantage) %>% 
  summarise(home_wp = mean(home_win), n = n()) %>% 
  filter(n > 30,
         abs(home_elo_advantage) < 400) %>% 
  ggplot(aes(x = reorder(display_time, kick_time), y = home_wp)) +
  geom_col() +
  facet_wrap(vars(home_elo_advantage))
# Note how good teams actually do well at 12, probably because they're more likely to have
# bad matchups in noon kicks
# Maybe try grouping by avg. elo period, not their advantage

games %>% 
  clean_kick_times() %>% 
  mutate(home_elo = round(home_pregame_elo, -2)) %>% 
  group_by(kick_time, display_time, home_elo) %>% 
  summarise(home_wp = mean(home_win), n = n()) %>% 
  filter(n > 15) %>% 
  ggplot(aes(x = reorder(display_time, kick_time), y = home_wp)) +
  geom_col() +
  facet_wrap(vars(home_elo))

# Group by home team

games %>% 
  clean_kick_times() %>% 
  group_by(home_team, kick_time, display_time) %>% 
  summarise(home_wp = mean(home_win), n = n()) %>% 
  arrange((kick_time), desc(home_wp)) %>% 
  filter(n > 10)

# Group by favorite/underdog
vec <- c(12 = "blue", 7 = "blue", 8 = "blue")

games %>% 
  clean_kick_times() %>% 
  inner_join(betting, by = c("id")) %>% 
  mutate(home_favorite = if_else(spread < 0, TRUE, FALSE)) %>% 
  group_by(home_favorite, kick_time, display_time) %>% 
  summarise(home_wp = mean(home_win), n = n()) %>% 
  filter(n > 10, kick_time > 11) %>% 
  ggplot(aes(x = reorder(display_time, kick_time), y = home_wp)) +
  geom_col() +
  facet_wrap(vars(home_favorite)) +
  geom_text(aes(label = scales::percent(round(home_wp, 2)), y = home_wp + .1), size = 5) +
  geom_text(aes(label = n, y = home_wp - .1), size = 5) +
  scale_fill_manual(values = c(12 = "blue"))
