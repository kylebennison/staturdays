library(nflfastR)
library(nflreadr)
library(dplyr)
library(stringr)

season <- 2021

# Test season

nfl_stats_season <- nflreadr::load_player_stats(seasons = season)

# Get each player's total epa for the season
nfl_df <- nfl_stats_season %>% 
  select(player_id, player_display_name, position, fantasy_points_ppr) %>% 
  group_by(player_id, player_display_name, position) %>% 
  summarise(fantasy_points_ppr = sum(fantasy_points_ppr, na.rm = TRUE)) %>% 
  select(player_id, player_display_name, position, fantasy_points_ppr) %>% 
  mutate(first_name = word(player_display_name, 1)) %>% 
  mutate(last_name = word(player_display_name, -1))

# Train seasons

nfl_stats_1y <- nflreadr::load_player_stats(seasons = season - 1)

df_1y <- nfl_stats_1y %>% 
  group_by(player_id, player_display_name, position) %>% 
  summarise(total_ppr = sum(fantasy_points_ppr, na.rm = TRUE),
            avg_weekly_ppr = mean(fantasy_points_ppr, na.rm = TRUE),
            sd_weekly_ppr = sd(fantasy_points_ppr, na.rm = TRUE),
            games_played = n(),
            worst_week = min(fantasy_points_ppr, na.rm = TRUE),
            best_week = max(fantasy_points_ppr, na.rm = TRUE),
            ptile_25 = quantile(fantasy_points_ppr, probs = .25, na.rm = TRUE),
            ptile_50 = quantile(fantasy_points_ppr, probs = .50, na.rm = TRUE),
            ptile_75 = quantile(fantasy_points_ppr, probs = .75, na.rm = TRUE),
            ) %>% 
  rename_with(.fn = ~paste0(.x, "_1yr"), .cols = where(is.numeric))
