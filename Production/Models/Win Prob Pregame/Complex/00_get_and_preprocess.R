library(dplyr)
library(tidyr)
library(statRdaysCFB)

# Read in data from simple model
bet_df <- readRDS("Data/betting_prepped.rds")

# Get betting data

# Join in to bet_df
start_year <- 2013
end_year <- 2021
# Get betting data
# betting <- get_betting(start_year = start_year, end_year = end_year,
#             start_week = 1, end_week = 15)
# 
# Clean betting data
# betting <- 
#   betting %>% 
#   rename_with(.fn = ~paste0("bet_", .x), .cols = c("spread", "spreadOpen", 
#                                                    "overUnder", "overUnderOpen",
#                                                    "homeMoneyline", "awayMoneyline", 
#                                                    "formattedSpread"))


# Save for faster loading next time
# saveRDS(betting, "Data/betting_clean_2013-2021")
betting <- readRDS("Data/betting_clean_2013-2021")

# Join bet_df with lines
bet_df <- bet_df %>% 
  inner_join(betting, by = 
             c("id", 
               "home_team" = "homeTeam", 
               "away_team" = "awayTeam",
               "season",
               "week"))

# Calc spread history for home teams
for (lookback in c(2,4,6,8)){
  message("Working on lookback ", lookback)
  
    bet_df <- 
      bet_df %>% 
      arrange(id) %>% 
      group_by(home_team) %>% 
      mutate(across(.cols = starts_with("bet_") & !contains("_ma_") & where(~is.numeric(.x)),
                    .fns = ~ zoo::rollapply(.x,
                                            width = list(c(seq(-lookback, -1, 1))),
                                            FUN = mean, # TODO: should I average stats or sum them? Or maybe some should be added while others should be averaged?
                                            na.rm = TRUE,
                                            fill = NA
                    ),
                    .names = "home_{.col}_ma_{lookback}gms"
                    )
             )
  }

# Calc spread history for away teams
for (lookback in c(2,4,6,8)){
  message("Working on lookback ", lookback)
  
  bet_df <- 
    bet_df %>% 
    arrange(id) %>% 
    group_by(away_team) %>% 
    mutate(across(.cols = starts_with("bet_") & !contains("_ma_") & where(~is.numeric(.x)),
                  .fns = ~ zoo::rollapply(.x,
                                          width = list(c(seq(-lookback, -1, 1))),
                                          FUN = mean, # TODO: should I average stats or sum them? Or maybe some should be added while others should be averaged?
                                          na.rm = TRUE,
                                          fill = NA
                  ),
                  .names = "away_{.col}_ma_{lookback}gms"
    )
    )
}

# Prep for modeling
# Drop first season without betting lookbacks
bet_df <- bet_df %>% 
  ungroup() %>% 
  filter(season != start_year)

# Clean
# drop any forward-looking or non-useful columns

bet_df <- bet_df %>%
  select(
    !c(
      "id",
      "start_date",
      "start_time_tbd",
      "venue",
      "home_team",
      "home_line_scores",
      "home_post_win_prob",
      "away_team",
      "away_line_scores",
      "away_post_win_prob",
      "excitement_index",
      "highlights",
      "notes",
      "conference_home",
      "conference_away",
      "division_home",
      "division_away",
      "first_name_home",
      "last_name_home",
      "hire_date_home",
      "coach_home",
      "first_name_away",
      "last_name_away",
      "hire_date_away",
      "coach_away"
    )
  )

# Drop betting columns not useful
bet_df <- bet_df %>% 
  select(!c("seasonType",
            "bet_formattedSpread"))

# One-hot encode
library(mltools)
library(data.table)
bet_df <- as.data.table(bet_df %>% 
                          mutate(across(.cols = c("season_type",
                                                  "neutral_site",
                                                  "conference_game",
                                                  "home_conference",
                                                  "home_division",
                                                  "away_conference",
                                                  "away_division"),
                                        .fns = as.factor)))

bet_df <- one_hot(bet_df, cols = c(
  "season_type",
  "neutral_site",
  "conference_game",
  "home_conference",
  "home_division",
  "away_conference",
  "away_division"
),
sparsifyNAs = TRUE,
dropCols = TRUE) %>% 
  select(!contains("FALSE"))


# Convert to numeric
bet_df <- bet_df %>%
  mutate(across(
    .cols = c(
      "points_home",
      "points_away",
      "srs_home",
      "sp_overall_home",
      "sp_offense_home",
      "sp_defense_home",
      "srs_away",
      "sp_overall_away",
      "sp_offense_away",
      "sp_defense_away"
    ),
    .fns = as.numeric
  ))

# Clean column names
names(bet_df) <- janitor::make_clean_names(names(bet_df))

# Remove more forward-looking columns
bet_df <- bet_df %>% 
  select(!c(
    season,
    attendance,
    venue_id,
    home_id,
    home_postgame_elo,
    home_points,
    away_points,
    away_postgame_elo,
    away_id,
    year_home,
    year_away,
    year_home_home,
    year_away_away
  ))

saveRDS(bet_df, "Production/Models/Win Prob Pregame/Complex/00_output_df_prepped.rds")