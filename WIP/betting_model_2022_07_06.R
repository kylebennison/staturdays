bet_df <- readRDS("Data/betting_prepped.rds")


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
      "seasonType",
      "conference_home",
      "conference_away",
      "conference_home_home",
      "division_home",
      "conference_away_away",
      "division_away",
      "conference_home_home_home",
      "conference_away_away_away",
      "conference_home_home_home_home",
      "date_home",
      "conference_away_away_away_away",
      "date_away",
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

# One-hot encode
library(mltools)
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
      "talent_home",
      "talent_away",
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
    season_y,
    week_x,
    week_y,
    season_y,
    season_away,
    year_home,
    year_away,
    year_home_home,
    year_away_away
  ))

# Pull out responses
response_home_win <- bet_df %>% 
  pull(response_home_win)
response_home_spread <- bet_df %>% 
  pull(response_home_spread)
response_total_points <- bet_df %>% 
  pull(response_total_points)

bet_df <- bet_df %>% 
  select(!contains("response"))

# Impute medians
impute_median <- function(column){
  
  column[is.na(column)] <- median(column, na.rm = TRUE)
  
  return(column)
}

bet_df <- bet_df %>% 
  mutate(across(.cols = everything(),
                .fns = ~ impute_median(.x)))

# Normalize all columns
normalize_columns <- function(column){
  
  return((column - min(column)) / (max(column) - min(column)))
  
}

bet_df <- bet_df %>% 
  mutate(across(.cols = everything(),
                .fns = normalize_columns))

bet_df <- bet_df %>% 
  select(!where(~all(is.nan(.x))))

# Boosted Tree

# Random Forest

# Bagging

# Lasso/Ridge Regression
library(glmnet)

x <- matrix(bet_df)
y <- response_home_spread

model_glm <- glmnet(x = x,
                    y = y,
                    family = "gaussian",
                    alpha = 1)
