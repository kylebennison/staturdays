library(dplyr)
library(mltools)
library(data.table)
library(tibble)

# Join college to pros
df <- left_join(college_df, nfl_df, by = c("name" = "player_display_name", "position"))

# Prep for modeling

extra_cols <- df %>% 
  ungroup() %>% 
  select(id, name, player_id, first_name, last_name)

features <- df %>% 
  ungroup() %>% 
  select(!c(id, name, player_id, first_name, last_name))

tibble(mltools::one_hot(data.table(features), cols = c("position", "conference")))

features_dt <- data.table(features)

features_dt$position <- as.factor(features_dt$position)
features_dt$conference <- as.factor(features_dt$conference)

df_x <- tibble(one_hot(features_dt, cols = c("position", "conference")))

df_x <- df_x %>% 
  select(!total_epa)

df_y <- features_dt$total_epa

glm()