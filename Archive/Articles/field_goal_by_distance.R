library(tidyverse)
library(stringi)
library(ggplot2)

source("Production/source_everything.R")

dt <- get_plays(start_week = 1, end_week = 20, start_year = 2021, end_year = 2021)


x <- dt %>% group_by(play_type) %>% count()

field_goal_plays <- c("Field Goal Good", "Field Goal Missed", "Blocked Field Goal", "Missed Field Goal Return",
  "Blocked Field Goal Touchdown", "	Missed Field Goal Return Touchdown")

dt_fg <- dt %>% filter(play_type %in% field_goal_plays) %>% 
  mutate(made_fg = ifelse(play_type=="Field Goal Good",1,0),
         new_fg_attempt_yards = stri_extract_first_regex(play_text, "[0-9]+"),
         fg_kicker = str_extract(play_text, '\\D*(?=\\d)'))

dt_fg_averages <- dt_fg %>% group_by(new_fg_attempt_yards) %>% 
  summarise(make_probability = mean(made_fg),
            new_fg_attempt_yards = as.numeric(new_fg_attempt_yards)) %>% 
  unique()

#not enough data

#by player
dt_fg_averages_player <- dt_fg %>% group_by(new_fg_attempt_yards, fg_kicker, offense) %>% 
  add_count() %>% 
  summarise(make_probability = mean(made_fg),
            new_fg_attempt_yards = as.numeric(new_fg_attempt_yards),
            c = mean(n)) %>% 
  unique()

#overall graph
dt_fg_averages %>% 
  ungroup() %>% 
  filter(new_fg_attempt_yards>1, new_fg_attempt_yards<56) %>% 
  ggplot(aes(x=new_fg_attempt_yards, y=make_probability)) +
  geom_point() + geom_smooth()

#player boxplot - might need to group by buckets to have enough data
dt_fg_averages_player %>% 
  ggplot(aes(x=as.factor(new_fg_attempt_yards), y=make_probability)) + 
  geom_boxplot()

#percentile in 40+ and 30 and below
#minimum kicks


#are the bottom 25% of characters worse at every distance than the top 25%

