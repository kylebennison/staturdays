library(tidyverse)
library(ggrepel)

dt <- read.csv("C:/Users/drewb/Desktop/2020_plays_epa_wpa.csv")

make_buckets <- tibble(distance=c(1,2,3,4,5,6,7,8,9,10),
                       bucket = c("1&2", "1&2", "3&4", "3&4", "5&6", "5&6", "7&8", "7&8", "9&10", "9&10"))

third_downs <- dt %>% 
  filter(Goal_To_Go == FALSE, down==3) %>% 
  select(offense_play, play_type, down, distance, yards_gained) %>% 
  filter(play_type!= "Timeout", play_type != "Kickoff") %>% 
  mutate(converted = ifelse(yards_gained >= distance, 1, 0)) %>% 
  left_join(make_buckets)

league_averages <- third_downs %>% 
  group_by(bucket) %>%
  summarise(converstion_rate = mean(converted))

team_third_downs <- third_downs %>% 
  group_by(offense_play, bucket) %>% 
  add_count() %>% 
  filter(n>=10) %>% 
  select(-n) %>% 
  summarise(converstion_rate = mean(converted))

combined <- team_third_downs %>% 
  left_join(league_averages, by=c("bucket")) %>% 
  mutate(above_league_average = converstion_rate.x-converstion_rate.y)



league_average_distance <- third_downs %>% 
  summarise(average_distance = mean(distance)) %>% 
  pull()

team_distance <- third_downs %>%
  group_by(offense_play) %>% 
  add_count() %>% 
  filter(n>=50) %>% 
  select(-n) %>% 
  summarise(average_distance = mean(distance))

combined_distance <- team_distance %>% 
  mutate(league_average = league_average_distance,
         above_league_average = average_distance-league_average)


master <- combined %>% left_join(combined_distance, by=c("offense_play"))

master %>% filter(!is.na(bucket)) %>% 
  ggplot(aes(x=above_league_average.x, y=above_league_average.y)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(~bucket) +
  #annotate(geom="text", x=-.3, y=1, label="Bad on downs 1&2 AND 3rd downs",
  #           color="red") +
  geom_text(aes(label=ifelse(above_league_average.x>.2 | above_league_average.x < -0.2,as.character(offense_play),'')),hjust=0,vjust=0) +
  #geom_label_repel(aes(label=ifelse(above_league_average.x>.2,as.character(offense_play),''))) +
  labs(x="3rd down conversion rate above/below league average",
       y="3rd down yards to go above/below league average")

  
