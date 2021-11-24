library(tidyverse)
library(ggrepel)
library(gt)
library(scales)
library(ggimage)
library(jsonlite)
library(RCurl)
source("Production/source_everything.R")

staturdays_palette <- c("#041e42", "#22345a", "#394871", "#4c5872", "#5c6272", "#de703b")

team_colors <- get_colors()

team_colors <- team_colors %>% select(school, light, conference)

dt <- get_plays(start_week = 1, end_week = 20, start_year = 2021, end_year = 2021)

make_buckets <- tibble(distance=c(1,2,3,4,5,6,7,8,9,10),
                       bucket = c("1&2", "1&2", "3&4", "3&4", "5&6", "5&6", "7&8", "7&8", "9&10", "9&10"))

#filter for third downs
third_downs <- dt %>% 
  filter(down==3) %>% 
  select(offense, play_type, down, distance, yards_gained) %>% 
  filter(play_type!= "Timeout", play_type != "Kickoff", play_type!="Punt",
         play_type!="Field Goal Missed", play_type!="Field Goal Good",
         play_type!="Blocked Field Goal", play_type!="Kickoff Return (Offense)",
         play_type!="Uncategorized") %>% 
  mutate(converted = ifelse(yards_gained >= distance, 1, 0)) %>% 
  left_join(make_buckets) %>% 
  filter(distance<11)

#league average conversion rate by bucket
league_averages <- third_downs %>% 
  group_by(bucket) %>%
  summarise(converstion_rate = mean(converted)) %>% 
  filter(!is.na(bucket))

#league acerage coversion rate by bucket plot
league_averages_plot <- league_averages %>% 
  rename(Bucket = bucket, `Conversion Rate` = converstion_rate) %>% 
  mutate(`Conversion Rate` = percent(`Conversion Rate`)) %>% 
  gt() %>% 
  tab_header(title = "CFB 3rd Down Conversion Rate by Distance",
             subtitle = "2021 season") %>% 
  tab_source_note("@staturdays - Data: @cfb_data")

gtsave(data = league_averages_plot, 
       filename = paste0("league_averages.png"),
       path = "C:/Users/drewb/Desktop",
       )

#team conversion rate by bucket
team_third_downs <- third_downs %>% 
  group_by(offense, bucket) %>% 
  add_count() %>% 
  filter(n>=10) %>% 
  select(-n) %>% 
  summarise(converstion_rate = mean(converted))

combined <- team_third_downs %>% 
  left_join(league_averages, by=c("bucket")) %>% 
  mutate(above_league_average = converstion_rate.x-converstion_rate.y)


#average third down distance for all third downs by league and team
league_average_distance <- third_downs %>% 
  summarise(average_distance = mean(distance)) %>% 
  pull()

team_distance <- third_downs %>%
  group_by(offense) %>% 
  add_count() %>% 
  filter(n>=50) %>% 
  select(-n) %>% 
  summarise(average_distance = mean(distance))

combined_distance <- team_distance %>% 
  mutate(league_average = league_average_distance,
         above_league_average = average_distance-league_average)

#################################################################

#average third down distance for all third downs by bucket, league, and team
league_average_distance_bucket <- third_downs %>% 
  group_by(bucket) %>% 
  summarise(average_distance_league = mean(distance)) 

team_distance_bucket <- third_downs %>%
  group_by(offense, bucket) %>% 
  add_count() %>% 
  filter(n>=10) %>% 
  select(-n) %>% 
  summarise(average_distance = mean(distance))

combined_distance_bucket <- team_distance_bucket %>% 
  left_join(league_average_distance_bucket) %>% 
  mutate(above_league_average_yards = average_distance-average_distance_league)
###############################################################

master <- combined %>% left_join(combined_distance_bucket, by=c("offense", "bucket")) %>% 
  left_join(team_colors, by=c("offense"="school"))
  

master %>% filter(!is.na(bucket), conference %in% c("SEC", "ACC", "Pac-12", "Big 12", 
                                                    "FBS Independents", "Big Ten")) %>% 
  ggplot(aes(x=above_league_average, y=above_league_average_yards)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_image(aes(image = light), size = .1, by = "width", asp = 1, alpha = 0.8) +
  theme(aspect.ratio = 1) +
  facet_wrap(~bucket, ncol = 1) +
  scale_x_continuous(labels = percent) +
  #annotate(geom="text", x=-.3, y=1, label="Bad on downs 1&2 AND 3rd downs",
  #           color="red") +
  #geom_text(aes(label=ifelse(above_league_average.x>.2 | above_league_average.x < -0.2,as.character(offense_play),'')),hjust=0,vjust=0) +
  #geom_label_repel(aes(label=ifelse(above_league_average.x>.2,as.character(offense_play),''))) +
  labs(title = "Average distance to go on 3rd downs\nabove/below average vs.\n3rd down conversion rate by\ndistance above/below average",
    x="3rd down conversion rate above/below league average at that bucket",
       y="3rd down yards to go above/below league average at that bucket")

ggsave("C:/Users/drewb/Desktop/mainplot.png", height = 15, width = 5)


ov <- mean(third_downs$converted)

overall_third <- third_downs %>% 
  group_by(offense_play) %>% 
  summarise(good = mean(converted)) %>% 
  left_join(master, by="offense_play") %>% 
  filter(!is.na(bucket)) %>% 
  mutate(overall_third_down_above_average = good-ov)


x<-overall_third %>% 
  filter(overall_third_down_above_average>0,above_league_average.x<0) %>% 
  group_by(offense_play) %>% add_count()


third_downs %>% filter(offense_play %in% c("Alabama", "Clemson", "Ohio State",
                                           "Notre Dame", "Texas A&M", "Georgia",
                                           "Florida", "Cincinnati", "Oklahoma",
                                           "Iowa State")) %>% 
  group_by(offense_play, bucket) %>% count() %>% 
  ggplot() +
  geom_col(aes(x=bucket, y=n)) +
  facet_wrap(~offense_play) +
  labs(x="Yards to go",
       y="Count",
       caption = "@staturdays | staturdays.com") +
  theme_bw()

