library(data.table)
library(tidyverse)
library(ggplot2)
source("Production/source_everything.R")

dt <- get_plays(start_week = 1, end_week = 16, start_year = 2021, end_year = 2021)

dt2 <- dt %>% mutate(ppa = as.numeric(ppa)) %>% 
  filter(!is.na(ppa), period<5)

offense <- dt2 %>% select(offense, ppa, period)
defense <- dt2 %>% select(defense, ppa, period)

sum_offense <- offense %>% group_by(offense, period) %>%
  summarise(avg_ppa = mean(ppa)) %>% 
  rename(team=offense) %>% 
  mutate(side="offense")

sum_defense <- defense %>% group_by(defense, period) %>% 
  summarise(avg_ppa = -1 * mean(ppa)) %>% 
  rename(team=defense) %>% 
  mutate(side="defense")

combined <- rbind(sum_offense, sum_defense)

# Get conferences
conf <- cfbd_api("https://api.collegefootballdata.com/teams/fbs?year=2021",my_key)
conf <- conf %>% select(school, conference)

combined <- combined %>% left_join(conf, by = c("team" = "school"))

combined %>% filter(conference == "SEC") %>% 
  ggplot(aes(x=period, y=avg_ppa, fill=side)) + geom_col(position = position_dodge2(preserve = "single")) +
  facet_wrap(~team) +
  staturdays_theme +
  labs(x="Quarter",
       y="Avg. PPA/Play",
       fill="When team is on...",
       title = "How team performance varies by quarter",
       caption = "@Staturdays | Data: @CFB_Data",
       subtitle = "Positive PPA is good for offense and defense") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.05, color="grey" ))+
  scale_fill_manual(values= c("#f4a261","#264653"))

ggsave("C:/Users/drewb/Desktop/plot.png", height=6, width = 9)
