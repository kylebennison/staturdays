library(cfbscrapR)
library(tidyverse)
library(ggimage)
library(grid)
library(png)
library(RCurl)
library(data.table)

plays.data <- tibble()

for(i in c(1:16)) {
#for(i in c(1)) {
  message("Pulling week: ", i)
  plays.data.temp <- cfb_pbp_data(2020, week=i, epa_wpa = TRUE,
                                  season_type = "both")
  plays.data <- rbind(plays.data, plays.data.temp)
}
  

#plays.data <- fread("C:/Users/drewb/Desktop/2020_plays_epa_wpa.csv")

#negative EPA is bad for both offense and defense
epa_play_type <- plays.data %>% 
  group_by(game_id, offense_play, defense_play, pass) %>% 
  summarise(offense_epa_per_game = mean(EPA, na.rm = TRUE),
            defense_epa_per_game = -offense_epa_per_game) %>% 
  pivot_wider(names_from = pass, values_from = c(pass, offense_epa_per_game, defense_epa_per_game)) %>% 
  rename(`OFF EPA/RUSH` = offense_epa_per_game_0,
         `OFF EPA/PASS` = offense_epa_per_game_1,
         `DEF EPA/RUSH` = defense_epa_per_game_0,
         `DEF EPA/PASS` = defense_epa_per_game_1) %>% 
  select(-pass_0, -pass_1) %>% 
  pivot_longer(!c(offense_play, defense_play), names_to = "stat_category", values_to = "stat") %>% 
  mutate(team = ifelse(grepl("OFF", stat_category), offense_play, defense_play)) %>% 
  group_by(team, stat_category) %>% 
  summarise(season_stat = mean(stat)) %>% 
  filter(stat_category != "game_id")
  

epa_overall <- plays.data %>% 
  group_by(game_id,offense_play, defense_play) %>% 
  summarise(`OFF EPA/GAME` = mean(EPA, na.rm = TRUE),
            `DEF EPA/GAME` = -`OFF EPA/GAME`) %>% 
  pivot_longer(!c(offense_play, defense_play), names_to = "stat_category", values_to = "stat") %>% 
  mutate(team = ifelse(grepl("OFF", stat_category), offense_play, defense_play)) %>% 
  group_by(team, stat_category) %>% 
  summarise(season_stat = mean(stat))  %>% 
  filter(stat_category != "game_id")
  

success_rate <- plays.data %>%
  group_by(game_id, offense_play, defense_play) %>% 
  summarise(`OFF SUCCESS` = mean(success, na.rm = TRUE),
            `DEF SUCCESS` = 1 - `OFF SUCCESS`) %>% 
  pivot_longer(!c(offense_play, defense_play), names_to = "stat_category", values_to = "stat") %>% 
  mutate(team = ifelse(grepl("OFF", stat_category), offense_play, defense_play)) %>% 
  group_by(team, stat_category) %>% 
  summarise(season_stat = mean(stat)) %>% 
  filter(stat_category != "game_id")

master_stats <- rbindlist(list(epa_play_type, epa_overall, success_rate))



colors <- cfb_team_info()

team_logos <- colors %>% unnest(cols = logos) %>% 
  mutate(logo_color = if_else(str_detect(logos, "dark"), "dark", "light")) %>% 
  pivot_wider(names_from = logo_color, values_from = logos) %>% 
  select(school, light)








#create results for single game
game <- master_stats  %>% filter(team %in% c("Alabama", "Ohio State")) %>%
  pivot_wider(names_from = "team", values_from = "season_stat")

colNameD <- paste0(names(game[2]), "_Edge")
team_one_name <- names(game[2])
team_two_name <- names(game[3])

team_color_one <- colors %>% filter(school == team_one_name) %>% 
  select(color)
team_color_one <- team_color_one$color

team_color_two <- colors %>% filter(school == team_two_name) %>% 
  select(color)
team_color_two <- team_color_two$color

team_logo_one <- team_logos %>% filter(school == team_one_name) %>% 
  select(light) %>% pull()

team_logo_one <-  readPNG(getURLContent(team_logo_one))
t1 <- rasterGrob(team_logo_one, interpolate = TRUE)

team_logo_two<- team_logos %>% filter(school == team_two_name) %>% 
  select(light) %>% pull()

team_logo_two <-  readPNG(getURLContent(team_logo_two))
t2 <- rasterGrob(team_logo_two, interpolate = TRUE)

game2 <- game %>% mutate(team_one_edge = .[[2]] - .[[3]]) %>% 
  pivot_wider()

game2$stat_category <- factor(game2$stat_category,levels = c("OFF EPA/GAME", "OFF EPA/PASS", "OFF EPA/RUSH",
                                                             "DEF EPA/GAME", "DEF EPA/PASS", "DEF EPA/RUSH",
                                                             "OFF SUCCESS", "DEF SUCCESS"))

game2 <- game2 %>% 
  mutate(team_one_indicator = ifelse(.[[2]] - .[[3]] >0, "1", "0"))

game2$team_one_indicator <- factor(game2$team_one_indicator, levels = c("1", "0"))

max_val <- game2 %>% pull(team_one_edge) %>% max()
min_val <- game2 %>% pull(team_one_edge) %>% min()

ggplot(data = game2, aes(x=stat_category, y=team_one_edge,fill=as.factor(team_one_indicator))) +
  geom_col() +
  labs(y=paste0("Net Advantage"), x ="",
       title = paste0(team_one_name, " vs. ", team_two_name, ", who's better in each category?"),
       fill = paste0(team_one_name, " advantage"),
       subtitle = "Success rate measured on a 0-1 scale; EPA measured on a points/play scale",
       caption = "@staturdays | staturdays.com") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c(team_color_one, team_color_two)) +
  annotation_custom(t1, xmin=3, xmax=5, ymin=0, ymax=max_val+.1) +
  annotation_custom(t2, xmin=3, xmax=5, ymin= min_val-.1, ymax=0) +
  ylim(min_val-.2, max_val+.2) +
  geom_vline(xintercept = 6.5) +
  annotate("label", x = 7.5, y = 0, label = "Success Rate") +
  annotate("label", x = 3.5, y = 0, label = "EPA") +
  annotate("label", x = 5, y = .05, label = paste0(team_one_name," better\non this side")) +
  annotate("label", x = 1.5, y = -.05, label = paste0(team_two_name," better\non this side")) +
  coord_cartesian(clip = "off") +
  coord_flip() +
  scale_y_continuous(labels=abs)
  

ggsave(paste0("C:/Users/drewb/OneDrive/Documents/Staturdays/Plots/", "_", team_one_name,
       "_", team_two_name, "_bowls.png"), height = 4, width = 8)



  
  
  