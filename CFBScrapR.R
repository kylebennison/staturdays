rm(list=ls())
library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(rlang)
library(cfbscrapR)
library(devtools)
library(gt)
library(ggthemes)
library(ggrepel)
library(ggforce)
library(concaveman)
#library(MASS)

#Staturdays Colors
staturdays_col_list <- c(
  lightest_blue = "#5c6272",
  lighter_blue = "#4c5872",
  light_blue = "#394871",
  medium_blue = "#22345a",
  dark_blue = "#041e42",
  orange = "#de703b",
  sign = "#1e1e1e",
  white = "#FFFFFF"
)
staturdays_colors <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (staturdays_col_list)
  staturdays_col_list[cols]
}

staturdays_theme <- theme(plot.caption = element_text(size = 12, hjust = 1, color = staturdays_colors("orange")), 
                          plot.title = element_text(color = staturdays_colors("dark_blue"), size = 30, face = "bold"),
                          plot.subtitle = element_text(color = staturdays_colors("lightest_blue"), size = 20),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          axis.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 15)
)

# 538 Theme - Can update this with Staturdays preferences in future
theme_538 <- function(base_size = 12, font = "Lato") {
  
  # Text setting
  txt <- element_text(size = base_size + 2, colour = "black", face = "plain")
  bold_txt <- element_text(
    size = base_size + 2, colour = "black",
    family = "Montserrat", face = "bold"
  )
  large_txt <- element_text(size = base_size + 4, color = "black", face = "bold")
  
  
  theme_minimal(base_size = base_size, base_family = font) +
    theme(
      # Legend Settings
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      
      # Backgrounds
      strip.background = element_blank(),
      strip.text = large_txt,
      plot.background = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      
      # Axis & Titles
      text = txt,
      axis.text = txt,
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = bold_txt,
      plot.title = large_txt,
      
      # Panel
      panel.grid = element_line(colour = NULL),
      panel.grid.major = element_line(colour = "#D2D2D2"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()
    )
}

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")

scrimmage_plays_all <- 
  c(
    "Rush", 
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Rushing Touchdown", 
    "Sack", 
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown", 
    "Fumble Recovery (Own)", 
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown"
  )

scrimmage_plays_non_turnover <-
  c(
    "Rush", 
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Rushing Touchdown", 
    "Sack", 
    "Fumble Recovery (Own)"
  )

scrimmage_plays_turnover <-
  c(
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown", 
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown"
  )

scrimmage_plays_pass <-
  c(
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Sack",
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown"
  )

scrimmage_plays_rush <-
  c(
    "Rush", 
    "Rushing Touchdown"
  )

############

plays.master <- data.frame()
for(i in 1:15) {
  data <- cfb_pbp_data(year = 2019, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  plays.master <- bind_rows(plays.master, df)
}

glimpse(plays.master)
head(plays.master)
levels(factor(plays.master$play_type))
plays.master %>% count(play_type, sort = TRUE)

games.master <- cfb_game_info(year = 2019, season_type = "both")
games.master.temp <- games.master %>% mutate(winning_team = case_when(
  home_points > away_points ~ home_team,
  away_points > home_points ~ away_team,
  TRUE ~ "Tie"
))
games.master <- games.master.temp
rm(games.master.temp)

teams.ref <- cfb_team(fbs_all = TRUE)
teams.ref <- teams.ref %>% 
  mutate(power_5 = if_else(conference %in% power_5, TRUE, FALSE))
### Some basic summarisations

# Add First Downs

plays.master2 <- plays.master %>% 
  mutate(play_specifics = play_type) %>% 
  mutate(first_down = 
           case_when(
             play_type %in% scrimmage_plays_all ~
               ifelse(yards_gained >= distance 
                      & lead(down, n=1, order_by = id_play.x) == 1 
                      & lead(offense_play, n = 1, order_by = id_play.x) == offense_play
                      & lead(drive_id, n = 1, order_by = id_play.x) == drive_id 
                      & (!play_specifics %in% scrimmage_plays_turnover), TRUE, 
                      ifelse(play_specifics %in% c("Passing Touchdown", "Rushing Touchdown") 
                             | ((str_detect(play_text, "TOUCHDOWN?") 
                                 | str_detect(play_text, "1ST down?")) 
                                & (!play_specifics %in% scrimmage_plays_turnover)), TRUE, FALSE)),
             TRUE ~ FALSE
           )
  )

plays.master <- plays.master2
rm(plays.master2)

# Add Offense and Defense Conferences
plays.temp <- plays.master %>% 
  left_join(teams.ref[,c("school", "conference")], by = c("offense_play" = "school")) %>% 
  left_join(teams.ref[,c("school", "conference")], by = c("defense_play" = "school")) %>% 
  rename(c("offense_conference" = "conference.x", "defense_conference" = "conference.y"))
plays.master <- plays.temp
rm(plays.temp)

# Add Line Yards Stat

plays.master.temp <- plays.master %>% 
  mutate(line_Yards = 
           case_when(
             rush == 1 & yards_gained < 0 ~ yards_gained * 1.2,
             rush == 1 & yards_gained >= 0 & yards_gained <= 4 ~ yards_gained * 1,
             rush == 1 & yards_gained > 4 & yards_gained <= 10 ~ ((yards_gained-4) * 0.5) + 4,
             rush == 1 & yards_gained > 10 ~ 7,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)

# Add a Pass or Rush Boolean
plays.temp <- plays.master %>% 
  mutate(RunPass =
           case_when(
             rush == 1 ~ "Rush",
             pass == 1 ~ "Pass",
             TRUE ~ "NA"
           )
         )
plays.master <- plays.temp
rm(plays.temp)

##
# Each down rate

each_down_rate <- plays.master %>% filter(pass == 1 | rush == 1) %>% 
  group_by(offense_play, down, offense_conference) %>% 
  summarise(first_down_rate = sum(first_down == TRUE) / n(),
            first_down_rate = if_else(is.na(first_down_rate), 0, first_down_rate),
            pass_rate = sum(pass == TRUE) / n(),
            rush_rate = sum(rush == TRUE) / n(),
            avg_to_go = mean(distance),
            yards_gained = mean(if_else(play_type %in% scrimmage_plays_turnover & yards_gained > 0, yards_gained * -1, yards_gained * 1)), # Corrects for "positive" yards gained when offense turns the ball over
            count = n()) %>% 
  arrange(desc(first_down_rate))

#Rate by Down
each_down_rate_summation <- each_down_rate %>% group_by(down) %>% summarise(mean_first_down_rate = mean(first_down_rate), mean_pass_rate = mean(pass_rate), mean_rush_rate = mean(rush_rate), mean_avg_to_go = mean(avg_to_go), mean_yards_gained = mean(yards_gained))
# each_down_rate_summation %>% mutate(rank_pass_rate = row_number((desc(each_down_rate_summation$mean_pass_rate)))) How to rank stuff for future reference

each_down_rate_summation %>% 
  ggplot() +
  geom_path(aes(x = down, y = mean_first_down_rate)) +
  geom_path(aes(x = down, y = mean_avg_to_go))

# Yards Gained by Offense and Down
each_down_rate %>% group_by(down, offense_play) %>% summarise(mean_yds_gained = mean(yards_gained), mean_first_down_rate = mean(first_down_rate), mean_pass_rate = mean(pass_rate), count = count) %>% arrange(desc(mean_yds_gained))

# Average Yards to Go vs. First Down Rate w/ Pass Rate
each_down_distance_vs_rate <- each_down_rate %>% 
  filter(offense_conference %in% power_5, count > 10) %>% 
  ggplot(aes(x = avg_to_go, y = first_down_rate, colour = pass_rate, size = 2)) +
  geom_point() +
  facet_wrap(vars(down), nrow = 4, ncol = 1) +
  geom_text_repel(data = filter(each_down_rate, # This labels data based on my filter specifications
                                ((first_down_rate < 0.05) |
                                   (down == 4 & first_down_rate < .2))
                                & offense_conference %in% power_5),
                  aes(label = offense_play), color = staturdays_colors("dark_blue"),
                  force = 1, point.padding = 0.1,
                  segment.size = 0.2
  ) +
  scale_color_gradient(low = "#9cbff7", high = staturdays_colors("dark_blue")) + # Sets the gradient color scale for the color level
  scale_size(guide = "none") + # This gets rid of the level "size" from the legend"
  labs(x = "Average Yards to Go", 
       y = "First Down Rate", 
       title = "First Down Conversion Rate vs. \nAverage Yards to Go for each down", 
       subtitle = "2019 Data - Darker bubbles represent \nteams that pass at a higher rate on that down",
       caption = "@staturdays - Data: @CFB_Data",
       color = "Pass Rate") +
  staturdays_theme
each_down_distance_vs_rate

ggsave(filename = "each_down_distance_vs_rate_mobile.png", device = "png", path = "~/Documents/Documents/Kyle/Staturdays/R Plots", width = 200, height = 400, units = "mm") # Save png - you need to run the plot before using the save function

each_down_distance_vs_pass <- each_down_rate %>% 
  filter(offense_conference %in% power_5, count > 10) %>% 
  ggplot(aes(x = avg_to_go, y = pass_rate, colour = yards_gained, size = 2)) +
  geom_point() +
  facet_wrap(vars(down), nrow = 4, ncol = 1) +
  geom_text_repel(data = filter(each_down_rate, # This labels data based on my filter specifications
                                ((pass_rate > 0.75) |
                                   (pass_rate < 0.25) |
                                   (yards_gained > 9) |
                                   (pass_rate < .5 & avg_to_go > 5 & down == 4))
                                & offense_conference %in% power_5),
                  aes(label = offense_play), color = staturdays_colors("dark_blue"),
                  force = 1, point.padding = 0.1,
                  segment.size = 0.2
  ) +
scale_color_gradient(low = "#9cbff7", high = staturdays_colors("dark_blue")) + # Sets the gradient color scale for the color level
  scale_size(guide = "none") + # This gets rid of the level "size" from the legend"
  labs(x = "Average Yards to Go", 
       y = "Pass Rate", 
       title = "Pass Rate vs. Average Yards to Go \nfor each down", 
       subtitle = "2019 Data - Darker bubbles represent teams that gain \nmore yards on that down. Teams that passed > 75% \nor < 25% highlighted, along with teams that \ngained > 9 yards on average (more than 10 attempts)",
       caption = "@staturdays - Data: @CFB_Data",
       color = "Yards Gained") +
  staturdays_theme
each_down_distance_vs_pass

ggsave(filename = "each_down_distance_vs_pass_mobile.png", plot = last_plot(), device = "png", path = "~/Documents/Documents/Kyle/Staturdays/R Plots", width = 200, height = 400, units = "mm", dpi = "retina") # Save png - you need to run the plot before using the save function

each_down_distance_vs_yds_gained <- each_down_rate %>% 
  filter(offense_conference %in% power_5, count > 10) %>% 
  ggplot(aes(x = avg_to_go, y = yards_gained, colour = pass_rate, size = 2)) +
  geom_point(alpha = 0.5) +
  facet_wrap(vars(down), nrow = 4, ncol = 1) +
  geom_text_repel(data = filter(each_down_rate, # This labels data based on my filter specifications
                                ((yards_gained < 0) |
                                   (yards_gained > 9) |
                                   (yards_gained > avg_to_go & down != 4))
                                & offense_conference %in% power_5),
                  aes(label = offense_play), color = staturdays_colors("sign"),
                  force = 3, point.padding = 0.5,
                  segment.size = 0.2, fontface = "bold",
                  nudge_x = -0.5
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "longdash", color = staturdays_colors("orange")) +
  scale_color_gradient(low = "#9cbff7", high = staturdays_colors("dark_blue")) + # Sets the gradient color scale for the color level
  scale_size(guide = "none") + # This gets rid of the level "size" from the legend"
  labs(x = "Average Yards to Go", 
       y = "Yards Gained", 
       title = "Yards Gained vs. Avg. Yards to Go", 
       subtitle = "Teams that gained > 9 yards, < 0, or \navg. 1st down highlighted (min 10 att).\nLSU averaged 9.47 on 543 att. Alabama was 2nd, \nmore than 1 yard shorter and 100 att less\n1st down line added for reference",
       caption = "@staturdays - Data: @CFB_Data",
       color = "Pass Rate") +
  staturdays_theme
each_down_distance_vs_yds_gained

ggsave(filename = "each_down_distance_vs_yds_gained_mobile.png", plot = last_plot(), device = "png", path = "~/Documents/Documents/Kyle/Staturdays/R Plots", width = 200, height = 400, units = "mm", dpi = "retina") # Save png - you need to run the plot before using the save function

# 3rd Down Conversion Rate for each team

third_down_rate <- plays.master %>% filter(down == 3, pass == 1 | rush == 1) %>% 
  group_by(offense_play, offense_conference) %>% 
  summarise(Third_Down_Rate = sum(first_down == TRUE) / n(),
            pass_rate = sum(pass == TRUE) / n(),
            rush_rate = sum(rush == TRUE) / n(),
            avg_to_go = mean(distance)) %>% 
  arrange(desc(Third_Down_Rate))

third_down_distance_vs_rate <- third_down_rate %>% 
  filter(offense_conference %in% power_5) %>% 
  ggplot(aes(x = avg_to_go, y = Third_Down_Rate, colour = pass_rate, size = 2)) +
  geom_point() +
  geom_text_repel(data = filter(third_down_rate, # This labels data based on my filter specifications
                                ((avg_to_go > 8 & Third_Down_Rate > 0.3) | 
                                   (avg_to_go < 6.5 & Third_Down_Rate > 0.45) | 
                                   (Third_Down_Rate > .5) | 
                                   (avg_to_go < 7 & Third_Down_Rate < .375) |
                                   (Third_Down_Rate < 0.3)) 
                                & offense_conference %in% power_5),
                  aes(label = offense_play), color = staturdays_colors("dark_blue"),
                  force = 1, point.padding = 0.1,
                  segment.size = 0.2
                  ) +
  scale_color_gradient(low = "#9cbff7", high = staturdays_colors("dark_blue")) + # Sets the gradient color scale for the color level
  scale_size(guide = "none") + # This gets rid of the level "size" from the legend"
  labs(x = "Average Yards to Go", 
       y = "Third Down Rate", 
       title = "3rd Down Conversion Rate \nvs. Average Yards to Go", 
       subtitle = "2019 Data - Darker bubbles represent teams \nthat pass at a higher rate on 3rd Downs",
       caption = "@staturdays - Data: @CFB_Data",
       color = "Pass Rate,\n3rd Down") +
  staturdays_theme
third_down_distance_vs_rate

ggsave(filename = "third_down_distance_vs_rate_mobile.png", plot = last_plot(), device = "png", path = "~/Documents/Documents/Kyle/Staturdays/R Plots", width = 200, height = 200, units = "mm", dpi = "retina") # Save png - you need to run the plot before using the save function

third_down_pass_vs_rate <- third_down_rate %>% 
  ggplot(aes(x = pass_rate, y = Third_Down_Rate)) +
  geom_point() +
  labs(x = "Pass Rate", 
       y = "Third Down Conversion Rate", 
       title = "Pass Rate by Distance to Go on 3rd Down", 
       subtitle = "2019 Data",
       caption = "@staturdays - Data: @CFB_Data") +
  theme(plot.caption = element_text(size = 10, hjust = 1, color = staturdays_colors("orange")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold"),
        plot.subtitle = element_text(color = staturdays_colors("lightest_blue"), size = 12)
  )
third_down_pass_vs_rate

third_down_pass_vs_distance <- third_down_rate %>% 
  filter(offense_conference %in% power_5) %>% 
  ggplot(aes(x = pass_rate, y = avg_to_go, color = Third_Down_Rate)) +
  geom_point() +
  geom_text_repel(data = filter(third_down_rate, # This labels data based on my filter specifications
                                ((avg_to_go > 8) | 
                                   (pass_rate > .8)) 
                                & offense_conference %in% power_5),
                  aes(label = offense_play), color = staturdays_colors("dark_blue"),
                  force = 1, point.padding = 0.1,
                  segment.size = 0.2
  ) +
  scale_color_gradient(low = "#9cbff7", high = staturdays_colors("dark_blue")) + # Sets the gradient color scale for the color level
  labs(x = "Pass Rate", 
       y = "Average Distance to Go", 
       title = "Pass Rate by Distance to Go on 3rd Down", 
       subtitle = "2019 Data",
       caption = "@staturdays - Data: @CFB_Data") +
  theme(plot.caption = element_text(size = 10, hjust = 1, color = staturdays_colors("orange")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold"),
        plot.subtitle = element_text(color = staturdays_colors("lightest_blue"), size = 12)
        )
third_down_pass_vs_distance

# 4th Down Conversion Rate for each team

fourth_down_rate <- plays.master %>% filter(down == 4, pass == 1 | rush == 1) %>% 
  group_by(offense_play, offense_conference) %>% 
  summarise(Fourth_Down_Rate = sum(first_down == TRUE) / n(),
            pass_rate = sum(pass == TRUE) / n(),
            rush_rate = sum(rush == TRUE) / n(),
            avg_to_go = mean(distance),
            count = n()) %>% 
  arrange(desc(Fourth_Down_Rate))

fourth_down_distance_vs_rate <- fourth_down_rate %>% 
  filter(offense_conference %in% power_5, count > 10) %>% 
  ggplot(aes(x = avg_to_go, y = Fourth_Down_Rate, colour = pass_rate, size = 2)) +
  geom_point() +
  geom_text_repel(data = filter(fourth_down_rate, # This labels data based on my filter specifications
                                ((avg_to_go > 5.5 & Fourth_Down_Rate > 0.4) |
                                   (avg_to_go < 4 & Fourth_Down_Rate < .4) |
                                   (avg_to_go > 5.5 & Fourth_Down_Rate > .5) |
                                   (avg_to_go < 4 & Fourth_Down_Rate > .7) |
                                   (avg_to_go > 4 & Fourth_Down_Rate > .65) |
                                   (avg_to_go > 5 & Fourth_Down_Rate < .35))
                                & offense_conference %in% power_5),
                  aes(label = offense_play), color = staturdays_colors("dark_blue"),
                  force = 1, point.padding = 0.1,
                  segment.size = 0.2
  ) +
  scale_color_gradient(low = "#9cbff7", high = staturdays_colors("dark_blue")) + # Sets the gradient color scale for the color level
  scale_size(guide = "none") + # This gets rid of the level "size" from the legend"
  labs(x = "Average Yards to Go", 
       y = "Fourth Down Rate", 
       title = "4th Down Conversion Rate \nvs. Average Yards to Go", 
       subtitle = "2019 Data - Darker bubbles represent teams \nthat pass at a higher rate on 4th Downs\nMore than 10 attempts",
       caption = "@staturdays - Data: @CFB_Data",
       color = "Pass Rate,\n4th Down") +
  staturdays_theme
fourth_down_distance_vs_rate

ggsave(filename = "fourth_down_distance_vs_rate_mobile.png", plot = last_plot(), device = "png", path = "~/Documents/Documents/Kyle/Staturdays/R Plots", width = 200, height = 200, units = "mm", dpi = "retina") # Save png - you need to run the plot before using the save function

### - Some descriptive statistics

# Does 3rd Down Success indicate winning?

off_rates <- plays.master %>% filter(down == 3, pass == 1 | rush == 1) %>%
  group_by(offense_play, defense_play, game_id) %>%
  summarise(Third_Down_Rate_offense = sum(first_down == TRUE) / n()) %>% 
  group_by(game_id) %>% 
  mutate(Order = seq_along(game_id)) %>% 
  spread(key = Order, value = Third_Down_Rate_offense) %>% 
  filter(is.na(`2`) == TRUE)

def_rates <- plays.master %>% filter(down == 3, pass == 1 | rush == 1) %>%
  group_by(offense_play, defense_play, game_id) %>%
  summarise(Third_Down_Rate_offense = sum(first_down == TRUE) / n()) %>% 
  group_by(game_id) %>% 
  mutate(Order = seq_along(game_id)) %>% 
  spread(key = Order, value = Third_Down_Rate_offense) %>% 
  filter(is.na(`2`) == FALSE)
  
combined_rates <- left_join(off_rates, def_rates, by = "game_id")
combined_rates <- combined_rates %>% select(offense_play.x, defense_play.x, game_id, `1.x`, `2.y`) %>% rename(offense_rate = `1.x`, defense_rate = `2.y`)
combined_rates
rate_by_game <- left_join(combined_rates, games.master, by = c("game_id" = "id")) %>% 
  select(offense_play.x, defense_play.x, game_id, offense_rate, defense_rate, season, week, winning_team) %>% 
  mutate(third_down_winner_game_winner = case_when(
    winning_team == offense_play.x & offense_rate > defense_rate ~ 1,
    winning_team == defense_play.x & defense_rate > offense_rate ~ 1,
    TRUE ~ 0
  ))
sum(rate_by_game$third_down_winner_game_winner)/nrow(rate_by_game)

# How about 3rd down conversion rate plus 4th down scoring rate?

off_3rdand4th_rates <- plays.master %>%
  mutate(third_or_fourth_success = case_when(
    down == 3 & (pass == 1 | rush == 1) & first_down == TRUE ~ 1,
    down == 4 & (pass != 1 & rush != 1) & play_type %in% c("Passing Touchdown", "Rushing Touchdown", "Field Goal Good") ~ 1,
    TRUE ~ 0)) %>% 
  group_by(offense_play, defense_play, game_id) %>%
  filter(down == 3 | down == 4) %>% 
  group_by(offense_play, defense_play, game_id) %>% 
  summarise(third_fourth_rate = sum(third_or_fourth_success) / n()) %>% 
  group_by(game_id) %>% 
  mutate(Order = seq_along(game_id)) %>% 
  spread(key = Order, value = third_fourth_rate) %>% 
  filter(is.na(`2`) == TRUE)

def_3rdand4th_rates <- plays.master %>%
  mutate(third_or_fourth_success = case_when(
    down == 3 & (pass == 1 | rush == 1) & first_down == TRUE ~ 1,
    down == 4 & (pass != 1 & rush != 1) & drive_result %in% c("Passing Touchdown", "Rushing Touchdown", "Field Goal Good") ~ 1,
    TRUE ~ 0)) %>% 
  group_by(offense_play, defense_play, game_id) %>%
  filter(down == 3 | down == 4) %>% 
  group_by(offense_play, defense_play, game_id) %>% 
  summarise(third_fourth_rate = sum(third_or_fourth_success) / n()) %>% 
  group_by(game_id) %>% 
  mutate(Order = seq_along(game_id)) %>% 
  spread(key = Order, value = third_fourth_rate) %>% 
  filter(is.na(`2`) == FALSE)

combined_rates <- left_join(off_3rdand4th_rates, def_3rdand4th_rates, by = "game_id")
combined_rates <- combined_rates %>% select(offense_play.x, defense_play.x, game_id, `1.x`, `2.y`) %>% rename(offense_rate = `1.x`, defense_rate = `2.y`)
combined_rates
rate_by_game <- left_join(combined_rates, games.master, by = c("game_id" = "id")) %>% 
  select(offense_play.x, defense_play.x, game_id, offense_rate, defense_rate, season, week, winning_team) %>% 
  mutate(third_down_winner_game_winner = case_when(
    winning_team == offense_play.x & offense_rate > defense_rate ~ 1,
    winning_team == defense_play.x & defense_rate > offense_rate ~ 1,
    TRUE ~ 0
  ))
sum(rate_by_game$third_down_winner_game_winner)/nrow(rate_by_game)

## Calculate any first down probability based on game situation

first_down_prob_df <- data.frame()
for (j in 1:4){
for (i in 1:30){
first_down_prob <- plays.master %>% filter(down == j, distance == i, play_specifics %in% scrimmage_plays_all) %>% summarise(first_prob = (sum(first_down == TRUE)/length(first_down)))
first_down_prob <- first_down_prob %>% mutate(down = j, distance = i)
first_down_prob_df <- rbind(first_down_prob_df, first_down_prob)
}
}
first_down_prob_df

## Do coaches get scared of passing on 2nd down after throwing incomplete on 1st down - No

next_play_pass_incomplete <- plays.master %>% group_by(drive_id) %>% 
  mutate(pass_incomplete = case_when(
    down == 1 & pass == 1 & yards_gained == 0 ~ 1,
    TRUE ~ 0
  ),
  rush_incomplete = case_when(
    down == 1 & rush == 1 & yards_gained == 0 ~ 1,
    TRUE ~ 0
  )
  ) %>% 
  mutate(next_play_type = lead(pass, n = 1L, order_by = id_play.x), 
         next_play_yards_gained = lead(yards_gained, n = 1L, order_by = id_play.x)
         )

next_play_pass_incomplete %>% group_by(pass_incomplete) %>% 
  summarise(pass_again = mean(next_play_type, na.rm = T), 
            yards_gained_avg = mean(next_play_yards_gained, na.rm = T))

next_play_pass_incomplete %>% 
  group_by(offense_play,pass_incomplete) %>% 
  summarise(pass_again = mean(next_play_type, na.rm = T), 
            yards_gained_avg = mean(next_play_yards_gained, na.rm = T),
            count = n()
            )

# Data by individual team
next_play_pass_incomplete %>% 
  filter(offense_conference %in% power_5) %>% 
  group_by(offense_play,pass_incomplete) %>% 
  summarise(pass_again = mean(next_play_type, na.rm = T)
  ) %>% 
  pivot_wider(names_from = pass_incomplete, values_from = pass_again, names_prefix = c("pass_complete", "pass_incomplete")) %>% 
  mutate(diff = pass_incomplete1 - pass_complete0)

# Data for rushing failures on 1st and 10
next_play_pass_incomplete %>% group_by(rush_incomplete) %>% 
  summarise(pass_again = mean(next_play_type, na.rm = T), 
            yards_gained_avg = mean(next_play_yards_gained, na.rm = T))
