{# Get CFB Data API for Play by Play - Use to generate expected points added (EPA)
rm(list=ls())
library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(gt)
library(lubridate)
library(ggimage)

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

staturdays_palette <- c("#041e42", "#22345a", "#394871", "#4c5872", "#5c6272", "#de703b")

staturdays_colors <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (staturdays_col_list)
  
  staturdays_col_list[cols]
}

staturdays_theme <- theme(plot.caption = element_text(size = 12, hjust = 1, color = staturdays_colors("orange")), 
                          plot.title = element_text(color = staturdays_colors("dark_blue"), size = 30, face = "bold"),
                          plot.subtitle = element_text(color = staturdays_colors("light_blue"), size = 20),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 14),
                          axis.title = element_text(color = staturdays_colors("lighter_blue"), size = 16, face = "bold"),
                          legend.title = element_text(color = staturdays_colors("lighter_blue"), size = 16, face = "bold"),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 14)#,
                          #panel.background = element_blank(),
                          #panel.grid = element_line(color = staturdays_colors("lightest_blue"))
)

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
    "Fumble Return Touchdown",
    "Safety"
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

no_action_plays <- 
  c(
    "Timeout",
    "End Period",
    "End of Half",
    "End of Game",
    "Kickoff"
  )

scrimmage_plays_kicks <- 
  c(
    "Punt",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Field Goal Good",
    "Blocked Field Goal",
    "Missed Field Goal Return"
  )

base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

plays.master = data.frame()
for (j in 2020:2020) {
  for (i in 1:15) {
    cat('Loading Plays', j, 'Week', i, '\n')
    full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
    full_url_plays_encoded <- URLencode(full_url_plays) # If there are spaces in query, formats them correctly
    plays <- fromJSON(getURL(full_url_plays_encoded)) # Pull in API using url
    if(is_empty(plays) == F){
      clockcolumns <- plays %>% unnest_legacy(clock) # Takes clock data out as it's own columns
      plays <- plays %>%
        select(-clock) %>%
        as_tibble() %>%
        mutate(minutes = clockcolumns$minutes, seconds = clockcolumns$seconds) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
        mutate_at(c("minutes", "seconds"), ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
      plays <- tibble(plays)
      plays$week = i
      plays$year = j
      plays.master = rbind(plays.master, plays, make.row.names=TRUE)
    }
  }
}
rm(clockcolumns, plays)

games.master = data.frame()
for (j in 2020:2020) {
  for (i in 1:15) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

drives.master = data.frame()
for (j in 2020:2020) {
  cat('Loading Drives', j, '\n')
  full_url_drives <- paste0(base_url_drives, "seasonType=both&", "year=", as.character(j))
  full_url_drives_encoded <- URLencode(full_url_drives)
  drives <- fromJSON(getURL(full_url_drives_encoded))
  start_time_columns <- as_tibble(drives$start_time) # Takes clock data out as it's own columns
  end_time_columns <- as_tibble(drives$end_time)
  elapsed_time_columns <- as_tibble(drives$elapsed)
  drives <- drives %>% 
    select(-start_time, -end_time, -elapsed) %>% 
    as_tibble() %>% 
    mutate(start_minutes = start_time_columns$minutes, start_seconds = start_time_columns$seconds, 
           end_minutes = end_time_columns$minutes, end_seconds = end_time_columns$seconds,
           elapsed_minutes = elapsed_time_columns$minutes, elapsed_seconds = elapsed_time_columns$seconds,) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
    mutate_at(
      c("start_minutes", "start_seconds", "end_minutes", "end_seconds", "elapsed_minutes", "elapsed_seconds"),
      ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
  drives <- as_tibble(drives)
  drives.master = rbind(drives.master, drives)
}

# # Join Plays with Games to get additional info
# games.temp <- games.master
# games.temp <- mutate(games.temp, id = as.character(id))
# plays.temp <- plays.master
# plays.temp <- mutate(plays.temp, playid = id, id = substr(playid, 1, 9))
# plays_games_joined.master <- left_join(plays.master, games.temp, by = 'id') # Get regular or post season from games data
# games.temp <- data.frame()

# # Join Plays with Drives to Get Start Yardline
# drives.temp <- drives.master
# drives.temp <- mutate(drives.temp, id = as.character(id))
# plays.temp <- plays.master
# plays.temp <- mutate(plays.temp, playid = id, driveid = substr(playid, 1, 10))
# plays_drives.master <- left_join(plays.temp, drives.temp, by = c("drive_id" = "id"))
# drives.temp <- data.frame()
# plays.temp <- data.frame()

# Adjust yards_gained and start_yardline stats
# plays_drives.master <- plays_drives.master %>% 
#   mutate(start_yardline = if_else(offense.x != home, 100 - start_yardline, as.double(start_yardline)), end_yardline = if_else(offense.x != home, 100 - end_yardline, as.double(end_yardline)))

## Mutate Plays with First Downs (Smart), Pass/Rush, Success
plays.master_temp <- plays.master %>% 
  mutate(play_specifics = play_type) %>% 
  mutate(first_down = 
           case_when(
             play_type %in% scrimmage_plays_all ~
               ifelse(yards_gained >= distance 
                      & lead(down, n=1, order_by = id) == 1 
                      & lead(offense, n = 1, order_by = id) == offense 
                      & lead(drive_id, n = 1, order_by = id) == drive_id 
                      & (!play_specifics %in% scrimmage_plays_turnover), TRUE, 
                      ifelse(play_specifics %in% c("Passing Touchdown", "Rushing Touchdown") 
                             | ((str_detect(play_text, "TOUCHDOWN") 
                                 | str_detect(play_text, "1ST down")) 
                                & (!play_specifics %in% scrimmage_plays_turnover)), TRUE, FALSE)),
             TRUE ~ NA
           )
  )

plays.master <- plays.master_temp
rm(plays.master_temp)
# Calculate loss of yards on turnovers
plays.master_temp <- plays.master %>% 
  mutate(turnover_yards = case_when(play_type %in% scrimmage_plays_turnover ~ 70L - lead(yards_to_goal, order_by = id), #subtract starting field position of next drive from avg. starting field position to find how many yards that turnover cost you
                                    TRUE ~ 0L))

plays.master_temp <- plays.master_temp %>% mutate(yards_gained = case_when(play_type %in% scrimmage_plays_turnover ~ -turnover_yards,
                                                                           TRUE ~ yards_gained))

plays.master <- plays.master_temp
rm(plays.master_temp)

# Calculate avg. starting field pos on turnovers vs. normal
drives.master %>% summarise(mean(start_yards_to_goal)) # 70 avg.

plays.master %>% mutate(lead_start_yards_to_goal = 
                          case_when(play_type %in% scrimmage_plays_turnover ~ lead(yards_to_goal, order_by = id),
                                    TRUE ~ 0L)) %>% 
  filter(play_type %in% scrimmage_plays_turnover) %>% 
  group_by(play_type) %>% 
  filter(!str_detect(play_type,"Touchdown")) %>% 
  summarise(avg_yds_to_goal = mean(lead_start_yards_to_goal), count = n()) # 54.5 avg, so a turnover is worth -15.5 yds. vs. avg.

# Classify the multiple play types into a simpler just rush or pass
plays.master$pass_rush[plays.master$play_type %in% scrimmage_plays_pass] <- "Pass"
plays.master$pass_rush[plays.master$play_type %in% scrimmage_plays_rush] <- "Rush"
# Rush Fumble Rows
rush_rows <- plays.master %>% 
  filter(play_specifics %in% c("Fumble Recovery (Own)", 
                               "Fumble Recovery (Opponent)", 
                               "Fumble Return Touchdown", 
                               "Safety"), 
         str_detect(play_text, "run") 
         & !str_detect(play_text, "kick") 
         & !str_detect(play_text, "punt")) %>% 
  mutate(pass_rush = "Rush")
# Pass Fumble Rows
pass_rows <- plays.master %>% 
  filter(play_specifics %in% c("Fumble Recovery (Own)", "Fumble Recovery (Opponent)"), str_detect(play_text, "pass") | str_detect(play_text, "sack")) %>% 
  mutate(pass_rush = "Pass")
# Change fumbles to a pass or rush
plays.master[which(plays.master$id %in% rush_rows$id), "pass_rush"] <- "Rush"
plays.master[which(plays.master$id %in% pass_rows$id), "pass_rush"] <- "Pass"

#Add Success Column
plays.master_temp <- plays.master %>% 
  mutate(success = 
           case_when(
             play_type %in% scrimmage_plays_turnover ~ 0,
             down == 1 & yards_gained >= 0.5 * distance ~ 1,
             down == 2 & yards_gained >= 0.7 * distance ~ 1,
             down == 3 & yards_gained >= 1 * distance ~ 1,
             down == 4 & yards_gained >= 1 * distance ~ 1,
             str_detect(play_text, "1ST down") == TRUE ~ 1,
             str_detect(play_text, "TD") == TRUE ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master_temp
rm(plays.master_temp)
##

## Add Line Yards Stat

plays.master.temp <- plays.master %>% 
  mutate(lineYards = 
           case_when(
             pass_rush == "Rush" & yards_gained < 0 ~ yards_gained * 1.2,
             pass_rush == "Rush" & yards_gained > 0 & yards_gained <= 4 ~ yards_gained * 1,
             pass_rush == "Rush" & yards_gained > 4 & yards_gained <= 10 ~ 4 + yards_gained * 0.5,
             pass_rush == "Rush" & yards_gained > 10 ~ 7,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

## Garbage Time Indicator WIP

plays.master.temp <- plays.master %>% 
  mutate(garbage_time = 
           case_when(
             period == 2 & abs(offense_score - defense_score) > 38 ~ 1,
             period == 3 & abs(offense_score - defense_score) > 28 ~ 1,
             period == 4 & abs(offense_score - defense_score) > 22 ~ 1,
             period == 4 & minutes < 2 & abs(offense_score - defense_score) > 16 ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

# Plays by percentile to help guide defining explosive
percentile_explosiveness <- plays.master %>% group_by(pass_rush) %>% 
  summarise(avg_yds = mean(yards_gained), 
            stand_dev = sd(yards_gained), 
            percentile = quantile(yards_gained, .90))

explosive_pass <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Pass") %>% pull(percentile))
explosive_rush <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Rush") %>% pull(percentile))

# Passing vs. Standard Downs, Explosive Plays
plays.master.temp <- plays.master %>% 
  mutate(passing_down = case_when(down == 2 & distance >= 7 ~ 1,
                                  down == 3 & distance >= 5 ~ 1,
                                  down == 4 & distance >= 5 ~ 1,
                                  TRUE ~ 0),
         explosive = case_when(play_specifics %in% scrimmage_plays_turnover ~ 0,
                               yards_gained >= explosive_pass 
                               & play_specifics %in% scrimmage_plays_non_turnover 
                               & pass_rush == "Pass" ~ 1,
                               yards_gained >= explosive_rush
                               & play_specifics %in% scrimmage_plays_non_turnover 
                               & pass_rush == "Rush" ~ 1,
                               TRUE ~ 0))

plays.master <- plays.master.temp
rm(plays.master.temp)

# Create a master clock completely in seconds
plays.master.temp <- plays.master %>% 
  mutate(clock_in_seconds = 2700-(900*(period-1)) + minutes*60 + seconds)

plays.master <- plays.master.temp
rm(plays.master.temp)


# Summary Stats -----------------------------------------------------------
team_colors <- fromJSON(getURL("https://api.collegefootballdata.com/teams/fbs?year=2020"))

team_colors <- team_colors %>% unnest(cols = logos) %>% 
  mutate(logo_color = if_else(str_detect(logos, "dark"), "dark", "light")) %>% 
  pivot_wider(names_from = logo_color, values_from = logos)

# Calculate league averages
plays.master_succ <- plays.master %>% 
  filter(!play_specifics %in% no_action_plays) %>% 
  filter(!play_specifics %in% c("Punt", "Blocked Punt", "Blocked Punt Touchdown")) %>% 
  group_by(year) %>% 
  mutate(avg_succ_rate = mean(success)) %>% 
  group_by(down) %>% 
  mutate(avg_succ_down = mean(success)) %>% 
  relocate(down, .after = avg_succ_down)

# Team Success Rate
top <- plays.master_succ %>% 
  group_by(offense, down) %>% 
  summarise(off_succ_rate = mean(success), off_play_count = n())

bottom <- plays.master_succ %>% 
  group_by(defense, down) %>% 
  summarise(def_succ_rate = mean(success), def_play_count = n())

team_succ_rate <- left_join(top, bottom, by = c("offense" = "defense", "down")) %>% 
  mutate(net_success = off_succ_rate - def_succ_rate) %>% 
  rename(team = offense) %>% 
  left_join(team_colors, by = c("team" = "school"))

# Starting Field Position
off_field_pos <- drives.master %>% 
  group_by(offense, offense_conference) %>% 
  summarise(avg_start_field_pos = mean(start_yards_to_goal))

def_field_pos <- drives.master %>% 
  group_by(defense, defense_conference) %>% 
  summarise(avg_start_field_pos = mean(start_yards_to_goal))

field_pos <- left_join(off_field_pos, def_field_pos, by = c("offense" = "defense"), suffix = c("_off", "_def")) %>% 
  mutate(net_field_pos = avg_start_field_pos_def - avg_start_field_pos_off)

# Success Rate on passing downs
pass_down_success <- plays.master %>% 
  filter(!play_specifics %in% no_action_plays) %>% 
  filter(!play_specifics %in% c("Punt", "Blocked Punt", "Blocked Punt Touchdown")) %>% 
  group_by(offense, offense_conference, passing_down, pass_rush) %>% 
  summarise(succ_rate = mean(success), count = n())

# Explosive Rate
explosive_rate <- plays.master %>% 
  filter(!play_specifics %in% no_action_plays, is.na(pass_rush) == F) %>% 
  filter(!play_specifics %in% c("Punt", "Blocked Punt", "Blocked Punt Touchdown")) %>% 
  group_by(offense, offense_conference) %>% 
  mutate(team_explosive_rate = mean(explosive)) %>% 
  group_by(offense, offense_conference, pass_rush) %>% 
  summarise(explosive_rate = mean(explosive), team_explosive_rate = mean(team_explosive_rate), count = n())
}
# Plots -------------------------------------------------------------------

conf_name <- "Big Ten"
down_num <- 1

# Team Success Rate Plot
plays.master_succ %>% 
  filter(offense == "Penn State") %>% 
  group_by(offense, down) %>% 
  summarise(succ_rate = mean(success), mean(avg_succ_rate), mean(avg_succ_down), count = n()) %>% 
  ggplot() +
  geom_col(aes(x = down, y = succ_rate)) +
  geom_hline(yintercept = plays.master_succ$avg_succ_rate, linetype = "dashed", color = staturdays_colors("orange")) +
  geom_label(aes(x = down, y = succ_rate, label = paste0("Plays: ", count)))

# All Teams Offense Success Rate Plot
team_succ_rate %>% 
  ungroup() %>% 
  filter(conference == conf_name) %>% 
  group_by(down) %>% 
  mutate(first_rank = rank(desc(off_succ_rate), ties.method = "min")) %>% 
  group_by(team) %>% 
  ggplot(aes(x = first_rank, y = off_succ_rate, fill = color)) +
  geom_col(position = "dodge") +
  geom_image(aes(image = light), size = .1, by = "width", asp = 1, nudge_y = .01) +
  theme(aspect.ratio = 1) +
  facet_wrap(vars(down)) +
  scale_x_reverse() +
  scale_fill_identity() +
  geom_label(aes(label = off_play_count), nudge_y = -.25, size = 3, fill = "white") +
  labs(title = paste0(conf_name," Success Rate through \nWeek 2"),
       subtitle = "Percent of plays successful and # of Plays",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
       x = "Ranking",
       y = "Success Rate") +
  staturdays_theme +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  

ggsave(filename = paste0(conf_name, "_success", "_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       dpi = 300, width = 200, height = 200, units = "mm")

# All Teams Defense Success Rate Plot
team_succ_rate %>% 
  ungroup() %>% 
  filter(conference == conf_name) %>% 
  group_by(down) %>% 
  mutate(first_rank = rank((def_succ_rate), ties.method = "min")) %>% 
  group_by(team) %>% 
  ggplot(aes(x = first_rank, y = def_succ_rate, fill = color)) +
  geom_col(position = "dodge") +
  geom_image(aes(image = light), size = .1, by = "width", asp = 1, nudge_y = .01) +
  theme(aspect.ratio = 1) +
  facet_wrap(vars(down)) +
  scale_x_reverse(breaks = c(1,5,10,14)) +
  scale_fill_identity() +
  geom_label(aes(label = def_play_count), nudge_y = -.25, size = 3, fill = "white") +
  labs(title = paste0(conf_name," Defense Success Rate through \nWeek 2"),
       subtitle = "Percent of plays successful and # of Plays\nLower is better",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
       x = "Ranking",
       y = "Success Rate") +
  staturdays_theme +
  scale_y_continuous(labels = percent, limits = c(0, 1))

ggsave(filename = paste0(conf_name, "_def_success", "_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       dpi = 300, width = 200, height = 200, units = "mm")

# Passing Down Success Rate by Play Type Plot
pass_down_success %>% 
  filter(offense_conference == "ACC", is.na(pass_rush) == F) %>% 
  ggplot(aes(x = as.factor(passing_down), y = succ_rate, fill = pass_rush)) + 
  geom_col(position = "dodge") +
  facet_wrap(vars(offense))

# Explosive Rate Plot
explosive_plot <- explosive_rate %>% 
  pivot_wider(names_from = pass_rush, values_from = c(explosive_rate, count)) %>% 
  left_join(team_colors, by = c("offense" = "school")) %>% 
  filter(conference %in% conf_name) %>% 
  ggplot(aes(x = explosive_rate_Pass, y = explosive_rate_Rush)) +
  geom_image(aes(image = light), size = .1, by = "width", asp = 1, alpha = 0.8) +
  theme(aspect.ratio = 1) +
  scale_x_continuous(labels = percent, limits = c(0, .2)) +
  scale_y_continuous(labels = percent, limits = c(0, .2)) +
  geom_abline(linetype = "dashed", color = staturdays_colors("orange")) +
  annotate(geom = "label", x = .02, y = .18, label = "Explosive \nRushing", 
           fill = staturdays_colors("orange"), color = "white", alpha = .75) +
  annotate(geom = "label", x = .16, y = .02, label = "Explosive \nPassing", 
           fill = staturdays_colors("orange"), color = "white", alpha = .75) +
  labs(title = "Explosiveness on Offense",
       subtitle = "Percent of explosive runs and passes,\ndefined as 90th percentile plays",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
       x = paste0("Explosive Pass Rate (>= ", explosive_pass," yds)"),
       y = paste0("Explosive Rush Rate (>= ", explosive_rush," yds)")) +
  staturdays_theme

ggsave(filename = paste0(conf_name, "_explosive_plot", "_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       plot = explosive_plot,
       dpi = 300, width = 200, height = 200, units = "mm")

# Turnover Yards Plot
turnover_yards <- plays.master %>% 
  filter(play_type %in% scrimmage_plays_turnover, offense_conference %in% conf_name) %>% 
  group_by(offense) %>% 
  summarise(avg_turnover_yards = -mean(turnover_yards), count = n()) %>% 
  left_join(team_colors, by = c("offense" = "school")) %>% 
  ggplot(aes(x = avg_turnover_yards, y = count)) +
  geom_image(aes(image = light), size = .1, by = "width", asp = 1, alpha = 0.8) +
  theme(aspect.ratio = 1) +
  scale_x_continuous() +
  scale_y_continuous(limits = c(0, 15)) +
  annotate(geom = "label", x = -10, y = 14, label = "Turnovers in \nfavorable positions",
           fill = staturdays_colors("orange"), color = "white", alpha = 0.75) +
  annotate(geom = "label", x = -40, y = 14, label = "Turnovers in \nunfavorable positions",
           fill = staturdays_colors("orange"), color = "white", alpha = 0.75) +
  annotate(geom = "label", x = -25, y = 10, label = "Average starting field position is at \nown 30. A turnover that puts opponent at \ntheir own 40 would be -10 Turnover Yds.",
           fill = staturdays_colors("light_blue"), color = "white", alpha = 0.75, size = 3) +  
  staturdays_theme +
  labs(title = paste0(conf_name, " Average Turnover Yards"),
       subtitle = "Free yards given up to opponents\non turnovers",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
       x = "Average Net Yards on Turnovers",
       y = "# of Turnovers")

ggsave(filename = paste0(conf_name, "_turnover_yards", "_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       plot = turnover_yards,
       dpi = 300, width = 200, height = 200, units = "mm")

# Net Field Position Plot
field_pos_plot <- field_pos %>% 
  filter(offense_conference %in% conf_name) %>% 
  group_by(offense) %>% 
  left_join(team_colors, by = c("offense" = "school")) %>% 
  ungroup() %>% 
  mutate(first_rank = rank(desc(net_field_pos), ties.method = "min")) %>% 
  ggplot(aes(x = net_field_pos, y = first_rank)) +
  geom_image(aes(image = light), size = .1, by = "width", asp = 1, alpha = 0.8) +
  theme(aspect.ratio = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = staturdays_colors("orange")) +
  annotate(geom = "label", x = -7, y = 4, label = "Worse field position \nthan opponents",
           fill = staturdays_colors("orange"), color = "white") +
  annotate(geom = "label", x = 7, y = 12, label = "Better field position \nthan opponents",
           fill = staturdays_colors("orange"), color = "white") +
  labs(title = paste0(conf_name, " Net Field Positions"),
       subtitle = "Negative is bad",
       x = "Net Field Position",
       y = "Rank") +
  staturdays_theme

ggsave(filename = paste0(conf_name, "_field_pos_plot", "_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       plot = field_pos_plot,
       dpi = 300, width = 200, height = 200, units = "mm")
