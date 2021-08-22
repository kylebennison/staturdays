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
library(grid)
library(png)
library(bit64)
library(data.table)
library(ggrepel)

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")  
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Play%20Types%20and%20Power%20Conference%20Names.R")


base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of

# Read in existing 2020 plays data
plays.historic <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/2020_plays_ytd.csv")

# Make ids characters
plays.master.temp <- plays.historic %>% 
  mutate(id = as.character(id), game_id = as.character(game_id), drive_id = as.character(drive_id))

plays.historic <- plays.master.temp
rm(plays.master.temp)

# Get max week from data to see what to check for from the cfbdata API
max_week <- plays.historic %>% pull(week) %>% max()
start_week <- max_week + 1

plays.master = data.frame()
for (j in 2020:2020) {
  for (i in start_week:15) {
    cat('Loading Plays', j, 'Week', i, '\n')
    full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
    full_url_plays_encoded <- URLencode(full_url_plays) # If there are spaces in query, formats them correctly
    plays <- cfbd_api(full_url_plays_encoded, my_key)
    plays$week = i
    plays$year = j
    plays.master = rbind(plays.master, plays, make.row.names=TRUE)
    }
}

# Rename columns to match historic
plays.master.temp <- plays.master %>% 
  rename(minutes = clock.minutes,
         seconds = clock.seconds) %>% 
  select(-wallclock)

plays.master <- plays.master.temp

rm(plays.master.temp)

# Plays by percentile to help guide defining explosive
percentile_explosiveness <- plays.historic %>% group_by(pass_rush) %>% 
  summarise(avg_yds = mean(yards_gained), 
            stand_dev = sd(yards_gained), 
            percentile = quantile(yards_gained, .90))

explosive_pass <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Pass") %>% pull(percentile))
explosive_rush <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Rush") %>% pull(percentile))

# If there are new plays from this past week of games, run all the below calculations
if(is_empty(plays.master) == F){
  
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
  #drives.master %>% summarise(mean(start_yards_to_goal)) # 70 avg.
  
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
  
  # Read in expected success rates
  success_expected <- fread("expected_success_rate.csv")
  
  # Join to plays
  plays.master_temp <- plays.master %>% 
    left_join(success_expected, by = c("down", "distance")) %>% 
    select(-count)
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
  
  # Initial write of 2020 plays to csv
  # fwrite(plays.master, file = "C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays/2020_plays_ytd.csv", append = FALSE, col.names = TRUE)
  
  # Convert plays.master ids to characters
  plays.master_temp <- plays.master %>% 
    mutate(id = as.character(id), game_id = as.character(game_id), drive_id = as.character(drive_id))
  
  plays.master <- plays.master_temp
  rm(plays.master_temp)
}

# Join historic and new plays locally for the rest of the code
plays.master <- rbind(plays.historic, plays.master)


# Add Quarterback names ---------------------------------------------------

# CFBSCrapr method
# plays_temp <- plays.master %>% 
# mutate(
#   pass_player = 
#     ifelse(.data$pass_rush == "Pass" & .data$play_type != "Passing Touchdown", 
#            stringr::str_extract(.data$play_text, 
#                                 "pass from (.*?) \\(|(.{0,30} )pass |(.+) sacked by|(.+) sacked for|(.{0,30} )incomplete "), NA_character_),
#   pass_player = stringr::str_remove(.data$pass_player, "pass | sacked by| sacked for| incomplete"),
#   pass_player = dplyr::if_else(.data$play_type == "Passing Touchdown", 
#                                stringr::str_extract(.data$play_text, "pass from(.+)"), .data$pass_player),
#   pass_player = stringr::str_remove(.data$pass_player, "pass from "), 
#   pass_player = stringr::str_remove(.data$pass_player, "\\(.+\\)"),
#   pass_player = stringr::str_remove(.data$pass_player, " \\,"),
#   pass_player = ifelse(.data$play_type == "Passing Touchdown" & is.na(.data$pass_player),
#                        stringr::str_extract(.data$play_text, "(.+)pass complete to"), .data$pass_player),
#   pass_player = stringr::str_remove(.data$pass_player, " pass complete to(.+)"),
#   pass_player = stringr::str_remove(.data$pass_player, " pass complete to"),
#   pass_player = ifelse(.data$play_type == "Passing Touchdown" & is.na(.data$pass_player),
#                        stringr::str_extract(.data$play_text, "(.+)pass,to"), .data$pass_player),
#   pass_player = stringr::str_remove(.data$pass_player, " pass,to(.+)"),
#   pass_player = stringr::str_remove(.data$pass_player, " pass,to"))

# Check the data
# plays_temp %>% filter(pass_rush == "Pass") %>% count(is.na(pass_player))

## Explaining the regex

# Start of string, either upper or lower case a-z or literal . - or ' for as many characters as there are, 
# literal space, repeat it all over again for the last name and that's it
# ---
# ^[A-Z|a-z|\\.|\\-|\\']+\\s[A-Z|a-z|\\.|\\-|\\']+

plays_temp_fast <- plays.master %>% 
  mutate(pass_player = if_else(pass_rush == "Pass",
                               str_extract(play_text, "^[A-Z|a-z|\\.|\\-|\\']+\\s[A-Z|a-z|\\.|\\-|\\']+"),
                               NA_character_),
         rush_player = if_else(pass_rush == "Rush",
                               str_extract(play_text, "^[A-Z|a-z|\\.|\\-|\\']+\\s[A-Z|a-z|\\.|\\-|\\']+"),
                               NA_character_))

# Checks to make sure data looks okay
# plays_temp_fast %>% filter(pass_rush == "Pass") %>% count(is.na(pass_player))
# 
# plays_temp_fast %>% filter(pass_rush == "Pass" & !is.na(pass_player)) %>% select(play_text, pass_player) %>% View()
# 
# plays_temp_fast %>% filter(pass_rush == "Pass" & !is.na(pass_player)) %>% select(pass_player) %>% unique() %>% View()

plays.master <- plays_temp_fast

rm(plays_temp_fast)


# Analysis ----------------------------------------------------------------

# Add game states
qb_plays <- plays.master %>% 
  mutate(losing_game = if_else(offense_score < defense_score, TRUE, FALSE),
         last_two_minutes = if_else(clock_in_seconds <= 120, TRUE, FALSE),
         close_game = if_else(abs(offense_score - defense_score) < 11, TRUE, FALSE))

# Calculate passing metrics
qb_plays <- qb_plays %>% 
  group_by(pass_player) %>% 
  filter(n() > 100, pass_rush == "Pass", is.na(pass_player) == FALSE) %>% 
  mutate(pass_attempt = if_else(play_type %in% scrimmage_plays_pass & play_type != "Sack", 1, 0),
         pass_completion = if_else(play_type %in% c("Pass Reception", "Pass Completion", "Passing Touchdown"), 1, 0),
         pass_touchdown = if_else(play_type == "Passing Touchdown", 1, 0),
         pass_intercepted = if_else(play_type %in% c("Pass Interception", "Pass Interception Return", "Interception Return Touchdown"), 1, 0),
         passer_sacked = if_else(play_type == "Sack", 1, 0))

# Build Graph
qb_grouped_summary <- qb_plays %>% 
  group_by(losing_game, pass_player) %>% 
  summarise(completion_rate = sum(pass_completion)/sum(pass_attempt),
            touchdown_rate = sum(pass_touchdown)/sum(pass_attempt),
            interception_rate = sum(pass_intercepted)/sum(pass_attempt),
            sack_rate = sum(passer_sacked)/n(),
            success_rate = sum(success) /n(),
            n = n())

# Inconsistent Passers when losing
qb_grouped_summary %>% 
  mutate(comp_rate_vs_median = completion_rate - median(qb_grouped_summary$completion_rate),
         int_rate_vs_median = interception_rate - median(qb_grouped_summary$interception_rate)) %>% 
  arrange(pass_player, losing_game) %>% 
  group_by(pass_player) %>% 
  mutate(cmp_rate_diff = comp_rate_vs_median - lag(comp_rate_vs_median, 1L),
         int_rate_diff = int_rate_vs_median - lag(int_rate_vs_median, 1L)) %>% 
  filter(n > 30) %>% 
  group_by(pass_player) %>% 
  mutate(n_2 = n()) %>% 
  filter(n_2 == 2) %>% 
  filter(losing_game == T) %>% 
  ggplot(aes(x = cmp_rate_diff, y = int_rate_diff)) +
  geom_point(aes(alpha = if_else(abs(cmp_rate_diff) > .15 | abs(int_rate_diff) > .05, 0.9, 0.1)),
             color = staturdays_colors("orange")) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_vline(xintercept = 0, alpha = .5) +
  geom_text_repel(aes(label = if_else(abs(cmp_rate_diff) > .15 | abs(int_rate_diff) > .05, pass_player, "")),
                  max.overlaps = 20) +
  theme(legend.position = "none") +
  annotate(geom = "text", x = .25, y = .15, label = "Takes More Risks", color = staturdays_colors("dark_blue"), fontface = "bold", size = 5) +
  annotate(geom = "text", x = .25, y = -.15, label = "Up Their Game", color = staturdays_colors("dark_blue"), fontface = "bold", size = 5) +
  annotate(geom = "text", x = -.25, y = .15, label = "Makes More Mistakes", color = "darkred", fontface = "bold", size = 5) +
  annotate(geom = "text", x = -.25, y = -.15, label = "Plays More Conservative", color = "darkred", fontface = "bold", size = 5) +
  labs(title = "Most Inconsistent QBs When Playing Down",
       subtitle = "Minimum 30 attempts in 2020",
       x = "Change in Completion Rate",
       y = "Change in Interception Rate") +
  xlim(-.3, .3) +
  ylim(-.2, .2) +
  staturdays_theme

ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
                         today(),
                         "_",
                         "losing_inconsistent",
                         ".jpeg"),
       plot = last_plot(),
       width = 400,
       height = 200,
       dpi = 300,
       units = "mm")
  

# Consistent Passers when losing
qb_grouped_summary %>% 
  mutate(comp_rate_vs_median = completion_rate - median(qb_grouped_summary$completion_rate),
         int_rate_vs_median = interception_rate - median(qb_grouped_summary$interception_rate)) %>% 
  arrange(pass_player, losing_game) %>% 
  group_by(pass_player) %>% 
  mutate(cmp_rate_diff = comp_rate_vs_median - lag(comp_rate_vs_median, 1L),
         int_rate_diff = int_rate_vs_median - lag(int_rate_vs_median, 1L)) %>% 
  filter(n > 30) %>% 
  group_by(pass_player) %>% 
  mutate(n_2 = n()) %>% 
  filter(n_2 == 2) %>% 
  filter(losing_game == T) %>% 
  ggplot(aes(x = cmp_rate_diff, y = int_rate_diff)) +
  geom_point(aes(alpha = if_else(abs(cmp_rate_diff) < .02, 0.9, 0.1)),
             color = staturdays_colors("orange")) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_vline(xintercept = 0, alpha = .5) +
  geom_text_repel(aes(label = if_else(abs(cmp_rate_diff) < .02, pass_player, "")),
                  max.overlaps = 50) +
  theme(legend.position = "none") +
  annotate(geom = "text", x = .25, y = .15, label = "Takes More Risks", color = staturdays_colors("dark_blue"), fontface = "bold", size = 5) +
  annotate(geom = "text", x = .25, y = -.15, label = "Up Their Game", color = staturdays_colors("dark_blue"), fontface = "bold", size = 5) +
  annotate(geom = "text", x = -.25, y = .15, label = "Makes More Mistakes", color = "darkred", fontface = "bold", size = 5) +
  annotate(geom = "text", x = -.25, y = -.15, label = "Plays More Conservative", color = "darkred", fontface = "bold", size = 5) +
  labs(title = "Most Consistent QBs When Playing Down",
       subtitle = "Minimum 30 attempts in 2020",
       x = "Change in Completion Rate",
       y = "Change in Interception Rate") +
  xlim(-.3, .3) +
  ylim(-.2, .2) +
  staturdays_theme

ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
                         today(),
                         "_",
                         "losing_consistent",
                         ".jpeg"),
       plot = last_plot(),
       width = 400,
       height = 200,
       dpi = 300,
       units = "mm")

# Success Rate when losing
qb_grouped_summary %>% 
  mutate(comp_rate_vs_median = completion_rate - median(qb_grouped_summary$completion_rate),
         succ_rate_vs_median = success_rate - median(qb_grouped_summary$success_rate)) %>% 
  arrange(pass_player, losing_game) %>% 
  group_by(pass_player) %>% 
  mutate(cmp_rate_diff = comp_rate_vs_median - lag(comp_rate_vs_median, 1L),
         succ_rate_diff = succ_rate_vs_median - lag(succ_rate_vs_median, 1L)) %>% 
  filter(n > 30) %>% 
  group_by(pass_player) %>% 
  mutate(n_2 = n()) %>% 
  filter(n_2 == 2) %>% 
  filter(losing_game == T) %>% 
  ggplot(aes(x = cmp_rate_diff, y = succ_rate_diff)) +
  geom_point(aes(alpha = if_else(succ_rate_diff > .1 | succ_rate_diff < -.15, 0.9, 0.1)),
             color = staturdays_colors("orange")) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_vline(xintercept = 0, alpha = .5) +
  geom_text_repel(aes(label = if_else(succ_rate_diff > .1 | succ_rate_diff < -.15, pass_player, "")),
                  max.overlaps = 50) +
  theme(legend.position = "none") +
  annotate(geom = "text", x = .2, y = .3, label = "Plays Better", color = staturdays_colors("dark_blue"), fontface = "bold", size = 5) +
  annotate(geom = "text", x = -.2, y = -.3, label = "Plays Worse", color = "darkred", fontface = "bold", size = 5) +
  labs(title = "Success Rate When Playing Down",
       subtitle = "Minimum 30 attempts in 2020",
       x = "Change in Completion Rate",
       y = "Change in Success Rate") +
  staturdays_theme

ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
                         today(),
                         "_",
                         "losing_success",
                         ".jpeg"),
       plot = last_plot(),
       width = 400,
       height = 200,
       dpi = 300,
       units = "mm")

# Close game grouping
qb_grouped_summary_close <- qb_plays %>% 
  group_by(close_game, pass_player) %>% 
  summarise(completion_rate = sum(pass_completion)/sum(pass_attempt),
            touchdown_rate = sum(pass_touchdown)/sum(pass_attempt),
            interception_rate = sum(pass_intercepted)/sum(pass_attempt),
            sack_rate = sum(passer_sacked)/n(),
            success_rate = sum(success) /n(),
            n = n())

# Success Rate when close game
qb_grouped_summary_close %>% 
  mutate(comp_rate_vs_median = completion_rate - median(qb_grouped_summary$completion_rate),
         succ_rate_vs_median = success_rate - median(qb_grouped_summary$success_rate)) %>% 
  arrange(pass_player, close_game) %>% 
  group_by(pass_player) %>% 
  mutate(cmp_rate_diff = comp_rate_vs_median - lag(comp_rate_vs_median, 1L),
         succ_rate_diff = succ_rate_vs_median - lag(succ_rate_vs_median, 1L)) %>% 
  filter(close_game == T) %>% 
  ggplot(aes(x = cmp_rate_diff, y = succ_rate_diff)) +
  geom_point(aes(alpha = if_else(abs(succ_rate_diff) > .1, 0.9, 0.1))) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_vline(xintercept = 0, alpha = .5) +
  geom_text_repel(aes(label = if_else(abs(succ_rate_diff) > .1, pass_player, "")),
                  max.overlaps = 20) +
  theme(legend.position = "none") +
  annotate(geom = "text", x = .3, y = .4, label = "Play Better", color = "blue", fontface = "bold") +
  annotate(geom = "text", x = -.3, y = -.4, label = "Play Worse", color = "red", fontface = "bold")

# Late game grouping
qb_grouped_summary_late <- qb_plays %>% 
  group_by(last_two_minutes, pass_player) %>% 
  summarise(completion_rate = sum(pass_completion)/sum(pass_attempt),
            touchdown_rate = sum(pass_touchdown)/sum(pass_attempt),
            interception_rate = sum(pass_intercepted)/sum(pass_attempt),
            sack_rate = sum(passer_sacked)/n(),
            success_rate = sum(success) /n(),
            n = n())

# Success Rate when late game - sample size is an issue here, could try 4th quarter instead
qb_grouped_summary_late %>% 
  filter(n > 10) %>% 
  mutate(comp_rate_vs_median = completion_rate - median(qb_grouped_summary$completion_rate),
         succ_rate_vs_median = success_rate - median(qb_grouped_summary$success_rate)) %>% 
  arrange(pass_player, last_two_minutes) %>% 
  group_by(pass_player) %>% 
  mutate(cmp_rate_diff = comp_rate_vs_median - lag(comp_rate_vs_median, 1L),
         succ_rate_diff = succ_rate_vs_median - lag(succ_rate_vs_median, 1L)) %>% 
  filter(last_two_minutes == T) %>% 
  ggplot(aes(x = cmp_rate_diff, y = succ_rate_diff)) +
  geom_point(aes(alpha = if_else(abs(succ_rate_diff) > .1, 0.9, 0.1))) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_vline(xintercept = 0, alpha = .5) +
  geom_text_repel(aes(label = if_else(abs(succ_rate_diff) > .1, pass_player, "")),
                  max.overlaps = 20) +
  theme(legend.position = "none") +
  annotate(geom = "text", x = .3, y = .4, label = "Play Better", color = "blue", fontface = "bold") +
  annotate(geom = "text", x = -.3, y = -.4, label = "Play Worse", color = "red", fontface = "bold")

# Quarter grouping
qb_grouped_summary_quarter <- qb_plays %>% 
  filter(offense_conference %in% power_5) %>% 
  group_by(period, pass_player) %>% 
  summarise(completion_rate = sum(pass_completion)/sum(pass_attempt),
            touchdown_rate = sum(pass_touchdown)/sum(pass_attempt),
            interception_rate = sum(pass_intercepted)/sum(pass_attempt),
            sack_rate = sum(passer_sacked)/n(),
            success_rate = sum(success) /n(),
            n = n())

# Success Rate by quarter
qb_in_1 <- qb_grouped_summary_quarter %>% 
  filter(n > 25, between(period, 1, 4)) %>%
  group_by(pass_player) %>% 
  mutate(count = n()) %>% 
  filter(count >= 3) %>% # Keep only players with at least 3 quarters of data
  mutate(comp_rate_vs_median = completion_rate - median(qb_grouped_summary$completion_rate),
         succ_rate_vs_median = success_rate - median(qb_grouped_summary$success_rate)) %>% 
  arrange(pass_player) %>% 
  mutate(variation = (success_rate - lag(success_rate, 1L, order_by = period)) / lag(success_rate, 1L, order_by = period))

# SAVE
qb_in_1 %>% 
  ggplot(aes(x = period, y = success_rate)) +
  geom_smooth(color = staturdays_colors("orange")) +
  facet_wrap(vars(pass_player)) +
  theme(legend.position = "none") +
  staturdays_theme +
  labs(title = "Passing Performance by Quarter",
       subtitle = "2020 Power-5 Quarterbacks, min. 25 att. per quarter",
       x = "Quarter",
       y = "Success Rate",
       caption = "By @kylebeni012 for @staturdays | Data: @cfb_data") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_rect(data = subset(qb_in_1, variation >= .2 | variation <= -.2),
            fill = staturdays_colors("lightest_blue"), alpha = .1, 
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf)

# Completion Rate by quarter
qb_in_2 <- qb_grouped_summary_quarter %>% 
  filter(n > 25, between(period, 1, 4)) %>%
  group_by(pass_player) %>% 
  mutate(count = n()) %>% 
  filter(count >= 3) %>% # Keep only players with at least 3 quarters of data
  mutate(comp_rate_vs_median = completion_rate - median(qb_grouped_summary$completion_rate),
         succ_rate_vs_median = success_rate - median(qb_grouped_summary$success_rate)) %>% 
  arrange(pass_player) %>% 
  mutate(variation = (completion_rate - lag(completion_rate, 1L, order_by = period)) / lag(completion_rate, 1L, order_by = period))

# SAVE
qb_in_2 %>% 
  ggplot(aes(x = period, y = completion_rate)) +
  geom_smooth(color = staturdays_colors("orange")) +
  facet_wrap(vars(pass_player)) +
  theme(legend.position = "none") +
  staturdays_theme +
  labs(title = "Passing Performance by Quarter",
       subtitle = "2020 Power-5 Quarterbacks, min. 25 att. per quarter",
       x = "Quarter",
       y = "Completion Rate",
       caption = "By @kylebeni012 for @staturdays | Data: @cfb_data") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_rect(data = subset(qb_in_2, variation >= .2 | variation <= -.2),
            fill = staturdays_colors("lightest_blue"), alpha = .1, 
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf)
