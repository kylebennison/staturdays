# Get CFB Data API for Play by Play - Use to generate expected points added (EPA)

rm(list=ls())

library(scales)

library(tidyverse)

library(RCurl)

library(XML)

library(rjson)

library(jsonlite)

library(stringr)

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

base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

plays.master = data.frame()
for (j in 2019:2019) {
  for (i in 1:15) {
    cat('Loading Plays', j, 'Week', i, '\n')
    full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
    full_url_plays_encoded <- URLencode(full_url_plays) # If there are spaces in query, formats them correctly
    plays <- fromJSON(getURL(full_url_plays_encoded)) # Pull in API using url
    clockcolumns <- as_tibble(unnest(plays$clock)) # Takes clock data out as it's own columns
    plays <- plays %>% 
      select(-clock) %>% 
      as_tibble() %>% 
      mutate(minutes = clockcolumns$minutes, seconds = clockcolumns$seconds) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
      mutate_at(c("minutes", "seconds"), ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
    plays$week = i
    plays$year = j
    plays.master = rbind(plays.master, plays)
  }
}
rm(clockcolumns, pass_rows, plays)

games.master = data.frame()
for (j in 2017:2019) {
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
for (j in 2017:2019) {
    cat('Loading Drives', j, '\n')
    full_url_drives <- paste0(base_url_drives, "seasonType=both&", "year=", as.character(j))
    full_url_drives_encoded <- URLencode(full_url_drives)
    drives <- fromJSON(getURL(full_url_drives_encoded))
    start_time_columns <- as_tibble(unnest(drives$start_time)) # Takes clock data out as it's own columns
    end_time_columns <- as_tibble(unnest(drives$end_time))
    elapsed_time_columns <- as_tibble(unnest(drives$elapsed))
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

# Join Plays with Games to get additional info
games.temp <- games.master
games.temp <- mutate(games.temp, id = as.character(id))
plays.temp <- plays.master
plays.temp <- mutate(plays.temp, playid = id, id = substr(playid, 1, 9))
plays_games_joined.master <- left_join(plays.master, games.temp, by = 'id') # Get regular or post season from games data
games.temp <- data.frame()

# Join Plays with Drives to Get Start Yardline
drives.temp <- drives.master
drives.temp <- mutate(drives.temp, id = as.character(id))
plays.temp <- plays.master
plays.temp <- mutate(plays.temp, playid = id, driveid = substr(playid, 1, 10))
plays_drives.master <- left_join(plays.temp, drives.temp, by = c("drive_id" = "id"))
drives.temp <- data.frame()
plays.temp <- data.frame()

# Adjust yards_gained and start_yardline stats
plays_drives.master <- plays_drives.master %>% 
  mutate(start_yardline = if_else(offense.x != home, 100 - start_yardline, as.double(start_yardline)), end_yardline = if_else(offense.x != home, 100 - end_yardline, as.double(end_yardline)))

## Mutate Plays with First Downs (Smart), Pass/Rush, Success
plays.master2 <- plays.master %>% 
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
                      | ((str_detect(play_text, "TOUCHDOWN?") 
                      | str_detect(play_text, "1ST down?")) 
                      & (!play_specifics %in% scrimmage_plays_turnover)), TRUE, FALSE)),
             TRUE ~ NA
             )
         )
# Classify the multiple play types into a simpler just rush or pass
plays.master2$play_type[plays.master2$play_type %in% scrimmage_plays_pass] <- "Pass"
plays.master2$play_type[plays.master2$play_type %in% scrimmage_plays_rush] <- "Rush"
# Rush Fumble Rows
rush_rows <- plays.master2 %>% 
  filter(play_specifics %in% c("Fumble Recovery (Own)", "Fumble Recovery (Opponent)"), str_detect(play_text, "run?") & !str_detect(play_text, "kick?") & !str_detect(play_text, "punt?")) %>% 
  mutate(play_type = "Rush")
# Pass Fumble Rows
pass_rows <- plays.master2 %>% 
  filter(play_type %in% c("Fumble Recovery (Own)", "Fumble Recovery (Opponent)"), str_detect(play_text, "pass?") | str_detect(play_text, "sack?")) %>% 
  mutate(play_type = "Pass")
# Change fumbles to a pass or rush
plays.master2[which(plays.master2$id %in% rush_rows$id), "play_type"] <- "Rush"
plays.master2[which(plays.master2$id %in% pass_rows$id), "play_type"] <- "Pass"
#Add Success Column
plays.master2 <- plays.master2 %>% 
  mutate(success = 
           case_when(
             down == 1 & yards_gained >= 0.5 * distance ~ 1,
             down == 2 & yards_gained >= 0.75 * distance ~ 1,
             down == 3 & yards_gained >= 1 * distance ~ 1,
             down == 4 & yards_gained >= 1 * distance ~ 1,
             str_detect(play_text, "1ST down?") == TRUE ~ 1,
             str_detect(play_text, "TD?") == TRUE ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master2
rm(plays.master2)
##

## Add Line Yards Stat

plays.master.temp <- plays.master %>% 
  mutate(lineYards = 
           case_when(
             play_type == "Rush" & yards_gained < 0 ~ yards_gained * 1.2,
             play_type == "Rush" & yards_gained > 0 & yards_gained <= 4 ~ yards_gained * 1,
             play_type == "Rush" & yards_gained > 4 & yards_gained <= 10 ~ yards_gained * 0.5,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

## Garbage Time Indicator WIP

plays.master.temp <- plays.master %>% 
  mutate(garbageTime = 
           case_when(
             period == 4 & minutes < 2 & abs(offense - defense) > 16 ~ 1,
             period == 4 & minutes < 6 & minutes >= 2 & abs(offense - defense) > 24 ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

# Create a pass and rush only database
pass_rush_yards_gained <- plays.master %>% 
  filter(play_type %in% c("Pass", "Rush"))

# Calculate Success Rate for PSU Backs
pass_rush_yards_gained %>% 
  filter(offense == "Penn State", year == 2019, play_type == "Rush") %>% 
  mutate(running_back = if_else(str_detect(play_text, "Noah Cain"), "Noah Cain", if_else(str_detect(play_text, "Journey Brown"), "Journey Brown", if_else(str_detect(play_text, "Sean Clifford"), "Sean Clifford", if_else(str_detect(play_text, "Devyn Ford"), "Devyn Ford", if_else(str_detect(play_text, "Ricky Slade"), "Ricky Slade", "Other")))))) %>% 
  group_by(running_back) %>% 
  summarise(success_rate = sum(success)/n())

# Calculate Success Rate for LSU and Clemson - Offense
pass_rush_yards_gained %>% 
  filter(offense %in% c("Clemson", "LSU"), year == 2019) %>% 
  group_by(offense, play_type) %>% 
  summarise(success_rate = sum(success)/n()) %>% 
  ggplot(aes(play_type, success_rate)) +
  geom_col(aes(fill = offense), position = "dodge") +
  scale_fill_manual(values = c("#F66733", "#461D7C")) +
  labs(x = "Play Type", y = "Success Rate", fill = "Offense", title = "2019 National Championship: Success Rate on Offense", 
       caption = "@staturdays", subtitle = "Clemson has the higher success rate on both passing and rushing downs") +
  theme(plot.caption = element_text(size = 10, hjust = 1, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold"))

# Calculate Success Rate for Team - Defense
pass_rush_yards_gained %>% 
  filter(defense %in% c("Clemson", "LSU"), year == 2019) %>% 
  group_by(defense, play_type) %>% 
  summarise(success_rate = sum(success)/n()) %>% 
  ggplot(aes(play_type, success_rate)) +
  geom_col(aes(fill = defense), position = "dodge") +
  scale_fill_manual(values = c("#F66733", "#461D7C")) +
  labs(x = "Play Type", y = "Success Rate", fill = "Defense", title = "2019 National Championship: Success Rate on Defense", 
       caption = "@staturdays", subtitle = "Lower is better. Pretty even but LSU (surprisingly) has the better record.") +
  theme(plot.caption = element_text(size = 10, hjust = 1, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold"))

# - Yards Gained Distribution on Run Plays by Running Back 
pass_rush_yards_gained %>% 
  filter(offense == "Penn State" & year == 2019 & play_type == "Rush" & (!play_specifics %in% c("Pass Interception", "Fumble Recovery (Opponent)", "Pass Interception Return", "Interception Return Touchdown"))) %>% 
  mutate(running_back = if_else(str_detect(play_text, "Noah Cain"), "Noah Cain", if_else(str_detect(play_text, "Journey Brown"), "Journey Brown", if_else(str_detect(play_text, "Sean Clifford"), "Sean Clifford", if_else(str_detect(play_text, "Devyn Ford"), "Devyn Ford", if_else(str_detect(play_text, "Ricky Slade"), "Ricky Slade", "Other")))))) %>% 
  ggplot(aes(yards_gained, fill = running_back)) +
  geom_histogram(aes(yards_gained, stat(density)), binwidth = 5, center = 2.5, color = "white", closed = "left") +
  facet_wrap(vars(running_back)) +
  scale_x_continuous(breaks = seq(-5, 95, by = 10), limits = c(-5.5, 95.5)) +
  scale_fill_manual(values = c("#5c6272", "#4c5872", "#394871", "#22345a", "#041e42", "#de703b")) +
  labs(x = "Yards Gained", y = "Percent of Plays", fill = "Rusher", title = "Distribution of Yards Gained on Run Plays by Rusher for PSU - 2019", 
       caption = "@staturdays", subtitle = "Journey Brown is very explosive with 6 rushes > 20 yards") +
  theme(plot.caption = element_text(size = 10, hjust = 1.3, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold")) +
  scale_y_continuous(labels = scales::percent)

# Mean and SD of Runs
pass_rush_yards_gained %>% 
  filter(offense == "Penn State" & year == 2019 & play_type == "Rush" & (!play_specifics %in% c("Pass Interception", "Fumble Recovery (Opponent)", "Pass Interception Return", "Interception Return Touchdown"))) %>% 
  mutate(running_back = if_else(str_detect(play_text, "Noah Cain"), "Noah Cain", if_else(str_detect(play_text, "Journey Brown"), "Journey Brown", if_else(str_detect(play_text, "Sean Clifford"), "Sean Clifford", if_else(str_detect(play_text, "Devyn Ford"), "Devyn Ford", if_else(str_detect(play_text, "Ricky Slade"), "Ricky Slade", "Other")))))) %>% 
  group_by(running_back) %>% 
  summarise(avg_rush = mean(yards_gained), sd_rush = sd(yards_gained), count = n()) %>% 
  arrange(desc(avg_rush)) %>% 
  rename('Running Back' = running_back, 'Average Yards Per Attempt' = avg_rush, 'Standard Deviation' = sd_rush, 'Number of Rushes' = count)
  
# Different Play Types in the Dataset
pass_rush_yards_gained %>% group_by(year) %>% count(play_specifics)

# Percent of First Down Gained
pass_rush_yards_gained %>% 
  group_by(play_type, down, distance) %>% 
  filter(distance > 0 & distance <= 15, year %in% 2017:2019) %>% 
  summarise(avg_yards = mean(yards_gained), count = n()) %>% 
  mutate(pct_made = avg_yards/distance) %>% # what percent of distance to go do you make up with this play
  arrange(desc(down, distance, yards_gained, pct_made))

# Graph

# Histogram of Yards Gained Dist
yards_gained_hist <- pass_rush_yards_gained %>% 
  filter(play_type %in% c("Rush", "Pass"), offense %in% c("LSU", "Clemson"), year == 2019) %>% 
  ggplot(aes(x = yards_gained)) +
  geom_histogram(color = "black", binwidth = 5, breaks = c(-5, 0, 5, 10, 15, 20, 25), aes(fill = offense)) +
  facet_wrap(vars(offense)) +
  scale_fill_manual(values = c("#F66733", "#461D7C")) +
  labs(x = "Yards Gained", y = "Count of Plays", fill = "Team", title = "Memphis vs. PSU Yards Gained Distribution", 
     caption = "@staturdays", subtitle = "Memphis has a higher proportion of plays go for negative or < 5 yards") +
  theme(plot.caption = element_text(size = 10, hjust = 1.3, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold"))

# By Down
pass_rush_yards_gained %>% 
  filter(distance > 0 & distance <= 15) %>% 
  group_by(play_type, down) %>% 
  summarise(avg_yards = mean(yards_gained), count = n()) %>% 
  arrange(desc(down, yards_gained, pct_made)) %>% 
  ggplot(aes(x = down, y = avg_yards)) +
  geom_col() +
  facet_wrap(vars(play_type)) # So just never rush?

# By Down and Distance
pass_rush_yards_gained %>% 
  filter(year %in% 2017:2019) %>% 
  group_by(play_type, down, distance) %>% 
  filter(distance > 0 & distance <= 10) %>% 
  summarise(avg_yards = mean(yards_gained), count = n()) %>% 
  mutate(pct_made = avg_yards/distance) %>% # what percent of distance to go do you make up with this play
  arrange(desc(down, distance, yards_gained, pct_made)) %>% 
  ggplot(aes(x = distance, y = avg_yards, fill = play_type)) +
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(vars(down), ncol = 2) +
  scale_x_continuous(breaks = seq(1,10, by = 1)) +
  labs(x = "Yards to Go", y = "Average Yards Gained", fill = "Play Type", title = "Run vs. Pass 2008 - 2015", 
       caption = "@staturdays", subtitle = "Average Yards Gained on passes vs. runs based on down and yards to go") +
  theme(plot.caption = element_text(size = 10, hjust = 1.3, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold"))

# By Pct of First Down Gained - Should better visulize which choice gets you the end-goal, a first down or close to it
pass_rush_yards_gained %>% 
  filter(year %in% 2017:2019, play_type %in% c('Pass', 'Rush')) %>% 
  group_by(play_type, down, distance) %>% 
  filter(distance > 0 & distance <= 10) %>% 
  summarise(avg_yards = mean(yards_gained), count = n()) %>% 
  mutate(pct_made = avg_yards/distance) %>% # what percent of distance to go do you make up with this play
  arrange(desc(down, distance, yards_gained, pct_made)) %>% 
  ggplot(aes(x = distance, y = pct_made, fill = play_type)) +
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(vars(down), ncol = 2) +
  scale_x_continuous(breaks = seq(1,10, by = 1)) +
  labs(x = "Yards to Go", y = "Yards Gained / Yards to Go", fill = "Play Type", title = "What Percent of a First Down Can You Make Up?", 
       caption = "@staturdays", subtitle = "Percent of a 1st Down earned on passes vs. runs based on down and yards to go. Anything over 1.0 \n(the dashed line) is a first down. 2nd and 1 passes typically earn 6.5 yards, or 6.5x what's needed for a 1st down.") +
  theme(plot.caption = element_text(size = 10, hjust = 1, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold")) +
  geom_hline(yintercept = 1, alpha = 0.5, linetype = "longdash") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,6, by = 1))

# By Yards Gained Minus the Distance to Go

pass_rush_yards_gained %>% 
  filter(year %in% 2017:2019) %>% 
  group_by(play_type, down, distance) %>% 
  filter(distance > 0 & distance <= 10) %>% 
  summarise(avg_yards = mean(yards_gained), count = n()) %>% 
  mutate(vs_line_to_gain = avg_yards-distance) %>% # what percent of distance to go do you make up with this play
  arrange(desc(down, distance, yards_gained, vs_line_to_gain)) %>% 
  ggplot(aes(x = distance, y = vs_line_to_gain, fill = play_type)) +
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(vars(down), ncol = 2) +
  scale_x_continuous(breaks = seq(1,10, by = 1)) +
  labs(x = "Yards to Go", y = "Yards Past or Short of First Down Line", fill = "Play Type", title = "How Far Will You Be From The Line to Gain?", 
       caption = "@staturdays", subtitle = "Average yards gained on each play type of that down and distance, minus the yards to go, to show how far behind \n(or ahead) of the sticks you'll be on average for plays of this type and situation") +
  theme(plot.caption = element_text(size = 10, hjust = 1, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold")) +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = "longdash")

# Probability of a first down by play type

pass_rush_yards_gained %>% 
  filter(play_type %in% c("Pass", "Rush"), year %in% 2017:2019) %>% 
  group_by(play_type, down, distance, running_back) %>% 
  summarise(first_down_prob = (sum(first_down == TRUE)/length(first_down)), count = n()) %>% 
  filter(down != 0, distance <= 15 & distance > 0) %>% 
  arrange(desc(down, distance, first_down_prob)) %>% 
  ggplot(aes(x = distance, y = first_down_prob, fill = play_type)) +
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(vars(down), ncol = 2) +
  scale_x_continuous(breaks = seq(1,10, by = 1), limits = c(0.5,10.5)) +
  labs(x = "Yards to Go", y = "First Down Probability", fill = "Play Type", title = "Probability of a First Down", 
       caption = "@staturdays", subtitle = "Percent of plays resulting in a first down from this down and distance, by play type") +
  theme(plot.caption = element_text(size = 10, hjust = 1, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold")) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1, by = 0.1))

# Pass vs. Rush Percent by Team - Total
pass_rush_yards_gained %>% filter(year %in% 2019) %>% group_by(offense) %>% count(play_type) %>% 
  spread(key = play_type, value = n) %>% 
  mutate(PctPass = Pass/(Pass+Rush)) %>% 
  arrange(desc(PctPass))

# Pass vs. Rush Percent by Team - 2019 by Situation
pass_rush_yards_gained %>% filter(year == 2019, down > 0, distance > 0 & distance <= 10, offense == "Penn State") %>% group_by(offense, down, distance) %>% count(play_type) %>% 
  spread(key = play_type, value = n) %>% 
  mutate(PctPass = Pass/(Pass+Rush)) %>% 
  arrange(desc(offense, down, distance))

# Pass vs. Rush Percent - Individual Team
pass_rush_yards_gained %>% 
  filter(year %in% 2019) %>% 
  group_by(offense, play_type, down, distance) %>% 
  filter(distance > 0 & distance <= 10, offense == "Penn State") %>% 
  summarise(avg_yards = mean(yards_gained), count = n()) %>% 
  mutate(pct_made = avg_yards/distance) %>% # what percent of distance to go do you make up with this play
  arrange(desc(down, distance, yards_gained, pct_made)) %>% 
  ggplot(aes(x = distance, y = avg_yards, fill = play_type)) +
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(vars(down), ncol = 2) +
  scale_x_continuous(breaks = seq(1,10, by = 1)) +
  labs(x = "Yards to Go", y = "Average Yards Gained", fill = "Play Type", title = "Penn State Sees a Few Cases Worth Running For It", 
       caption = "@staturdays", subtitle = "Average Yards Gained on passes vs. runs based on down and yards to go") +
  theme(plot.caption = element_text(size = 10, hjust = 1.3, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold"))

# 4th and Goal inside the 10 Stats for Penn State
pass_rush_yards_gained %>% 
  filter(play_type %in% c("Pass", "Rush"), year %in% 2017:2019, down == 4, offense %in% c("LSU", "Clemson"), if_else(home == offense, yard_line >= 90, yard_line <= 10)) %>% 
  group_by(offense, down, distance) %>% 
  summarise(first_down_prob = (sum(first_down == TRUE)/length(first_down)), count = n()) %>% 
  filter(down != 0, distance <= 15 & distance > 0) %>% 
  arrange(desc(down, distance, first_down_prob))

# Which teams score early, give up points early?

# AND need to find a way to make sure it's the OFFENSE that scored - maybe filter where play_type 
# in offense score list

# Offense scores early - Teams that score early in games
offense_scores_early <- plays.master %>%  
  filter(year == 2019, period == 1 & minutes >= 10) %>% # Take only early starts to games
  group_by(week,offense) %>% slice(which.max(offense_score)) %>% # Group by week/team and take max score
  select(offense, offense_score, week, minutes, seconds, play_type, period, everything()) %>% # reorder columns
  arrange(week, desc(minutes, seconds), offense) %>% # Sort by week, min, sec, team
  group_by(offense) %>% 
  mutate(count = n()) %>% # this is right now
  filter(count >= 12) %>% # filter out teams that don't have 12 games of stats
  summarise(earlyscorespergame = mean(offense_score)) %>% 
  arrange(desc(earlyscorespergame)) %>% 
  mutate(earlyscorespergame = round(earlyscorespergame, 2), z_score = (earlyscorespergame-mean(earlyscorespergame))/sd(earlyscorespergame))

offense_scores_early %>% top_n(15, earlyscorespergame)

# Graph Offense Scores Early
offense_scores_early %>% 
  top_n(15, earlyscorespergame) %>% 
  ggplot(aes(x = reorder(offense,earlyscorespergame),y = earlyscorespergame, label = earlyscorespergame)) +
  geom_col(fill = staturdays_colors("light_blue")) +
  coord_flip() +
  geom_text(nudge_y = 0.5, color = staturdays_colors("dark_blue"), family = "sans") +
  labs(y = "Average Points Scored in First 5 Minutes of Games", x = "", title = "Teams Who Score Early", 
       subtitle = "Top 15 teams who score in the first 5 minutes of games \n during the 2018 season") +
  theme(axis.text = element_text(size = 10))
  #theme(plot.title = element_text(hjust = 0.5))

# Histogram of Offense Scores
offense_scores_early %>% 
  ggplot(aes(x = earlyscorespergame)) +
  geom_histogram(binwidth = 0.5, fill = staturdays_colors("light_blue"), color = "white", center = 0.25) +
  labs(x = "Early Points Per Game", y = "Count of Teams", title = "Distribution of Average Points Scored in First 5 Minutes of 2018")

# Offense doesn't score early
offense_no_score_early <- plays.master %>%  
  filter(year == 2019, period == 1 & minutes >= 10) %>% # Take only early starts to games
  group_by(week,offense) %>% slice(which.max(offense_score)) %>% # Group by week/team and take max score
  select(offense, offense_score, week, minutes, seconds, play_type, period, everything()) %>% # reorder columns
  arrange(week, desc(minutes, seconds), offense) %>% # Sort by week, min, sec, team
  group_by(offense) %>% 
  mutate(count = n()) %>% # this is right now
  filter(count >= 12) %>% # filter out teams that don't have 12 games of stats
  summarise(earlyscorespergame = mean(offense_score)) %>% 
  arrange(earlyscorespergame) %>% 
  mutate(earlyscorespergame = round(earlyscorespergame, 2), z_score = (earlyscorespergame-mean(earlyscorespergame))/sd(earlyscorespergame))

offense_no_score_early

# Graph Offense No Score Early
offense_no_score_early %>% 
  top_n(-15, earlyscorespergame) %>% 
  ggplot(aes(x = reorder(offense,-earlyscorespergame),y = earlyscorespergame, label = earlyscorespergame)) +
  geom_col(fill = staturdays_colors("light_blue")) +
  coord_flip() +
  geom_text(nudge_y = 0.1, color = staturdays_colors("dark_blue"), family = "sans") +
  labs(y = "Average Points Scored in First 5 Minutes of Games", x = "", title = "Teams Who Don't Score Early", 
       subtitle = "Bottom 15 teams in the first 5 minutes of games \n during the 2018 season") +
  theme(axis.text = element_text(size = 10))

# Defense gets scored on quickly - Teams that are down early in games
defense_scored_on <- plays.master %>%  
  filter(year == 2019, period == 1 & minutes >= 10) %>% # Take only early starts to games
  group_by(week,defense) %>% slice(which.max(offense_score)) %>% # Group by week/team and take max score
  select(defense, offense_score, week, minutes, seconds, play_type, period, everything()) %>% # reorder columns
  arrange(week, desc(minutes, seconds), defense) %>% # Sort by week, min, sec, team
  group_by(defense) %>% 
  mutate(count = n()) %>% # this is right now
  filter(count >= 12) %>% # filter out teams that don't have 12 games of stats
  summarise(earlyscoreallowedpergame = mean(offense_score)) %>% 
  arrange(desc(earlyscoreallowedpergame)) %>% 
  mutate(earlyscoreallowedpergame = round(earlyscoreallowedpergame, 2), z_score = (earlyscoreallowedpergame-mean(earlyscoreallowedpergame))/sd(earlyscoreallowedpergame))

defense_scored_on %>% 
  top_n(15, earlyscoreallowedpergame)

# Defense Scored On Graph
defense_scored_on %>% 
  top_n(15, earlyscoreallowedpergame) %>% 
  ggplot(aes(x = reorder(defense,earlyscoreallowedpergame),y = earlyscoreallowedpergame, label = earlyscoreallowedpergame)) +
  geom_col(fill = staturdays_colors("light_blue")) +
  coord_flip() +
  geom_text(nudge_y = 0.3, color = staturdays_colors("dark_blue"), family = "sans") +
  labs(y = "Average Points Scored Against in First 5 Minutes of Games", x = "", title = "Teams Who Get Scored On Early", 
       subtitle = "Bottom 15 teams who get scored on in the first \n 5 minutes of games during the 2019 season", caption = "@staturdays") +
  theme(axis.text = element_text(size = 10)) +
  theme(plot.caption = element_text(size = 10, hjust = 1.3, color = staturdays_colors("dark_blue")), 
        plot.title = element_text(color = staturdays_colors("dark_blue"), size = 14, face = "bold"))

# which conferences are the slowest starters?
# Conference Scores Early Offense

conference_scores_early <- plays.master %>%  
  filter(year == 2018, period == 1 & minutes >= 10) %>% # Take only early starts to games
  group_by(week,offense) %>% slice(which.max(offense_score)) %>% # Group by week/team and take max score
  select(offense, offense_score, week, minutes, seconds, play_type, period, everything()) %>% # reorder columns
  arrange(week, desc(minutes, seconds), offense, offense_conference) %>% # Sort by week, min, sec, team
  group_by(offense_conference) %>% 
  mutate(count = n()) %>% # this is right now
  filter(count >= 12) %>% # filter out teams that don't have 12 games of stats
  summarise(earlyscorespergame = mean(offense_score)) %>% 
  arrange(desc(earlyscorespergame)) %>% 
  mutate(earlyscorespergame = round(earlyscorespergame, 2), z_score = (earlyscorespergame-mean(earlyscorespergame))/sd(earlyscorespergame))

conference_scores_early %>% top_n(15, earlyscorespergame)

# 3 year trend - top 5 teams averaged over 3 years, then track just their trend facet_wrap by year
scores_early_3year_top5 <- plays.master %>%  
  filter(period == 1 & minutes >= 10) %>% # Take only early starts to games
  group_by(year,week,offense) %>% slice(which.max(offense_score)) %>% # Group by year/week/team and take max score
  select(year, offense, offense_score, week, minutes, seconds, play_type, period, everything()) %>% # reorder columns
  arrange(year, week, desc(minutes, seconds), offense) %>% # Sort by year, week, min, sec, team
  group_by(offense) %>% 
  mutate(count = n()) %>% # this is right now
  filter(count >= 30) %>% # filter out teams that don't have 30 games of stats (10/year)
  summarise(earlyscorespergame = mean(offense_score)) %>% 
  arrange(desc(earlyscorespergame)) %>% 
  mutate(earlyscorespergame = round(earlyscorespergame, 2), z_score = (earlyscorespergame-mean(earlyscorespergame))/sd(earlyscorespergame))

scores_early_3year_top5 # top teams over the past 3 years at scoring early

scores_early_3year_top5%>%
  top_n(10, earlyscorespergame) %>% 
  ggplot(aes(x = reorder(offense,earlyscorespergame),y = earlyscorespergame, label = earlyscorespergame)) +
  geom_col(fill = staturdays_colors("light_blue")) +
  coord_flip() +
  geom_text(nudge_y = -.55, color = "white", family = "sans") +
  labs(y = "Average Points Scored in First 5 Minutes of Games", x = "", title = "Top Teams Who Score Early - 3 Year Average", 
       subtitle = "Top 10 teams who score in the first 5 minutes of games \n over the 2017 - 2018 season") +
  theme(axis.text = element_text(size = 10))

scores_early_3year_top5 %>% top_n(-15) # bottom 15 teams over the past 3 years at scoring early

# Now, using the top 5 from above, use this data for the graph - Avg score for each year for the top teams
scores_early_3years <- plays.master %>%  
  filter(period == 1 & minutes >= 10) %>% # Take only early starts to games
  group_by(year,week,offense) %>% slice(which.max(offense_score)) %>% # Group by year/week/team and take max score
  select(year, offense, offense_score, week, minutes, seconds, play_type, period, everything()) %>% # reorder columns
  arrange(year, week, desc(minutes, seconds), offense) %>% # Sort by year, week, min, sec, team
  group_by(offense) %>% 
  mutate(count = n()) %>% # this is right now
  filter(count >= 30) %>% # filter out teams that don't have 30 games of stats (10/year)
  group_by(year, offense) %>% 
  summarise(earlyscorespergame = mean(offense_score)) %>% 
  arrange(desc(earlyscorespergame)) %>% 
  mutate(earlyscorespergame = round(earlyscorespergame, 2), z_score = (earlyscorespergame-mean(earlyscorespergame))/sd(earlyscorespergame))

# Need to grab only the rows from the entire data that have the top 5 in it
scores_early_top_10 <- scores_early_3years %>% 
  filter(offense %in% scores_early_3year_top5$offense[1:10])

# Graph Top 10 - 3 years - Offense Scores Early
scores_early_top_10 %>%
  ggplot(aes(x = reorder(offense,earlyscorespergame),y = earlyscorespergame, label = earlyscorespergame)) +
  geom_col(fill = staturdays_colors("light_blue")) +
  coord_flip() +
  facet_wrap(vars(year), scales = "fixed", ncol = 3) +
  geom_text(nudge_y = -.55, color = "white", family = "sans") +
  labs(y = "Average Points Scored in First 5 Minutes of Games", x = "", title = "Top Teams Who Score Early", 
       subtitle = "Top 10 teams who score in the first 5 minutes of games \n from the 2017 - 2018 season") +
  theme(axis.text = element_text(size = 10))
#theme(plot.title = element_text(hjust = 0.5)) This would center it

# Line chart of trend over time - needs work
scores_early_top_10 %>% 
  ggplot(aes(x = year, y = earlyscorespergame, label = earlyscorespergame)) +
  geom_line(aes(color = offense))

# Drives Data

count(drives.master, drive_result) # How to get a summary count

# First Drive Scores
first_drive_score <- drives.master %>%  
  group_by(game_id, offense) %>% # Take only early starts to games
  slice(which.min(id)) %>% 
  select(offense, scoring, drive_result, everything()) %>% # reorder columns
  group_by(offense) %>% 
  mutate(count = n()) %>% # Count number of drives per team to filter out junk data
  filter(count >= 36) %>% # filter out teams that don't have 12 games of stats per year
  mutate(points_scored = if_else(drive_result == "TD", 7, if_else(drive_result == "FG", 3, 0))) %>% 
  summarise(firstdrivescorespergame = mean(points_scored)) %>% 
  arrange(desc(firstdrivescorespergame)) %>% 
  mutate(firstdrivescorespergame = round(firstdrivescorespergame, 2), z_score = (firstdrivescorespergame-mean(firstdrivescorespergame))/sd(firstdrivescorespergame))

first_drive_score %>% top_n(15, firstdrivescorespergame) #top15
first_drive_score %>% top_n(-15, firstdrivescorespergame) #bottom15


# Graph Offense Scores Early - First Drive
first_drive_score %>% 
  top_n(-30, firstdrivescorespergame) %>% 
  ggplot(aes(x = reorder(offense,-firstdrivescorespergame),y = firstdrivescorespergame, label = firstdrivescorespergame)) +
  geom_col(fill = staturdays_colors("light_blue")) +
  coord_flip() +
  geom_text(nudge_y = 0.5, color = staturdays_colors("dark_blue"), family = "sans") +
  labs(y = "Average Points Scored in First Drive Games", x = "", title = "Teams Who Don't Score On First Drives", 
       subtitle = "Bottom 30 teams based on scores in the first drive of games \n from the 2017-2018 seasons", caption="@staturdays") +
  theme(axis.text = element_text(size = 10))
#theme(plot.title = element_text(hjust = 0.5))

# Special Teams - Count of Big Play Ability

special_teams_explosive <- plays.master %>% 
  filter(play_type %in% c('Blocked Field Goal', 'Blocked Punt Touchdown', "Punt Return Touchdown", "Blocked Field Goal Touchdown", "Blocked Punt", "Kickoff Return Touchdown", "Missed Field Goal Return Touchdown", "Missed Field Goal Return")) %>% 
  group_by(defense) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Special teams - Histogram
special_teams_explosive %>% 
  ggplot(aes(x = count, label = count)) +
  geom_histogram(binwidth = 1, fill = staturdays_colors("light_blue"), color = "white", center = 0) +
  labs(x = "Big Plays on Special Teams", y = "Count of Teams", title = "Distribution of Explosive Special Teams Plays 2017-2018", caption = "@staturdays", subtitle = "Sum of Punts, Kickoffs & FGs Blocked or Returned for TDs")

# Special teams - Bar chart top 15
special_teams_explosive %>% 
  top_n(15, count) %>% 
  ggplot(aes(reorder(defense,count), count, label = count)) +
  geom_col(fill = staturdays_colors("light_blue")) +
  coord_flip() +
  geom_text(nudge_y = 0.5, color = staturdays_colors("dark_blue"), family = "sans") +
  labs(y = "Number of Blocked FGs & Punts + FGs, Punts, & Kickoffs \n Returned for TDs", x = "", title = "Big Plays by Special Teams", 
       subtitle = "Teams that have had the biggest impact on special teams \n in the last 3 seasons", caption = "@staturdays") +
  theme(axis.text = element_text(size = 10), panel.background = element_rect(fill = "gray86"), plot.title = element_text(color = staturdays_colors("medium_blue")))

# Mark whether play is a first down or not
plays.first_downs <- plays.master %>% 
  mutate(first_down = ifelse(yards_gained>=distance, TRUE, FALSE))

# Calculate first down rate for each team in the power 5
first_down_rate <- plays.first_downs %>% 
  filter(year == 2019, offense_conference %in% power_5, down != 0, play_type != "Penalty") %>% 
  group_by(offense) %>% 
  summarise(firstdownrate = round(sum(first_down, na.rm = TRUE)/length(first_down), 3), avg_yards_gained = round(mean(yards_gained), 2)) %>% 
  arrange((firstdownrate))

first_down_rate %>% top_n(15, firstdownrate)
first_down_rate %>% top_n(-15, firstdownrate)

# Calculate first down rate by down for each team in the power 5
first_down_rate_downs <- plays.master %>% 
  filter(year == 2019, offense_conference %in% power_5, down != 0, play_type != "Penalty") %>% 
  group_by(offense, down) %>% 
  summarise(firstdownrate = round(sum(first_down, na.rm = TRUE)/length(first_down), 3), avg_yards_gained = round(mean(yards_gained), 2)) %>% 
  arrange((down))

# Penn State First Down Rate by Down - #Cheating and multiplying pct. first downs to make it more visible

first_down_rate_downs %>% 
  filter(offense == "Penn State", down != 0) %>% 
  ggplot(aes(x = down, y = firstdownrate*10, label = paste0(firstdownrate*100, "%"))) +
  geom_col(fill = staturdays_colors("light_blue")) +
  geom_text(nudge_y = 0.5, color = staturdays_colors("dark_blue")) +
  geom_line(aes(x = down, y = avg_yards_gained), color = staturdays_colors("orange")) +
  labs(x = "Down", y = "Average Yards Gained", title = "Penn State's 3rd Down Woes", subtitle = "PSU Struggles on 3rd Down, converting just 21% of the time", caption = "@staturdays") +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 12), axis.text = element_text(size = 10), plot.caption = element_text(color = staturdays_colors("lightest_blue"), size = 14))

# trying to get multiple teams
first_down_rate %>% 
  filter(down != 0) %>% 
  top_n(16, firstdownrate) %>% 
  ggplot(aes(x = down, y = firstdownrate*10, label = paste0(firstdownrate*100, "%"))) +
  geom_col(fill = staturdays_colors("light_blue")) +
  geom_text(nudge_y = 0.5, color = staturdays_colors("dark_blue")) +
  geom_line(aes(x = down, y = avg_yards_gained), color = staturdays_colors("orange")) +
  facet_wrap(vars(offense))+
  labs(x = "Down", y = "Average Yards Gained", title = "Penn State's 3rd Down Woes", subtitle = "PSU Struggles on 3rd Down, converting just 21% of the time", caption = "@staturdays") +
  theme(plot.title = element_text(size = 18), axis.title = element_text(size = 12), axis.text = element_text(size = 10), plot.caption = element_text(color = staturdays_colors("lightest_blue"), size = 14))


third_down_rate <- plays.master %>% 
  filter(year == 2019, down == 3, offense_conference %in% power_5, play_type != "Penalty") %>% 
  group_by(offense) %>% 
  summarise(thirddownrate = sum(first_down, na.rm = TRUE)/length(first_down)) %>% 
  arrange(desc(thirddownrate))

# Get Team First Down Rate in Specific Situations
firstdownrate <- plays.first_downs %>% 
  filter(offense == "North Carolina", year == 2019, down == 4, distance == 3, play_type != "Penalty") %>% 
  summarise(firstdownrate = sum(first_down, na.rm = TRUE)/length(first_down), count = n()) %>% 
  arrange(desc(firstdownrate))

third_down_rate %>% top_n(15, thirddownrate)
third_down_rate %>% top_n(-15, thirddownrate) %>% arrange(thirddownrate)

# Start of an idea about expected points based on situation

plays.master %>% 
  filter(yard_line <= 20 & down == 1) %>% 
  group_by(yard_line) %>% 
  summarise(exp_yards_gained = mean(yards_gained))

# Define values of different plays

td <- 6.95
inttd <- -6.95
safety <- -2
fg <- 3

plays.master %>% 
  filter(offense == "Penn State" & offense_score >= 41 & period == 2 & defense_score == 0) %>% 
  group_by(year,week,defense) %>% 
  slice(which.max(offense_score))
