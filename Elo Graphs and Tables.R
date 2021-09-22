
# Required themes, pkgs, and colors ----------------------------------------------
rm(list = ls())
library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(lubridate)
library(gt)
library(webshot)
library(data.table)

# Required Themes and Data ------------------------------------------------

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

staturdays_palette <- c("#5c6272", "#ffffff", "#de703b")

staturdays_ramp <- function(x) rgb(colorRamp(c(staturdays_palette))(x), maxColorValue = 255)

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
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 14),
                          panel.background = element_blank(),
                          panel.grid = element_line(color = "#d6d6d6"),
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_line(color = "#d6d6d6")
)

# Team Colors and Logos
team_colors <- fromJSON(getURL("https://api.collegefootballdata.com/teams/fbs?year=2020"))

team_colors <- team_colors %>% unnest(cols = logos) %>% 
  mutate(logo_color = if_else(str_detect(logos, "dark"), "dark", "light")) %>% 
  pivot_wider(names_from = logo_color, values_from = logos)

# Logo
#logo <- grid::rasterGrob(png::readPNG("C:/Users/Kyle/Documents/Kyle/Staturdays/Logo Final/4thdownmarkerlogo.png"), interpolate = T)

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC", "FBS Independents")
group_of_5 <- c("American Athletic", "Conference USA", "Mid-American", "Mountain West", "Sun Belt")

staturdays_theme <- theme(plot.caption = element_text(size = 12, hjust = 1, color = staturdays_colors("orange")), 
                          plot.title = element_text(color = staturdays_colors("dark_blue"), size = 30, face = "bold"),
                          plot.subtitle = element_text(color = staturdays_colors("lightest_blue"), size = 20),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          axis.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 15)
)


# Required Data -----------------------------------------------------------

# New Season Regression Factor
regress <- (.95)
# k-factor
k <- 85
# home-field advantage (in elo points)
home_field_advantage <- 55
# Conference adjustors
g5 <- 1200
d3 <- 500

# Expected Score and Updated Elo Rating Functions -------------------------

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

calc_new_elo_rating <- function(team_rating, actual_score, expected_score, k=85){
  return(team_rating + k * (actual_score - expected_score))
}


elo_ratings <- read_csv(file = "https://raw.githubusercontent.com/kylebennison/staturdays/master/elo_ratings_historic.csv",
                        col_types = list(col_character(), col_character(), col_double(), col_integer(), col_integer(), col_date(format = "%Y-%m-%d")))

elo_conf <- elo_ratings %>% 
  mutate(conference_class = case_when(conference %in% power_5 ~ 1500,
                                      conference %in% group_of_5 ~ g5,
                                      conference %in% "FBS Independents" ~ 1500,
                                      TRUE ~ d3))

# Graphs ------------------------------------------------------------------

# Elo of the top 10 teams in average Elo all-time
elo_ratings %>% 
  left_join(team_colors, by = c("team" = "school")) %>% 
  filter(team %in% {elo_ratings %>% group_by(team) %>% summarise(avg_elo = mean(elo_rating)) %>% slice_max(order_by = avg_elo, n=10)}$team) %>% 
  ggplot(aes(date, elo_rating, colour = color)) +
  geom_line() +
  scale_color_identity()

# Top teams this year
elo_ratings %>% 
  filter(season == 2020) %>% 
  group_by(team) %>% 
  slice_max(date, n = 1) %>% 
  ungroup() %>% 
  slice_max(elo_rating, n = 10) %>% 
  ggplot(aes(team, elo_rating)) +
  geom_col()

# Elo of Penn State
elo_ratings %>% 
  filter(team %in% "Penn State") %>% 
  ggplot(aes(date, elo_rating, colour = team)) +
  geom_line()

# Function that creates an Elo plot of two teams for a set date range
Elo_head_to_head <- function(team_a, team_b, start_season=min(elo_ratings$season), end_season=max(elo_ratings$season)){
  elo_ratings %>% 
    left_join(team_colors, by = c("team" = "school")) %>% 
    filter(team %in% c(team_a, team_b), season >= start_season & season <= end_season) %>% 
    ggplot(aes(date, elo_rating, colour = color)) +
    geom_line(size = 2) +
    scale_color_identity()
}

team_a <- "Clemson"
team_b <- "LSU"

elo_h2h_plot <- Elo_head_to_head(team_a, team_b, 2019, 2019) +
   labs(
    x = "Date",
    y = "Elo Rating",
    color = "Team",
    title = "Rise to the National Championship",
    subtitle = "1500 is the average Elo rating in the Power-5",
    caption = "@staturdays | @kylebeni012 - Data: @cfb_data") +
  staturdays_theme + 
  theme(legend.position = c(.85, .25),
        legend.background = element_blank(),
        legend.text = element_text(color = staturdays_colors("dark_blue")),
        legend.title = element_text(color = staturdays_colors("dark_blue"))) +
  geom_hline(yintercept = 1500, color = staturdays_colors("orange"), size = 1, linetype = "dashed")

ggsave(filename = paste0("elo_h2h", "_", team_a, "_", team_b, str_replace_all(now(), ":", "."), ".png"), 
       plot = elo_h2h_plot, 
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       width = 200,
       height = 200,
       units = "mm"
)

### Additional tables

# See what conference has the toughest opponent elo ratings
lhs.tmp <- upcoming.games %>% group_by(home_conference) %>% filter(home_conference != away_conference) %>% summarise(m_away_elo = mean(away_elo), count = n()) %>% arrange(m_away_elo)
rhs.tmp <- upcoming.games %>% group_by(away_conference) %>% filter(home_conference != away_conference) %>% summarise(m_home_elo = mean(home_elo), count = n()) %>% arrange(m_home_elo)
conf_non_conf_SOS <- left_join(lhs.tmp, rhs.tmp, by = c("home_conference" = "away_conference")) %>% mutate(avg.elo = (m_away_elo*count.x + m_home_elo*count.y)/(count.x+count.y)) %>% arrange(avg.elo)

# Table
conf_non_conf_sos_tbl <- conf_non_conf_SOS %>% 
  select(home_conference, avg.elo) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Non-Conference Strength of Schedule"),
             subtitle = "Average Elo of opponents in non-conference games, by conference.") %>% 
  cols_label(home_conference = "Conference", avg.elo = "Average Opponent Elo") %>% 
  fmt_number(vars(avg.elo), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(avg.elo), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = conf_non_conf_sos_tbl, 
       filename = "2020_preseason_conf_non_conf_sos_tbl_8.31.20.png",
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

## Strength of Schedule

lhs.tmp <- upcoming.games %>% group_by(home_team) %>% 
  rename(opponent = away_team, team = home_team) %>% 
  summarise(avg_opponent_elo = mean(away_elo), elo = mean(home_elo), count = n())

rhs.tmp <- upcoming.games %>% group_by(away_team) %>% 
  rename(opponent = home_team, team = away_team) %>% 
  summarise(avg_opponent_elo = mean(home_elo), elo = mean(away_elo), count = n())

team_strength_of_schedule <- left_join(lhs.tmp, rhs.tmp, by = "team") %>% 
  group_by(team) %>% 
  mutate(avg_opponent_elo = ((avg_opponent_elo.x*count.x)/(sum(count.x, count.y))) + ((avg_opponent_elo.y*count.y)/(sum(count.x, count.y))),
         count = sum(count.x, count.y),
         elo = elo.x) %>% 
  select(team, elo, avg_opponent_elo, count) %>% 
  arrange(desc(avg_opponent_elo)) %>% 
  ungroup() %>% 
  mutate(rank = row_number()) %>% 
  mutate(difference = elo - avg_opponent_elo)

team_strength_of_schedule_tbl <- team_strength_of_schedule %>% 
  select(-count) %>% 
  slice_max(order_by = avg_opponent_elo, n = 25) %>% 
  gt() %>% 
  tab_header(title = "2020 Strength of Schedule",
             subtitle = "Average Opponent Elo Rating") %>% 
  cols_label(team = "Team", elo = "Elo Rating", avg_opponent_elo = "Average Opponent Elo", difference = "Elo Advantage/Disadvantage", rank = "SOS Rank") %>% 
  fmt_number(vars(elo, avg_opponent_elo, difference), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(avg_opponent_elo, difference), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

team_difficultly_of_schedule_tbl <- team_strength_of_schedule %>% 
  select(-count) %>% 
  arrange(difference) %>% 
  mutate(rank_difference = row_number()) %>% 
  slice_min(order_by = difference, n = 25) %>% 
  gt() %>% 
  tab_header(title = "2020 Difficulty of Schedule (DOS)",
             subtitle = "Difference between Elo Ratings and Average Opponent Elo Rating") %>% 
  cols_label(team = "Team", elo = "Elo Rating", avg_opponent_elo = "Average Opponent Elo", difference = "Elo Advantage/Disadvantage", rank = "SOS Rank", rank_difference = "DOS Rank") %>% 
  fmt_number(vars(elo, avg_opponent_elo, difference), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(avg_opponent_elo, difference), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = team_difficultly_of_schedule_tbl, 
       filename = paste0("2020_team_difficultly_of_schedule_tbl__", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")


# Weekly Elo Top 10 Animation ---------------------------------------------
library(gganimate)
library(gifski)

elo_rankings <- tibble()
for(wk in 1:8){
  elo_iter <- elo_ratings %>% 
    filter(season == 2020, week <= wk) %>% 
    group_by(team) %>% 
    filter(week == max(week)) %>% 
    mutate(current_elo = elo_rating, week_of_szn = wk)
  
  elo_rankings <- rbind(elo_rankings, elo_iter)
}

anim <- elo_rankings %>% 
  group_by(week_of_szn) %>% 
  slice_max(elo_rating, n = 10) %>% 
  arrange(desc(elo_rating)) %>% 
  mutate(rank = rank(desc(elo_rating))) %>% 
  arrange(desc(rank)) %>% 
  ggplot() +
  geom_col(aes(x = rank, y = elo_rating)) +
  coord_flip() +
  scale_x_reverse(breaks = c(seq(1, 10, by = 1))) +
  geom_label(aes(x = rank, y = elo_rating, label = team)) +
  transition_states(week_of_szn, transition_length = 2, state_length = 1) +
  labs(title = "Week: {current_state}")

anim

gifski::gifski(anim, gif_file = "test.gif", )
