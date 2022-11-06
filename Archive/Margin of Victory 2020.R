# Libraries and Themes ----------------------------------------------------
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
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 14),
                          panel.background = element_blank(),
                          panel.grid = element_line(color = "#d6d6d6"),
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_line(color = "#d6d6d6")
)

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC", "FBS Independents")
group_of_5 <- c("American Athletic", "Conference USA", "Mid-American", "Mountain West", "Sun Belt")

# Pull in Games Data ------------------------------------------------------

base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data

games.master = data.frame()
for (j in 2014:2020) {
  for (i in 1:20) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

blowouts <- games.master %>% 
  mutate(margin_of_victory = abs(home_points - away_points), 
         start_date = as_datetime(start_date), 
         start_time = hour(with_tz(start_date, tzone = "EST"))) %>% 
  group_by(season, start_time) %>% 
  summarise(avg_margin = mean(margin_of_victory), count = n()) %>% 
  arrange(desc(avg_margin))

season_comp <- games.master %>% 
  mutate(margin_of_victory = abs(home_points - away_points), 
         start_date = as_datetime(start_date), 
         start_time = hour(with_tz(start_date, tzone = "EST"))) %>% 
  group_by(season) %>% 
  summarise(avg_margin = mean(margin_of_victory), count = n())

margin_2019 <- season_comp %>% filter(season == 2019) %>% pull(avg_margin) %>% round(digits = 1)
margin_2020 <- season_comp %>% filter(season == 2020) %>% pull(avg_margin) %>% round(digits = 1)


blowouts %>% 
  filter(season >= 2019) %>% 
  ggplot(aes(x = start_time, y = avg_margin, fill = as.factor(season))) + 
  geom_col(position = "dodge", width = 0.5) +
  labs(title = "Just something about those night games",
       subtitle = "Average Margin of Victory by Game Start Time (EST)",
       x = "Start Time Hour (EST)",
       y = "Avg. Margin of Victory",
       fill = "Season",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data") +
  annotate(geom = "text", 
           label = paste0("2019 Avg. Margin: ", margin_2019, 
                          "\n2020 Avg. Margin: ", margin_2020), x = 16, y = 23,
           color = staturdays_colors("dark_blue"), fontface = "bold",
           size = 7) +
  staturdays_theme +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = c(staturdays_colors("orange"), staturdays_palette[5])) +
  theme(plot.caption = element_text(size = 15))

ggsave(filename = paste0("margin_of_victory_", str_replace_all(now(), ":", "."), ".png"), 
       plot = last_plot(),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       dpi = 300, width = 400, height = 200, units = "mm")
