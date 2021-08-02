# Libraries and Themes ----------------------------------------------------

library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(jsonlite)
library(stringr)
library(lubridate)
library(gt)
library(data.table)
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")

# Pull in conference data for each year for all teams (FBS only) --------

conference_url <- "https://api.collegefootballdata.com/teams/fbs?year="
conference.master = data.frame()
for (j in 2000:2020) {
  cat('Loading Conferences ', j, '\n')
  full_url_conf <- paste0(conference_url, as.character(j))
  conf <- cfbd_api(full_url_conf, my_key)
  conf <- conf %>% mutate(year = j)
  conference.master = rbind(conference.master, conf)
}

records_url <- "https://api.collegefootballdata.com/records?year="
records.master = data.frame()
for (j in 2000:2020) {
  cat('Loading Records ', j, '\n')
  full_url_rec <- paste0(records_url, as.character(j))
  records <- cfbd_api(full_url_rec, my_key)
  records <- records %>% mutate(year = j)
  records.master = rbind(records.master, records)
}

attendance.master <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/games_historic.csv")


# Mutate Data -------------------------------------------------------------

# Average annual attendance by team
attendance_avg <- attendance.master %>% 
  filter(attendance != 0 & !is.na(attendance)) %>% 
  group_by(home_team, season) %>% 
  summarise(avg_attendance = mean(attendance))

# Win percentage
team_pct <- records.master %>% 
  group_by(year, team) %>% 
  summarise(win_pct = total.wins / total.games,
            home_pct = homeGames.wins / homeGames.games,
            away_pct = awayGames.wins / awayGames.games,
            total.wins, total.games, homeGames.games, homeGames.wins, awayGames.games, awayGames.wins)

# Join stadium capacities with records
team_pct <- team_pct %>% 
  left_join(conference.master, by = c("team" = "school", "year"))

# Raw home win rate by year
team_pct %>% 
  group_by(year) %>% 
  summarise(hgames = sum(homeGames.games), hwins = sum(homeGames.wins)) %>% 
  mutate(hrate = hwins / hgames) %>% 
  arrange(desc(year)) %>% 
  ggplot(aes(x = year, y = hrate)) +
  geom_col() +
  geom_hline(yintercept = .6) +
  labs(x = "Year",
       y = "Home Win %",
       title = "Average winning % at home for all teams in a season") +
  staturdays_theme

# Distribution of home wins by season - SAVE
team_pct %>% 
  filter(year >= 2015) %>%
  ungroup() %>% 
  ggplot(aes(x = home_pct)) + 
  geom_density(aes(fill = factor(year)), 
               position = "identity", 
               alpha = .3) +
  labs(x = "Home Winning %",
       y = "Density",
       fill = "Year",
       title = "Distribution of Home Records by Season",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  staturdays_theme +
  scale_fill_viridis_d() +
  scale_x_continuous(labels = scales::percent)

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "home_wins_by_season_density",
#                          ".png"),
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)

# SAVE Distribution of home wins by season - filtering out teams that played few home games
team_pct %>% 
  filter(year >= 2015, homeGames.games > 5) %>%
  ungroup() %>% 
  ggplot(aes(x = home_pct)) + 
  geom_density(aes(fill = factor(year)), 
                 position = "identity", 
                 alpha = .3) +
  labs(x = "Home Winning %",
       y = "Density",
       fill = "Year",
       title = "Distribution of Home Records by Season",
       subtitle = "Filtered for teams that played 6 or more home games in 2020",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  staturdays_theme +
  scale_fill_viridis_d() +
  scale_x_continuous(labels = scales::percent)

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "home_wins_by_season_density_6_games",
#                          ".png"),
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)

# Statistical significance of difference between 2020 and other years
team_pct %>% 
  filter(year >= 2015, homeGames.games > 5) %>%
  group_by(year) %>% 
  summarise(games = sum(homeGames.games), wins = sum(homeGames.wins)) %>% 
  mutate(wins / games) %>% group_by(year == 2020) %>% 
  summarise(sum(games), sum(wins)) %>% 
  mutate(`sum(wins)`/`sum(games)`)

prop.test(n = c(174, 3836), x = c(124, 2456), alternative = "greater")

# Dist of home games in 2020 vs. 2019 - SAVE
team_pct %>% 
  filter(year >= 2019) %>% 
  group_by(year) %>% 
  count(homeGames.games) %>% 
  ggplot(aes(x = homeGames.games, y = n, fill = factor(year))) + 
  geom_col(position = "dodge", alpha = 1) +
  scale_fill_viridis_d(end = .5) +
  labs(x = "Home Games Played",
       y = "Number of Teams",
       fill = "Year",
       title = "Distribution of Home Games by Season",
       subtitle = "More teams played a shortened home schedule in 2020",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  staturdays_theme

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "home_games_played_dist",
#                          ".png"),
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)

# See if elo factors in to this
elo.master <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/elo_ratings_historic.csv",
             encoding = "UTF-8")

elo <- elo.master %>% 
  filter(season == 2020 & week == 0)

team_pct %>% 
  filter(year == 2020) %>% 
  mutate(full_season = if_else(homeGames.games > 5, TRUE, FALSE)) %>% 
  left_join(elo, by = "team") %>% 
  group_by(full_season) %>% 
  summarise(avg_elo = mean(elo_rating))

# Home record by full_season
team_pct %>% 
  filter(year == 2020) %>% 
  mutate(full_season = if_else(homeGames.games > 5, TRUE, FALSE)) %>% 
  ggplot(aes(x = reorder(team, home_pct), y = home_pct)) + 
  geom_col(position = "identity", 
           aes(fill = factor(full_season))) +
  scale_fill_viridis_d(end = .5) +
  theme(axis.text = element_blank())

# Home record by full_season
team_pct %>% 
  filter(year == 2020) %>% 
  mutate(full_season = if_else(homeGames.games > 5, TRUE, FALSE)) %>% 
  ggplot(aes(x = home_pct)) + 
  geom_histogram(position = "dodge", 
                 aes(fill = factor(full_season)), alpha = .3, bins = 7) +
  scale_fill_viridis_d(end = .5)

# Stadium Size Investigation ----------------------------------------------

# Keep relevant columns and mutate buckets
team_pct <- team_pct %>% 
  select(team, year, win_pct, home_pct, away_pct, location.capacity, location.elevation,
         total.wins, total.games, homeGames.games, homeGames.wins, awayGames.games, awayGames.wins) %>% 
  ungroup() %>% 
  mutate(win_pct_bucket = round(win_pct, digits = 1),
         home_pct_bucket = round(home_pct, digits = 1),
         away_pct_bucket = round(away_pct, digits = 1))

# Join in attendance data
team_pct <- team_pct %>% 
  left_join(attendance_avg, by = c("team" = "home_team", "year" = "season"))

# Get average home record by Capacity and away buckets
pivot_tbl <- team_pct %>% 
  group_by(year, team) %>% 
  mutate(capacity_bucket = round(location.capacity, digits = -4)) %>% 
  group_by(away_pct_bucket, capacity_bucket) %>% 
  summarise(avg_home_pct = sum(homeGames.wins) / sum(homeGames.games), n = n()) %>% 
  arrange(desc(away_pct_bucket), desc(capacity_bucket))

# Stadium Attendnce
pivot_2 <- team_pct %>% 
  group_by(year, team) %>% 
  mutate(capacity_bucket = round(location.capacity, digits = -4),
         attendance_bucket = round(avg_attendance, digits = -4)) %>% 
  group_by(away_pct_bucket, attendance_bucket) %>% 
  summarise(avg_home_pct = sum(homeGames.wins) / sum(homeGames.games), n = n()) %>% 
  arrange(desc(away_pct_bucket), desc(attendance_bucket))

# Exploratory Analysis ----------------------------------------------------

locations_dist <- team_pct %>% ungroup() %>% distinct(team, location.capacity)

# Distribution of venue sizes - SAVE
locations_dist %>% 
  ggplot(aes(x = location.capacity)) +
  geom_histogram(binwidth = 5000, fill = staturdays_colors("orange"),
                 alpha = 0.7) +
  labs(title = "Distribution of Stadium Sizes",
       x = "Capacity",
       y = "Count",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  scale_x_continuous(labels = scales::comma_format()) +
  staturdays_theme +
  theme(panel.grid = element_blank())

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "stadium_size_dist",
#                          ".png"),
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)

# hist of attendance in conference games
attendance.master %>% 
  filter(is.na(attendance) == F & attendance > 0) %>% 
  ggplot(aes(x = attendance, fill = conference_game)) +
  geom_histogram(position = "identity", alpha = 0.7)

# Boxplot of home and away win percentages - SAVE
team_pct %>% 
  filter(is.nan(away_pct_bucket) == F) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(away_pct_bucket), y = home_pct)) + 
  geom_boxplot(fill = staturdays_colors("lightest_blue")) +
  labs(title = "Home and Away Win % Are Correlated",
       x = "Away Win %",
       y = "Home Win %",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  scale_x_discrete(labels = function(x) as.double(x) %>% scales::percent()) +
  scale_y_continuous(labels = scales::percent) +
  staturdays_theme +
  theme(panel.grid = element_blank())

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "home_away_boxplot",
#                          ".png"),
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)

# Boxplot of Capacity and Home Wins - SAVE
pivot_tbl %>% 
  filter(is.nan(away_pct_bucket) == F) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(capacity_bucket), y = avg_home_pct)) + 
  geom_boxplot(fill = staturdays_colors("lightest_blue")) +
  labs(title = "Capacity and Home Win %",
       x = "Stadium Capacity",
       y = "Home Win %",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  scale_x_discrete(labels = function(x) scales::number(as.integer(x), big.mark = ",")) +
  scale_y_continuous(labels = scales::percent) +
  staturdays_theme +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 10))

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "capacity_win_boxplot",
#                          ".png"),
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)

# Same as above but with attendance - SAVE

pivot_2 %>% 
  filter(is.nan(away_pct_bucket) == F) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(attendance_bucket), y = avg_home_pct)) + 
  geom_boxplot(fill = staturdays_colors("lightest_blue")) +
  labs(title = "Attendance and Home Win %",
       x = "Average Attendance",
       y = "Home Win %",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  scale_x_discrete(labels = function(x) scales::number(as.integer(x), big.mark = ",")) +
  scale_y_continuous(labels = scales::percent) +
  staturdays_theme +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 10))

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "attendance_win_boxplot",
#                          ".png"),
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)

# Same as above but with win percentage
team_pct %>% mutate(pct_cap = avg_attendance / location.capacity) %>% 
  group_by(team) %>% 
  summarise(
  avg_pct_cap = mean(pct_cap, na.rm = T),
  hgames = sum(homeGames.games),
  hwins = sum(homeGames.wins),
  n_games = n()
) %>% 
  mutate(hrecord = hwins / hgames) %>% 
  mutate(pct_cap_bucket = round(avg_pct_cap, 1)) %>% 
  filter(pct_cap_bucket > .4 & pct_cap_bucket < 1.1) %>% 
  ggplot(aes(x = factor(pct_cap_bucket), y = hrecord)) + 
  geom_boxplot(fill = staturdays_colors("lightest_blue")) +
  labs(title = "Percent of Capacity and Home Win %",
       x = "Average Percent of Stadium Filled",
       y = "Home Win %",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  scale_x_discrete(labels = function(x) scales::percent(as.double(x))) +
  scale_y_continuous(labels = scales::percent) +
  staturdays_theme +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 10))

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "pct_cap_filled_boxplot",
#                          ".png"),
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)


# Both plots side by side
p1 <- team_pct %>% 
  ggplot(aes(x = location.capacity, y = home_pct)) + 
  geom_point(alpha = 0.3, position = "jitter",
             color = staturdays_colors("lightest_blue")) +
  geom_smooth(method = "lm", color = staturdays_colors("orange")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(x = "Capacity",
       y = "Home Win %")

p2 <- team_pct %>% 
  ggplot(aes(x = avg_attendance, y = home_pct)) +
  geom_point(alpha = 0.3, position = "jitter",
             color = staturdays_colors("lightest_blue")) +
  geom_smooth(method = "lm", color = staturdays_colors("orange")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
  labs(x = "Average Attendance",
       y = "Home Win %")

p3 <- team_pct %>% 
  mutate(pct_cap = avg_attendance / location.capacity) %>% 
  filter(pct_cap < 1.1) %>% 
  ggplot(aes(x = pct_cap, y = home_pct)) +
  geom_point(alpha = 0.3, position = "jitter",
             color = staturdays_colors("lightest_blue")) +
  geom_smooth(method = "lm", color = staturdays_colors("orange")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent, n.breaks = 8) +
  labs(x = "Average % of Total Capacity",
       y = "Home Win %")

library(g)

gridExtra::grid.arrange(p1, p2, p3, nrow = 1,
                        top = textGrob("Stadium Capacity and Attendance are Similarly Correlated with Wins",
                                       gp = gpar(fontsize = 20)),
                        bottom = textGrob("Season averages by team from 2000 - 2020",
                                          gp = gpar(fontsize = 20)))

g1 <- gridExtra::arrangeGrob(p1, p2, p3, nrow = 1,
                             top = textGrob("Stadium Capacity and Attendance are Similarly Correlated with Wins",
                                            gp = gpar(fontsize = 20)),
                             bottom = textGrob("Season averages by team from 2000 - 2020",
                                               gp = gpar(fontsize = 20)))

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "3x1_comparison_grid",
#                          ".png"),
#        plot = g1,
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)

# Correlation of stadium size and home record by year
t1 <- team_pct
t1_cor <- t1 %>% group_by(year) %>% summarise(cor_val = cor(location.capacity, home_pct))

# Change in importance of size of stadium by year
team_pct %>% 
  left_join(t1_cor, by = "year") %>% 
  ggplot(aes(x = location.capacity, y = home_pct)) + 
  facet_wrap(~ year) +
  geom_point(alpha = 0.3, position = "jitter",
             color = staturdays_colors("lightest_blue")) +
  geom_smooth(method = "lm", color = staturdays_colors("orange")) +
  geom_text(aes(x = 60000, 
                y = .9, 
                label = if_else(team == "Oklahoma State", paste0("Cor = ", round(cor_val, 2)), "")), # using ok state arbitrarily so only one value per year is plotted
            fontface = "bold",
            size = 8) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::number_format(big.mark = ","), breaks = c(25000, 50000, 75000, 100000)) +
  labs(x = "Capacity",
       y = "Home Win %",
       title = "Big teams lost their edge in 2020",
       subtitle = "The relationship between big stadiums and better home records almost disappeared",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  staturdays_theme

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "capacity_by_year_cor",
#                          ".png"),
#        width = 400,
#        height = 400,
#        units = "mm",
#        dpi = 300)

# Summarise across all teams and seasons
p4 <- team_pct %>%
  mutate(bucket_cap = round(location.capacity, -4), 
         bucket_att = round(avg_attendance, -4)) %>% 
  group_by(bucket_cap) %>% 
  summarise(home_wins = sum(homeGames.wins), 
            home_games = sum(homeGames.games)) %>% 
  mutate(win_rate = home_wins / home_games) %>% 
  ggplot(aes(x = bucket_cap, y = win_rate)) +
  geom_smooth() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 110000))

p5 <- team_pct %>%
  mutate(bucket_cap = round(location.capacity, -4), 
         bucket_att = round(avg_attendance, -4)) %>% 
  group_by(bucket_att) %>% 
  summarise(home_wins = sum(homeGames.wins), 
            home_games = sum(homeGames.games)) %>% 
  mutate(win_rate = home_wins / home_games) %>% 
  filter(bucket_att > 0) %>% 
  ggplot(aes(x = bucket_att, y = win_rate)) +
  geom_smooth() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 110000))

gridExtra::grid.arrange(p4, p5, nrow = 1) # SAVE

# Make sure attendance and location correlate as expected
team_pct %>% 
  ungroup() %>% 
  ggplot(aes(x = location.capacity, y = avg_attendance)) +
  geom_point(alpha = 0.3) +
  geom_abline() +
  ggrepel::geom_text_repel(aes(label = if_else(avg_attendance / location.capacity < .25, team, "")),
                           force = 2)

# Start Modeling ----------------------------------------------------------

library(tidymodels)

# Summarise across seasons by team and location cap, excluding 2020
team_df <- team_pct %>% 
  filter(!is.na(avg_attendance)) %>% 
  group_by(team, location.capacity) %>% 
  summarise(across(c(homeGames.wins, homeGames.games, awayGames.wins, awayGames.games,
                     total.wins, total.games), sum), across(avg_attendance, mean)) %>% 
  mutate(home_record = homeGames.wins / homeGames.games,
            away_record = awayGames.wins / awayGames.games,
            total_record = total.wins / total.games,
         pct_capacity = avg_attendance / location.capacity) %>% 
  mutate(pct_capacity = case_when(pct_capacity > 1 ~ 1,
                                  TRUE ~ pct_capacity))

# Summarise by pct_capacity bucket
team_df_2 <- team_pct %>%
  filter(!is.na(avg_attendance)) %>% 
  mutate(pct_capacity = avg_attendance / location.capacity,
         capacity_bucket = round(pct_capacity, 1)) %>% 
  group_by(capacity_bucket) %>% 
  summarise(home_wins = sum(homeGames.wins), 
            home_games = sum(homeGames.games),
            n = n()) %>% 
  mutate(win_rate = home_wins / home_games) %>% 
  filter(n > 30)

set.seed(1234)
capacity_split <- team_df %>% 
  initial_split()
  
capacity_split

capacity_train <- training(capacity_split)
capacity_test <- testing(capacity_split)

lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")

lm_fit <- lm_spec %>% 
  fit(home_record ~ location.capacity + away_record + pct_capacity,
      data = capacity_train)

lm_fit

# Opting not to use raw attendeance data because it's highly correlated to stadium size
# However, pct_capacity is not as highly correlated, indicating there are 
# some teams that have large stadiums but can't fill them up
car::vif(lm_fit$fit) # Check VIF of variables in model (under 5 is acceptable)

lm_summary <- tidy(lm_fit)

lm_summary

summary(lm_fit$fit)

lm_summary$estimate[4] / 10 # for every 10% increase in % capacity, home wins improve 1.2%


# Graphs of Relationships -------------------------------------------------

team_df %>% 
  ggplot(aes(x = avg_attendance, y = home_record)) +
  geom_point() +
  geom_smooth(method = "lm")

team_df %>% 
  filter(pct_capacity < 1.5) %>% 
  ggplot(aes(x = pct_capacity, y = home_record)) +
  geom_point() +
  geom_smooth(method = "lm")

team_df %>% 
  ggplot(aes(x = avg_attendance, y = away_record)) +
  geom_point() +
  geom_smooth(method = "lm")

team_df %>% 
  ggplot(aes(x = avg_attendance, y = home_record)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "blue") +
  geom_point(aes(y = away_record), color = "red") +
  geom_smooth(aes(y = away_record), method = "lm", color = "red") +
  labs(y = "Win Percentage",
       subtitle = ("Blue is home record, Red is away record\nAttendance is more highly correlated with away record than home\nMaybe winning away drives fan excitement for home games")
  ) +
  geom_text(aes(x = 90000, y = .9), label = "Home Record", color = "blue") +
  geom_text(aes(x = 90000, y = .4), label = "Away Record", color = "red")


team_df %>% 
  ggplot(aes(x = home_record, y = away_record)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm")

cor(team_df$home_record, team_df$away_record)
cor(team_df$home_record, team_df$avg_attendance)
cor(team_df$avg_attendance, team_df$away_record)
cor(team_df$avg_attendance, team_df$location.capacity)
cor(team_df$location.capacity, team_df$pct_capacity)

# SAVE Home and Away records correlation with attendance at games - .57 cor
team_df %>% 
  mutate(away_record_bucket = round(away_record, digits = 1)) %>% 
  ggplot(aes(x = home_record, y = away_record, size = avg_attendance*1)) +
  geom_point(alpha = 0.3, color = "blue") +
  scale_size(range = c(1, 9))

# SAVE Percent Capacity vs. away record and home record - .62 cor
team_df %>% 
  mutate(away_record_bucket = round(away_record, digits = 1)) %>% 
  pivot_longer(cols = c(home_record, away_record), names_to = "record") %>% 
  ggplot(aes(x = value, y = pct_capacity, color = record)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE, method = "lm", size = 2) +
  scale_color_manual(values = c(staturdays_colors("orange"), as.character(staturdays_colors("dark_blue"))),
                     labels = c("Away", "Home")) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_continuous(labels = scales::label_percent()) +
  staturdays_theme +
  labs(x = "Winning Percentage",
       y = "Average Percent of Stadium Capacity",
       color = "Location",
       title = "What Drives Attendance?",
       subtitle = "Playing well away from home sells more tickets than actually playing well at home",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data")

# ggsave(filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                          today(),
#                          "_",
#                          "home_vs_away_attendance",
#                          ".png"),
#        width = 400,
#        height = 200,
#        units = "mm",
#        dpi = 300)

# LM by Year --------------------------------------------------------------

# Year DF
year_df <- team_pct %>% 
  group_by(year, cap_bucket = round(location.capacity, -4)) %>% 
  summarise(across(c(homeGames.wins, homeGames.games, awayGames.wins, awayGames.games,
                     total.wins, total.games), sum), across(avg_attendance, mean)) %>% 
  mutate(home_record = homeGames.wins / homeGames.games,
         away_record = awayGames.wins / awayGames.games,
         total_record = total.wins / total.games,
         pct_capacity = avg_attendance / cap_bucket) %>% 
  mutate(pct_capacity = case_when(pct_capacity > 1 ~ 1,
                                  TRUE ~ pct_capacity)) %>% 
  mutate(is_2020 = if_else(year == 2020, TRUE, FALSE))

year_split <- year_df %>% 
  initial_split()

year_split

year_train <- training(year_split)
year_test <- testing(year_split)

lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")

lm_fit_year <- lm_spec %>% 
  fit(home_record ~ cap_bucket + away_record + is_2020,
      data = year_train)

lm_fit

summary(lm_fit$fit)

results_gt_lm <- tidy(lm_fit) %>% 
  cbind(tibble("Term2" = c("Intercept", "Stadium Size", "Away Record", "Percent Capacity"))) %>% 
  select(Term2, everything()) %>% 
  select(-term) %>% 
  rename(term = Term2) %>% 
  gt::gt() %>% 
  tab_header(title = "Results of Linear Model") %>% 
  fmt_number(columns = c(2:5),
             decimals = 2) %>% 
  cols_label(term = "Term",
             estimate = "Estimate",
             std.error = "Standard Error",
             statistic = "Statistic",
             p.value = "P-Value") %>% 
  data_color(columns = c(p.value),
             colors = scales::col_numeric(
               palette = as.character(c("blue", "grey", "white", "red")),
               domain = NULL
             ),
             alpha = .3)

# gt::gtsave(results_gt_lm,
#            filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
#                              today(),
#                              "_",
#                              "lin_reg_results_gt",
#                              ".png"))
# Random Forest -----------------------------------------------------------

rf_spec <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

rf_fit <- rf_spec %>% 
  fit(home_record ~ location.capacity + away_record + pct_capacity + avg_attendance,
      data = capacity_train)

rf_fit


# Evaluate Results --------------------------------------------------------

results_train <- lm_fit %>% 
  predict(new_data = capacity_train) %>% 
  mutate(truth = capacity_train$home_record,
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data = capacity_train) %>% 
              mutate(truth = capacity_train$home_record,
                     model = "rf"))

results_test <- lm_fit %>% 
  predict(new_data = capacity_test) %>% 
  mutate(truth = capacity_test$home_record,
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data = capacity_test) %>% 
              mutate(truth = capacity_test$home_record,
                     model = "rf"))

results_train %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_test %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)


# Visualize Results -------------------------------------------------------

results_test %>%
  mutate(train = "testing") %>%
  bind_rows(results_train %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~train) +
  labs(
    x = "Truth",
    y = "Predicted Home Record"
  ) +
  geom_smooth(method = "lm")


# Predict individual game results -----------------------------------------

conference.small <- conference.master %>% 
  select(school, location.capacity, location.elevation, location.latitude,
         location.longitude, year)

games_clean <- attendance.master %>% left_join(
  conference.small, by = c("home_team" = "school", "season" = "year")
) %>% 
  tibble()

elo_join <- elo.master %>% arrange(date) %>% 
  group_by(team) %>% 
  mutate(previous_elo = lag(elo_rating, 1L))

games_2 <- games_clean %>% 
  mutate(home_team_won = factor(if_else(home_points > away_points, 1, 0)),
         pct_capacity = attendance / location.capacity) %>% 
  left_join(elo_join, by = c("home_team" = "team", "start_date" = "date", "season" = "season")) %>% 
  rename(home_elo = previous_elo) %>% 
  left_join(elo_join, by = c("away_team" = "team", "start_date" = "date", "season" = "season")) %>% 
  rename(away_elo = previous_elo) %>% 
  mutate(elo_diff = home_elo - away_elo)
  
set.seed(1234)

games_split <- games_2 %>% 
  initial_split()

games_split

games_train <- training(games_split)
games_test <- testing(games_split)

log_spec <- logistic_reg() %>% 
  set_engine(engine = "glm")

log_fit <- log_spec %>% 
  fit(home_team_won ~ location.capacity + pct_capacity + elo_diff + attendance,
      data = games_train)

log_fit

# Opting not to use raw attendeance data because it's highly correlated to stadium size
# However, pct_capacity is not as highly correlated, indicating there are 
# some teams that have large stadiums but can't fill them up
car::vif(log_fit$fit) # Check VIF of variables in model (under 5 is acceptable)

log_summary <- tidy(log_fit)

log_summary

summary(log_fit$fit)

# Apply model to data

log_results_train <- log_fit %>% 
  predict(new_data = games_train) %>% 
  mutate(truth = games_train$home_team_won)

roc_auc(log_results_train,
        truth = truth,
        .preds = .pred_class)

## Try old way
games_3 <- games_2 %>% 
  mutate(home_team_won = if_else(home_team_won == 1, 1L, 0L)) %>% 
  filter(!if_any(c(location.capacity, 
                  pct_capacity, 
                  elo_diff, 
                  attendance),
                ~ is.na(.)))

games_split <- games_3 %>% 
  initial_split()

games_train <- training(games_split)
games_test <- testing(games_split)

games_model <- glm(home_team_won ~ location.capacity + pct_capacity + elo_diff + attendance,
    data = games_train, family = "binomial")

games_model <- glm(home_team_won ~ elo_diff,
                   data = games_train)

summary(games_model)
coef(summary(games_model))[,4]

results_gt <- tidy(games_model) %>% 
  cbind(tibble("Term2" = c("Intercept", "Stadium Size", "Percent Capacity", "Elo Difference", "Attendance"))) %>% 
  select(Term2, everything()) %>% 
  select(-term) %>% 
  rename(term = Term2) %>% 
  gt::gt() %>% 
  tab_header(title = "Results of Logistic Model") %>% 
  fmt_number(columns = c(2:5),
             decimals = 2) %>% 
  cols_label(term = "Term",
             estimate = "Estimate",
             std.error = "Standard Error",
             statistic = "Statistic",
             p.value = "P-Value") %>% 
  data_color(columns = c(p.value),
             colors = scales::col_numeric(
               palette = as.character(c("blue", "grey", "white", "red")),
               domain = NULL
             ),
             alpha = .3)

#gt::gtsave(results_gt,
 #          filename = paste0("C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots/",
  #                           today(),
   #                          "_",
    #                         "log_reg_results_gt",
     #                        ".png"))

# Predict
games_test$prediction <- predict(games_model, newdata = games_test, allow.new.levels = TRUE)
games_test$pred_win_prob <- exp(games_test$prediction)/(1+exp(games_test$prediction))

# Add what elo would predict
calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

games_test$pred_win_prob_elo <- calc_expected_score(games_test$home_elo, games_test$away_elo)

# Residuals
games_test$resid <- games_test$home_team_won - games_test$pred_win_prob

mean(abs(games_test$resid), na.rm = T)

games_test %>% 
  ggplot(aes(x = as.factor(home_team_won), y = pred_win_prob)) +
  geom_point(alpha = 0.1)

games_test %>% 
  ggplot(aes(x = elo_diff)) +
  geom_point(aes(y = pred_win_prob), colour = "blue", alpha = 0.1)

games_test %>% 
  ggplot(aes(x = location.capacity)) +
  geom_point(aes(y = pred_win_prob), colour = "blue", alpha = 0.1)

library(pROC)
game_roc <- roc(games_test$home_team_won, predict(games_model, newdata = games_test))
game_auc <- toString(game_roc$auc)

# plot of AUC
ggwin_roc <- ggroc(game_roc)

ggwin_roc +
  geom_text(mapping = aes(x = 0.5, y = 0.5, label = paste0('AUC of ', round(as.double(game_auc), 5))))

games_test %>% 
  mutate(pred_bucket = round(pred_win_prob, 2)) %>% 
  group_by(pred_bucket) %>% 
  summarise(avg_win_rate = mean(home_team_won)) %>% 
  ggplot(aes(x = pred_bucket, y = avg_win_rate)) +
  geom_point() +
  geom_abline(linetype = 2) +
  xlim(0, 1) +
  ylim(0, 1)

# Redo Random Forest with Folds -------------------------------------------

capacity_folds <- vfold_cv(capacity_train)

mixed_model_wf <- workflow() %>%
  add_model(rf_spec, formula = home_record ~ location.capacity + away_record) %>%
  add_variables(outcomes = home_record, predictors = c(location.capacity, away_record))

rf_res <- fit_resamples(
  mixed_model_wf,
  capacity_folds,
  control = control_resamples(save_pred = TRUE)
)

# Evaluate
rf_res %>% 
  collect_metrics()

# Graph folds
rf_res %>%
  unnest(.predictions) %>%
  ggplot(aes(home_record, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted home_record",
    color = NULL
  )

# First Try ---------------------------------------------------------------

reg_line <- summary(lm(home_pct ~ away_pct + location.capacity, data = team_pct))
reg_line
win_pct_change_per_ten_k_capacity <- reg_line$coefficients[3]*10000

pivot_tbl %>% 
  filter(n > 20) %>% 
  ggplot(aes(x = capacity_bucket, y = avg_home_pct)) +
  geom_point(aes(size = n), alpha = 0.4) +
  geom_smooth(method = "lm") +
  facet_wrap(facets = "away_pct_bucket") +
  labs(title = "Does size matter?",
       subtitle = "Struggling teams get a lift from big home crowds")

summary(lm(avg_home_pct ~ away_pct_bucket + capacity_bucket, data = pivot_tbl))


team_pct %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(x = location.capacity, y = home_pct)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(facets = "year") +
  labs(title = "Does size matter?",
       x = "Stadium Capacity",
       y = "Home Record")

team_pct %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(x = location.capacity, y = home_pct)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(facets = "win_pct_bucket") +
  labs(title = "Does size matter?",
       x = "Stadium Capacity",
       y = "Home Record")

team_pct %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(x = location.capacity, y = home_pct)) +
  geom_smooth(method = 'lm', formula = y~x) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(facets = "away_pct_bucket") +
  labs(title = "Does size matter?",
       x = "Stadium Capacity",
       y = "Home Record")

# Home Field Advantage by Location of Stadium -----------------------------
### Theorizing that teams up north may have a home field advantage due to weather

conference.master %>% 
  geom_map(map = c(x = location.longitude, y = location.latitude))
