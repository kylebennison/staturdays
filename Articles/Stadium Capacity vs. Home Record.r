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
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/cfbd_api_key_function.R")

# Themes and Colors -------------------------------------------------------

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

# Get average home record by capacity and away buckets
pivot_tbl <- team_pct %>% 
  group_by(year, team) %>% 
  mutate(capacity_bucket = round(location.capacity, digits = -4)) %>% 
  group_by(away_pct_bucket, capacity_bucket) %>% 
  summarise(avg_home_pct = sum(homeGames.wins) / sum(homeGames.games), n = n()) %>% 
  arrange(desc(away_pct_bucket), desc(capacity_bucket))


# Exploratory Analysis ----------------------------------------------------

locations_dist <- team_pct %>% ungroup() %>% distinct(team, location.capacity)

# Distribution of venue sizes
locations_dist %>% 
  ggplot(aes(x = location.capacity)) +
  geom_histogram(binwidth = 5000, fill = staturdays_colors("orange")) +
  labs(title = "Distribution of Stadium Sizes",
       x = "Capacity",
       y = "Count",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  scale_x_continuous(labels = scales::comma_format()) +
  staturdays_theme +
  theme(panel.grid = element_blank())

# hist
attendance.master %>% 
  filter(is.na(attendance) == F & attendance > 0) %>% 
  ggplot(aes(x = attendance, fill = conference_game)) +
  geom_histogram(position = "identity", alpha = 0.7)

# Boxplot
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

# Boxplot of Capacity and Home Wins
pivot_tbl %>% 
  filter(is.nan(away_pct_bucket) == F) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(capacity_bucket), y = avg_home_pct)) + 
  geom_boxplot(fill = staturdays_colors("lightest_blue")) +
  labs(title = "Capacity and Home Win %",
       x = "Stadium Capacity",
       y = "Home Win %",
       caption = "@kylebeni012 | @staturdays — Data: @cfb_data") +
  scale_x_discrete(labels = function(x) as.integer(x)) +
  scale_y_continuous(labels = scales::percent) +
  staturdays_theme +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 10))

# Make sure attendance and location correlate as expected
team_pct %>% 
  ungroup() %>% 
  ggplot(aes(x = location.capacity, y = avg_attendance)) +
  geom_point() +
  geom_abline()

# Start Modeling ----------------------------------------------------------

library(tidymodels)

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
car::vif(lm_fit$fit)

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

team_df %>% 
  mutate(away_record_bucket = round(away_record, digits = 1)) %>% 
  ggplot(aes(x = home_record, y = away_record, size = avg_attendance*1)) +
  geom_point(alpha = 0.3, color = "blue") +
  scale_size(range = c(1, 9))

team_df %>% View()


# Random Forest -----------------------------------------------------------

rf_spec <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

rf_fit <- rf_spec %>% 
  fit(home_record ~ location.capacity + away_record,
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
  ggplot(aes(truth, .pred, color = model)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~train) +
  labs(
    x = "Truth",
    y = "Predicted Home Record",
    color = "Type of model"
  )


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
