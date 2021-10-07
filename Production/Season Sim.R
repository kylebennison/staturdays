rm(list = ls())
library(data.table)
library(lubridate)
library(jsonlite)
library(RCurl)
library(tidyverse)
library(gt)
options(scipen=999)

#Staturdays Colors
{
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
                          plot.subtitle = element_text(color = staturdays_colors("lightest_blue"), size = 20),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          axis.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 15))
}
source("Production/source_everything.R")
elo_ratings <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/elo_ratings_historic.csv")

#get the current Elo ratings for all teams
elo_ratings$date <- as_datetime(elo_ratings$date)
current_elo_ratings <- elo_ratings[elo_ratings[, .I[which.max(date)], by = team]$V1]
current_elo_ratings <- current_elo_ratings[,c(1,3,6)]

#bring in 2021 schedule
cfb_schedule_url <- "https://api.collegefootballdata.com/games?year=2021&seasonType=regular"
schedule <- cfbd_api(cfb_schedule_url,my_key)

schedule$start_date <- as_date(schedule$start_date)

already_played <- schedule[!is.na(schedule$home_points),]
already_played$home_result <- ifelse(already_played$home_points > already_played$away_points, 1, 0)

# Summarise wins and losses for each team
lhs <- already_played %>% 
  group_by(home_team) %>% 
  summarise(home_wins = sum(home_result),
            home_losses = sum(1 - home_result))

rhs <- already_played %>% 
  group_by(away_team) %>% 
  summarise(away_wins = sum(1 - home_result),
            away_losses = sum(home_result))

joined_already_played <- full_join(lhs, rhs, by = c("home_team" = "away_team"))

already_played_sum <- joined_already_played %>% 
  rename(team = home_team) %>% 
  group_by(team) %>% 
  mutate(wins = sum(home_wins,away_wins, na.rm = T),
                                 losses = sum(home_losses, away_losses, na.rm = T)) %>% 
  select(team, wins, losses)

to_be_played <- schedule[is.na(schedule$home_points),]

#order for sim
to_be_played <- to_be_played[order(to_be_played$start_date),]

results <- data.table()

#testing
for(szn in c(1:5000)){
  season_elo_ratings <- current_elo_ratings
  season_results <- data.table()
  message("Simulating Season ", szn, "/5000")
  
  for(wk in c(min(to_be_played$week):max(to_be_played$week))){
    to_be_played_week <- to_be_played[to_be_played$week == wk,]
    if(nrow(to_be_played_week) !=0) {
      
    to_be_played_week <- merge(to_be_played_week, season_elo_ratings, by.x = "home_team", by.y = "team", all.x = TRUE)
    setnames(to_be_played_week, "elo_rating", "elo_rating_home")
    to_be_played_week <- merge(to_be_played_week, season_elo_ratings, by.x = "away_team", by.y = "team", all.x = TRUE)
    setnames(to_be_played_week, "elo_rating", "elo_rating_away")
       
    #fill NA to 1200
    to_be_played_week$elo_rating_home[is.na(to_be_played_week$elo_rating_home)] <- 1200
    to_be_played_week$elo_rating_away[is.na(to_be_played_week$elo_rating_away)] <- 1200
       
    #add in home field advantage
    to_be_played_week$elo_home_win_prob <- 1/(1+ (10^ ( (to_be_played_week$elo_rating_away - to_be_played_week$elo_rating_home + 55)/400)))
      
    to_be_played_week$rand_draw <- runif(nrow(to_be_played_week), 0, 1)
    to_be_played_week$home_result <- ifelse(to_be_played_week$rand_draw < to_be_played_week$elo_home_win_prob, 1, 0)
    
    #update elo ratings
    to_be_played_week$elo_rating_home_new <- to_be_played_week$elo_rating_home + 85*(to_be_played_week$home_result - to_be_played_week$elo_home_win_prob)
    to_be_played_week$elo_rating_away_new <- to_be_played_week$elo_rating_away + 85*( (1-to_be_played_week$home_result) - (1-to_be_played_week$elo_home_win_prob))
    new_elo <- to_be_played_week[,c("home_team", "away_team", "elo_rating_home_new", "elo_rating_away_new", "start_date")]
    new_home_elo <- new_elo[,c("home_team", "elo_rating_home_new", "start_date")]
    setnames(new_home_elo, c("home_team", "elo_rating_home_new", "start_date"), c("team", "elo_rating", "date"))
    new_away_elo <- new_elo[,c("away_team", "elo_rating_away_new", "start_date")]
    setnames(new_away_elo, c("away_team", "elo_rating_away_new", "start_date"), c("team", "elo_rating", "date"))
    new_elo <- rbind(new_home_elo, new_away_elo)
    new_elo$date <- as_datetime(new_elo$date)
    season_elo_ratings$date <- as_datetime(season_elo_ratings$date)
    season_elo_ratings <- rbind(season_elo_ratings, new_elo)
    season_elo_ratings <- season_elo_ratings[season_elo_ratings[, .I[which.max(date)], by = team]$V1]
    temp_tbp <- to_be_played_week
    temp_tbp$szn <- szn
    season_results <- rbind(season_results, temp_tbp)
    }
  }
  results <- rbind(results, season_results)
}

#process simulated games
summary_results_home <- dcast(results, home_team+szn~home_result, value.var = "home_result")
summary_results_away <- dcast(results, away_team+szn~home_result, value.var = "home_result", )
setnames(summary_results_away, c("away_team","0", "1"), c("team","1", "0"))
setnames(summary_results_home, c("home_team"), c("team"))
combined_results <- rbind(summary_results_home, summary_results_away)

#wins and losses per season
combined_results <- dcast(combined_results, team+szn~., value.var = c("0", "1"), fun.aggregate = sum)

# Add in already_played results, then sum before calculating win_perc and undefeated
cr_1 <- combined_results %>% left_join(already_played_sum, by = c("team"))

cr_2 <- cr_1 %>% 
  group_by(team, szn) %>% 
  mutate(win_total = sum(`1`, wins, na.rm = T),
                        loss_total = sum(`0`, losses, na.rm = T)) %>% 
  select(team, szn, win_total, loss_total) %>% 
  rename(`0` = loss_total, `1` = win_total)

combined_results <- cr_2 %>% ungroup()

combined_results$win_perc <- combined_results$`1`/(combined_results$`1`+combined_results$`0`)
combined_results$undefeated <- ifelse(combined_results$`0` == 0,1,0)

combined_results_3 <- combined_results %>% 
  group_by(team) %>% 
  summarise(wins = mean(`1`),
            losses = mean(`0`),
            games = mean(`1`)+mean(`0`), # total games on schedule
            win_perc = mean(win_perc),
            std_dev_wins = sd(`1`), # std dev of wins (same as losses)
            prob_undefeated = mean(undefeated))

# Raw wins percentile based on the bootstrap results (not using z-score calculation)
x<-combined_results %>% group_by(team) %>% 
  summarise(percentile_025 = quantile(`1`, .025),
            percentile_975 = quantile(`1`, .975))

# Adding a 95th percentile confidence interval for wins and losses, assuming normal distribution
combined_results_4 <- combined_results_3 %>% 
  mutate(upper_95_wins = if_else(wins + std_dev_wins*1.96 > games, games, wins + std_dev_wins*1.96),
         lower_95_wins = if_else(wins - std_dev_wins*1.96 < 0, 0, wins - std_dev_wins*1.96),
         upper_95_losses = if_else(losses + std_dev_wins*1.96 > games, games, losses + std_dev_wins*1.96),
         lower_95_losses = if_else(losses - std_dev_wins*1.96 < 0, 0, losses - std_dev_wins*1.96))

# Adding 95th percentile using bootstrapped results because we can not assume normal distribution
# as there is an upper limit on number of games won
combined_results_4 <- combined_results_3 %>% 
  left_join(x, by="team") %>% 
  rename(upper_95_wins = percentile_975 ,
         lower_95_wins = percentile_025)

# Add table-friendly win and loss range columns
win_loss_tbl <- combined_results_4 %>% 
  mutate(upper_95_wins = round(upper_95_wins, 1),
         lower_95_wins = round(lower_95_wins, 1),
         #upper_95_losses = round(upper_95_losses, 1), 
         #lower_95_losses = round(lower_95_losses, 1),
         win_range = paste0(lower_95_wins, " - ", upper_95_wins),
         #loss_range = paste0(lower_95_losses, " - ", upper_95_losses)
         )

# Plot of wins distribution for a team
combined_results %>% 
  filter(team == "Clemson") %>% 
  ggplot(aes(x = `1`)) +
  geom_histogram()

# Get conferences
conf <- cfbd_api("https://api.collegefootballdata.com/teams/fbs?year=2021",my_key)
conf <- conf %>% select(school, conference)

for(conf_name in c("ACC", "Big Ten", "Big 12", "Pac-12", "SEC")) {

# GT Table
win_loss_gt <- win_loss_tbl %>% 
  select(team, wins, losses, win_perc, prob_undefeated, win_range, games, std_dev_wins) %>% 
  arrange((team)) %>% 
  left_join(conf, by = c("team" = "school")) %>% 
  filter(conference == conf_name) %>% 
  relocate(games, .before = win_range) %>% 
  gt() %>% 
  tab_header(title = md(paste0("**",conf_name," - 2021 Season Sim**")),
             subtitle = paste0("Results as of ", today())) %>% 
  cols_hide(columns = vars(conference)) %>% 
  cols_label(team = "Team", wins = "Expected Wins", losses = "Expected Losses", 
             win_perc = "Win Percentage", prob_undefeated = "Undefeated Probability", 
             win_range = "95% Wins", games = "Games", 
             std_dev_wins = "Std. Dev.") %>% 
  cols_align(align = "auto") %>% 
  cols_align(align = "left", columns = vars(team)) %>% 
  fmt_number(vars(wins, losses, std_dev_wins), decimals = 1, use_seps = FALSE) %>% 
  fmt_percent(vars(win_perc, prob_undefeated), decimals = 1, use_seps = FALSE) %>% 
  data_color(columns = vars(win_perc, prob_undefeated), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_style(style = list(cell_borders(
    sides = "right",
    color = staturdays_colors("dark_blue"),
    weight = px(3)
  )
  ),
  locations = list(
    cells_body(
      columns = vars(games)
    )
  )
  ) %>% 
  tab_source_note("@staturdays — Data: @cfb_data")

gtsave(data = win_loss_gt, 
       filename = paste0("sim_win_loss_tbl_", str_replace_all(now(), ":", "."), conf_name, ".png"),
       path = "R Plots/")



win_loss_tbl %>% 
  left_join(conf, by = c("team" = "school")) %>% 
  filter(conference == conf_name) %>% 
  pivot_longer(cols = c(lower_95_wins, upper_95_wins)) %>% 
  ggplot(aes(x = value, y = fct_rev(fct_reorder2(team, name, value)), color = name)) +
  geom_point(size = 5) +
  geom_line(aes(group = team), color = staturdays_colors("light_blue"), size = 3, alpha = .5) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  staturdays_theme +
  labs(x = "Wins",
       y = "Team",
       title = paste0(conf_name, " Win Distribution"),
       subtitle = "Based on Season Simulation using Elo Ratings",
       caption = "@staturdays | Data: @cfb_data") +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c(staturdays_colors("lightest_blue"), as.character(staturdays_colors("orange"))), labels = c("Lower 95th Percentile", "Upper 95th Percentile"))

ggsave(plot = last_plot(),
       filename = paste0(today(), "_",
                         conf_name,
                         "win_dist_sim",
                         ".png"),
       height = 200,
       width = 400,
       units = "mm",
       dpi = 300,
       path = "R Plots/")

}

win_loss_tbl %>% 
  filter(games == 12, prob_undefeated > 0) %>% 
  ggplot(aes(x = prob_undefeated, y = reorder(team, prob_undefeated))) +
  geom_col(fill = staturdays_colors("dark_blue")) +
  staturdays_theme +
  theme(axis.title = element_blank()) + 
  labs(title = "Probability of Going Undefeated",
       subtitle = paste0("Through ", max(already_played$week), " Weeks")) +
  scale_x_continuous(labels = scales::percent)

ggsave(plot = last_plot(),
       filename = paste0(today(), "_",
                         "undefeated_prob",
                         ".jpg"),
       height = 200,
       width = 400,
       units = "mm",
       dpi = 300,
       path = "R Plots/")
