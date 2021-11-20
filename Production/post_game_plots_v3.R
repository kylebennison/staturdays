#' Will need to start after 12 pm on Saturday and check every
#' 30 minutes or so for new game results
#' Need a way to know whether a game is complete or not, although
#' I'm pretty sure the plays data doesn't get published until after
#' the game.
#'
#' Last updated with 3.1.13 model on 11/17/21 by kylebennison

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/source_everything.R")

library(rtweet)
library(xgboost)

# Twitter Auth
app_name <- "Staturdays CFB Tweets"
consumer_key <- Sys.getenv("twtr_staturdays_api_key")
consumer_secret <- Sys.getenv("twtr_staturdays_api_secret")
access_token <- Sys.getenv("twtr_staturdays_access_tok")
access_secret <- Sys.getenv("twtr_staturdays_access_secret")

tok <- create_token(app=app_name,
                    consumer_key = consumer_key,
                    consumer_secret = consumer_secret,
                    access_token = access_token,
                    access_secret = access_secret)

setwd("C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays")

elo <- get_elo()
calendar <- get_anything("https://api.collegefootballdata.com/calendar", 2021, 2021, key = my_key)
max_week <- calendar %>% 
  filter(lastGameStart <= lubridate::today()) %>% 
  pull(week) %>% 
  max()
current_week <- calendar %>% 
  filter(lastGameStart >= lubridate::today()) %>% 
  pull(week) %>% 
  min()
current_year <- max(elo$season)
plays <- get_plays(current_week, current_week, current_year, current_year)
plays <- plays %>% add_success()
games <- get_games(current_year, current_year, current_week, current_week)
# Get betting data
betting <- get_betting(current_year, current_year, current_week, current_week)

# Cumulative QB PPA

games_done <- tibble(games_done = c(0, 1)) # Store games that have already been run

games_done <- data.table::fread("Data/games_done.csv") # Read in games that have already been tweeted

# Remove "id_" from start of string
games_done <- games_done %>% mutate(games_done = stringr::str_sub(games_done, start = 4L))

game_ids <- plays$game_id %>% unique() # Get all game ids from today

qbs <- plays$pass_player %>% unique()

cum_sum_qb <- plays %>%
  mutate(
    player = case_when(
      is.na(pass_player) & !is.na(rush_player) ~ rush_player,
      is.na(rush_player) &
        !is.na(pass_player) ~ pass_player,
      TRUE ~ NA_character_
    ),
    ppa = as.double(ppa)
  ) %>%
  filter(player %in% qbs, is.na(ppa) == FALSE) %>%
  group_by(player) %>%
  mutate(cum_ppa = cumsum(ppa))

# Validate Data
# cum_sum_qb %>% 
#   select(player, id, game_id, play_number, play_text, ppa, cum_ppa) %>% 
#   filter(player == "Sean Clifford") %>% 
#   arrange(id) %>% View()

# Plot data

team_colors <- get_colors()

# Change any teams that are pure white to their backup color - only applies to Toledo
team_colors <- team_colors %>% 
  mutate(color = if_else(color == "#ffffff", alt_color, color))

cum_sum_qb_plot_data <- cum_sum_qb %>% 
  left_join(team_colors, by = c("offense" = "school")) %>% 
  group_by(player) %>% 
  mutate(n = n()) %>% 
  filter(n > 4, !str_detect(player, "^TEAM\\s")) # QBs involved in at least 5 plays, not "Team" plays

game_ids <- game_ids[which(!game_ids %in% games_done$games_done)] # Filter out any games that have already been run

# In-Game WP

# Read in model
XGBm <- readRDS("Production Models/in_game_wp_v3.1.13.rds")

# Prep plays data

games.temp <- games %>% 
  select(id, home_team, home_points, away_team, away_points, neutral_site) %>% 
  mutate(id = as.character(id))

plays.master.win_prob <- plays %>%
  mutate(
    home_score = case_when(home == offense ~ offense_score, # Get home lead/deficit
                           TRUE ~ defense_score),
    away_score = case_when(away == offense ~ offense_score,
                           TRUE ~ defense_score),
    home_score_lead_deficit = home_score - away_score
  ) %>%
  left_join(games.temp,
            by = c(
              "home" = "home_team",
              "away" = "away_team",
              "game_id" = "id"
            )) # Join games to get final result for each play

# Add win/loss boolean
plays.master.win_prob2 <-
  plays.master.win_prob %>% mutate(home_outcome = case_when(
    home_points > away_points ~ 1,
    home_points < away_points ~ 0,
    TRUE ~ 0.5
  ))

# Add home possession flag if they have the ball or not
plays.master.win_prob2 <- plays.master.win_prob2 %>% 
  mutate(home_poss_flag = if_else(home == offense, 1, 0),
         home_timeouts = if_else(home == offense, offense_timeouts, defense_timeouts),
         away_timeouts = if_else(away == offense, offense_timeouts, defense_timeouts))

### NEW
# Adjust timeouts based on the half
plays.master.win_prob2 <- plays.master.win_prob2 %>% 
  mutate(home_timeouts_new = if_else(period %in% c(1,2), 
                                     home_timeouts + 3L,
                                     home_timeouts),
         away_timeouts_new = if_else(period %in% c(1,2), 
                                     away_timeouts + 3L,
                                     away_timeouts)
  )
### END NEW

rm(plays.master.win_prob)

#elo ratings
elo_ratings <- elo %>% 
  select(team, elo_rating, week, season)

elo_ratings_adj <- elo_ratings %>% mutate(week = week + 1)

# Having an issue here where I end up with more rows than before. Join keys may not be unique i.e. multiple matches on rhs for certain plays on lhs
plays.master.win_prob3 <- plays.master.win_prob2 %>% left_join(elo_ratings_adj, by = c("home" = "team", "week", "year" = "season")) %>% 
  rename(home_elo = elo_rating) %>% 
  left_join(elo_ratings_adj, by = c("away" = "team", "week", "year" = "season")) %>% 
  rename(away_elo = elo_rating) %>% 
  distinct() %>% 
  mutate(clock_in_seconds = 2700-(900*(period-1)) + minutes*60 + seconds) %>% 
  tidyr::replace_na(list(home_timeouts = 0, away_timeouts = 0, home_elo = 1300, away_elo=1300,
                         offense_timeouts = 0, defense_timeouts=0,
                         home_timeouts_new = 0,
                         away_timeouts_new = 0))


# Add home_elo_diff
plays.master.win_prob3 <- plays.master.win_prob3 %>% 
  mutate(home_elo_diff = home_elo - away_elo)

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

### NEW
# Add elo home wp

plays.master.win_prob3 <- plays.master.win_prob3 %>% 
  mutate(home_elo_wp = calc_expected_score(home_elo + 
                                             if_else(neutral_site == FALSE |
                                                       is.na(neutral_site) == TRUE, 
                                                     55L, 
                                                     0L),
                                           away_elo))
### END NEW

### Get number of plays and % of plays completed
plays.master.win_prob3 <- plays.master.win_prob3 %>% 
  group_by(game_id) %>% 
  mutate(play_num = row_number(),
         n_plays = n(),
         pct_done = play_num / n_plays)


### keep only the first play when there are duplicate times ####
#MAKE END ROW FOR EACH GAME THAT SHOWS WHO WON - only for games that are finished
plays.make.end.rows <- plays.master.win_prob3 %>% 
  group_by(game_id) %>% 
  filter(row_number()==n()) %>% 
  ungroup()

# #filter out timeout rows?
# plays.master.win_prob3 <- plays.master.win_prob3 %>% group_by(game_id, clock_in_seconds) %>% 
#   filter(row_number()==1) %>%  #n()) %>% 
#   ungroup()

x<-plays.make.end.rows %>% 
  mutate(period=20,
         home_timeouts=0,
         away_timeouts=0,
         home_timeouts_new=0,
         away_timeouts_new=0,
         clock_in_seconds=-10000,
         down=5,
         distance=100,
         home_score_lead_deficit=home_score_lead_deficit,
         yards_to_goal=100,
         home_poss_flag=if_else(home_score_lead_deficit > 0, 1, 0),
  )

#add on user created row
plays.master.win_prob4 <- rbind(plays.master.win_prob3, x)


plays.master.win_prob4 <- plays.master.win_prob4 %>% 
  mutate(game_over = ifelse(period==20,1,0))

# Join in betting data
plays.master.win_prob4 <- plays.master.win_prob4 %>% 
  left_join(betting %>% mutate(id = as.character(id)) %>% select(id, spread), by = c("game_id" = "id")) %>% 
  mutate(spread = if_else(is.na(spread) == TRUE, 0, spread)) %>% 
  mutate(spread = spread * (clock_in_seconds/3600)^3) # Decrease spread as game goes on to reduce it's effect

### NEW
# Add kickoff indicator
plays.master.win_prob4 <- plays.master.win_prob4 %>% 
  mutate(is_kickoff = if_else(play_type %in% c("Kickoff", "Kickoff Return (Offense)"), 1, 0)) %>% 
  ungroup()

# Predict In-Game WP

plays_wp <- plays.master.win_prob4

x.test <- plays_wp %>% 
  select(home_score_lead_deficit, down, distance,
         yards_to_goal, home_poss_flag, home_timeouts_new, away_timeouts_new, 
         home_elo_wp, game_over, spread,
         pct_done, is_kickoff) %>% 
  as.matrix()

dtest <- xgb.DMatrix(x.test,missing=NA)

plays_wp$home_wp <- predict(XGBm, newdata = dtest)

# Loop through each game --------------------------------------------------

if(length(game_ids) > 0) {
  
  last_run <- data.table::fread("Data/tweet_id.csv")
  
  if (last_run$date_ran < lubridate::today()-5){
    
    status_text <- paste0(current_year, " Week ", current_week, "\n\n",
                          "Thread for today's Win Prob/PPA/QB Charts")
    
    post_tweet(status = status_text,
               token = tok)
    
    Sys.sleep(10)
    
    first_id <- get_timeline(user = "staturdays", n = 1L, token = tok)$status_id
    
    last_run$date_ran <- lubridate::today()
    last_run$reply_to_id <- as.character(first_id)
    last_run$n_tweets_sent <- 0L
    
    data.table::fwrite(last_run, "Data/tweet_id.csv", append = FALSE)
  }

for (i in 1:length(game_ids)) {
  
  message("Building Plot for Game ", i, "/", length(game_ids))
  
  last_run <- data.table::fread("Data/tweet_id.csv")
  thread_id <- last_run$reply_to_id
  
max_ids <- cum_sum_qb_plot_data %>% 
  filter(game_id == game_ids[i]) %>% #game_ids[i]
  group_by(player) %>% 
  slice_max(id.x, n = 1L) %>% 
  pull(id.x)

plot <- cum_sum_qb_plot_data %>% 
  filter(game_id == game_ids[i]) %>% 
  ggplot(aes(x = clock_in_seconds, y = cum_ppa, color = color, group = player)) +
  geom_line(size = 2) +
  ggrepel::geom_text_repel(aes(label = if_else(id.x %in% max_ids, player, ""),
                               color = color),
                           size = 8) +
  annotate(geom = "rect",
           xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = 0,
            fill = "red",
            alpha = 0.2) +
  scale_x_reverse() +
  scale_color_identity() +
  staturdays_theme +
  geom_vline(xintercept = c(2700, 1800, 900), linetype = c(2, 1, 2)) +
  annotate(geom = "label", x = c(3150, 2250, 1350, 450), 
           y = c(0, 0, 0, 0), 
           label = c("Q1", "Q2", "Q3", "Q4"),
           alpha = 0.5,
           fill = staturdays_colors("light_blue"),
           color = "white",
           size = 10) +
  labs(y = "Cumulative PPA",
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data",
       title = "QB Cumulative Predicted Points Added (PPA)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 20),
        axis.title.y = element_blank())

file <- "R Plots/tmp_file.jpg"

ggsave(filename = file,
              plot = plot,
              width = 200,
              height = 200,
              units = "mm",
              dpi = 300)


# In-Game WP Chart --------------------------------------------------------
this_game_data <- plays_wp %>% 
  left_join(team_colors, by = c("home" = "school")) %>% 
  left_join(team_colors, by = c("away" = "school"),
            suffix = c("_home", "_away")) %>% 
  filter(game_id == game_ids[i])

this_post_game_data <- games %>%
  filter(id == game_ids[i]) %>%
  mutate(
    winner = if_else(home_points > away_points, home_team, away_team),
    winner_post_game_wp = if_else(
      home_points > away_points,
      as.numeric(home_post_win_prob),
      as.numeric(away_post_win_prob)
    )
  )

# Get avg. in-game wp
avg_wp <- this_game_data %>% 
  group_by(home, away, home_outcome) %>% 
  summarise(avg_wp = mean(home_wp)) %>% 
  mutate(winner = paste0(if_else(home_outcome == 1, home, away)),
         winner_wp = if_else(home_outcome == 1, avg_wp, 1-avg_wp))

quarters <- this_game_data %>% filter(game_id == game_ids[i]) %>% 
  filter(period == 3 & lead(period) == 4 |
           period == 2 & lead(period) == 3 |
           period == 1 & lead(period) == 2) %>% pull(play_num) + .5

mid_quarters <- this_game_data %>% filter(game_id == game_ids[i]) %>% 
  group_by(period) %>% 
  summarise(min = min(play_num),
            max = max(play_num)) %>% 
  mutate(med = (min+max)/2) %>% 
  filter(period != 20) %>% 
  pull(med)

library(ggtext)

# Plot In-Game WP
plot2 <- this_game_data %>% 
  mutate(clock_in_seconds = if_else(clock_in_seconds == -10000, -1.5, clock_in_seconds)) %>% 
  ggplot(aes(x = play_num, y = home_wp)) +
  geom_line(aes(color = home_wp),
            size = 2) +
  scale_color_gradient(low = (unique(this_game_data$color_away)), 
                           high = (unique(this_game_data$color_home)),
                       guide = "none") +
  ggimage::geom_image(aes(image = light_home),
                      size = .1,
                      by = "width",
                      asp = 2,
                      x = 30, y = .9) +
  ggimage::geom_image(aes(image = light_away),
                      size = .1,
                      by = "width",
                      asp = 2,
                      x = 30, y = .1) +
  staturdays_theme +
  {if(length(quarters) == 3){geom_vline(xintercept = c(quarters), linetype = c(2,1,2))}} +
  {if(length(quarters) == 3){annotate(geom = "label", x = c(mid_quarters), y = 0,
           label = paste0("Q", 1:length(mid_quarters)),
           alpha = .5,
           fill = staturdays_colors("light_blue"),
           color = "white") }} +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  labs(title = paste0("<span style='color: ",
                      unique(this_game_data$color_away),
                      ";'>",
                      unique(this_game_data$away),
                      "</span> vs. <span style='color: ",
                      unique(this_game_data$color_home),
                      ";'>",
                      unique(this_game_data$home),
                      "</span> Win Probability"),
       subtitle = paste0(avg_wp$winner,
                         " Average Win Probability of <b>",
                         scales::percent(round(avg_wp$winner_wp, 2)),
                         "</b>"),
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data") +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        aspect.ratio = 1/2
        ) +
  coord_cartesian(clip = "off")

file2 <- "R Plots/tmp_file2.jpg"

ggsave(filename = file2,
       plot = plot2,
       width = 400,
       height = 200,
       units = "mm",
       dpi = 300)

# Tweet plot --------------------------------------------------------------

s_rates <- plays %>% 
  filter(game_id == game_ids[i]) %>% 
  filter(play_type %in% scrimmage_plays_all) %>% 
  group_by(offense) %>% 
  summarise(success_rate = mean(success)) %>% 
  mutate(success_rate = scales::percent(success_rate))

ppas <- cum_sum_qb_plot_data %>% 
  filter(game_id == game_ids[i]) %>% 
  group_by(player) %>% 
  mutate(n = n()) %>% 
  group_by(player, offense) %>% 
  slice_max(order_by = id.x, n = 1L) %>% 
  select(player, cum_ppa, n) %>% 
  group_by(offense) %>% 
  slice_max(order_by = n, n = 1L) %>% 
  mutate(cum_ppa = round(cum_ppa, digits = 2)) %>% 
  ungroup() %>% 
  select(player, cum_ppa)

home_score <- games %>% 
  filter(id == game_ids[i]) %>% 
  pull(home_points)

away_score <- games %>% 
  filter(id == game_ids[i]) %>% 
  pull(away_points)

text <- plays %>% 
  left_join(team_colors, by = c("home" = "school")) %>% 
  left_join(team_colors, by = c("away" = "school"), suffix = c(".home", ".away")) %>% 
  ungroup() %>% 
  filter(game_id == game_ids[i]) %>% 
  slice_max(id.x, n = 1L) %>% 
  mutate(tweet_text = paste0(away,
                             " ",
                            away_score, 
                            "-", 
                            home_score,
                            " ",
                            home,
                            "\n\n",
                            "Success Rates:\n",
                            s_rates[1,1], ": ", s_rates[1,2], "\n",
                            s_rates[2,1], ": ", s_rates[2,2], "\n\n",
                            "Net Predicted Points Added:\n",
                            ppas[1,1], ": ", ppas[1,2], "\n",
                            ppas[2,1], ": ", ppas[2,2], "\n",
                            "#CFBData #",
                            abbreviation.away,
                            " vs. #",
                            abbreviation.home)) %>% 
  pull(tweet_text)


 post_tweet(status = text,
            media = file,
            token = tok,
            in_reply_to_status_id = thread_id)
 
 (Sys.sleep(10))
 
 # Get tweet id to reply to
 reply_to_status_id <- get_timeline(user = "staturdays", n = 1L, token = tok)$status_id
 
 text2 <- paste0("In-Game Win Probability Chart\n\n",
                 this_post_game_data$winner, " Post Game Win Probability: ",
                 scales::percent(round(this_post_game_data$winner_post_game_wp, 2)),
                 "\n\n",
                 avg_wp$winner, " Average Win Probability: ",
                 scales::percent(round(avg_wp$winner_wp, 2)),
                 "\n\n",
                 "#", unique(this_game_data$abbreviation_away),
                 " vs. ",
                 "#", unique(this_game_data$abbreviation_home),
                 " #CFBData")
 
 post_tweet(status = text2,
            media = file2,
            token = tok,
            in_reply_to_status_id = reply_to_status_id)
 
# Write games_done to csv for reference the next time the job runs in 30 mins

game_ids_df <- tibble(games_done = game_ids[i])

game_ids_df <- game_ids_df %>% mutate(games_done = paste0("id_", games_done))

data.table::fwrite(game_ids_df, file = "Data/games_done.csv", append = TRUE)

# message("tweet posted: ", text, "\n",
#         "plot:")
# 
# plot

last_run$n_tweets_sent <- last_run$n_tweets_sent + 1L

data.table::fwrite(last_run, "Data/tweet_id.csv", append = FALSE)

Sys.sleep(10)

}

}
  
