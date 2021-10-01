#' Will need to start after 12 pm on Saturday and check every
#' 30 minutes or so for new game results
#' Need a way to know whether a game is complete or not, although
#' I'm pretty sure the plays data doesn't get published until after
#' the game.
#'

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/source_everything.R")

library(rtweet)

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
max_week <- max(elo$week)
current_week <- max_week + 1L
current_year <- max(elo$season)
plays <- get_plays(current_week, current_week, current_year, current_year)
plays <- plays %>% add_success()

# Cumulative QB PPA

games_done <- tibble(games_done = c(0, 1)) # Store games that have already been run

games_done <- data.table::fread("Data/games_done.csv") # Read in games that have already been tweeted

# Remove "id_" from start of string
games_done <- stringr::str_sub(games_done, start = 4L)

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

cum_sum_qb_plot_data <- cum_sum_qb %>% 
  left_join(team_colors, by = c("offense" = "school")) %>% 
  group_by(player) %>% 
  mutate(n = n()) %>% 
  filter(n > 4, !str_detect(player, "^TEAM\\s")) # QBs involved in at least 5 plays, not "Team" plays

game_ids <- game_ids[which(!game_ids %in% games_done)] # Filter out any games that have already been run

for (i in 1:length(game_ids)) {
  
  message("Building Plot for Game ", i, "/", length(game_ids))
  
max_ids <- cum_sum_qb_plot_data %>% 
  filter(game_id == game_ids[i]) %>% #game_ids[i]
  group_by(player) %>% 
  slice_max(id.x, n = 1L) %>% 
  pull(id.x)

plot <- cum_sum_qb_plot_data %>% 
  filter(game_id == game_ids[i]) %>% 
  ggplot(aes(x = clock_in_seconds, y = cum_ppa, color = color, group = player)) +
  geom_line() +
  ggrepel::geom_text_repel(aes(label = if_else(id.x %in% max_ids, player, ""),
                               color = color)) +
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
           color = "white") +
  labs(y = "Cumulative PPA",
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data",
       title = "QB Cumulative Predicted Points Added (PPA)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 20))

file <- "R Plots/tmp_file.jpg"

ggsave(filename = file,
              plot = plot,
              width = 200,
              height = 200,
              units = "mm",
              dpi = 300)


# In-Game WP Chart --------------------------------------------------------

# Bring in and apply Drew's model to plays data

# Tweet plot --------------------------------------------------------------

text <- plays %>% 
  left_join(team_colors, by = c("home" = "school")) %>% 
  left_join(team_colors, by = c("away" = "school"), suffix = c(".home", ".away")) %>% 
  ungroup() %>% 
  filter(game_id == game_ids[i]) %>% 
  slice_max(id.x, n = 1L) %>% 
  mutate(tweet_text = paste0(away,
                             " ",
                            if_else(away == offense, 
                                          offense_score,
                                          defense_score), 
                            "-", 
                            if_else(home == offense, 
                                    offense_score,
                                    defense_score),
                            " ",
                            home,
                            "\n\n",
                            "#CFBData #",
                            abbreviation.away,
                            " vs. #",
                            abbreviation.home)) %>% 
  pull(tweet_text)


 post_tweet(status = text,
            media = file,
            token = tok)

game_ids_df <- tibble(games_done = game_ids[i])

games_done <- rbind(games_done, game_ids_df)

# message("tweet posted: ", text, "\n",
#         "plot:")
# 
# plot

Sys.sleep(10)

}

# Write games_done to csv for reference the next time the job runs in 30 mins
games_done <- games_done %>% mutate(games_done = paste0("id_", games_done))

data.table::fwrite(games_done, file = "Data/games_done.csv", append = TRUE)
