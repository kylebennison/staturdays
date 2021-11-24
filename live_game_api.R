source("Production/source_everything.r")

games.master <- get_games(start_week = 1, end_week = 1, start_year = 2021, end_year = 2021)

plays.master <- get_plays(start_week = 1, end_week = 1, start_year = 2021, end_year = 2021)

live_games <- get_anything("https://api.collegefootballdata.com/scoreboard",
                           start_year = 2021,
                           end_year = 2021,
                           key = my_key)

live_games <- live_games %>% 
  mutate(id = as.character(id)) %>% 
  filter(status == "in_progress")

live_ids <- live_games %>% pull(id)

live_ids <- c("401281946")



  
  game_id <- live_ids[id]
  url <- "http://cdn.espn.com/core/college-football/playbyplay?gameId=401281946&xhr=1&render=false&userab=18"
  
  response <- httr::GET(url = url), content_type_json()),
                        content_type_json())

response %>% fromJSON()
  
  data <- content(response, as="text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(flatten = TRUE) %>% 
    tibble() %>% 
    readr::type_convert()

response["gamepackageJSON"]  

