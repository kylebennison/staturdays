source("Production/source_everything.r")

live_games <- get_anything("https://api.collegefootballdata.com/scoreboard",
                           start_year = 2021,
                           end_year = 2021,
                           key = my_key)

live_games <- live_games %>% 
  mutate(id = as.character(id)) %>% 
  filter(status == "in_progress")

live_ids <- live_games %>% pull(id)


for(id in 1:length(live_ids)){
  
game_id <- live_ids[id]
url <- paste0("http://cdn.espn.com/core/college-football/playbyplay?gameId=,", 
              game_id,
              "&xhr=1&render=false&userab=18")
response <- httr::GET(url = url,
          content_type_json())

data <- content(response, as="text", encoding = "UTF-8") %>% 
  jsonlite::fromJSON(flatten = TRUE) %>% 
  tibble() %>% 
  readr::type_convert()

}