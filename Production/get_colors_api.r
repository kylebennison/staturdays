source("Production/get_anything.r")

get_colors <- function(){

team_colors <- get_anything(url = "https://api.collegefootballdata.com/teams",
             key = my_key)

team_colors <- team_colors %>% unnest(cols = logos) %>% 
  mutate(logo_color = if_else(str_detect(logos, "dark"), "dark", "light")) %>% 
  pivot_wider(names_from = logo_color, values_from = logos)

return(team_colors)

}
