source("Production/source_everything.r")

player_ppa <- get_anything(url = "https://api.collegefootballdata.com/ppa/players/season",
                           start_year = 2021,
                           end_year = 2021,
                           key = my_key)

colors <- get_colors()

orange_pal <- function(x) rgb(colorRamp(c("#e6bba5", "#de703b"))(x), maxColorValue = 255)

qb_df <- player_ppa %>% 
  left_join(colors, by = c("team" = "school")) %>% 
  filter(position == "QB",
         countablePlays >= 20) %>% 
  select(name, light, averagePPA.pass, averagePPA.rush, averagePPA.thirdDown, averagePPA.all)

library(reactable)

reactable_tbl <- reactable::reactable(qb_df,
                     columns = list(
                       name = colDef(name = "Player"),
                       light = colDef(name = "Team",
                                      cell = function(value) {
                         image <- htmltools::img(src = value, height = "50px", alt = "")
                         htmltools::tagList(
                           htmltools::div(style = list(display = "inline-block", width = "25px"), 
                                          image)
                         )
                       }),
                       averagePPA.pass = colDef(name = "Passing",
                                                format = colFormat(digits = 3)),
                       averagePPA.rush = colDef(name = "Rushing",
                                                format = colFormat(digits = 3)),
                       averagePPA.thirdDown = colDef(name = "3rd Downs",
                                                     format = colFormat(digits = 3)),
                       averagePPA.all = colDef(name = "All Plays",
                                               format = colFormat(digits = 3),
                                               style = function(value) {
                                                 normalized <- (value - min(qb_df$averagePPA.all)) / (max(qb_df$averagePPA.all) - min(qb_df$averagePPA.all))
                                                 color <- orange_pal(normalized)
                                                 list(background = color)
                                               })
                     ),
                     columnGroups = list(
                       colGroup(name = "Average PPA",
                                columns = c("averagePPA.pass",
                                            "averagePPA.rush",
                                            "averagePPA.thirdDown",
                                            "averagePPA.all"))
                     ),
                     theme = reactableTheme(
                       style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")),
                     defaultSortOrder = "desc",
                     defaultSorted = c("averagePPA.all"),
                     defaultColDef = colDef(format = colFormat(digits = 3)),
                     searchable = TRUE,
                     minRows = 30,
                     defaultPageSize = 30,
                     pagination = FALSE,
                     striped = TRUE
                     )

t2 <- htmlwidgets::prependContent(reactable_tbl,
                            htmltools::h1(class = "title",
                               "Top QBs in PPA Through Week 1",
                               style = "text-align:center; font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif;"))

t3 <- htmlwidgets::appendContent(t2,
                                 htmltools::p(class = "footer",
                                              "@kylebeni012 for @staturdays | Data: @cfb_data",
                                              style = "color: #de703b; text-align:left; font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif;"))

html <- "R Plots/t3.html"
htmlwidgets::saveWidget(t3, html)
webshot2::webshot(html, file = paste0("R Plots/qb_ppa_",
                                      lubridate::today(),
                                      ".png"),
                 delay = 1)
