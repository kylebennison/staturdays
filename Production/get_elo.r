#' @importFrom from magrittr "%>%"
#' @export

get_elo <- function(start_year = 2021, end_year = 2021, teams){
  
  elo <- data.table::fread("Production/elo_ratings_historic.csv", encoding = "UTF-8")
  
  elo <- elo %>%
    dplyr::filter(season >= start_year & season <= end_year)
  
  if(missing(teams) == FALSE){ 
    elo <- elo %>% dplyr::filter(team %in% teams)
  }

  return(elo)
  
}
