library(docstring)

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")

get_anything <- function(url, start_year=2021, end_year=2021, start_week, end_week, key){
  
  #' Get data from any API endpoint on cfbdata.com
  #' 
  #' @param url string. The url of the API endpoint WITHOUT any parameters added onto the end.
  #' If you have any parameters to add, please use the provided fields and we will add them
  #' to the url for you.
  
  if(missing(key)){
    
    message("You must supply an API key. You can get one for free at collegefootballdata.com")
    
  } else if(stringr::str_detect(url, "\\?") == TRUE){
    
    message("Please remove any parameters and the '?' from your url field")
    
  } else if(missing(start_week) & missing(end_week)){
  
  response <- tibble()
  for(yr in start_year:end_year){
    
    response_url <- paste0(url, "?year=", as.character(yr))
    r1 <- cfbd_api(response_url, key = key)
    response <- rbind(response, r1)
    message("Done year ", yr)
    
  }
  } else {
    
    response <- tibble()
    for(yr in start_year:end_year){
      for(wk in start_week:end_week){
        
        response_url <- paste0(url, 
                               "?year=", as.character(yr), 
                               "&week=", as.character(wk))
        r1 <- cfbd_api(response_url, key = key)
        response <- rbind(response, r1)
        message("Done year ", yr, " week ", wk)
        
      }
      
    }
    
  }
  
return(response)
  
}
