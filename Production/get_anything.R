source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")

get_anything <- function(url, start_year=2021, end_year=2021, start_week, end_week, key){
  
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
