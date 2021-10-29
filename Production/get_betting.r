# Get bets
get_betting <-
  function(start_year,
           end_year,
           start_week = 1,
           end_week = 20) {
    
    source(
      "https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R"
    )
    
    if (missing(start_year) & missing(end_year)) {
      start_year <- lubridate::year(today())
      end_year <- start_year
      warning("Setting start year and end year to ", start_year,
              " since none was provided.")
    }
    
    if (missing(start_year)) {
      start_year <- end_year
      warning("Setting start year to match end year ", start_year,
              " since none was provided.")
    }
    
    if (missing(end_year)) {
      end_year <- start_year
      warning("Setting end year to match start year ", start_year,
              " since none was provided.")
    }
    
    if (start_year < 2013 & end_year < 2013) {
      start_year <- 2013
      end_year <- 2013
      warning("Setting start year and end year to 2013 (earliest year data is available)")
    }
    
    if (start_year < 2013 & end_year >= 2013) {
      start_year <- 2013
      warning("Setting start year to 2013 (earliest year data is available)")
    }
    
    if (end_year < start_year) {
      stop("Start year must be less than or equal to end year")
    }
    
    betting.master = data.frame()
    
    for (j in start_year:end_year) {
      for (i in start_week:end_week) {
        message("Getting betting data for ", j, " Week ", i)
        betting_url <-
          paste0("https://api.collegefootballdata.com/lines?year=",
                 j,
                 "&")
        full_url_betting <- paste0(betting_url, "week=", as.character(i))
        
        full_url_betting_encoded <- URLencode(full_url_betting)
        betting <- cfbd_api(full_url_betting_encoded, my_key)
        betting <- as_tibble(betting)
        if(nrow(betting) > 0){
        betting <- unnest(betting, cols = c(lines))
        }
        betting.master = rbind(betting.master, betting)
      }
      
    }
    
    return(betting.master)
    
  }
