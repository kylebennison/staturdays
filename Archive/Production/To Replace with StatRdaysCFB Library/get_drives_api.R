# Function to get drives from cfbd api
get_drives <- function(start_year, end_year, start_week, end_week){
  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Play%20Types%20and%20Power%20Conference%20Names.R")
  
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

if(missing(start_week) | missing(end_week)){
  
  drives.master = dplyr::tibble()
  for (j in start_year:end_year) {
      cat('Loading drives for year', j, '\n')
      full_url_drives <- paste0(base_url_drives, "year=", as.character(j), "&seasonType=both")
      drives <- cfbd_api(full_url_drives, my_key)
      drives <- dplyr::as_tibble(drives)
      drives.master = dplyr::bind_rows(drives.master, drives)
    }
  } else {
  
  drives.master = tibble()
  for (j in start_year:end_year) {
    for (i in start_week:end_week) {
      cat('Loading drives', j, 'Week', i, '\n')
      full_url_drives <- paste0(base_url_drives, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
      drives <- cfbd_api(full_url_drives, my_key)
      drives <- dplyr::as_tibble(drives)
      drives.master = dplyr::bind_rows(drives.master, drives)
    }
  }
  
}

return(drives.master)

}
