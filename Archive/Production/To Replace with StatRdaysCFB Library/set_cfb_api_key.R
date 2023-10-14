library(docstring)

set_cfb_api_key <- function(key){
  #' Set your personal collegefootballdata.com key to your environment.
  #' @description Takes your input and assigns it to the "cfbd_staturdays_key" key
  #' in your .Renviron profile.
  #' @param key character. Your personal API key. If you don't have a key, you can register for one for
  #' free at https://collegefootballdata.com/key
  #' @details This will create a new key in your .Renviron file with the value you assign to it.
  #' You will then access that key using the variable my_key which will be created for you when calling 
  #' most "get" functions. You can also access your key at any time using
  #' Sys.getenv("cfbd_staturdays_key") or change it by using set_cfb_api_key() or directly using
  #' Sys.setenv("cfbd_staturdays_key" = "yourapikeyhere")
  
  Sys.setenv("cfbd_staturdays_key" = as.character(key))
  
}