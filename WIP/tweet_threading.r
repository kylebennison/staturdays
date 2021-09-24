rm(list = ls())
library(tidyverse)
library(htmltools)
library(rvest)
library(lubridate)
library(jsonlite)
library(rtweet)

# Twitter Auth
app_name <- "Staturdays CFB Tweets"
consumer_key <- Sys.getenv("twtr_staturdays_api_key")
consumer_secret <- Sys.getenv("twtr_staturdays_api_secret")
access_token <- Sys.getenv("twtr_staturdays_access_tok")
access_secret <- Sys.getenv("twtr_staturdays_access_secret")

tok <- create_token(app=app_name,
                    consumer_key = consumer_key,
                    consumer_secret = consumer_secret,
                    access_token = access_token,
                    access_secret = access_secret)

post_tweet(status = "")


# -------------------------------------------------------------------------



old_available <- tibble("currentTime" = character(), "city" = character(), "state" = character(), "status" = character(), "isBookingCompleted" = logical())
master <- tibble()

# Start While Loop (infinite)

h <- 1
while(today() < today()+1){
  
  message("Running for time #", h, " at ", now())
  h <- h+1
  
  
  master <- tibble()
  
  dt <- fromJSON(paste0("https://www.cvs.com/immunizations/covid-19-vaccine/immunizations/covid-19-vaccine.vaccine-status.NJ.json?vaccineinfo?"))
  
  dt2 <- dt$responsePayloadData
  
  dt3 <- dt2 %>% as_tibble(dt2) %>% unnest(cols="data")
  
  master <- dt3
  
  # previous_available <- available
  
  new_available <- master %>% filter(status != "Fully Booked")
  
  change_available <- anti_join(new_available, old_available, by = c("city", "status"))
  
  
  # Tweet -------------------------------------------------------------------
  
  # search_results <- search_tweets("cvs", n=100, include_rts = FALSE,
  #                                 token = tok)
  
  # post_tweet(status = "Testing from R @kylebeni012",
  #            token = tok)
  
  # Tweet if there are new appointments and a change from the last check
  if(nrow(change_available)>0 & nrow(new_available)>0){
    
    current_time <- format(now(), "%b %d %Y %I:%M %p")
    
    # Start Tweet Thread
    city_list <- change_available %>% select(city) %>% as.vector()
    city_string <- city_list %>% flatten_chr() %>% str_c(sep = ",") %>% str_to_sentence()
    
    # Generate tweet string
    tweet_thread <- paste0(
      "New CVS COVID-19 Vaccine Appointments in NJ as of ", 
      current_time,
      " EST: (thread)\n", "#NJCVSVax\nBook at https://www.cvs.com/immunizations/covid-19-vaccine?icid=cvs-home-hero1-link2-coronavirus-vaccine\n",
      paste(city_string, collapse = ", "))
    
    thread_char <- nchar(tweet_thread)
    n_tweets <- ceiling(thread_char/280)
    avail_char <- 280-16
    starting_point <- 1L
    last_comma <- 279L
    
    i <- 1
    remaining_thread <- character()
    # Post Thread
    for (i in 1:n_tweets){
      
      message(current_time, ": posting tweet ", i, "/", n_tweets)
      
      tweet <- tweet_thread
      
      # If there's more than one tweet in the thread, we need to subset the string using str_sub
      if(n_tweets > 1){
        
        tweet <- case_when(i > 1 ~ paste0("@cvs_vax_finder ", # If it's on tweet #2 or more, thread it
                                          str_sub(remaining_thread, 
                                                  start = 1L, 
                                                  end = if_else(nchar(remaining_thread) < avail_char, 10000L, # If the characters in the remaining thread fit into the space available, take the entire remaining thread. Else, take the last comma within available characters
                                                                str_locate(str_sub(remaining_thread, start = 1L, end = avail_char), pattern = "(,)[^,]*$")[1]))),
                           # If it's on tweet  #1, just tweet the first tweet
                           TRUE ~ str_sub(tweet_thread, 
                                          start = 1L, 
                                          end = if_else(is.na(str_locate(str_sub(tweet_thread, start = starting_point, end = starting_point + last_comma), pattern = "(,)[^,]*$")[1]) == T, 10000L, # If it can't find the last comma (end of tweet thread), set value to 10000 to avoid getting an NA
                                                        str_locate(str_sub(tweet_thread, start = starting_point, end = starting_point + last_comma), pattern = "(,)[^,]*$")[1])))
      }
      
      new_start <- nchar(tweet) + 1L
      
      # The remainder of the thread that's yet to be tweeted
      remaining_thread <- str_sub(tweet_thread, start = new_start, end = 10000L)
      
      # Tweet initial post to reply to with thread
      post_tweet(status = tweet, token = tok)
      
      Sys.sleep(10)
      
      id <- rtweet::search_tweets("from:cvs_vax_finder", n = 1, token = tok) %>% pull(status_id)
      
    }
  }
  
  # Save the current available appointments as the old ones now
  old_available <- new_available
  
  # Wait 5 minutes before checking again for new appointments
  message("Sleeping for 5 minutes")
  Sys.sleep(60*5)
  
}
