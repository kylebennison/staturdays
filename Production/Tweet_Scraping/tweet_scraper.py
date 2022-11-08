import tweepy
import csv
import pandas as pd
import time
import os

# Read in OAuth token from environment variable
token = os.environ.get('tweepy_auth')

# Authentication
auth = tweepy.OAuth2BearerHandler(token)
api = tweepy.API(auth)

tweets = tweepy.Cursor(api.search_tweets, q="College", lang="en", tweet_mode='extended').items(2)
for tweet in tweets:
    print(tweet._json['full_text'])