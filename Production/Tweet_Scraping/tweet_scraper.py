import tweepy
import pandas as pd
import os

# Read in OAuth token from environment variable
token = os.environ.get('tweepy_auth')

# Authentication
auth = tweepy.OAuth2BearerHandler(token)
api = tweepy.API(auth)


# Store and scrape tweets
dt = []
tweets = tweepy.Cursor(api.search_tweets, q="#CollegeGameday -filter:retweets Pat -Kirk -Lee -Chris", lang="en", tweet_mode='extended').items(100)
for tweet in tweets:
	dt2 = pd.DataFrame(data={"Tweet": [tweet._json['full_text']]})
	dt.append(dt2)

dt = pd.concat(dt)
dt.to_csv("C:/Users/drewb/OneDrive/Documents/Staturdays/Archive/Articles/College Gameday Sentiment/gameday_tweets.csv",
index = False)