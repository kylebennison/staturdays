import tweepy
import pandas as pd
import os
import time

# Read in OAuth token from environment variable
token = os.environ.get('tweepy_auth')

# Authentication
auth = tweepy.OAuth2BearerHandler(token)
api = tweepy.API(auth)

n_hours = 3.5
n_increments = n_hours * 4
increments = 0
dt = []

while increments < n_increments:

	# Store and scrape tweets
	tweets = tweepy.Cursor(api.search_tweets, q="#CollegeGameday -filter:retweets Pat -Kirk -Lee -Rece", lang="en", tweet_mode='extended').items(100)
	for tweet in tweets:
		dt2 = pd.DataFrame(data={"Tweet": [tweet._json['full_text']],
								"Host": ["Pat"]})
		dt.append(dt2)

	tweets = tweepy.Cursor(api.search_tweets, q="#CollegeGameday -filter:retweets -Pat Kirk -Lee -Rece", lang="en", tweet_mode='extended').items(100)
	for tweet in tweets:
		dt2 = pd.DataFrame(data={"Tweet": [tweet._json['full_text']],
								"Host": ["Kirk"]})
		dt.append(dt2)

	tweets = tweepy.Cursor(api.search_tweets, q="#CollegeGameday -filter:retweets -Pat -Kirk Lee -Rece", lang="en", tweet_mode='extended').items(100)
	for tweet in tweets:
		dt2 = pd.DataFrame(data={"Tweet": [tweet._json['full_text']],
								"Host": ["Lee"]})
		dt.append(dt2)

	tweets = tweepy.Cursor(api.search_tweets, q="#CollegeGameday -filter:retweets -Pat -Kirk -Lee Rece", lang="en", tweet_mode='extended').items(100)
	for tweet in tweets:
		dt2 = pd.DataFrame(data={"Tweet": [tweet._json['full_text']],
								"Host": ["Rece"]})
		dt.append(dt2)

	print("Completed increment {}".format(increments+1))
	increments += 1
	time.sleep(15*60)

dt = pd.concat(dt)
dt.to_csv("C:/Users/drewb/OneDrive/Documents/Staturdays/Archive/Articles/College Gameday Sentiment/gameday_tweets.csv",
index = False)