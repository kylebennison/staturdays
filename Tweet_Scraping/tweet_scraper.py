import tweepy
import csv
import pandas as pd
import time

# Twitter API credentials
CONSUMER_KEY ="igpDkcsomnDOIcEsfyHPVNYyI"
CONSUMER_SECRET = "PV25oiBkpZ8ytfewRuuya4PvWZZiR89zgPpNN0wlXxmHmrB8E9"   
ACCESS_KEY = "404429205-oIB7wCREbOzVm4bGylCYaLK64c8RegNwjCfRkioz"    
ACCESS_SECRET = "SK67e9sNL8v1Kuh8OesRC8hyNUFkNojs9VfnaAisUx421"

# Create the api endpoint

auth = tweepy.OAuthHandler(CONSUMER_KEY, CONSUMER_SECRET)
api = tweepy.API(auth)

# Mention the maximum number of tweets that you want to be extracted.

num_tweets = int(input('Tweets per 5 minutes: '))

# Mention the hashtag that you want to look out for

hashtag = input('hastag: ')

df = pd.DataFrame({"tweets":[], "created_at":[]})

three_hours = 0

while three_hours < 45:
	x = tweepy.Cursor(api.search, q='#' + hashtag,rpp=100,since = "2019-04-25",).items(num_tweets)
	print("Gathered tweets")
	for tweet in x:
			df2 = pd.DataFrame({"tweets":[str(tweet.text.encode('utf-8'))], "created_at":[tweet.created_at]})
			df = df.append(df2, ignore_index=True)
	print("Appended")
	print("About to sleep")
	time.sleep(300)
	print("Woke up")
	three_hours +=1

print("Done")
df.to_csv("tweets.csv")