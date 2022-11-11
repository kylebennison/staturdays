import pandas as pd
import nltk
from textblob import TextBlob
import sys
import tweepy
import pandas as pd
import numpy as np
import os
import nltk
import re
import string

from wordcloud import WordCloud, STOPWORDS
from PIL import Image
from nltk.sentiment.vader import SentimentIntensityAnalyzer
from langdetect import detect
from nltk.stem import SnowballStemmer
from sklearn.feature_extraction.text import CountVectorizer

#https://towardsdatascience.com/step-by-step-twitter-sentiment-analysis-in-python-d6f650ade58d

dt = pd.read_csv("C:/Users/drewb/OneDrive/Documents/Staturdays/Archive/Articles/College Gameday Sentiment/gameday_tweets.csv")
dt = dt.drop_duplicates()

analyzer = SentimentIntensityAnalyzer()
dt['compound'] = [analyzer.polarity_scores(x)['compound'] for x in dt['Tweet']]
dt['neg'] = [analyzer.polarity_scores(x)['neg'] for x in dt['Tweet']]
dt['neu'] = [analyzer.polarity_scores(x)['neu'] for x in dt['Tweet']]
dt['pos'] = [analyzer.polarity_scores(x)['pos'] for x in dt['Tweet']]
print(dt.head())
print(dt['compound'].mean())

'''
for i in tweets:
 
 #print(tweet.text)
 tweet_list.append(tweet.text)
 analysis = TextBlob(tweet.text)
 score = SentimentIntensityAnalyzer().polarity_scores(tweet.text)
 neg = score[‘neg’]
 neu = score[‘neu’]
 pos = score[‘pos’]
 comp = score[‘compound’]
 polarity += analysis.sentiment.polarity
 '''