import pandas as pd
import nltk

#https://towardsdatascience.com/step-by-step-twitter-sentiment-analysis-in-python-d6f650ade58d

dt = pd.repipad_csv("C:/Users/drewb/OneDrive/Documents/Staturdays/Archive/Articles/College Gameday Sentiment/gameday_tweets.csv")
dt = dt.drop_duplicates()
print(dt.head())