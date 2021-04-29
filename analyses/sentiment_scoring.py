import pandas as pd
import datetime as dt
import os
import numpy as np
import nltk
from nltk.corpus import stopwords
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import matplotlib.pyplot as plt
import seaborn as sns

# set working directory
os.chdir("/Users/joemarlo/Dropbox/Data/Projects/wsb-discourse")

# read in the scraped posts
comments_df = pd.read_csv("data/comments_cleaned.csv")

# only retain id and comment text
comments_df = comments_df[['id_comment', 'comment_text']]

# remove NaNs
comments_df = comments_df.dropna().reset_index(drop=True)

# instantiate the SentimentIntensityAnalyzer
vader = SentimentIntensityAnalyzer()

# add words from McDonald financial corpus
positive_url = "https://raw.githubusercontent.com/jperla/sentiment-data/master/finance/LoughranMcDonald_Negative.csv"
negative_url = "https://raw.githubusercontent.com/jperla/sentiment-data/master/finance/LoughranMcDonald_Positive.csv"
neutral_url = "https://raw.githubusercontent.com/jperla/sentiment-data/master/finance/LoughranMcDonald_Uncertainty.csv"
positive_df = pd.read_csv(positive_url, header=None)
negative_df = pd.read_csv(negative_url, header=None)
neutral_df = pd.read_csv(neutral_url, header=None)

# add sentiment scores
positive_df.loc[:,1] = 10
negative_df.loc[:,1] = -10
neutral_df.loc[:,1] = 0

# convert to dictionary
positive_dict = positive_df.set_index(0).to_dict()
negative_dict = negative_df.set_index(0).to_dict()
neutral_dict = neutral_df.set_index(0).to_dict()

# add the words
vader.lexicon.update(positive_dict)
vader.lexicon.update(negative_dict)
vader.lexicon.update(neutral_dict)

# add custom words words
WSB_lingo = pd.read_csv('data/wsb_language.csv')
WSB_lingo = WSB_lingo.set_index('phrase').to_dict()['sentiment']

# add custom words
vader.lexicon.update(WSB_lingo)

# run the analyzer on the original post body
scores = [vader.polarity_scores(text) for text in comments_df.comment_text]

# pull out the scores
scores_compound = []
scores_negative = []
scores_neutral = []
scores_positive = []
for score in range(0, len(scores)):
    scores_compound.append(scores[score]["compound"])
    scores_negative.append(scores[score]["neg"])
    scores_neutral.append(scores[score]["neu"])
    scores_positive.append(scores[score]["pos"])


# create dataframe of ids and scores
comments_df = comments_df.drop(columns=['comment_text'])
comments_df[["sentiment_compound"]] = scores_compound
comments_df[["sentiment_negative"]] = scores_negative
comments_df[["sentiment_neutral"]] = scores_neutral
comments_df[["sentiment_positive"]] = scores_positive

# write out dataframe
comments_df.to_csv("data/sentiment.csv", index=False)
