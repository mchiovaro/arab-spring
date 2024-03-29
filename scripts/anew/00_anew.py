"""
Author: Qi Xia
Code adapted from Doris Zhou, https://github.com/dwzhou/SentimentAnalysis.git
Date: September 30, 2021
Performs sentiment analysis on a text file using ANEW.
Parameters:
    --dir [path of directory]
        specifies directory of files to analyze
    --file [path of csv file]
        specifies location of specific file to analyze
    --out [path of directory]
        specifies directory to create output files
    --mode [mode]
        takes either "median" or "mean"; determines which is used to calculate sentence sentiment values
"""
# add parameter to exclude duplicates? also mean or median analysis
import pandas as pd
import csv
import sys
import os
import statistics
import time
import argparse
from stanfordcorenlp import StanfordCoreNLP
#nlp = StanfordCoreNLP('/Users/qixia/Git/arab-spring/stanford-corenlp-4.2.2')
from nltk.stem.wordnet import WordNetLemmatizer
lmtzr = WordNetLemmatizer()
from nltk import tokenize
from nltk.corpus import stopwords
import nltk

#prepare data
stops = set(stopwords.words("english"))
anew = "/Users/qixia/Git/arab-spring/data/EnglishShortened.csv"
input_file = "/Users/qixia/Git/arab-spring/data/raw/syria_merge_sort.csv"
output_dir = "/Users/qixia/Git/arab-spring/data/formatted"
mode = "sum"

# read file into list of string
read_file = pd.read_csv (input_file)
tweets = list(read_file['text'])

# end method if file is empty
if len(tweets) < 1:
    print('Empty file.')



# otherwise, split into sentences
sentences = tweets
i = 1 # to store sentence index
# check each word in sentence for sentiment and write to output_file
with open(output_file, 'w', newline='') as csvfile:
    fieldnames = ['Sentence ID', 'Sentence', 'Sentiment', 'Sentiment Label', 'Arousal', 'Dominance',
                      '# Words Found', 'Found Words', 'All Words']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()

    # analyze each sentence for sentiment
    for s in sentences:
        # print("S" + str(i) +": " + s)
        all_words = []
        found_words = []
        total_words = 0
        v_list = []  # holds valence scores
        a_list = []  # holds arousal scores
        d_list = []  # holds dominance scores

        # search for each valid word's sentiment in ANEW
        words = nltk.pos_tag(s.lower().split(" "))
        for index, p in enumerate(words):
            # don't process stops or words w/ punctuation
            w = p[0]
            print(w)
            pos = p[1]
            if w in stops or not w.isalpha():
                continue

            # check for negation in 3 words before current word
            j = index-1
            neg = False
            while j >= 0 and j >= index-3:
                if words[j][0] == 'not' or words[j][0] == 'no' or words[j][0] == 'n\'t':
                    neg = True
                    break
                j -= 1

            # lemmatize word based on pos
            if pos[0] == 'N' or pos[0] == 'V':
                lemma = lmtzr.lemmatize(w, pos=pos[0].lower())
            else:
                lemma = w

            all_words.append(lemma)

            # search for lemmatized word in ANEW
            with open(anew) as csvfile:
                reader = csv.DictReader(csvfile)
                for row in reader:
                    if row['Word'].casefold() == lemma.casefold():
                        if neg:
                            found_words.append("neg-"+lemma)
                        else:
                            found_words.append(lemma)
                        v = float(row['valence'])
                        a = float(row['arousal'])
                        d = float(row['dominance'])

                        if neg:
                            # reverse polarity for this word
                            v = 5 - (v - 5)
                            a = 5 - (a - 5)
                            d = 5 - (d - 5)

                        v_list.append(v)
                        a_list.append(a)
                        d_list.append(d)

        if len(found_words) == 0:  # no words found in ANEW for this sentence
            writer.writerow({'Sentence ID': i,
                             'Sentence': s,
                             'Sentiment': 'N/A',
                             'Sentiment Label': 'N/A',
                             'Arousal': 'N/A',
                             'Dominance': 'N/A',
                             '# Words Found': 0,
                             'Found Words': 'N/A',
                             'All Words': all_words
                             })
            i += 1
        else:  # output sentiment info for this sentence

            # get values
            if mode == 'median':
                sentiment = statistics.median(v_list)
                arousal = statistics.median(a_list)
                dominance = statistics.median(d_list)
            elif mode == 'mean':
                sentiment = statistics.mean(v_list)
                arousal = statistics.mean(a_list)
                dominance = statistics.mean(d_list)
            elif mode == 'sum':
                sentiment = sum(v_list)
                arousal = sum(a_list)
                dominance = sum(d_list)

            # set sentiment label
            label = 'neutral'
            if sentiment > 6:
                label = 'positive'
            elif sentiment < 4:
                label = 'negative'

            writer.writerow({'Sentence ID': i,
                             'Sentence': s,
                              'Sentiment': sentiment,
                              'Sentiment Label': label,
                              'Arousal': arousal,
                              'Dominance': dominance,
                              '# Words Found': ("%d out of %d" % (len(found_words), len(all_words))),
                              'Found Words': found_words,
                              'All Words': all_words
                               })
            i += 1