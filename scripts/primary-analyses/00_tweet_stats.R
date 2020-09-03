######## Tweet Stats ########
#
# This script explores statistics of the tweet corpus
# for the Arab Spring project.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_08_31

#### 1. Set up ####

setwd("./Documents/_github/arab-spring/")

# clear environment
rm(list=ls())

# read in the data 
raw_tweets <- read.delim("./data/raw/syria_tweets_en.txt", 
                         header = FALSE, 
                         sep = "\t", 
                         stringsAsFactors = FALSE,
                         encoding='UTF-8')

#### 2. Parse data ####
formatted_tweets <- raw_tweets %>%

  # parse date
  mutate(tweet = sub("^.*?,", "", raw_tweets$V1))
  
# fix encoding
Encoding(formatted_tweets$tweet) <- "latin1"

# filter for dates and times
formatted_tweets <- formatted_tweets %>%

  # remove whitespace from beginning of tweet
  mutate(tweet = str_trim(tweet)) %>%
  
  # parse day level
  mutate(date = (stri_extract_first_regex(V1, "[0-9]{8}+"))) %>%
  
  # convert to date format
  mutate(date = anytime::anydate(date, "%Y%m%d")) %>%
  
  # truncate to date range used
  filter(date >= as.Date("2012-03-30") 
         & date <= as.Date("2012-06-15")) %>%
  
  # number of days since new year
  mutate(num_days = yday(date)) %>%
  
  # parse time
  mutate(time = str_sub(V1,9,14)) %>%
  
  # subset hour
  mutate(hour = as.integer(str_sub(time,1,2))) %>%
  
  # subset minutes
  mutate(minute = as.integer(str_sub(time,3,4))) %>%
  
  # subset seconds
  mutate(seconds = as.integer(str_sub(time,5,6))) %>%
  
  # calculate total seconds for later analyses
  mutate(total_seconds = (num_days*(60^2)*24) + (hour*(60^2)) + (minute*60) + seconds) %>%

  # sort by time of tweet
  arrange(total_seconds)

# calculate descriptive stats
stats <- formatted_tweets %>%
  
  # calculate time since the previous tweet
  mutate(time_to_tweet = total_seconds - dplyr::lag(total_seconds, 1))

# plot the distribution of time between tweets
hist(stats$time_to_tweet)

  