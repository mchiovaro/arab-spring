######## Tweet Stats ########
#
# This script explores statistics of the tweet corpus
# for the Arab Spring project.
#
# Code by: M. Chiovaro (@mchiovaro) and A. Paxton (@a-paxton)
# University of Connecticut
# Last updated: 2020_12_22

#### 1. Set up ####

#setwd("./Documents/_github/arab-spring/")

library(data.table)
library(readxl)

# clear environment
rm(list=ls())

# read in the data 
raw_tweets <- read_excel("./data/raw/syria_merge.xlsx")
coh_data <- read.csv("./data/raw/syria_cohesion.csv",
                     header = TRUE,
                     sep = ',')

#### 2. Format date-time ####
formatted_tweets <- raw_tweets %>%
  
  # parse day level
  mutate(date = (stri_extract_first_regex(created_at, "[0-9]{8}+"))) %>%
  
  # convert to date format
  mutate(date = anytime::anydate(date, "%Y%m%d")) %>%
  
  # truncate to date range used
  filter(date >= as.Date("2012-03-31") 
         & date <= as.Date("2012-06-15")) %>%
  
  # number of days since new year
  mutate(num_days = yday(date)) %>%
  
  # parse time
  mutate(time = str_sub(created_at, 10, 15)) %>%
  
  # subset hour
  mutate(hour = as.integer(str_sub(time,1,2))) %>%
  
  # subset minutes
  mutate(minute = as.integer(str_sub(time,3,4))) %>%
  
  # subset seconds
  mutate(seconds = as.integer(str_sub(time,5,6))) %>%
  
  # calculate total seconds for later analyses
  mutate(total_seconds = (num_days*(60^2)*24) + (hour*(60^2)) + (minute*60) + seconds) %>%
  
  # sort by time of tweet
  arrange(total_seconds) %>%
  
  # calculate time since the previous tweet
  mutate(time_to_tweet = total_seconds - dplyr::lag(total_seconds, 1))

#### 3. Raw corpus descriptive stats ####

# plot the distribution of time between tweets
hist(formatted_tweets$time_to_tweet, xlim=c(0, 55))

# get empty data frame
stats = data.frame(matrix(NA, ncol=15))
colnames(stats) <- c("avg_time_to_tweet",
                     "max_time_to_tweet",
                     "min_time_to_tweet",
                     "total_tweets",
                     "avg_daily_tweets", 
                     "max_daily_tweets", 
                     "min_daily_tweets",
                     "total_retweets_beginning",
                     "total_retweets_all",
                     "percent_retweets_beginning",
                     "num_users",
                     "total_replies_mentions",
                     "percent_replies_mentions",
                     "percent_original_tweets",
                     "total_original_tweets")

# get time between tweets
stats$avg_time_to_tweet <- mean(formatted_tweets$time_to_tweet[2:length(formatted_tweets)])
stats$max_time_to_tweet <- max(formatted_tweets$time_to_tweet[2:length(formatted_tweets)])
stats$min_time_to_tweet <- min(formatted_tweets$time_to_tweet[2:length(formatted_tweets)])

# get number of users
stats$num_users <- length(unique(formatted_tweets$user.id))

# get daily number of tweets
daily_stats <- formatted_tweets %>%
  
  group_by(date) %>%
  
  mutate(daily_tweets = n()) %>%
  
  distinct(date, daily_tweets) %>%
  
  ungroup()

# get daily counts of tweets
stats$avg_daily_tweets <- mean(daily_stats$daily_tweets)
stats$max_daily_tweets <- max(daily_stats$daily_tweets)
stats$min_daily_tweets <- min(daily_stats$daily_tweets)
stats$total_tweets <- nrow(formatted_tweets)
hist(daily_stats$daily_tweets)

# filter for retweets
retweets = formatted_tweets %>%
  dplyr::filter(str_detect(text, "\\WRT\\W"))

# # tweets where RT appears not in the middle of a word
retweets_beginning <- formatted_tweets %>%
  dplyr::filter(str_detect(text,"^RT"))

# tweets where RT appears anywhere in the text
retweets_all <- formatted_tweets %>%
  filter(str_detect(text, "RT"))

# create corpus of replies and mentions
replies_mentions <- formatted_tweets %>% 
  filter(str_detect(text, "@[[:alnum:]]"))

# create corpus of original tweets
original_tweets <- formatted_tweets %>% 
  
  # remove RTs
  filter(!str_detect(text, "\\WRT\\W"))

  # tweets where RT appears in the beginning of the text
  # filter(!str_detect(text, "^RT")) %>%

  # tweets where RT appears anywhere in the text
  # filter(!str_detect(text, "RT")) %>%

  # create corpus of replies and mentions
  filter(!str_detect(text, "@[[:alnum:]]"))
  
# count retweets
stats$total_retweets_beginning <- nrow(retweets_beginning)
stats$total_retweets_all <- nrow(retweets_all)
stats$percent_retweets_beginning <- nrow(retweets_beginning)/nrow(formatted_tweets)*100

# count replies/mentions
stats$total_replies_mentions <- nrow(replies_mentions)
stats$percent_replies_mentions <- nrow(replies_mentions)/nrow(formatted_tweets)*100

# count original tweets
stats$total_original_tweets <- nrow(formatted_tweets) - nrow(retweets_all)
stats$percent_original_tweets <- stats$total_original_tweets/nrow(formatted_tweets)*100

#### 4. Tweet group stats ####

# check max and mins
max(coh_data$Duration..s.)
min(coh_data$Duration..s.)
sort(unique(coh_data$Duration..s.))  
  
# plot distribution of durations
hist(coh_data$Duration..s., xlim=c(-210,500), breaks = 2500)

#### 5. Save everything ####

# write formatted tweets to file
write.table(x = formatted_tweets,
            file='./data/formatted/primary/formatted_tweets.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write daily stats to file
write.table(x = daily_stats,
            file='./data/formatted/primary/daily_stats.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write stats to file
write.table(x = stats,
            file='./data/formatted/primary/stats.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)
