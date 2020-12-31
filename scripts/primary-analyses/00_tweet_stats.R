######## Tweet Stats ########
#
# This script explores statistics of the tweet corpus
# for the Arab Spring project.
#
# Code by: M. Chiovaro (@mchiovaro) and A. Paxton (@a-paxton)
# University of Connecticut
# Last updated: 2020_12_31

#### 1. Set up ####

# clear environment
rm(list=ls())

# read in the data 
raw_tweets <- read.csv("./data/raw/syria_merge_sort.csv",
                       header = TRUE,
                       sep = ',')
coh_data <- read.csv("./data/raw/syria_coherence_sort_5.csv",
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
stats = data.frame(matrix(NA, ncol=17))
colnames(stats) <- c("avg_time_to_tweet",
                     "max_time_to_tweet",
                     "min_time_to_tweet",
                     "total_tweets",
                     "avg_daily_tweets", 
                     "max_daily_tweets", 
                     "min_daily_tweets",
                     "retweets",
                     "percent_retweets",
                     "num_users",
                     "total_replies",
                     "percent_replies",
                     "percent_original_tweets",
                     "total_original_tweets",
                     "avg_duration",
                     "max_duration",
                     "min_duration")

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

# identify retweets
retweets = formatted_tweets %>%
  dplyr::filter(!is.na(retweet_id))

# identify replies
replies = formatted_tweets %>%
  dplyr::filter(!is.na(in_reply_to_status_id))

# identify original tweets by stripping out retweets and replies
original_tweets = anti_join(formatted_tweets,
                                replies) %>%
  anti_join(., 
            retweets)

# count retweets
stats$retweets <- nrow(retweets)
stats$percent_retweets <- nrow(retweets)/nrow(formatted_tweets)*100

# count replies/mentions
stats$total_replies <- nrow(replies)
stats$percent_replies <- nrow(replies)/nrow(formatted_tweets)*100

# count original tweets
stats$total_original_tweets <- nrow(original_tweets)
stats$percent_original_tweets <- stats$total_original_tweets/nrow(formatted_tweets)*100

# check that we have all tweets accounted for
percent_sum <- stats$percent_retweets + stats$percent_replies + stats$percent_original_tweets

#### 4. Tweet group stats ####

# check max and mins
stats$avg_duration = mean(coh_data$Duration..s.)
stats$max_duration = max(coh_data$Duration..s.)
stats$min_duration = min(coh_data$Duration..s.)

# check out distribution of unique values
sort(unique(coh_data$Duration..s.))  
  
# plot distribution of durations
hist(coh_data$Duration..s., xlim=c(0,500), breaks = 1000)

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
