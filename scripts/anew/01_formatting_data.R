######## CRQA ########
#
# This script runs CRQA on the affect data from ANEW
# for the Arab Spring project.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2021_10_11

#### 1. Set up ####

# clear environment
rm(list=ls())

# set working directory
setwd("./Documents/github/arab-spring/")

# read in ANEW data 
anew <- read.csv("./data/formatted/Output_Anew_Sentiment_all_syria_merge_sort_trimmed.csv",
                 header = TRUE,
                 sep = ',')

# read in event data
event <- read.csv("./data/formatted/primary/formatted_data.csv")

# load required packages
library(tidyverse)

#### 2. Format data ####

# rename valence
colnames(anew)[2] <- "Valence"

# take average scores for the tweets
anew_formatted <- anew %>%
  
  # replace NAs with 0s
  replace(is.na(.), 0) %>%
  
  # take mean for each tweet
  mutate(mean_val = Valence/tweet.word.count) %>%
  mutate(mean_aro = Arousal/tweet.word.count) %>%
  mutate(mean_dom = Dominance/tweet.word.count) %>%
  
  # turn to date format to use select for date range
  mutate_at(vars(Date), as.Date, format = "%Y-%m-%d") %>%
  
  # grab the dates that align with other data
  filter(Date >= as.Date("2012-03-31") 
         & Date <= as.Date("2012-06-15")) %>%
  
  # group by date
  group_by(Date) %>%
  
  # create mean daily scores
  summarise(across(c(mean_val, mean_aro, mean_dom), ~ sum(.x, na.rm = TRUE)/n())) %>%
  
  # create deciles
  mutate(daily_val_dec = ntile(mean_val, 10)) %>%
  mutate(daily_aro_dec = ntile(mean_aro, 10)) %>%
  mutate(daily_dom_dec = ntile(mean_dom, 10))

# save formatted data
write.table(x = anew_formatted,
            file='./data/formatted/formatted_anew.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)
