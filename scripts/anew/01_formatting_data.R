######## Formatting Data ########
#
# This script formats affect data from ANEW
# for the Arab Spring project.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2021_10_13

#### 1. Set up ####

# clear environment
rm(list=ls())

# set working directory
#setwd("./Documents/github/arab-spring/")

# read in ANEW data 
anew <- read.csv("./data/formatted/Output_Anew_Sentiment_all_syria_merge_sort_trimmed.csv",
                 header = TRUE,
                 sep = ',')

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

#### 3. Create randomized time series for permutation testing for CRQA ####

# set seed for reproducibility
set.seed(123)

## valence ##

# create empty data frame
shuffled_val = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  val_shuffled <- sample(anew_formatted$daily_val_dec, replace = FALSE)
  sample <- t(as.data.frame(val_shuffled))
  shuffled_val <- rbind(shuffled_val, sample)
}

# take the original time series and add it as a row
original <- as.data.frame(anew_formatted$daily_val_dec)
original <- as.data.frame(t(original))
shuffled_val <- rbind(shuffled_val, original)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled valence time series for CRQA: ",
             nrow(distinct(shuffled_val))))
if(nrow(distinct(shuffled_val)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}
# transform rows to columns for binding
shuffled_val <- as.data.frame(t(shuffled_val))

# remove real time series from shuffled dataframe
shuffled_val <- shuffled_val[c(1:1000)]

## arousal ##

# create empty data frame
shuffled_aro = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  aro_shuffled <- sample(anew_formatted$daily_aro_dec, replace = FALSE)
  sample <- t(as.data.frame(aro_shuffled))
  shuffled_aro <- rbind(shuffled_aro, sample)
}

# take the original time series and add it as a row
original <- as.data.frame(anew_formatted$daily_aro_dec)
original <- as.data.frame(t(original))
shuffled_aro <- rbind(shuffled_aro, original)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled arousal time series for CRQA: ",
             nrow(distinct(shuffled_aro))))
if(nrow(distinct(shuffled_aro)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}
# transform rows to columns for binding
shuffled_aro <- as.data.frame(t(shuffled_aro))

# remove real time series from shuffled dataframe
shuffled_aro <- shuffled_aro[c(1:1000)]

## dominance ##

# create empty data frame
shuffled_dom = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  dom_shuffled <- sample(anew_formatted$daily_dom_dec, replace = FALSE)
  sample <- t(as.data.frame(dom_shuffled))
  shuffled_dom <- rbind(shuffled_dom, sample)
}

# take the original time series and add it as a row
original <- as.data.frame(anew_formatted$daily_dom_dec)
original <- as.data.frame(t(original))
shuffled_dom <- rbind(shuffled_dom, original)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled dominance time series for CRQA: ",
             nrow(distinct(shuffled_dom))))
if(nrow(distinct(shuffled_dom)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}
# transform rows to columns for binding
shuffled_dom <- as.data.frame(t(shuffled_dom))

# remove real time series from shuffled dataframe
shuffled_dom <- shuffled_dom[c(1:1000)]

## save data ##

# bind the shuffled data
shuffled_anew_full <- cbind(shuffled_val, 
                            shuffled_aro,
                            shuffled_dom)

# write shuffled data to file
write.table(x = shuffled_anew_full,
            file='./data/formatted/shuffled_anew_full.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

#### 4. Create randomized time series for permutation testing for windowed CRQA ####

## valence ##

# create empty data frame
shuffled_val = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  val_shuffled <- sample(anew_formatted$daily_val_dec, size = 14, replace = FALSE)
  sample <- t(as.data.frame(val_shuffled))
  shuffled_val <- rbind(shuffled_val, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled valence time series for windowed CRQA: ",
             nrow(distinct(shuffled_val))))
if(nrow(distinct(shuffled_val)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_val <- as.data.frame(t(shuffled_val))

## arousal ##

# create empty data frame
shuffled_aro = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  aro_shuffled <- sample(anew_formatted$daily_aro_dec, size = 14, replace = FALSE)
  sample <- t(as.data.frame(aro_shuffled))
  shuffled_aro <- rbind(shuffled_aro, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled arousal time series for windowed CRQA: ",
             nrow(distinct(shuffled_aro))))
if(nrow(distinct(shuffled_aro)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_aro <- as.data.frame(t(shuffled_aro))

## dominance ##

# create empty data frame
shuffled_dom = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  dom_shuffled <- sample(anew_formatted$daily_dom_dec, size = 14, replace = FALSE)
  sample <- t(as.data.frame(dom_shuffled))
  shuffled_dom <- rbind(shuffled_dom, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled dominance time series for windowed CRQA: ",
             nrow(distinct(shuffled_dom))))
if(nrow(distinct(shuffled_dom)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_dom <- as.data.frame(t(shuffled_dom))

## save data ##

# bind the shuffled data
shuffled_anew_windowed <- cbind(shuffled_val, 
                            shuffled_aro,
                            shuffled_dom)

# write shuffled data to file
write.table(x = shuffled_anew_windowed,
            file='./data/formatted/shuffled_anew_windowed.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)
