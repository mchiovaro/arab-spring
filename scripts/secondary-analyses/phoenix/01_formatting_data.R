######## Formatting Arab Spring Data (Phoenix) ########
#
# Phoenix event data from the Cline center: 
# https://databank.illinois.edu/datasets/IDB-2796521
# "PhoenixBLN-SWB_1979-2019.csv" pulled on 2020_05_13
#
# This script formats data related to the Arab Spring.
# Daily Twitter social cohesion and event data are processed 
# and transformed into sextiles for CRQA and windowed CRQA
# analyses.
#
# Code by: M. Chiovaro (@mchiovaro) and A. Paxton (@a-paxton)
# University of Connecticut
# Last updated: 2020_07_28

#### 1. Set up ####

# clear environment
rm(list=ls())

# read in data
cohesion_df <- read.csv("./data/formatted/primary/formatted_data.csv")
phoenix_df <- read.csv("./data/raw/PhoenixBLN-SWB_1979-2019.csv", header = TRUE, sep = ",")

#### 2. Filter and format the time series ####

### drop unnecessary columns and keep cohesion data
cohesion_df <- cohesion_df %>%
  
  select(Date, MeanCohesion, n) %>%
  
  mutate(Date = as.Date(Date))

### filter for source and target ###

# filter full 2012 ICEWS data for correct dates and parameters
phoenix_df_filtered <- phoenix_df %>% 
  
  # fill blanks with NAs
  na_if("") %>%
  
  # drop observations without dates
  drop_na(story_date) %>%
  
  # turn factor to character for using grepl
  mutate(story_date = as.character(story_date)) %>%
  
  # turn to date format to use select for date range
  mutate_at(vars(story_date), as.Date, format = "%m/%d/%Y") %>%
  
  # grab the dates that align with old data
  filter(story_date >= as.Date("2012-03-30") 
         & story_date <= as.Date("2012-06-15")) %>%
  
  # change search params to characters for searching
  mutate_if(is.factor, as.character)  %>%
  
  # grab observations where either source or target is Syria
  filter(grepl("SYR", source) |
           grepl("SYR", target)) %>%
  
  # convert intensity to numeric
  mutate(goldstein = as.numeric(goldstein))

### prep the event count time series ###

# create new dataframe with counts of different events
phoenix_formatted_source_target <- phoenix_df_filtered %>% 
  
  # group by date
  group_by(story_date) %>%
  
  # count total number of events
  mutate(all_events_source_target = n()) %>%
  
  # count positive events
  mutate(pos_events_source_target = sum(goldstein > 0)) %>%
  
  # count negative events
  mutate(neg_events_source_target = sum(goldstein < 0)) %>%
  
  # sort descending so the mode will the be the more positive event
  mutate(Intensity = sort(goldstein, decreasing = TRUE)) %>%
  
  # calculate mode
  mutate(mode_source_target = statip::mfv1(goldstein, method = "mfv")) %>%
  
  # keep one unique row per day
  distinct(story_date, 
           all_events_source_target, 
           pos_events_source_target, 
           neg_events_source_target, 
           mode_source_target) %>%
  
  # make date variable name match cohesion_df
  rename(Date = story_date) %>%
  
  # ungroup data
  ungroup() %>%
  
  # fill in dates that turned up no observations
  tidyr::complete(Date = seq.Date(min(as.Date(Date)), max(as.Date(Date)), 
                                  by="day")) %>%
  
  # fill NAs with zeros
  replace(is.na(.), 0)

### filter again with just target and prep event count variables ###

# grab observations where target is Syria
phoenix_formatted_target <- phoenix_df_filtered %>%
  
  # grab observations where target is Syria
  filter(grepl("SYR", target)) %>%
  
  # group by date
  group_by(story_date) %>%
  
  # count total number of events
  mutate(all_events_target = n()) %>%
  
  # count positive events
  mutate(pos_events_target = sum(goldstein > 0)) %>%
  
  # count negative events
  mutate(neg_events_target = sum(goldstein < 0)) %>%
  
  # sort descending so the mode will be the more positive event
  mutate(Intensity = sort(goldstein, decreasing = TRUE)) %>%
  
  # calculate mode
  mutate(mode_target = statip::mfv1(goldstein, method = "mfv")) %>%
  
  # keep one unique row per day
  distinct(story_date, 
           all_events_target, 
           pos_events_target, 
           neg_events_target, 
           mode_target) %>%
  
  # make date variable name match cohesion_df
  rename(Date = story_date) %>%
  
  # ungroup data
  ungroup() %>%
  
  # fill in dates that turned up no observations
  tidyr::complete(Date = seq.Date(min(as.Date(Date)), max(as.Date(Date)), 
                                  by="day")) %>%
  
  # fill NAs with zeros
  replace(is.na(.), 0)

### combine all dataframes ###

# bind the event and social cohesion data frames
phoenix_df_formatted <- dplyr::full_join(phoenix_formatted_source_target,
                                         phoenix_formatted_target,
                                         by = c("Date")) %>%
  dplyr::full_join(., cohesion_df,
                   by = c("Date")) %>%
  
  # remove 2020_03_30 for incomplete twitter data
  slice(2:n())

#### 3. Create the sextiles for analyses ####

phoenix_df_formatted <- phoenix_df_formatted %>% ungroup() %>%
  
  # social cohesion
  mutate(coh_sextiles = ntile(MeanCohesion, 6)) %>%
  
  # source and target: all events
  mutate(all_sextiles_source_target = ntile(all_events_source_target, 6)) %>%
  
  # source and target: positive events
  mutate(pos_sextiles_source_target = ntile(pos_events_source_target, 6)) %>%
  
  # source and target: negative events
  mutate(neg_sextiles_source_target = ntile(neg_events_source_target, 6)) %>%
  
  # target only: all events
  mutate(all_sextiles_target = ntile(all_events_target, 6)) %>%
  
  # target only: positive events
  mutate(pos_sextiles_target = ntile(pos_events_target, 6)) %>%
  
  # target only: negative events
  mutate(neg_sextiles_target = ntile(neg_events_target, 6))

### save to file ###

# write data to file
write.table(x = phoenix_df_formatted,
            file='./data/formatted/secondary/phoenix/formatted_data.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

#### 4. Create randomized time series for permutation testing for CRQA ####

# set seed for reproducibility
set.seed(123)

## social cohesion ##

# create empty data frame
shuffled_coh = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  coh_shuffled_ts <- sample(phoenix_df_formatted$coh_sextiles, replace = FALSE)
  sample <- t(as.data.frame(coh_shuffled_ts))
  shuffled_coh <- rbind(shuffled_coh, sample)
}

# take the original time series and add it as a row
original_ts <- as.data.frame(phoenix_df_formatted$coh_sextiles)
original_ts <- as.data.frame(t(original_ts))
shuffled_coh <- rbind(shuffled_coh, original_ts)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled social cohesion time series for CRQA: ",
             nrow(distinct(shuffled_coh))))
if(nrow(distinct(shuffled_coh)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_coh <- as.data.frame(t(shuffled_coh))

# remove real time series from shuffled dataframe
shuffled_coh <- shuffled_coh[c(1:1000)]

### source and target ###

## count of events ##

# create empty data frame
shuffled_all_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  all_shuffled_ts <- sample(phoenix_df_formatted$all_sextiles_source_target, replace = FALSE)
  sample <- t(as.data.frame(all_shuffled_ts))
  shuffled_all_source_target <- rbind(shuffled_all_source_target, sample)
}

# take the original time series and add it as a row
original_ts_all <- as.data.frame(phoenix_df_formatted$all_sextiles_source_target)
original_ts_all <- as.data.frame(t(original_ts_all))
shuffled_all_source_target <- rbind(shuffled_all_source_target, original_ts_all)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled all-event time series (source and target) for CRQA: ",
             nrow(distinct(shuffled_all_source_target))))
if(nrow(distinct(shuffled_all_source_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_all_source_target <- as.data.frame(t(shuffled_all_source_target))

# remove real time series from shuffled dataframe
shuffled_all_source_target <- shuffled_all_source_target[c(1:1000)]

## count of positive events ##

# create empty data frame
shuffled_pos_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  pos_shuffled_ts <- sample(phoenix_df_formatted$pos_sextiles_source_target, replace = FALSE)
  sample <- t(as.data.frame(pos_shuffled_ts))
  shuffled_pos_source_target <- rbind(shuffled_pos_source_target, sample)
}

# take the original time series and add it as a row
original_ts_pos <- as.data.frame(phoenix_df_formatted$pos_sextiles_source_target)
original_ts_pos <- as.data.frame(t(original_ts_pos))
shuffled_pos_source_target <- rbind(shuffled_pos_source_target, original_ts_pos)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled positive-event time series (source and target) for CRQA: ",
             nrow(distinct(shuffled_pos_source_target))))
if(nrow(distinct(shuffled_pos_source_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_pos_source_target <- as.data.frame(t(shuffled_pos_source_target))

# remove real time series from shuffled dataframe
shuffled_pos_source_target <- shuffled_pos_source_target[c(1:1000)]

## count of negative events ##

# create empty data frame
shuffled_neg_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  neg_shuffled_ts <- sample(phoenix_df_formatted$neg_sextiles_source_target, replace = FALSE)
  sample <- t(as.data.frame(neg_shuffled_ts))
  shuffled_neg_source_target <- rbind(shuffled_neg_source_target, sample)
}

# take the original time series and add it as a row
original_ts_neg <- as.data.frame(phoenix_df_formatted$neg_sextiles_source_target)
original_ts_neg <- as.data.frame(t(original_ts_neg))
shuffled_neg_source_target <- rbind(shuffled_neg_source_target, original_ts_neg)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled negative-event time series (source and target) for CRQA: ",
             nrow(distinct(shuffled_neg_source_target))))
if(nrow(distinct(shuffled_neg_source_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_neg_source_target <- as.data.frame(t(shuffled_neg_source_target))

# remove real time series from shuffled dataframe
shuffled_neg_source_target <- shuffled_neg_source_target[c(1:1000)]

### target only ###

## count of events ##

# create empty data frame
shuffled_all_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  all_shuffled_ts <- sample(phoenix_df_formatted$all_sextiles_target, replace = FALSE)
  sample <- t(as.data.frame(all_shuffled_ts))
  shuffled_all_target <- rbind(shuffled_all_target, sample)
}

# take the original time series and add it as a row
original_ts_all <- as.data.frame(phoenix_df_formatted$all_sextiles_target)
original_ts_all <- as.data.frame(t(original_ts_all))
shuffled_all_target <- rbind(shuffled_all_target, original_ts_all)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled all-event time series (target only) for CRQA: ",
             nrow(distinct(shuffled_all_target))))
if(nrow(distinct(shuffled_all_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_all_target <- as.data.frame(t(shuffled_all_target))

# remove real time series from shuffled dataframe
shuffled_all_target <- shuffled_all_target[c(1:1000)]

## count of positive events ##

# create empty data frame
shuffled_pos_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  pos_shuffled_ts <- sample(phoenix_df_formatted$pos_sextiles_target, replace = FALSE)
  sample <- t(as.data.frame(pos_shuffled_ts))
  shuffled_pos_target <- rbind(shuffled_pos_target, sample)
}

# take the original time series and add it as a row
original_ts_pos <- as.data.frame(phoenix_df_formatted$pos_sextiles_target)
original_ts_pos <- as.data.frame(t(original_ts_pos))
shuffled_pos_target <- rbind(shuffled_pos_target, original_ts_pos)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled positive-event time series (target only) for CRQA: ",
             nrow(distinct(shuffled_pos_target))))
if(nrow(distinct(shuffled_pos_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_pos_target <- as.data.frame(t(shuffled_pos_target))

# remove real time series from shuffled dataframe
shuffled_pos_target <- shuffled_pos_target[c(1:1000)]

## count of negative events ##

# create empty data frame
shuffled_neg_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  neg_shuffled_ts <- sample(phoenix_df_formatted$neg_sextiles_target, replace = FALSE)
  sample <- t(as.data.frame(neg_shuffled_ts))
  shuffled_neg_target <- rbind(shuffled_neg_target, sample)
}

# take the original time series and add it as a row
original_ts_neg <- as.data.frame(phoenix_df_formatted$neg_sextiles_target)
original_ts_neg <- as.data.frame(t(original_ts_neg))
shuffled_neg_target <- rbind(shuffled_neg_target, original_ts_neg)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled negative-event time series (target only) for CRQA: ",
             nrow(distinct(shuffled_neg_target))))
if(nrow(distinct(shuffled_neg_target)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_neg_target <- as.data.frame(t(shuffled_neg_target))

# remove real time series from shuffled dataframe
shuffled_neg_target <- shuffled_neg_target[c(1:1000)]

### save to file ###

# bind shuffled data together to save as one file
shuffled_full <- cbind(shuffled_coh, 
                       shuffled_all_source_target, 
                       shuffled_pos_source_target, 
                       shuffled_neg_source_target, 
                       shuffled_all_target, 
                       shuffled_pos_target, 
                       shuffled_neg_target)

# write shuffled data to file
write.table(x = shuffled_full,
            file='./data/formatted/secondary/phoenix/shuffled_data_full.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

#### 5. Create randomized time series for permutation testing for windowed CRQA ####

## social cohesion ##

# create empty data frame
shuffled_coh = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  coh_shuffled_ts <- sample(phoenix_df_formatted$coh_sextiles, size = 14, replace = FALSE)
  sample <- t(as.data.frame(coh_shuffled_ts))
  shuffled_coh <- rbind(shuffled_coh, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled social cohesion time series for windowed CRQA: ",
             nrow(distinct(shuffled_coh))))
if(nrow(distinct(shuffled_coh)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_coh <- as.data.frame(t(shuffled_coh))

### source and target ###

## count of all events ##

# create empty data frame
shuffled_all_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  all_shuffled_ts <- sample(phoenix_df_formatted$all_sextiles_source_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(all_shuffled_ts))
  shuffled_all_source_target <- rbind(shuffled_all_source_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled all-event time series (source and target) for windowed CRQA: ",
             nrow(distinct(shuffled_all_source_target))))
if(nrow(distinct(shuffled_all_source_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_all_source_target <- as.data.frame(t(shuffled_all_source_target))

## count of positive events ##

# create empty data frame
shuffled_pos_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  pos_shuffled_ts <- sample(phoenix_df_formatted$pos_sextiles_source_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(pos_shuffled_ts))
  shuffled_pos_source_target <- rbind(shuffled_pos_source_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled positive-event time series (source and target) for windowed CRQA: ",
             nrow(distinct(shuffled_pos_source_target))))
if(nrow(distinct(shuffled_pos_source_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_pos_source_target <- as.data.frame(t(shuffled_pos_source_target))

## count of negative events ##

# create empty data frame
shuffled_neg_source_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  neg_shuffled_ts <- sample(phoenix_df_formatted$neg_sextiles_source_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(neg_shuffled_ts))
  shuffled_neg_source_target <- rbind(shuffled_neg_source_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled negative-event time series (source and target) for windowed CRQA: ",
             nrow(distinct(shuffled_neg_source_target))))
if(nrow(distinct(shuffled_neg_source_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_neg_source_target <- as.data.frame(t(shuffled_neg_source_target))

### target only ###

## count of all events ##

# create empty data frame
shuffled_all_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  all_shuffled_ts <- sample(phoenix_df_formatted$all_sextiles_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(all_shuffled_ts))
  shuffled_all_target <- rbind(shuffled_all_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled all-event time series (target only) for windowed CRQA: ",
             nrow(distinct(shuffled_all_target))))
if(nrow(distinct(shuffled_all_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_all_target <- as.data.frame(t(shuffled_all_target))

## count of positive events ##

# create empty data frame
shuffled_pos_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  pos_shuffled_ts <- sample(phoenix_df_formatted$pos_sextiles_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(pos_shuffled_ts))
  shuffled_pos_target <- rbind(shuffled_pos_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled positive-event time series (target only) for windowed CRQA: ",
             nrow(distinct(shuffled_pos_target))))
if(nrow(distinct(shuffled_pos_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_pos_target <- as.data.frame(t(shuffled_pos_target))

## count of negative events ##

# create empty data frame
shuffled_neg_target = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  neg_shuffled_ts <- sample(phoenix_df_formatted$neg_sextiles_target, size = 14, replace = FALSE)
  sample <- t(as.data.frame(neg_shuffled_ts))
  shuffled_neg_target <- rbind(shuffled_neg_target, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled negative-event time series (target only) for windowed CRQA: ",
             nrow(distinct(shuffled_neg_target))))
if(nrow(distinct(shuffled_neg_target)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
shuffled_neg_target <- as.data.frame(t(shuffled_neg_target))

### save to file ###

# bind shuffled data together to save as one file
shuffled_windowed <- cbind(shuffled_coh, 
                           shuffled_all_source_target, 
                           shuffled_pos_source_target, 
                           shuffled_neg_source_target, 
                           shuffled_all_target, 
                           shuffled_pos_target, 
                           shuffled_neg_target)

# write shuffled data to file
write.table(x = shuffled_windowed,
            file='./data/formatted/secondary/phoenix/shuffled_data_windowed.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)
