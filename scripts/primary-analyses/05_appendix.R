######## Appendix Analyses ########
#
# This script runs post-hoc analyses on raw number of
# tweets per day and number of retweets per day.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_02_08

#### 1. Set up ####

# clear environment
rm(list=ls())

# specify global plotting variables
all_event_color = "#CC79A7"
positive_event_color = "#0072B2"
negative_event_color = "#D55E00"

# read in the data 
raw_tweets <- read.csv("./data/raw/syria_merge_sort.csv",
                       header = TRUE,
                       sep = ',')
formatted_data <- read.csv("./data/formatted/primary/formatted_data.csv")
shuffled_full <- read.csv("./data/formatted/primary/shuffled_data_full.csv")

# split shuffled data into their own dataframes
shuffled_all_source_target <- shuffled_full[,c(1001:2000)]
shuffled_pos_source_target <- shuffled_full[,c(2001:3000)]
shuffled_neg_source_target <- shuffled_full[,c(3001:4000)]
shuffled_all_target <- shuffled_full[,c(4001:5000)]
shuffled_pos_target <- shuffled_full[,c(5001:6000)]
shuffled_neg_target <- shuffled_full[,c(6001:7000)]

# set seed for reproducibility
set.seed(123)

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
  arrange(total_seconds)


#### 3. Get daily counts of retweets, replies, and originals ####  

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

# count daily retweets
count_retweets <- retweets %>%
  group_by(date) %>%
  mutate(num_retweets = n()) %>%
  distinct(date, num_retweets) %>%
  ungroup() %>%
  mutate(deciles_retweets = ntile(num_retweets, 10))

# count daily replies
count_replies <- replies %>%
  group_by(date) %>%
  mutate(num_replies = n()) %>%
  distinct(date, num_replies) %>%
  ungroup() %>%
  mutate(deciles_replies = ntile(num_replies, 10))

# count daily original tweets
count_orig <- original_tweets %>%
  group_by(date) %>%
  mutate(num_orig_tweets = n()) %>%
  distinct(date, num_orig_tweets) %>%
  ungroup() %>%
  mutate(deciles_orig = ntile(num_orig_tweets, 10))

# bind counts together in one dataframe
daily_counts <- as.data.frame(cbind(count_retweets$date, 
                                    count_retweets$deciles_retweets, 
                                    count_replies$deciles_replies, 
                                    count_orig$deciles_orig))
# rename columns
colnames(daily_counts) <- c("date", 
                            "deciles_retweets", 
                            "deciles_replies", 
                            "deciles_orig")

#### 4. Create randomized tweet/retweet time series for permutation testing for CRQA ####

# set seed for reproducibility
set.seed(123)

## retweets ##

# create empty data frame
all_retweets_shuffled = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  retweets_shuffled <- sample(daily_counts$deciles_retweets, replace = FALSE)
  sample <- t(as.data.frame(retweets_shuffled))
  all_retweets_shuffled <- rbind(all_retweets_shuffled, sample)
}

# take the original time series and add it as a row
original <- as.data.frame(daily_counts$deciles_retweets)
original <- as.data.frame(t(original))
all_retweets_shuffled <- rbind(all_retweets_shuffled, original)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled social cohesion time series for CRQA: ",
             nrow(distinct(all_retweets_shuffled))))
if(nrow(distinct(all_retweets_shuffled)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}
# transform rows to columns for binding
all_retweets_shuffled <- as.data.frame(t(all_retweets_shuffled))

# remove real time series from shuffled dataframe
all_retweets_shuffled <- all_retweets_shuffled[c(1:1000)]

## originals ##

# create empty data frame
all_orig_shuffled = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  orig_shuffled <- sample(daily_counts$deciles_orig, replace = FALSE)
  sample <- t(as.data.frame(orig_shuffled))
  all_orig_shuffled <- rbind(all_orig_shuffled, sample)
}

# take the original time series and add it as a row
original <- as.data.frame(daily_counts$deciles_orig)
original <- as.data.frame(t(original))
all_orig_shuffled <- rbind(all_orig_shuffled, original)

# check to see if we have 1001 distinct time series
print(paste0("Total distinct shuffled social cohesion time series for CRQA: ",
             nrow(distinct(all_orig_shuffled))))
if(nrow(distinct(all_orig_shuffled)) != 1001){
  print("WARNING: Duplicates in surrogate time series.")
}
# transform rows to columns for binding
all_orig_shuffled <- as.data.frame(t(all_orig_shuffled))

# remove real time series from shuffled dataframe
all_orig_shuffled <- all_orig_shuffled[c(1:1000)]

#### 5. Create randomized tweet/retweet time series for permutation testing for windowed CRQA ####

## retweets ##

# create empty data frame
win_retweets_shuffled = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  retweets_shuffled <- sample(daily_counts$deciles_retweets, size = 14, replace = FALSE)
  sample <- t(as.data.frame(retweets_shuffled))
  win_retweets_shuffled <- rbind(win_retweets_shuffled, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled social cohesion time series for windowed CRQA: ",
             nrow(distinct(win_retweets_shuffled))))
if(nrow(distinct(win_retweets_shuffled)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
win_retweets_shuffled <- as.data.frame(t(win_retweets_shuffled))

## originals ##

# create empty data frame
win_orig_shuffled = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  orig_shuffled <- sample(daily_counts$deciles_orig, size = 14, replace = FALSE)
  sample <- t(as.data.frame(orig_shuffled))
  win_orig_shuffled <- rbind(win_orig_shuffled, sample)
}

# check to see if we have 1000 distinct time series
print(paste0("Total distinct shuffled social cohesion time series for windowed CRQA: ",
             nrow(distinct(win_orig_shuffled))))
if(nrow(distinct(win_orig_shuffled)) != 1000){
  print("WARNING: Duplicates in surrogate time series.")
}

# transform rows to columns for binding
win_orig_shuffled <- as.data.frame(t(win_orig_shuffled))

#### 6. Run CRQA for source-target data ####

### all events ###

## count of all events and number of tweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_orig,
                                 ts2=formatted_data$all_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_all_source_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_all_source_target,'./results/primary/appendix/crqa/originals/source_target-crqa_results-all_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_orig = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_orig_shuffled[,c(i)],
                                   ts2=shuffled_all_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_orig <- rbind(rqa_shuffled_orig, rqa_results)
  
}

# initialize data frame for saving significance results
significance_all_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_orig[,c(i)] > rqa_results_all_source_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_all_source_target <- cbind(significance_all_source_target, temp)
}

# rename variables
names(significance_all_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_all_source_target,'./results/primary/appendix/crqa/originals/source_target-metric_signif_all.csv',
            sep=",", row.names=FALSE)

## count of all events and number of retweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_retweets,
                                 ts2=formatted_data$all_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_all_source_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_all_source_target,'./results/primary/appendix/crqa/retweets/source_target-crqa_results-all_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_retweets = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_retweets_shuffled[,c(i)],
                                   ts2=shuffled_all_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_retweets <- rbind(rqa_shuffled_retweets, rqa_results)
  
}

# initialize data frame for saving significance results
significance_all_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_retweets[,c(i)] > rqa_results_all_source_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_all_source_target <- cbind(significance_all_source_target, temp)
}

# rename variables
names(significance_all_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_all_source_target,'./results/primary/appendix/crqa/retweets/source_target-metric_signif_all.csv',
            sep=",", row.names=FALSE)

### positive events ###

## count of positive events and number of tweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_orig,
                                 ts2=formatted_data$pos_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_pos_source_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_pos_source_target,'./results/primary/appendix/crqa/originals/source_target-crqa_results-pos_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_orig = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_orig_shuffled[,c(i)],
                                   ts2=shuffled_pos_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_orig <- rbind(rqa_shuffled_orig, rqa_results)
  
}

# initialize data frame for saving significance results
significance_pos_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_orig[,c(i)] > rqa_results_pos_source_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_pos_source_target <- cbind(significance_pos_source_target, temp)
}

# rename variables
names(significance_pos_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_pos_source_target,'./results/primary/appendix/crqa/originals/source_target-metric_signif_pos.csv',
            sep=",", row.names=FALSE)

## count of positive events and number of retweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_retweets,
                                 ts2=formatted_data$pos_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_pos_source_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_pos_source_target,'./results/primary/appendix/crqa/retweets/source_target-crqa_results-pos_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_retweets = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_retweets_shuffled[,c(i)],
                                   ts2=shuffled_pos_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_retweets <- rbind(rqa_shuffled_retweets, rqa_results)
  
}

# initialize data frame for saving significance results
significance_pos_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_retweets[,c(i)] > rqa_results_pos_source_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_pos_source_target <- cbind(significance_pos_source_target, temp)
}

# rename variables
names(significance_pos_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_pos_source_target,'./results/primary/appendix/crqa/retweets/source_target-metric_signif_pos.csv',
            sep=",", row.names=FALSE)

### negative events ###

## count of negative events and number of tweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_orig,
                                 ts2=formatted_data$neg_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_neg_source_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_neg_source_target,'./results/primary/appendix/crqa/originals/source_target-crqa_results-neg_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_orig = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_orig_shuffled[,c(i)],
                                   ts2=shuffled_neg_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_orig <- rbind(rqa_shuffled_orig, rqa_results)
  
}

# initialize data frame for saving significance results
significance_neg_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_orig[,c(i)] > rqa_results_neg_source_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_neg_source_target <- cbind(significance_neg_source_target, temp)
}

# rename variables
names(significance_neg_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_neg_source_target,'./results/primary/appendix/crqa/originals/source_target-metric_signif_neg.csv',
            sep=",", row.names=FALSE)

## count of negative events and number of retweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_retweets,
                                 ts2=formatted_data$neg_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_neg_source_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_neg_source_target,'./results/primary/appendix/crqa/retweets/source_target-crqa_results-neg_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_retweets = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_retweets_shuffled[,c(i)],
                                   ts2=shuffled_neg_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_retweets <- rbind(rqa_shuffled_retweets, rqa_results)
  
}

# initialize data frame for saving significance results
significance_neg_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_retweets[,c(i)] > rqa_results_neg_source_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_neg_source_target <- cbind(significance_neg_source_target, temp)
}

# rename variables
names(significance_neg_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_neg_source_target,'./results/primary/appendix/crqa/retweets/source_target-metric_signif_neg.csv',
            sep=",", row.names=FALSE)

#### 6. Run CRQA for target-only data ####

### all events ###

## count of all events and number of tweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_orig,
                                 ts2=formatted_data$all_deciles_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_all_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_all_target,'./results/primary/appendix/crqa/originals/target-crqa_results-all_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_orig = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_orig_shuffled[,c(i)],
                                   ts2=shuffled_all_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_orig <- rbind(rqa_shuffled_orig, rqa_results)
  
}

# initialize data frame for saving significance results
significance_all_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_orig[,c(i)] > rqa_results_all_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_all_target <- cbind(significance_all_target, temp)
}

# rename variables
names(significance_all_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_all_target,'./results/primary/appendix/crqa/originals/target-metric_signif_all.csv',
            sep=",", row.names=FALSE)

## count of all events and number of retweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_retweets,
                                 ts2=formatted_data$all_deciles_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_all_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_all_target,'./results/primary/appendix/crqa/retweets/target-crqa_results-all_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_retweets = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_retweets_shuffled[,c(i)],
                                   ts2=shuffled_all_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_retweets <- rbind(rqa_shuffled_retweets, rqa_results)
  
}

# initialize data frame for saving significance results
significance_all_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_retweets[,c(i)] > rqa_results_all_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_all_target <- cbind(significance_all_target, temp)
}

# rename variables
names(significance_all_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_all_target,'./results/primary/appendix/crqa/retweets/target-metric_signif_all.csv',
            sep=",", row.names=FALSE)

### positive events ###

## count of positive events and number of tweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_orig,
                                 ts2=formatted_data$pos_deciles_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_pos_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_pos_target,'./results/primary/appendix/crqa/originals/target-crqa_results-pos_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_orig = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_orig_shuffled[,c(i)],
                                   ts2=shuffled_pos_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_orig <- rbind(rqa_shuffled_orig, rqa_results)
  
}

# initialize data frame for saving significance results
significance_pos_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_orig[,c(i)] > rqa_results_pos_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_pos_target <- cbind(significance_pos_target, temp)
}

# rename variables
names(significance_pos_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_pos_target,'./results/primary/appendix/crqa/originals/target-metric_signif_pos.csv',
            sep=",", row.names=FALSE)

## count of positive events and number of retweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_retweets,
                                 ts2=formatted_data$pos_deciles_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_pos_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_pos_target,'./results/primary/appendix/crqa/retweets/target-crqa_results-pos_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_retweets = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_retweets_shuffled[,c(i)],
                                   ts2=shuffled_pos_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_retweets <- rbind(rqa_shuffled_retweets, rqa_results)
  
}

# initialize data frame for saving significance results
significance_pos_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_retweets[,c(i)] > rqa_results_pos_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_pos_target <- cbind(significance_pos_target, temp)
}

# rename variables
names(significance_pos_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_pos_target,'./results/primary/appendix/crqa/retweets/target-metric_signif_pos.csv',
            sep=",", row.names=FALSE)

### negative events ###

## count of negative events and number of tweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_orig,
                                 ts2=formatted_data$neg_deciles_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_neg_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_neg_target,'./results/primary/appendix/crqa/originals/target-crqa_results-neg_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_orig = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_orig_shuffled[,c(i)],
                                   ts2=shuffled_neg_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_orig <- rbind(rqa_shuffled_orig, rqa_results)
  
}

# initialize data frame for saving significance results
significance_neg_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_orig[,c(i)] > rqa_results_neg_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_neg_target <- cbind(significance_neg_target, temp)
}

# rename variables
names(significance_neg_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_neg_target,'./results/primary/appendix/crqa/originals/target-metric_signif_neg.csv',
            sep=",", row.names=FALSE)

## count of negative events and number of retweets ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=daily_counts$deciles_retweets,
                                 ts2=formatted_data$neg_deciles_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_neg_target <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_neg_target,'./results/primary/appendix/crqa/retweets/target-crqa_results-neg_events.csv',
            sep=",", row.names=FALSE)

# initialize data frame for saving permutation metrics
rqa_shuffled_retweets = data.frame()

# calculate crqa metrics for each shuffled time series
for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=all_retweets_shuffled[,c(i)],
                                   ts2=shuffled_neg_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_retweets <- rbind(rqa_shuffled_retweets, rqa_results)
  
}

# initialize data frame for saving significance results
significance_neg_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_retweets[,c(i)] > rqa_results_neg_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_neg_target <- cbind(significance_neg_target, temp)
}

# rename variables
names(significance_neg_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_neg_target,'./results/primary/appendix/crqa/retweets/target-metric_signif_neg.csv',
            sep=",", row.names=FALSE)

#### 7. Create presentable tables ####

### retweets ###

# read in metric names
metrics <- t(read.csv("./results/primary/appendix/crqa/retweets/target-crqa_results-all_events.csv",
                      header = FALSE)[-2,-1])

# read in metrics from crq analyses (target filtered)
metrics_all_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/target-crqa_results-all_events.csv",
                               header = FALSE)[-1,-1])
metrics_pos_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/target-crqa_results-pos_events.csv",
                               header = FALSE)[-1,-1])
metrics_neg_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/target-crqa_results-neg_events.csv",
                               header = FALSE)[-1,-1])

# read in significance from permutation tests (target filtered)
sig_all_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/target-metric_signif_all.csv",
                           header = FALSE)[-1,-1])
sig_pos_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/target-metric_signif_pos.csv",
                           header = FALSE)[-1,-1])
sig_neg_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/target-metric_signif_neg.csv",
                           header = FALSE)[-1,-1])

# read in metrics from crq analyses (source and target filtered)
metrics_all_source_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/source_target-crqa_results-all_events.csv",
                                      header = FALSE)[-1,-1])
metrics_pos_source_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/source_target-crqa_results-pos_events.csv",
                                      header = FALSE)[-1,-1])
metrics_neg_source_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/source_target-crqa_results-neg_events.csv",
                                      header = FALSE)[-1,-1])

# read in significance from permutation tests (source and target filtered)
sig_all_source_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/source_target-metric_signif_all.csv",
                                  header = FALSE)[-1,-1])
sig_pos_source_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/source_target-metric_signif_pos.csv",
                                  header = FALSE)[-1,-1])
sig_neg_source_targ <- t(read.csv("./results/primary/appendix/crqa/retweets/source_target-metric_signif_neg.csv",
                                  header = FALSE)[-1,-1])

## target table ##

# bind metrics and significance results
target_results <- as.data.frame(cbind(metrics,
                                      metrics_all_targ,
                                      sig_all_targ,
                                      metrics_pos_targ,
                                      sig_pos_targ,
                                      metrics_neg_targ,
                                      sig_neg_targ), stringsAsFactors = FALSE)

# covert to numerics for rounding
numerics <- c(2:7)
target_results[,numerics] <- sapply(target_results[,numerics],as.numeric)

# add significance stars
stars_all <- as.data.frame(stars.pval(target_results[,3]))
stars_pos <- as.data.frame(stars.pval(target_results[,5]))
stars_neg <- as.data.frame(stars.pval(target_results[,7]))

target_results <- cbind(target_results, stars_all, stars_pos, stars_neg)
target_results <- target_results[,c(1,2,3,8,4,5,9,6,7,10)]

# give intuitive names
colnames(target_results) <- c("metric",
                              "all events", "p", "sig.",
                              "positive events", "p", "sig.",
                              "negative events", "p", "sig.")

# exclude row names
rownames(target_results) <- NULL

# set to not split table and round to two decimal places
panderOptions("table.split.table", Inf)
panderOptions('round', 3)
panderOptions('keep.trailing.zeros', TRUE)

# style for Rmd
panderOptions("table.style", "rmarkdown")

# build table
pander(target_results, caption = "\\label{table-1}CRQA results for retweets and target-only data.\n. p < .10, * p < .05, ** p < .001")

## source-target table ##

# bind metrics and significance results
source_target_results <- as.data.frame(cbind(metrics,
                                             metrics_all_source_targ,
                                             sig_all_source_targ,
                                             metrics_pos_source_targ,
                                             sig_pos_source_targ,
                                             metrics_neg_source_targ,
                                             sig_neg_source_targ), stringsAsFactors = FALSE)

# covert to numerics for rounding
numerics <- c(2:7)
source_target_results[,numerics] <- sapply(source_target_results[,numerics],as.numeric)

# add significance stars
stars_all <- as.data.frame(stars.pval(source_target_results[,3]))
stars_pos <- as.data.frame(stars.pval(source_target_results[,5]))
stars_neg <- as.data.frame(stars.pval(source_target_results[,7]))

source_target_results <- cbind(source_target_results, stars_all, stars_pos, stars_neg)
source_target_results <- source_target_results[,c(1,2,3,8,4,5,9,6,7,10)]

# give intuitive names
colnames(source_target_results) <- c("metric",
                                     "all events", "p", "sig.",
                                     "positive events", "p", "sig.",
                                     "negative events", "p", "sig.")

# exclude row names
rownames(source_target_results) <- NULL

# set to not split table and round to two decimal places
panderOptions("table.split.table", Inf)
panderOptions('round', 3)
panderOptions('keep.trailing.zeros', TRUE)

# style for Rmd
panderOptions("table.style", "rmarkdown")

# build table
pander(source_target_results, caption = "\\label{table-1}CRQA results for retweets and target-only data.\n. p < .10, * p < .05, ** p < .001")

## originals ##

# read in metric names
metrics <- t(read.csv("./results/primary/appendix/crqa/originals/target-crqa_results-all_events.csv",
                      header = FALSE)[-2,-1])

# read in metrics from crq analyses (target filtered)
metrics_all_targ <- t(read.csv("./results/primary/appendix/crqa/originals/target-crqa_results-all_events.csv",
                               header = FALSE)[-1,-1])
metrics_pos_targ <- t(read.csv("./results/primary/appendix/crqa/originals/target-crqa_results-pos_events.csv",
                               header = FALSE)[-1,-1])
metrics_neg_targ <- t(read.csv("./results/primary/appendix/crqa/originals/target-crqa_results-neg_events.csv",
                               header = FALSE)[-1,-1])

# read in significance from permutation tests (target filtered)
sig_all_targ <- t(read.csv("./results/primary/appendix/crqa/originals/target-metric_signif_all.csv",
                           header = FALSE)[-1,-1])
sig_pos_targ <- t(read.csv("./results/primary/appendix/crqa/originals/target-metric_signif_pos.csv",
                           header = FALSE)[-1,-1])
sig_neg_targ <- t(read.csv("./results/primary/appendix/crqa/originals/target-metric_signif_neg.csv",
                           header = FALSE)[-1,-1])

# read in metrics from crq analyses (source and target filtered)
metrics_all_source_targ <- t(read.csv("./results/primary/appendix/crqa/originals/source_target-crqa_results-all_events.csv",
                                      header = FALSE)[-1,-1])
metrics_pos_source_targ <- t(read.csv("./results/primary/appendix/crqa/originals/source_target-crqa_results-pos_events.csv",
                                      header = FALSE)[-1,-1])
metrics_neg_source_targ <- t(read.csv("./results/primary/appendix/crqa/originals/source_target-crqa_results-neg_events.csv",
                                      header = FALSE)[-1,-1])

# read in significance from permutation tests (source and target filtered)
sig_all_source_targ <- t(read.csv("./results/primary/appendix/crqa/originals/source_target-metric_signif_all.csv",
                                  header = FALSE)[-1,-1])
sig_pos_source_targ <- t(read.csv("./results/primary/appendix/crqa/originals/source_target-metric_signif_pos.csv",
                                  header = FALSE)[-1,-1])
sig_neg_source_targ <- t(read.csv("./results/primary/appendix/crqa/originals/source_target-metric_signif_neg.csv",
                                  header = FALSE)[-1,-1])

## target table ##

# bind metrics and significance results
target_results <- as.data.frame(cbind(metrics,
                                      metrics_all_targ,
                                      sig_all_targ,
                                      metrics_pos_targ,
                                      sig_pos_targ,
                                      metrics_neg_targ,
                                      sig_neg_targ), stringsAsFactors = FALSE)

# covert to numerics for rounding
numerics <- c(2:7)
target_results[,numerics] <- sapply(target_results[,numerics],as.numeric)

# add significance stars
stars_all <- as.data.frame(stars.pval(target_results[,3]))
stars_pos <- as.data.frame(stars.pval(target_results[,5]))
stars_neg <- as.data.frame(stars.pval(target_results[,7]))

target_results <- cbind(target_results, stars_all, stars_pos, stars_neg)
target_results <- target_results[,c(1,2,3,8,4,5,9,6,7,10)]

# give intuitive names
colnames(target_results) <- c("metric",
                              "all events", "p", "sig.",
                              "positive events", "p", "sig.",
                              "negative events", "p", "sig.")

# exclude row names
rownames(target_results) <- NULL

# set to not split table and round to two decimal places
panderOptions("table.split.table", Inf)
panderOptions('round', 3)
panderOptions('keep.trailing.zeros', TRUE)

# style for Rmd
panderOptions("table.style", "rmarkdown")

# build table
pander(target_results, caption = "\\label{table-1}CRQA results for original tweets and target-only data.\n. p < .10, * p < .05, ** p < .001")

## source-target table ##

# bind metrics and significance results
source_target_results <- as.data.frame(cbind(metrics,
                                             metrics_all_source_targ,
                                             sig_all_source_targ,
                                             metrics_pos_source_targ,
                                             sig_pos_source_targ,
                                             metrics_neg_source_targ,
                                             sig_neg_source_targ), stringsAsFactors = FALSE)

# covert to numerics for rounding
numerics <- c(2:7)
source_target_results[,numerics] <- sapply(source_target_results[,numerics],as.numeric)

# add significance stars
stars_all <- as.data.frame(stars.pval(source_target_results[,3]))
stars_pos <- as.data.frame(stars.pval(source_target_results[,5]))
stars_neg <- as.data.frame(stars.pval(source_target_results[,7]))

source_target_results <- cbind(source_target_results, stars_all, stars_pos, stars_neg)
source_target_results <- source_target_results[,c(1,2,3,8,4,5,9,6,7,10)]

# give intuitive names
colnames(source_target_results) <- c("metric",
                                     "all events", "p", "sig.",
                                     "positive events", "p", "sig.",
                                     "negative events", "p", "sig.")

# exclude row names
rownames(source_target_results) <- NULL

# set to not split table and round to two decimal places
panderOptions("table.split.table", Inf)
panderOptions('round', 3)
panderOptions('keep.trailing.zeros', TRUE)

# style for Rmd
panderOptions("table.style", "rmarkdown")

# build table
pander(source_target_results, caption = "\\label{table-1}CRQA results for original tweets and target-only data.\n. p < .10, * p < .05, ** p < .001")

#### 8. Run Windowed CRQA for target-only data ####

# read in data
shuffled_windowed <- read.csv("./data/formatted/primary/shuffled_data_windowed.csv")

# split shuffled data into their own dataframes
shuffled_all_source_target <- shuffled_windowed[,c(1001:2000)]
shuffled_pos_source_target <- shuffled_windowed[,c(2001:3000)]
shuffled_neg_source_target <- shuffled_windowed[,c(3001:4000)]
shuffled_all_target <- shuffled_windowed[,c(4001:5000)]
shuffled_pos_target <- shuffled_windowed[,c(5001:6000)]
shuffled_neg_target <- shuffled_windowed[,c(6001:7000)]

### retweets ###

### source and target filtered data ###

## count of all events ##

# calculate windowed CRQA
windowed_all_source_target = wincrqa(ts1 = daily_counts$deciles_retweets, 
                                     ts2 = formatted_data$all_deciles_source_target, 
                                     windowstep = 1, 
                                     windowsize = 14,
                                     radius = .001,  
                                     delay = 1,       
                                     embed = 1, 
                                     rescale = 0, 
                                     normalize = 0,  
                                     mindiagline = 2, 
                                     minvertline = 2, 
                                     tw = 0,
                                     whiteline = FALSE,
                                     trend = TRUE) 

# transform into a dataframe for easier plotting 
# wincrqa_all_df_source_target <- as.data.frame(windowed_all_source_target$crqwin)
# colnames(wincrqa_all_df_source_target) <- c("RR", "DET", 
#                                             "NRLINE", "maxL", "L", 
#                                             "ENTR", "rENTR", 
#                                             "LAM", "TT", 
#                                             "window")

## count of positive events ##

# calculate windowed CRQA
windowed_pos_source_target = wincrqa(ts1 = daily_counts$deciles_retweets, 
                                     ts2 = formatted_data$pos_deciles_source_target, 
                                     windowstep = 1, 
                                     windowsize = 14,
                                     radius = .001,  
                                     delay = 1,        
                                     embed = 1, 
                                     rescale = 0, 
                                     normalize = 0, 
                                     mindiagline = 2,  
                                     minvertline = 2,  
                                     tw = 0,
                                     whiteline = FALSE,
                                     trend = TRUE)

# transform into a dataframe for easier plotting 
# wincrqa_pos_df_source_target <- as.data.frame(windowed_pos_source_target$crqwin)
# colnames(wincrqa_pos_df_source_target) <- c("RR", "DET", 
#                                             "NRLINE", "maxL", "L", 
#                                             "ENTR", "rENTR", 
#                                             "LAM", "TT", 
#                                             "window")

## count of negative events ##

# calculate windowed CRQA
windowed_neg_source_target = wincrqa(ts1 = daily_counts$deciles_retweets, 
                                     ts2 = formatted_data$neg_deciles_source_target, 
                                     windowstep = 1, 
                                     windowsize = 14,
                                     radius = .001, 
                                     delay = 1, 
                                     embed = 1, 
                                     rescale = 0, 
                                     normalize = 0, 
                                     mindiagline = 2, 
                                     minvertline = 2, 
                                     tw = 0,
                                     whiteline = FALSE,
                                     trend = TRUE) 

# transform into a dataframe for easier plotting 
# wincrqa_neg_df_source_target <- as.data.frame(windowed_neg_source_target$crqwin)
# colnames(wincrqa_neg_df_source_target) <- c("RR", "DET", 
#                                             "NRLINE", "maxL", "L", 
#                                             "ENTR", "rENTR", 
#                                             "LAM", "TT", 
#                                             "window")

### target filtered data ###

## count of all events ##

# calculate windowed CRQA
windowed_all_target = wincrqa(ts1 = daily_counts$deciles_retweets, 
                              ts2 = formatted_data$all_deciles_target, 
                              windowstep = 1, 
                              windowsize = 14,
                              radius = .001,  
                              delay = 1,       
                              embed = 1, 
                              rescale = 0, 
                              normalize = 0,  
                              mindiagline = 2, 
                              minvertline = 2, 
                              tw = 0,
                              whiteline = FALSE,
                              trend = TRUE) 

# transform into a dataframe for easier plotting 
# wincrqa_all_df_target <- as.data.frame(windowed_all_target$crqwin)
# colnames(wincrqa_all_df_target) <- c("RR", "DET", 
#                                      "NRLINE", "maxL", "L", 
#                                      "ENTR", "rENTR", 
#                                      "LAM", "TT", 
#                                      "window")

## count of positive events ##

# calculate windowed CRQA
windowed_pos_target = wincrqa(ts1 = daily_counts$deciles_retweets, 
                              ts2 = formatted_data$pos_deciles_target, 
                              windowstep = 1, 
                              windowsize = 14,
                              radius = .001,  
                              delay = 1,        
                              embed = 1, 
                              rescale = 0, 
                              normalize = 0, 
                              mindiagline = 2,  
                              minvertline = 2,  
                              tw = 0,
                              whiteline = FALSE,
                              trend = TRUE)

# transform into a dataframe for easier plotting 
# wincrqa_pos_df_target <- as.data.frame(windowed_pos_target$crqwin)
# colnames(wincrqa_pos_df_target) <- c("RR", "DET", 
#                                      "NRLINE", "maxL", "L", 
#                                      "ENTR", "rENTR", 
#                                      "LAM", "TT", 
#                                      "window")

## count of negative events ##

# calculate windowed CRQA
windowed_neg_target = wincrqa(ts1 = daily_counts$deciles_retweets, 
                              ts2 = formatted_data$neg_deciles_target, 
                              windowstep = 1, 
                              windowsize = 14,
                              radius = .001, 
                              delay = 1, 
                              embed = 1, 
                              rescale = 0, 
                              normalize = 0, 
                              mindiagline = 2, 
                              minvertline = 2, 
                              tw = 0,
                              whiteline = FALSE,
                              trend = TRUE) 

# transform into a dataframe for easier plotting 
# wincrqa_neg_df_target <- as.data.frame(windowed_neg_target$crqwin)
# colnames(wincrqa_neg_df_target) <- c("RR", "DET", 
#                                      "NRLINE", "maxL", "L", 
#                                      "ENTR", "rENTR", 
#                                      "LAM", "TT", 
#                                      "window")

#### Conduct permutation tests ####

### source and target filtered data ###

## count of all events ##

# initialize data frame for saving metrics
rqa_shuffled_all_source_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_retweets_shuffled[,c(i)],
                                   ts2=shuffled_all_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_all_source_target <- rbind(rqa_shuffled_all_source_target, 
                                          rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_all_source_target$DET[is.na(rqa_shuffled_all_source_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_all_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_all_source_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_all_source_target <- cbind(significance_all_source_target, temp)
}

# rename variables
names(significance_all_source_target) <- c("RR", "DET", 
                                           "NRLINE", "maxL", "L", 
                                           "ENTR", "rENTR", 
                                           "LAM", "TT")

## count of positive events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_pos_source_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_retweets_shuffled[,c(i)],
                                   ts2=shuffled_pos_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001,
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_pos_source_target <- rbind(rqa_shuffled_pos_source_target, 
                                          rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_pos_source_target$DET[is.na(rqa_shuffled_pos_source_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_pos_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_pos_source_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_pos_source_target <- cbind(significance_pos_source_target, temp)
}

# rename variables
names(significance_pos_source_target) <- c("RR", "DET", 
                                           "NRLINE", "maxL", "L", 
                                           "ENTR", "rENTR", 
                                           "LAM", "TT")

## count of negative events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_neg_source_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_retweets_shuffled[,c(i)],
                                   ts2=shuffled_neg_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2, 
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_neg_source_target <- rbind(rqa_shuffled_neg_source_target, 
                                          rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_neg_source_target$DET[is.na(rqa_shuffled_neg_source_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_neg_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_neg_source_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_neg_source_target <- cbind(significance_neg_source_target, temp)
}

# rename variables
names(significance_neg_source_target) <- c("RR", "DET", 
                                           "NRLINE", "maxL", "L", 
                                           "ENTR", "rENTR", 
                                           "LAM", "TT")

### target filtered data ###

## count of all events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_all_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_retweets_shuffled[,c(i)],
                                   ts2=shuffled_all_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_all_target <- rbind(rqa_shuffled_all_target, rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_all_target$DET[is.na(rqa_shuffled_all_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_all_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_all_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_all_target <- cbind(significance_all_target, temp)
}

# rename variables
names(significance_all_target) <- c("RR", "DET", 
                                    "NRLINE", "maxL", "L", 
                                    "ENTR", "rENTR", 
                                    "LAM", "TT")

## count of positive events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_pos_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_retweets_shuffled[,c(i)],
                                   ts2=shuffled_pos_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001,
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_pos_target <- rbind(rqa_shuffled_pos_target, rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_pos_target$DET[is.na(rqa_shuffled_pos_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_pos_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_pos_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_pos_target <- cbind(significance_pos_target, temp)
}

# rename variables
names(significance_pos_target) <- c("RR", "DET", 
                                    "NRLINE", "maxL", "L", 
                                    "ENTR", "rENTR", 
                                    "LAM", "TT")

## count of negative events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_neg_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_retweets_shuffled[,c(i)],
                                   ts2=shuffled_neg_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2, 
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_neg_target <- rbind(rqa_shuffled_neg_target, rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_neg_target$DET[is.na(rqa_shuffled_neg_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_neg_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_neg_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_neg_target <- cbind(significance_neg_target, temp)
}

# rename variables
names(significance_neg_target) <- c("RR", "DET", 
                                    "NRLINE", "maxL", "L", 
                                    "ENTR", "rENTR", 
                                    "LAM", "TT")

#### 4. Plot the results across windows ####

# set universal plotting parameters
plot_rr_ymin = 0
plot_rr_ymax = 28
plot_det_ymin = 0
plot_det_ymax = 77

### source and target filtered data ###

## count of all events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_all_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct and save the plot
plot_source_target_windowed_all_RR = ggplot(data = windowed_all_source_target,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_all_source_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_all_source_target[2,1], color = "red") +
  geom_hline(yintercept = significance_all_source_target[3,1], color = "red") +
  geom_hline(yintercept = significance_all_source_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: All event count') +
  ylab("RR") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_all_source_target$DET[is.na(windowed_all_source_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_all_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_all_DET = ggplot(data = windowed_all_source_target,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_all_source_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_all_source_target[2,2], color = "red") +
  geom_hline(yintercept = significance_all_source_target[3,2], color = "red") +
  geom_hline(yintercept = significance_all_source_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: All event count') +
  ylab("DET") +
  xlab("Window")

## count of positive events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_pos_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_pos_RR = ggplot(data = windowed_pos_source_target,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_source_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_pos_source_target[2,1], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[3,1], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: Positive event count') +
  ylab("") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_pos_source_target$DET[is.na(windowed_pos_source_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_pos_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_pos_DET = ggplot(data = windowed_pos_source_target,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_source_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_pos_source_target[2,2], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[3,2], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: Positive event count') +
  ylab("") +
  xlab("Window")

## count of negative events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_neg_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_neg_RR = ggplot(data = windowed_neg_source_target,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_neg_source_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_neg_source_target[2,1], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[3,1], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: Negative event count') +
  ylab("") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_neg_source_target$DET[is.na(windowed_neg_source_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_neg_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_neg_DET = ggplot(data = windowed_neg_source_target,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_neg_source_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_neg_source_target[2,2], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[3,2], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: Negative event count') +
  ylab("") +
  xlab("Window")

### join source-target plots ###

plot_source_target_windowed_all = gridExtra::grid.arrange(
  top = textGrob(paste("Windowed cross-recurrence",
                       "quantification analysis",
                       "\nDaily retweets and event count data",
                       "with source and target filtering",
                       sep = " "),
                 gp=gpar(fontsize=15)), 
  plot_source_target_windowed_all_RR,
  plot_source_target_windowed_pos_RR,
  plot_source_target_windowed_neg_RR,
  plot_source_target_windowed_all_DET,
  plot_source_target_windowed_pos_DET,
  plot_source_target_windowed_neg_DET,
  ncol = 3
)

# save them
ggsave(filename = "./results/primary/appendix/windowed-crqa/source_target-retweets-windowed_all.png",
       plot = plot_source_target_windowed_all,
       dpi = 300,
       height = 4,
       width = 9)

### target filtered data ###

## count of all events ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_all_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct and save the plot
plot_target_windowed_all_RR = ggplot(data = windowed_all_target,
                                     aes(y = RR,
                                         x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_all_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_all_target[2,1], color = "red") +
  geom_hline(yintercept = significance_all_target[3,1], color = "red") +
  geom_hline(yintercept = significance_all_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: All event count') +
  ylab("RR") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_all_target$DET[is.na(windowed_all_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_all_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_all_DET = ggplot(data = windowed_all_target,
                                      aes(y = DET,
                                          x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_all_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_all_target[2,2], color = "red") +
  geom_hline(yintercept = significance_all_target[3,2], color = "red") +
  geom_hline(yintercept = significance_all_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: All event count') +
  ylab("DET") +
  xlab("Window")

## count of positive events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_pos_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_pos_RR = ggplot(data = windowed_pos_target,
                                     aes(y = RR,
                                         x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_pos_target[2,1], color = "red") +
  geom_hline(yintercept = significance_pos_target[3,1], color = "red") +
  geom_hline(yintercept = significance_pos_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: Positive event count') +
  ylab("") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_pos_target$DET[is.na(windowed_pos_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_pos_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_pos_DET = ggplot(data = windowed_pos_target,
                                      aes(y = DET,
                                          x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_pos_target[2,2], color = "red") +
  geom_hline(yintercept = significance_pos_target[3,2], color = "red") +
  geom_hline(yintercept = significance_pos_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: Positive event count') +
  ylab("") +
  xlab("Window")

## count of negative events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_neg_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_neg_RR = ggplot(data = windowed_neg_target,
                                     aes(y = RR,
                                         x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_neg_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_neg_target[2,1], color = "red") +
  geom_hline(yintercept = significance_neg_target[3,1], color = "red") +
  geom_hline(yintercept = significance_neg_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: Negative event count') +
  ylab("") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_neg_target$DET[is.na(windowed_neg_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_neg_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_neg_DET = ggplot(data = windowed_neg_target,
                                      aes(y = DET,
                                          x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_neg_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_neg_target[2,2], color = "red") +
  geom_hline(yintercept = significance_neg_target[3,2], color = "red") +
  geom_hline(yintercept = significance_neg_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: Negative event count') +
  ylab("") +
  xlab("Window")

### join source-target plots ###

plot_target_windowed_all = gridExtra::grid.arrange(
  top = textGrob(paste("Windowed cross-recurrence",
                       "quantification analysis",
                       "\nDaily retweets and event count data",
                       "with target-only filtering",
                       sep = " "),
                 gp=gpar(fontsize=15)), 
  plot_target_windowed_all_RR,
  plot_target_windowed_pos_RR,
  plot_target_windowed_neg_RR,
  plot_target_windowed_all_DET,
  plot_target_windowed_pos_DET,
  plot_target_windowed_neg_DET,
  ncol = 3
)

# save them
ggsave(filename = "./results/primary/appendix/windowed-crqa/target-retweets-windowed_all.png",
       plot = plot_target_windowed_all,
       dpi = 300,
       height = 4,
       width = 9)

### originals ###

### source and target filtered data ###

## count of all events ##

# calculate windowed CRQA
windowed_all_source_target = wincrqa(ts1 = daily_counts$deciles_orig, 
                                     ts2 = formatted_data$all_deciles_source_target, 
                                     windowstep = 1, 
                                     windowsize = 14,
                                     radius = .001,  
                                     delay = 1,       
                                     embed = 1, 
                                     rescale = 0, 
                                     normalize = 0,  
                                     mindiagline = 2, 
                                     minvertline = 2, 
                                     tw = 0,
                                     whiteline = FALSE,
                                     trend = TRUE) 

# transform into a dataframe for easier plotting 
# wincrqa_all_df_source_target <- as.data.frame(windowed_all_source_target$crqwin)
# colnames(wincrqa_all_df_source_target) <- c("RR", "DET", 
#                                             "NRLINE", "maxL", "L", 
#                                             "ENTR", "rENTR", 
#                                             "LAM", "TT", 
#                                             "window")

## count of positive events ##

# calculate windowed CRQA
windowed_pos_source_target = wincrqa(ts1 = daily_counts$deciles_orig, 
                                     ts2 = formatted_data$pos_deciles_source_target, 
                                     windowstep = 1, 
                                     windowsize = 14,
                                     radius = .001,  
                                     delay = 1,        
                                     embed = 1, 
                                     rescale = 0, 
                                     normalize = 0, 
                                     mindiagline = 2,  
                                     minvertline = 2,  
                                     tw = 0,
                                     whiteline = FALSE,
                                     trend = TRUE)

# transform into a dataframe for easier plotting 
# wincrqa_pos_df_source_target <- as.data.frame(windowed_pos_source_target$crqwin)
# colnames(wincrqa_pos_df_source_target) <- c("RR", "DET", 
#                                             "NRLINE", "maxL", "L", 
#                                             "ENTR", "rENTR", 
#                                             "LAM", "TT", 
#                                             "window")

## count of negative events ##

# calculate windowed CRQA
windowed_neg_source_target = wincrqa(ts1 = daily_counts$deciles_orig, 
                                     ts2 = formatted_data$neg_deciles_source_target, 
                                     windowstep = 1, 
                                     windowsize = 14,
                                     radius = .001, 
                                     delay = 1, 
                                     embed = 1, 
                                     rescale = 0, 
                                     normalize = 0, 
                                     mindiagline = 2, 
                                     minvertline = 2, 
                                     tw = 0,
                                     whiteline = FALSE,
                                     trend = TRUE) 

# transform into a dataframe for easier plotting 
# wincrqa_neg_df_source_target <- as.data.frame(windowed_neg_source_target$crqwin)
# colnames(wincrqa_neg_df_source_target) <- c("RR", "DET", 
#                                             "NRLINE", "maxL", "L", 
#                                             "ENTR", "rENTR", 
#                                             "LAM", "TT", 
#                                             "window")

### target filtered data ###

## count of all events ##

# calculate windowed CRQA
windowed_all_target = wincrqa(ts1 = daily_counts$deciles_orig, 
                              ts2 = formatted_data$all_deciles_target, 
                              windowstep = 1, 
                              windowsize = 14,
                              radius = .001,  
                              delay = 1,       
                              embed = 1, 
                              rescale = 0, 
                              normalize = 0,  
                              mindiagline = 2, 
                              minvertline = 2, 
                              tw = 0,
                              whiteline = FALSE,
                              trend = TRUE) 

# transform into a dataframe for easier plotting 
# wincrqa_all_df_target <- as.data.frame(windowed_all_target$crqwin)
# colnames(wincrqa_all_df_target) <- c("RR", "DET", 
#                                      "NRLINE", "maxL", "L", 
#                                      "ENTR", "rENTR", 
#                                      "LAM", "TT", 
#                                      "window")

## count of positive events ##

# calculate windowed CRQA
windowed_pos_target = wincrqa(ts1 = daily_counts$deciles_orig, 
                              ts2 = formatted_data$pos_deciles_target, 
                              windowstep = 1, 
                              windowsize = 14,
                              radius = .001,  
                              delay = 1,        
                              embed = 1, 
                              rescale = 0, 
                              normalize = 0, 
                              mindiagline = 2,  
                              minvertline = 2,  
                              tw = 0,
                              whiteline = FALSE,
                              trend = TRUE)

# transform into a dataframe for easier plotting 
# wincrqa_pos_df_target <- as.data.frame(windowed_pos_target$crqwin)
# colnames(wincrqa_pos_df_target) <- c("RR", "DET", 
#                                      "NRLINE", "maxL", "L", 
#                                      "ENTR", "rENTR", 
#                                      "LAM", "TT", 
#                                      "window")

## count of negative events ##

# calculate windowed CRQA
windowed_neg_target = wincrqa(ts1 = daily_counts$deciles_orig, 
                              ts2 = formatted_data$neg_deciles_target, 
                              windowstep = 1, 
                              windowsize = 14,
                              radius = .001, 
                              delay = 1, 
                              embed = 1, 
                              rescale = 0, 
                              normalize = 0, 
                              mindiagline = 2, 
                              minvertline = 2, 
                              tw = 0,
                              whiteline = FALSE,
                              trend = TRUE) 

# transform into a dataframe for easier plotting 
# wincrqa_neg_df_target <- as.data.frame(windowed_neg_target$crqwin)
# colnames(wincrqa_neg_df_target) <- c("RR", "DET", 
#                                      "NRLINE", "maxL", "L", 
#                                      "ENTR", "rENTR", 
#                                      "LAM", "TT", 
#                                      "window")

#### Conduct permutation tests ####

### source and target filtered data ###

## count of all events ##

# initialize data frame for saving metrics
rqa_shuffled_all_source_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_orig_shuffled[,c(i)],
                                   ts2=shuffled_all_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_all_source_target <- rbind(rqa_shuffled_all_source_target, 
                                          rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_all_source_target$DET[is.na(rqa_shuffled_all_source_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_all_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_all_source_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_all_source_target <- cbind(significance_all_source_target, temp)
}

# rename variables
names(significance_all_source_target) <- c("RR", "DET", 
                                           "NRLINE", "maxL", "L", 
                                           "ENTR", "rENTR", 
                                           "LAM", "TT")

## count of positive events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_pos_source_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_orig_shuffled[,c(i)],
                                   ts2=shuffled_pos_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001,
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_pos_source_target <- rbind(rqa_shuffled_pos_source_target, 
                                          rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_pos_source_target$DET[is.na(rqa_shuffled_pos_source_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_pos_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_pos_source_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_pos_source_target <- cbind(significance_pos_source_target, temp)
}

# rename variables
names(significance_pos_source_target) <- c("RR", "DET", 
                                           "NRLINE", "maxL", "L", 
                                           "ENTR", "rENTR", 
                                           "LAM", "TT")

## count of negative events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_neg_source_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_orig_shuffled[,c(i)],
                                   ts2=shuffled_neg_source_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2, 
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_neg_source_target <- rbind(rqa_shuffled_neg_source_target, 
                                          rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_neg_source_target$DET[is.na(rqa_shuffled_neg_source_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_neg_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_neg_source_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_neg_source_target <- cbind(significance_neg_source_target, temp)
}

# rename variables
names(significance_neg_source_target) <- c("RR", "DET", 
                                           "NRLINE", "maxL", "L", 
                                           "ENTR", "rENTR", 
                                           "LAM", "TT")

### target filtered data ###

## count of all events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_all_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_orig_shuffled[,c(i)],
                                   ts2=shuffled_all_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_all_target <- rbind(rqa_shuffled_all_target, rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_all_target$DET[is.na(rqa_shuffled_all_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_all_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_all_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_all_target <- cbind(significance_all_target, temp)
}

# rename variables
names(significance_all_target) <- c("RR", "DET", 
                                    "NRLINE", "maxL", "L", 
                                    "ENTR", "rENTR", 
                                    "LAM", "TT")

## count of positive events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_pos_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_orig_shuffled[,c(i)],
                                   ts2=shuffled_pos_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001,
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2,
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_pos_target <- rbind(rqa_shuffled_pos_target, rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_pos_target$DET[is.na(rqa_shuffled_pos_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_pos_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_pos_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_pos_target <- cbind(significance_pos_target, temp)
}

# rename variables
names(significance_pos_target) <- c("RR", "DET", 
                                    "NRLINE", "maxL", "L", 
                                    "ENTR", "rENTR", 
                                    "LAM", "TT")

## count of negative events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_neg_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=win_orig_shuffled[,c(i)],
                                   ts2=shuffled_neg_target[,c(i)],
                                   delay=0,
                                   embed=1,
                                   rescale=0,
                                   radius=.001, 
                                   normalize=0,
                                   mindiagline=2,
                                   minvertline=2, 
                                   tw=0) 
  
  # save the crqa results
  rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
  
  # bind to dataframe
  rqa_shuffled_neg_target <- rbind(rqa_shuffled_neg_target, rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_neg_target$DET[is.na(rqa_shuffled_neg_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_neg_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_neg_target[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_neg_target <- cbind(significance_neg_target, temp)
}

# rename variables
names(significance_neg_target) <- c("RR", "DET", 
                                    "NRLINE", "maxL", "L", 
                                    "ENTR", "rENTR", 
                                    "LAM", "TT")

#### Plot the results across windows ####

# set universal plotting parameters
plot_rr_ymin = 0
plot_rr_ymax = 28
plot_det_ymin = 0
plot_det_ymax = 77

### source and target filtered data ###

## count of all events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_all_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct and save the plot
plot_source_target_windowed_all_RR = ggplot(data = windowed_all_source_target,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_all_source_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_all_source_target[2,1], color = "red") +
  geom_hline(yintercept = significance_all_source_target[3,1], color = "red") +
  geom_hline(yintercept = significance_all_source_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: All event count') +
  ylab("RR") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_all_source_target$DET[is.na(windowed_all_source_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_all_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_all_DET = ggplot(data = windowed_all_source_target,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_all_source_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_all_source_target[2,2], color = "red") +
  geom_hline(yintercept = significance_all_source_target[3,2], color = "red") +
  geom_hline(yintercept = significance_all_source_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: All event count') +
  ylab("DET") +
  xlab("Window")

## count of positive events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_pos_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_pos_RR = ggplot(data = windowed_pos_source_target,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_source_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_pos_source_target[2,1], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[3,1], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: Positive event count') +
  ylab("") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_pos_source_target$DET[is.na(windowed_pos_source_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_pos_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_pos_DET = ggplot(data = windowed_pos_source_target,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_source_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_pos_source_target[2,2], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[3,2], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: Positive event count') +
  ylab("") +
  xlab("Window")

## count of negative events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_neg_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_neg_RR = ggplot(data = windowed_neg_source_target,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_neg_source_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_neg_source_target[2,1], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[3,1], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: Negative event count') +
  ylab("") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_neg_source_target$DET[is.na(windowed_neg_source_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_neg_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_neg_DET = ggplot(data = windowed_neg_source_target,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_neg_source_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_neg_source_target[2,2], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[3,2], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: Negative event count') +
  ylab("") +
  xlab("Window")

### join source-target plots ###

plot_source_target_windowed_all = gridExtra::grid.arrange(
  top = textGrob(paste("Windowed cross-recurrence",
                       "quantification analysis",
                       "\nDaily original tweets and event count data",
                       "with source and target filtering",
                       sep = " "),
                 gp=gpar(fontsize=15)), 
  plot_source_target_windowed_all_RR,
  plot_source_target_windowed_pos_RR,
  plot_source_target_windowed_neg_RR,
  plot_source_target_windowed_all_DET,
  plot_source_target_windowed_pos_DET,
  plot_source_target_windowed_neg_DET,
  ncol = 3
)

# save them
ggsave(filename = "./results/primary/appendix/windowed-crqa/source_target-originals-windowed_all.png",
       plot = plot_source_target_windowed_all,
       dpi = 300,
       height = 4,
       width = 9)

### target filtered data ###

# construct and save the plot
plot_source_target_windowed_all_RR = ggplot(data = windowed_all_target,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_all_source_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_all_source_target[2,1], color = "red") +
  geom_hline(yintercept = significance_all_source_target[3,1], color = "red") +
  geom_hline(yintercept = significance_all_source_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: All event count') +
  ylab("RR") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_all_target$DET[is.na(windowed_all_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_all_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_all_DET = ggplot(data = windowed_all_target,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_all_source_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_all_source_target[2,2], color = "red") +
  geom_hline(yintercept = significance_all_source_target[3,2], color = "red") +
  geom_hline(yintercept = significance_all_source_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: All event count') +
  ylab("DET") +
  xlab("Window")

## count of positive events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_pos_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_pos_RR = ggplot(data = windowed_pos_target,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_source_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_pos_source_target[2,1], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[3,1], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: Positive event count') +
  ylab("") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_pos_target$DET[is.na(windowed_pos_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_pos_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_pos_DET = ggplot(data = windowed_pos_target,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_source_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_pos_source_target[2,2], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[3,2], color = "red") +
  geom_hline(yintercept = significance_pos_source_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: Positive event count') +
  ylab("") +
  xlab("Window")

## count of negative events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_neg_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_neg_RR = ggplot(data = windowed_neg_target,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_neg_source_target[1,1], color = "orange") +
  geom_hline(yintercept = significance_neg_source_target[2,1], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[3,1], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[4,1], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +

  # make uniform boundaries
  coord_cartesian(ylim = c(plot_rr_ymin, plot_rr_ymax)) +
  
  # add labels
  ggtitle('RR: Negative event count') +
  ylab("") +
  xlab("")

# DET plot #

# fill in NA determinism values with 0
windowed_neg_target$DET[is.na(windowed_neg_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_neg_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_neg_DET = ggplot(data = windowed_neg_target,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_neg_source_target[1,2], color = "orange") +
  geom_hline(yintercept = significance_neg_source_target[2,2], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[3,2], color = "red") +
  geom_hline(yintercept = significance_neg_source_target[4,2], color = "orange") +
  
  # add abline
  geom_abline(intercept = fit_intercept,
              slope = fit_slope,
              color="blue") +
  
  # make uniform boundaries
  coord_cartesian(ylim = c(plot_det_ymin, plot_det_ymax)) +
  
  # add labels
  ggtitle('DET: Negative event count') +
  ylab("") +
  xlab("Window")

### join source-target plots ###

plot_source_target_windowed_all = gridExtra::grid.arrange(
  top = textGrob(paste("Windowed cross-recurrence",
                       "quantification analysis",
                       "\nDaily original tweets and event count data",
                       "with source and target filtering",
                       sep = " "),
                 gp=gpar(fontsize=15)), 
  plot_source_target_windowed_all_RR,
  plot_source_target_windowed_pos_RR,
  plot_source_target_windowed_neg_RR,
  plot_source_target_windowed_all_DET,
  plot_source_target_windowed_pos_DET,
  plot_source_target_windowed_neg_DET,
  ncol = 3
)

# save them
ggsave(filename = "./results/primary/appendix/windowed-crqa/target-originals-windowed_all.png",
       plot = plot_source_target_windowed_all,
       dpi = 300,
       height = 4,
       width = 9)
