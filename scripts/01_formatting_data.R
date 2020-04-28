##### Formatting Arab Spring Data #####
# Code by: M. Chiovaro
# University of Connecticut

# clear environment
rm(list=ls())

# set working directory
setwd("./arab-spring/")

# load the required packages
library(dplyr)
library(readxl)

# read in Syria data 
cohesion <- read_excel("./data/raw/SyriaDataByDay-1.xlsx")  
event_data <- read.csv("./data/raw/ICEWS.Syria.3_2012-7_2012.csv")

#=============================
# Prep the event time series #
#=============================

# create new cleaned dataframe
processed_event <- event_data %>% 
  
  # group by date
  group_by(Event.Date) %>%
  
  # count total number of events
  mutate(count = n()) %>%
  
  # count positive events
  mutate(pos_event = sum(Intensity > 0)) %>%
  
  # count negative events
  mutate(neg_event = sum(Intensity < 0)) %>%
  
  # keep one unique row per day
  distinct(Event.Date, count, pos_event, neg_event) %>%
  
  rename(Date = Event.Date) %>%
  
  # ungroup data
  ungroup()
  
##### Quartiles #####

### count of events ###

# create the quartiles
processed_event$count_quart <- with(processed_event, cut(count, 
                                breaks=quantile(count, probs=seq(0, 1, by=0.25), na.rm=TRUE), 
                                include.lowest=TRUE))
# check that we have 4 levels
unique(processed_event$count_quart)

# re-label with intuitive quartile levels
levels(processed_event$count_quart) <- factor(c("1","2","3","4"))

### positive events ###

# create the quartiles
processed_event$pos_quart <- with(processed_event, cut(pos_event, 
                                  breaks=quantile(pos_event, probs=seq(0, 1, by=0.25), na.rm=TRUE), 
                                  include.lowest=TRUE))
# check that we have 4 levels
unique(processed_event$pos_quart)

# re-label with intuitive quartile levels
levels(processed_event$pos_quart) <- factor(c("1","2","3","4"))

### negative events ###

# create the quartiles
processed_event$neg_quart <- with(processed_event, cut(neg_event, 
                                    breaks=quantile(neg_event, probs=seq(0, 1, by=0.25), na.rm=TRUE), 
                                    include.lowest=TRUE))
# check that we have 4 levels
unique(processed_event$neg_quart)

# re-label with intuitive quartile levels
levels(processed_event$neg_quart) <- factor(c("1","2","3","4"))

##### Deciles #####

### count of events ###

# create the deciles
processed_event$count_dec <- with(processed_event, cut(count, 
                                  breaks=quantile(count, probs=seq(0, 1, by=0.1), na.rm=TRUE), 
                                  include.lowest=TRUE))
# check that we have 10 levels
unique(processed_event$count_dec)

# re-label with intuitive decile levels
levels(processed_event$count_dec) <- factor(c("1","2","3","4", "5", "6", "7", "8", "9", "10"))

### positive events ###

# create the deciles
processed_event$pos_dec <- with(processed_event, cut(pos_event, 
                                breaks=quantile(pos_event, probs=seq(0, 1, by=0.1), na.rm=TRUE), 
                                include.lowest=TRUE))
# check that we have 10 levels
unique(processed_event$pos_dec)

# re-label with intuitive decile levels
levels(processed_event$pos_dec) <- factor(c("1","2","3","4", "5", "6", "7", "8", "9", "10"))

### negative events ###

# create the deciles
processed_event$neg_dec <- with(processed_event, cut(neg_event, 
                                breaks=quantile(neg_event, probs=seq(0, 1, by=0.1), na.rm=TRUE), 
                                include.lowest=TRUE))
# check that we have 10 levels
unique(processed_event$neg_dec)

# re-label with intuitive decile levels
levels(processed_event$neg_dec) <- factor(c("1","2","3","4", "5", "6", "7", "8", "9", "10"))

#=======================================
# Prep the social cohesion time series #
#=======================================

##### Quartiles #####

# create the quartiles
cohesion$coh_quart <- with(cohesion, cut(MeanCohesion, 
                                  breaks=quantile(MeanCohesion, probs=seq(0, 1, by=0.25), na.rm=TRUE), 
                                  include.lowest=TRUE))
# check that we have 4 levels
unique(cohesion$coh_quart)

# re-label with intuitive quartile levels
levels(cohesion$coh_quart) <- factor(c("1","2","3","4"))

##### Deciles #####

# create the quartiles
cohesion$coh_dec <- with(cohesion, cut(MeanCohesion, 
                                         breaks=quantile(MeanCohesion, probs=seq(0, 1, by=0.1), na.rm=TRUE), 
                                         include.lowest=TRUE))
# check that we have 4 levels
unique(cohesion$coh_dec)

# re-label with intuitive decile levels
levels(cohesion$coh_dec) <- factor(c("1","2","3","4", "5", "6", "7", "8", "9", "10"))

#=================================================
# Merging dataframes and handling missing values #
#=================================================

# make date variable types to be both factors for merging
cohesion$Date <- as.factor(cohesion$Date)

# merge cohesion and event dataframes into single dataframe
all_data <- merge(cohesion, processed_event, by="Date")

# keep only columns we need
simple_data <- all_data[,c("Date", "coh_quart", "count_quart", "pos_quart", "neg_quart", "coh_dec", "count_dec", "pos_dec", "neg_dec")]

# create new dataframe for missing days (2012-03-12:2012-03-29) with unique values
missing <- data.frame(c("2012-03-12", "2012-03-13", "2012-03-14", "2012-03-15", "2012-03-16", "2012-03-17", "2012-03-18", "2012-03-19", "2012-03-20", "2012-03-21", "2012-03-22", "2012-03-23", "2012-03-24", "2012-03-25", "2012-03-26", "2012-03-27", "2012-03-28", "2012-03-29"), #Date
                      c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5), #coh_quart
                      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #count_quart
                      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #pos_quart
                      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #neg_quart
                      c(11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11), #coh_dec
                      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #count_dec
                      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #pos_dec
                      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) #neg_dec

# rename variables in missing dataframe
names(missing) <- c("Date", "coh_quart", "count_quart", "pos_quart", "neg_quart", "coh_dec", "count_dec", "pos_dec", "neg_dec")

# make all variables factors for binding
missing$Date <- as.factor(missing$Date) 
missing$coh_quart <- as.factor(missing$coh_quart)
missing$count_quart <- as.factor(missing$count_quart)
missing$pos_quart <- as.factor(missing$pos_quart)
missing$neg_quart <- as.factor(missing$neg_quart)
missing$coh_dec <- as.factor(missing$coh_dec)
missing$count_dec <- as.factor(missing$count_dec)
missing$pos_dec <- as.factor(missing$pos_dec)
missing$neg_dec <- as.factor(missing$neg_dec)

# merge missing and simple dataframes
final_data <- rbind(simple_data, missing)

# change from factor to date variable for sorting
final_data$Date <- as.Date(final_data$Date)

# sort data acohending by date
final_data <- final_data[order(final_data$Date),]

#===============================================================
# Create randomized time series for permutation testing
# crqa
#===============================================================

data_trimmed <- final_data[28:nrow(final_data),]

# set seed for reproducibility
set.seed(123)

### Social cohesion ###

# create empty data frame
shuffled_coh = data.frame()

# generate 100 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$coh_dec, replace = FALSE)
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_coh <- rbind(shuffled_coh, sample)
}

# take the original time series and add it as a row
original_ts <- as.data.frame(data_trimmed$coh_dec)
original_ts <- t(original_ts)
original_ts <- as.data.frame(original_ts)
shuffled_coh <- rbind(shuffled_coh, original_ts)

# check to see if we have 1001 distinct time series
nrow(distinct(shuffled_coh))

# tranform rows to columns for running analyses
shuffled_coh <- t(shuffled_coh)
shuffled_coh <- as.data.frame(shuffled_coh)

# remove real time series from shuffled dataframe
shuffled_coh <- shuffled_coh[c(1:1000)]

### Count of events ###

# create empty data frame
shuffled_count = data.frame()

# generate 100 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$count_dec, replace = FALSE)
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_count <- rbind(shuffled_count, sample)
}

# take the original time series and add it as a row
original_ts_count <- as.data.frame(data_trimmed$count_dec)
original_ts_count <- t(original_ts_count)
original_ts_count <- as.data.frame(original_ts_count)
shuffled_count <- rbind(shuffled_count, original_ts_count)

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_count))

# tranform rows to columns for running analyses
shuffled_count <- t(shuffled_count)
shuffled_count <- as.data.frame(shuffled_count)

# remove real time series from shuffled dataframe
shuffled_count <- shuffled_count[c(1:1000)]

### Count of positive events ###

# create empty data frame
shuffled_pos = data.frame()

# generate 100 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$pos_dec, replace = FALSE)
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_pos <- rbind(shuffled_pos, sample)
}

# take the original time series and add it as a row
original_ts_pos <- as.data.frame(data_trimmed$pos_dec)
original_ts_pos <- t(original_ts_pos)
original_ts_pos <- as.data.frame(original_ts_pos)
shuffled_pos <- rbind(shuffled_pos, original_ts_pos)

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_pos))

# tranform rows to columns for running analyses
shuffled_pos <- t(shuffled_pos)
shuffled_pos <- as.data.frame(shuffled_pos)

# remove real time series from shuffled dataframe
shuffled_pos <- shuffled_pos[c(1:1000)]

### Count of negative events ###

# create empty data frame
shuffled_neg = data.frame()

# generate 100 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$neg_dec, replace = FALSE)
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_neg <- rbind(shuffled_neg, sample)
}

# take the original time series and add it as a row
original_ts_neg <- as.data.frame(data_trimmed$neg_dec)
original_ts_neg <- t(original_ts_neg)
original_ts_neg <- as.data.frame(original_ts_neg)
shuffled_neg <- rbind(shuffled_neg, original_ts_neg)

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_neg))

# tranform rows to columns for running analyses
shuffled_neg <- t(shuffled_neg)
shuffled_neg <- as.data.frame(shuffled_neg)

# remove real time series from shuffled dataframe
shuffled_neg <- shuffled_neg[c(1:1000)]

### Save to file ###
  
# write sequential data to file
write.table(x = final_data,
            file='./data/formatted/processed_data_full.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write truncated data to file
write.table(x = data_trimmed,
            file='./data/formatted/data_trimmed.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled cohesion data to file
write.table(x = shuffled_coh,
            file='./data/formatted/shuffled_coh.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled event count data to file
write.table(x = shuffled_count,
            file='./data/formatted/shuffled_count.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled postive event data to file
write.table(x = shuffled_pos,
            file='./data/formatted/shuffled_pos.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled negative event data to file
write.table(x = shuffled_neg,
            file='./data/formatted/shuffled_neg.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

#======================================================
# Create randomized time series for permutation testing  
# Windowed crqa - Windowsize = 7
#======================================================

# set seed for reproducibility
set.seed(123)

### Social cohesion ###

# create empty data frame
shuffled_coh = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$coh_dec, size = 7, replace = FALSE) # only sample 7 times
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_coh <- rbind(shuffled_coh, sample)
}

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_coh))

# tranform rows to columns for running analyses
shuffled_coh <- t(shuffled_coh)
shuffled_coh <- as.data.frame(shuffled_coh)

### Count of all events ###

# create empty data frame
shuffled_count = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$count_dec, size = 7, replace = FALSE) # only sample 7 times
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_count <- rbind(shuffled_count, sample)
}

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_count))

# tranform rows to columns for running analyses
shuffled_count <- t(shuffled_count)
shuffled_count <- as.data.frame(shuffled_count)

### Count of positive events ###

# create empty data frame
shuffled_pos = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$pos_dec, size = 7, replace = FALSE) # only sample 7 times
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_pos <- rbind(shuffled_pos, sample)
}

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_pos))

# tranform rows to columns for running analyses
shuffled_pos <- t(shuffled_pos)
shuffled_pos <- as.data.frame(shuffled_pos)

### Count of negative events ###

# create empty data frame
shuffled_neg = data.frame()

# generate 100 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$neg_dec, size = 7, replace = FALSE) # only sample 7 times
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_neg <- rbind(shuffled_neg, sample)
}

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_neg))

# tranform rows to columns for running analyses
shuffled_neg <- t(shuffled_neg)
shuffled_neg <- as.data.frame(shuffled_neg)

### Save to file ###

# write shuffled cohesion data to file
write.table(x = shuffled_coh,
            file='./data/formatted/shuffled_coh_windowed-7.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled event count data to file
write.table(x = shuffled_count,
            file='./data/formatted/shuffled_count_windowed-7.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled postive event data to file
write.table(x = shuffled_pos,
            file='./data/formatted/shuffled_pos_windowed-7.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled negative event data to file
write.table(x = shuffled_neg,
            file='./data/formatted/shuffled_neg_windowed-7.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)


#======================================================
# Create randomized time series for permutation testing  
# Windowed crqa - Windowsize = 14
#======================================================

# set seed for reproducibility
set.seed(123)

### Social cohesion ###

# create empty data frame
shuffled_coh = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$coh_dec, size = 14, replace = FALSE) # only sample 14 times
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_coh <- rbind(shuffled_coh, sample)
}

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_coh))

# tranform rows to columns for running analyses
shuffled_coh <- t(shuffled_coh)
shuffled_coh <- as.data.frame(shuffled_coh)

### Count of all events ###

# create empty data frame
shuffled_count = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$count_dec, size = 14, replace = FALSE) # only sample 14 times
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_count <- rbind(shuffled_count, sample)
}

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_count))

# tranform rows to columns for running analyses
shuffled_count <- t(shuffled_count)
shuffled_count <- as.data.frame(shuffled_count)

### Count of positive events ###

# create empty data frame
shuffled_pos = data.frame()

# generate 1000 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$pos_dec, size = 14, replace = FALSE) # only sample 14 times
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_pos <- rbind(shuffled_pos, sample)
}

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_pos))

# tranform rows to columns for running analyses
shuffled_pos <- t(shuffled_pos)
shuffled_pos <- as.data.frame(shuffled_pos)

### Count of negative events ###

# create empty data frame
shuffled_neg = data.frame()

# generate 100 random time series and bind to rows
for (i in 1:1000){
  shuffled_ts <- sample(data_trimmed$neg_dec, size = 14, replace = FALSE) # only sample 14 times
  sample <- as.data.frame(shuffled_ts)
  sample <- t(sample)
  shuffled_neg <- rbind(shuffled_neg, sample)
}

# check to see if we have 101 distinct time series
nrow(distinct(shuffled_neg))

# tranform rows to columns for running analyses
shuffled_neg <- t(shuffled_neg)
shuffled_neg <- as.data.frame(shuffled_neg)

### Save to file ###

# write shuffled cohesion data to file
write.table(x = shuffled_coh,
            file='./data/formatted/shuffled_coh_windowed-14.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled event count data to file
write.table(x = shuffled_count,
            file='./data/formatted/shuffled_count_windowed-14.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled postive event data to file
write.table(x = shuffled_pos,
            file='./data/formatted/shuffled_pos_windowed-14.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)

# write shuffled negative event data to file
write.table(x = shuffled_neg,
            file='./data/formatted/shuffled_neg_windowed-14.csv',
            sep=",",
            col.names=TRUE,
            row.names=FALSE)
 
