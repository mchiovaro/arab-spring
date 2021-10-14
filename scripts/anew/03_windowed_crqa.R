######## Windowed CRQA ########
#
# This script runs windowed CRQA on the affect data from 
# ANEW for the Arab Spring project.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2021_10_13

#### 1. Set up ####

# clear environment
rm(list=ls())

# set working directory
#setwd("./Documents/github/arab-spring/")

# specify global plotting variables
all_event_color = "#CC79A7"
positive_event_color = "#0072B2"
negative_event_color = "#D55E00"

# read in data
anew <- read.csv("./data/formatted/formatted_anew.csv")
anew_shuff_wind <- read.csv("./data/formatted/shuffled_anew_windowed.csv")
shuffled_windowed <- read.csv("./data/formatted/primary/shuffled_data_windowed.csv")
data <- read.csv("./data/formatted/primary/formatted_data.csv") # real event data
 
# split shuffled data into their own dataframes
shuffled_val <- anew_shuff_wind[,c(1:1000)]
shuffled_aro <- anew_shuff_wind[,c(1001:2000)]
shuffled_dom <- anew_shuff_wind[,c(2001:3000)]

shuffled_pos_source_target <- shuffled_windowed[,c(2001:3000)]
shuffled_neg_source_target <- shuffled_windowed[,c(3001:4000)]
shuffled_pos_target <- shuffled_windowed[,c(5001:6000)]
shuffled_neg_target <- shuffled_windowed[,c(6001:7000)]

#### 2. Run windowed CRQA ####

## count of positive events and valence ##

# calculate windowed CRQA
windowed_pos_val = wincrqa(ts1 = anew$daily_val_dec, 
                                     ts2 = data$pos_deciles_source_target, 
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
wincrqa_pos_val <- as.data.frame(windowed_pos_val$crqwin)
colnames(wincrqa_pos_val) <- c("RR", "DET", 
                               "NRLINE", "maxL", "L", 
                               "ENTR", "rENTR", 
                               "LAM", "TT", 
                               "window")



