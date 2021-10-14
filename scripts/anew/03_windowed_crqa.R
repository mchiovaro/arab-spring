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

# specify global variable values
delay = 1; embed = 1; rescale = 0; radius = 0.001;
normalize = 0; mindiagline = 2; minvertline = 2;
tw = 0; whiteline = FALSE; recpt = FALSE; side = "both"
method = 'crqa'; metric = 'euclidean';  
datatype = "categorical"; 
windowsize =  14; windowstep = 1
trend = FALSE

## count of positive events and valence ##
windowed_pos_val = wincrqa(anew$daily_val_dec, data$pos_deciles_source_target, windowstep, windowsize, delay, embed,
                           radius, rescale, normalize, mindiagline, minvertline,
                           tw, whiteline, recpt, side, method, metric, 
                           datatype, trend)

## count of positive events and arousal ##
windowed_pos_aro = wincrqa(anew$daily_aro_dec, data$pos_deciles_source_target, windowstep, windowsize, delay, embed,
                           radius, rescale, normalize, mindiagline, minvertline,
                           tw, whiteline, recpt, side, method, metric, 
                           datatype, trend)


## count of positive events and dominance ##
windowed_pos_dom = wincrqa(anew$daily_dom_dec, data$pos_deciles_source_target, windowstep, windowsize, delay, embed,
                           radius, rescale, normalize, mindiagline, minvertline,
                           tw, whiteline, recpt, side, method, metric, 
                           datatype, trend)

## count of negative events and valence ##
windowed_neg_val = wincrqa(anew$daily_val_dec, data$neg_deciles_source_target, windowstep, windowsize, delay, embed,
                           radius, rescale, normalize, mindiagline, minvertline,
                           tw, whiteline, recpt, side, method, metric, 
                           datatype, trend)

## count of negative events and arousal ##
windowed_neg_aro = wincrqa(anew$daily_aro_dec, data$neg_deciles_source_target, windowstep, windowsize, delay, embed,
                           radius, rescale, normalize, mindiagline, minvertline,
                           tw, whiteline, recpt, side, method, metric, 
                           datatype, trend)

## count of negative events and dominance ##
windowed_neg_dom = wincrqa(anew$daily_dom_dec, data$neg_deciles_source_target, windowstep, windowsize, delay, embed,
                           radius, rescale, normalize, mindiagline, minvertline,
                           tw, whiteline, recpt, side, method, metric, 
                           datatype, trend)

#### 3. Conduct permutation tests ####

# set seed for reproducibility
set.seed(123)

## count of positive events and valence ##

# initialize data frame for saving metrics
rqa_shuffled_pos_val = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=shuffled_val[,c(i)],
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
  rqa_shuffled_pos_val <- rbind(rqa_shuffled_pos_val, 
                                          rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_pos_val$DET[is.na(rqa_shuffled_pos_val$DET)] <- 0

# initialize data frame for saving percentiles
significance_pos_val <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_pos_val[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_pos_val <- cbind(significance_pos_val, temp)
}

## count of positive events and arousal ##

# initialize data frame for saving metrics
rqa_shuffled_pos_aro = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=shuffled_aro[,c(i)],
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
  rqa_shuffled_pos_aro <- rbind(rqa_shuffled_pos_aro, 
                                rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_pos_aro$DET[is.na(rqa_shuffled_pos_aro$DET)] <- 0

# initialize data frame for saving percentiles
significance_pos_aro <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_pos_aro[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_pos_aro <- cbind(significance_pos_aro, temp)
}

## count of positive events and dominance ##

# initialize data frame for saving metrics
rqa_shuffled_pos_dom = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=shuffled_dom[,c(i)],
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
  rqa_shuffled_pos_dom <- rbind(rqa_shuffled_pos_dom, 
                                rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_pos_dom$DET[is.na(rqa_shuffled_pos_dom$DET)] <- 0

# initialize data frame for saving percentiles
significance_pos_dom <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_pos_dom[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_pos_dom <- cbind(significance_pos_dom, temp)
}

## count of negative events and valence ##

# initialize data frame for saving metrics
rqa_shuffled_neg_val = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=shuffled_val[,c(i)],
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
  rqa_shuffled_neg_val <- rbind(rqa_shuffled_neg_val, 
                                rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_neg_val$DET[is.na(rqa_shuffled_neg_val$DET)] <- 0

# initialize data frame for saving percentiles
significance_neg_val <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_neg_val[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_neg_val <- cbind(significance_neg_val, temp)
}

## count of negative events and arousal ##

# initialize data frame for saving metrics
rqa_shuffled_neg_aro = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=shuffled_aro[,c(i)],
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
  rqa_shuffled_neg_aro <- rbind(rqa_shuffled_neg_aro, 
                                rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_neg_aro$DET[is.na(rqa_shuffled_neg_aro$DET)] <- 0

# initialize data frame for saving percentiles
significance_neg_aro <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_neg_aro[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_neg_aro <- cbind(significance_neg_aro, temp)
}

## count of negative events and dominance ##

# initialize data frame for saving metrics
rqa_shuffled_neg_dom = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=shuffled_dom[,c(i)],
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
  rqa_shuffled_neg_dom <- rbind(rqa_shuffled_neg_dom, 
                                rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_neg_dom$DET[is.na(rqa_shuffled_neg_dom$DET)] <- 0

# initialize data frame for saving percentiles
significance_neg_dom <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_neg_dom[,c(i)], 
                              c(.01, .05, .95, .99), 
                              na.rm = TRUE))
  
  # bind to data frame
  significance_neg_dom <- cbind(significance_neg_dom, temp)
}

#### 4. Plot the results across windows ####

# set universal plotting parameters
plot_rr_ymin = 0
plot_rr_ymax = 25
plot_det_ymin = 0
plot_det_ymax = 60

## count of positive events and valence ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_pos_val)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_windowed_pos_val_RR = ggplot(data = windowed_pos_val,
                                            aes(y = RR,
                                                x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_val[1,1], color = "orange") +
  geom_hline(yintercept = significance_pos_val[2,1], color = "red") +
  geom_hline(yintercept = significance_pos_val[3,1], color = "red") +
  geom_hline(yintercept = significance_pos_val[4,1], color = "orange") +
  
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
windowed_pos_val$DET[is.na(windowed_pos_val$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_pos_val)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_windowed_pos_val_DET = ggplot(data = windowed_pos_val,
                                             aes(y = DET,
                                                 x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_val[1,2], color = "orange") +
  geom_hline(yintercept = significance_pos_val[2,2], color = "red") +
  geom_hline(yintercept = significance_pos_val[3,2], color = "red") +
  geom_hline(yintercept = significance_pos_val[4,2], color = "orange") +
  
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

## count of positive events and arousal ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_pos_aro)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_windowed_pos_aro_RR = ggplot(data = windowed_pos_aro,
                                  aes(y = RR,
                                      x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_aro[1,1], color = "orange") +
  geom_hline(yintercept = significance_pos_aro[2,1], color = "red") +
  geom_hline(yintercept = significance_pos_aro[3,1], color = "red") +
  geom_hline(yintercept = significance_pos_aro[4,1], color = "orange") +
  
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
windowed_pos_aro$DET[is.na(windowed_pos_aro$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_pos_aro)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_windowed_pos_aro_DET = ggplot(data = windowed_pos_aro,
                                   aes(y = DET,
                                       x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_aro[1,2], color = "orange") +
  geom_hline(yintercept = significance_pos_aro[2,2], color = "red") +
  geom_hline(yintercept = significance_pos_aro[3,2], color = "red") +
  geom_hline(yintercept = significance_pos_aro[4,2], color = "orange") +
  
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

## count of positive events and dominance ##

# RR plot #

# identify trend line
fit <- lm(RR ~ win, data = windowed_pos_dom)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_windowed_pos_dom_RR = ggplot(data = windowed_pos_dom,
                                  aes(y = RR,
                                      x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_dom[1,1], color = "orange") +
  geom_hline(yintercept = significance_pos_dom[2,1], color = "red") +
  geom_hline(yintercept = significance_pos_dom[3,1], color = "red") +
  geom_hline(yintercept = significance_pos_dom[4,1], color = "orange") +
  
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
windowed_pos_dom$DET[is.na(windowed_pos_dom$DET)] <- 0

# identify trend line
fit <- lm(DET ~ win, data = windowed_pos_dom)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_windowed_pos_dom_DET = ggplot(data = windowed_pos_dom,
                                   aes(y = DET,
                                       x = win)) +
  
  # add both points and lines for each observation
  geom_line() +
  geom_point() +
  
  # add upper and lower 95th and 99th percentile lines for significance tests
  geom_hline(yintercept = significance_pos_dom[1,2], color = "orange") +
  geom_hline(yintercept = significance_pos_dom[2,2], color = "red") +
  geom_hline(yintercept = significance_pos_dom[3,2], color = "red") +
  geom_hline(yintercept = significance_pos_dom[4,2], color = "orange") +
  
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



















