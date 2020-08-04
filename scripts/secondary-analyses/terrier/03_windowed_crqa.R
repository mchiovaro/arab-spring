######## Windowed Cross-Recurrence Quantification Analysis (Terrier) ########
#
# This script runs windowed cross-recurrence quantification 
# analysis the real-world events and Twitter social cohesion 
# of Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_08_03

#### 1. Set up ####

# clear environment
rm(list=ls())

# read in data 
data <- read.csv("./data/formatted/secondary/terrier/formatted_data.csv")
shuffled_windowed <- read.csv("./data/formatted/secondary/terrier/shuffled_data_windowed.csv")

# split shuffled data into their own dataframes
shuffled_coh <- shuffled_windowed[,c(1:1000)]
shuffled_all_source_target <- shuffled_windowed[,c(1001:2000)]
shuffled_pos_source_target <- shuffled_windowed[,c(2001:3000)]
shuffled_neg_source_target <- shuffled_windowed[,c(3001:4000)]
shuffled_all_target <- shuffled_windowed[,c(4001:5000)]
shuffled_pos_target <- shuffled_windowed[,c(5001:6000)]
shuffled_neg_target <- shuffled_windowed[,c(6001:7000)]

#### 2. Run Windowed CRQA ####

### source and target filtered data ###

## count of all events and social cohesion ##

# calculate windowed rqa
windowed_all_source_target = wincrqa(ts1 = data$coh_deciles, 
                                     ts2 = data$all_deciles_source_target, 
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

# tranform into a dataframe for easier plotting 
wincrqa_all_df_source_target <- as.data.frame(windowed_all_source_target$crqwin)
colnames(wincrqa_all_df_source_target) <- c("RR", "DET", 
                                            "NRLINE", "maxL", "L", 
                                            "ENTR", "rENTR", 
                                            "LAM", "TT", 
                                            "window")

## count of positive events and social cohesion ##

# calculate windowed rqa
windowed_pos_source_target = wincrqa(ts1 = data$coh_deciles, 
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

# tranform into a dataframe for easier plotting 
wincrqa_pos_df_source_target <- as.data.frame(windowed_pos_source_target$crqwin)
colnames(wincrqa_pos_df_source_target) <- c("RR", "DET", 
                                            "NRLINE", "maxL", "L", 
                                            "ENTR", "rENTR", 
                                            "LAM", "TT", 
                                            "window")

## count of negative events and social cohesion ##

# calculate windowed rqa
windowed_neg_source_target = wincrqa(ts1 = data$coh_deciles, 
                                     ts2 = data$neg_deciles_source_target, 
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

# tranform into a dataframe for easier plotting 
wincrqa_neg_df_source_target <- as.data.frame(windowed_neg_source_target$crqwin)
colnames(wincrqa_neg_df_source_target) <- c("RR", "DET", 
                                            "NRLINE", "maxL", "L", 
                                            "ENTR", "rENTR", 
                                            "LAM", "TT", 
                                            "window")

### target filtered data ###

## count of all events and social cohesion ##

# calculate windowed rqa
windowed_all_target = wincrqa(ts1 = data$coh_deciles, 
                              ts2 = data$all_deciles_target, 
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

# tranform into a dataframe for easier plotting 
wincrqa_all_df_target <- as.data.frame(windowed_all_target$crqwin)
colnames(wincrqa_all_df_target) <- c("RR", "DET", 
                                     "NRLINE", "maxL", "L", 
                                     "ENTR", "rENTR", 
                                     "LAM", "TT", 
                                     "window")

## count of positive events and social cohesion ##

# calculate windowed rqa
windowed_pos_target = wincrqa(ts1 = data$coh_deciles, 
                              ts2 = data$pos_deciles_target, 
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

# tranform into a dataframe for easier plotting 
wincrqa_pos_df_target <- as.data.frame(windowed_pos_target$crqwin)
colnames(wincrqa_pos_df_target) <- c("RR", "DET", 
                                     "NRLINE", "maxL", "L", 
                                     "ENTR", "rENTR", 
                                     "LAM", "TT", 
                                     "window")

## count of negative events and social cohesion ##

# calculate windowed rqa
windowed_neg_target = wincrqa(ts1 = data$coh_deciles, 
                              ts2 = data$neg_deciles_target, 
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

# tranform into a dataframe for easier plotting 
wincrqa_neg_df_target <- as.data.frame(windowed_neg_target$crqwin)
colnames(wincrqa_neg_df_target) <- c("RR", "DET", 
                                     "NRLINE", "maxL", "L", 
                                     "ENTR", "rENTR", 
                                     "LAM", "TT", 
                                     "window")

#### 3. Conduct permutation tests ####

# set seed for reproducibility
set.seed(123)

### source and target filtered data ###

## count of all events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_all_source_target = data.frame()

for (i in 1:1000) {
  
  # run cross recurrence
  cross_recurrence_analysis = crqa(ts1=shuffled_coh[,c(i)],
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
  cross_recurrence_analysis = crqa(ts1=shuffled_coh[,c(i)],
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
  cross_recurrence_analysis = crqa(ts1=shuffled_coh[,c(i)],
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
  cross_recurrence_analysis = crqa(ts1=shuffled_coh[,c(i)],
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
  cross_recurrence_analysis = crqa(ts1=shuffled_coh[,c(i)],
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
  cross_recurrence_analysis = crqa(ts1=shuffled_coh[,c(i)],
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
plot_rr_ymin = 5
plot_rr_ymax = 33
plot_det_ymin = 0
plot_det_ymax = 70

### source and target filtered data ###

## count of all events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ window, data = wincrqa_all_df_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct and save the plot
plot_source_target_windowed_all_RR = ggplot(data = wincrqa_all_df_source_target,
                                            aes(y = RR,
                                                x = window)) +
  
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
wincrqa_all_df_source_target$DET[is.na(wincrqa_all_df_source_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ window, data = wincrqa_all_df_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_all_DET = ggplot(data = wincrqa_all_df_source_target,
                                             aes(y = DET,
                                                 x = window)) +
  
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
fit <- lm(RR ~ window, data = wincrqa_pos_df_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_pos_RR = ggplot(data = wincrqa_pos_df_source_target,
                                            aes(y = RR,
                                                x = window)) +
  
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
wincrqa_pos_df_source_target$DET[is.na(wincrqa_pos_df_source_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ window, data = wincrqa_pos_df_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_pos_DET = ggplot(data = wincrqa_pos_df_source_target,
                                             aes(y = DET,
                                                 x = window)) +
  
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
fit <- lm(RR ~ window, data = wincrqa_neg_df_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_neg_RR = ggplot(data = wincrqa_neg_df_source_target,
                                            aes(y = RR,
                                                x = window)) +
  
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
wincrqa_neg_df_source_target$DET[is.na(wincrqa_neg_df_source_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ window, data = wincrqa_neg_df_source_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_source_target_windowed_neg_DET = ggplot(data = wincrqa_neg_df_source_target,
                                             aes(y = DET,
                                                 x = window)) +
  
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
                       "\nwith event count data",
                       "with source and target filtering (Terrier)",
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
ggsave(filename = "./results/secondary/terrier/windowed-crqa/source_target-windowed_all.png",
       plot = plot_source_target_windowed_all,
       dpi = 300,
       height = 4,
       width = 9)

### target filtered data ###

## count of all events and social cohesion ##

# RR plot #

# identify trend line
fit <- lm(RR ~ window, data = wincrqa_all_df_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct and save the plot
plot_target_windowed_all_RR = ggplot(data = wincrqa_all_df_target,
                                     aes(y = RR,
                                         x = window)) +
  
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
wincrqa_all_df_target$DET[is.na(wincrqa_all_df_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ window, data = wincrqa_all_df_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_all_DET = ggplot(data = wincrqa_all_df_target,
                                      aes(y = DET,
                                          x = window)) +
  
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
fit <- lm(RR ~ window, data = wincrqa_pos_df_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_pos_RR = ggplot(data = wincrqa_pos_df_target,
                                     aes(y = RR,
                                         x = window)) +
  
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
wincrqa_pos_df_target$DET[is.na(wincrqa_pos_df_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ window, data = wincrqa_pos_df_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_pos_DET = ggplot(data = wincrqa_pos_df_target,
                                      aes(y = DET,
                                          x = window)) +
  
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
fit <- lm(RR ~ window, data = wincrqa_neg_df_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_neg_RR = ggplot(data = wincrqa_neg_df_target,
                                     aes(y = RR,
                                         x = window)) +
  
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
wincrqa_neg_df_target$DET[is.na(wincrqa_neg_df_target$DET)] <- 0

# identify trend line
fit <- lm(DET ~ window, data = wincrqa_neg_df_target)
fit_intercept = fit$coefficients[1]
fit_slope = fit$coefficients[2]

# construct the plot
plot_target_windowed_neg_DET = ggplot(data = wincrqa_neg_df_target,
                                      aes(y = DET,
                                          x = window)) +
  
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
                       "\nwith event count data",
                       "with target-only filtering (Terrier)",
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
ggsave(filename = "./results/secondary/terrier/windowed-crqa/target-windowed_all.png",
       plot = plot_target_windowed_all,
       dpi = 300,
       height = 4,
       width = 9)
