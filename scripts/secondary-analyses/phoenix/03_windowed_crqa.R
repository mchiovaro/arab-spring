######## Windowed Cross-Recurrence Quantification Analysis (Phoenix) ########
#
# This script runs windowed cross-recurrence quantification 
# analysis the real-world events and Twitter social cohesion 
# of Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_05_21

#### 1. Set up ####

# clear environment
rm(list=ls())

# read in data 
data <- read.csv("./data/formatted/secondary/phoenix/formatted_data.csv")
shuffled_windowed <- read.csv("./data/formatted/secondary/phoenix/shuffled_data_windowed.csv")

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
windowed_count = wincrqa(ts1 = data$coh_sextiles, 
                         ts2 = data$all_sextiles_source_target, 
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
wincrqa_all_df_source_target <- as.data.frame(windowed_count$crqwin)
colnames(wincrqa_all_df_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT", "window")

## count of positive events and social cohesion ##

# calculate windowed rqa
windowed_pos = wincrqa(ts1 = data$coh_sextiles, 
                       ts2 = data$pos_sextiles_source_target, 
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
wincrqa_pos_df_source_target <- as.data.frame(windowed_pos$crqwin)
colnames(wincrqa_pos_df_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT", "window")

## count of negative events and social cohesion ##

# calculate windowed rqa
windowed_neg = wincrqa(ts1 = data$coh_sextiles, 
                       ts2 = data$neg_sextiles_source_target, 
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
wincrqa_neg_df_source_target <- as.data.frame(windowed_neg$crqwin)
colnames(wincrqa_neg_df_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT", "window")

### target filtered data ###

## count of all events and social cohesion ##

# calculate windowed rqa
windowed_count = wincrqa(ts1 = data$coh_sextiles, 
                         ts2 = data$all_sextiles_target, 
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
wincrqa_all_df_target <- as.data.frame(windowed_count$crqwin)
colnames(wincrqa_all_df_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT", "window")

## count of positive events and social cohesion ##

# calculate windowed rqa
windowed_pos = wincrqa(ts1 = data$coh_sextiles, 
                       ts2 = data$pos_sextiles_target, 
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
wincrqa_pos_df_target <- as.data.frame(windowed_pos$crqwin)
colnames(wincrqa_pos_df_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT", "window")

## count of negative events and social cohesion ##

# calculate windowed rqa
windowed_neg = wincrqa(ts1 = data$coh_sextiles, 
                       ts2 = data$neg_sextiles_target, 
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
wincrqa_neg_df_target <- as.data.frame(windowed_neg$crqwin)
colnames(wincrqa_neg_df_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT", "window")

#### 3. Conduct permutation tests ####

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
  rqa_shuffled_all_source_target <- rbind(rqa_shuffled_all_source_target, rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_all_source_target$DET[is.na(rqa_shuffled_all_source_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_all_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_all_source_target[,c(i)], c(.01, .05, .95, .99), na.rm = TRUE))
  
  # bind to data frame
  significance_all_source_target <- cbind(significance_all_source_target, temp)
}

# rename variables
names(significance_all_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

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
  rqa_shuffled_pos_source_target <- rbind(rqa_shuffled_pos_source_target, rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_pos_source_target$DET[is.na(rqa_shuffled_pos_source_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_pos_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_pos_source_target[,c(i)], c(.01, .05, .95, .99), na.rm = TRUE))
  
  # bind to data frame
  significance_pos_source_target <- cbind(significance_pos_source_target, temp)
}

# rename variables
names(significance_pos_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

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
  rqa_shuffled_neg_source_target <- rbind(rqa_shuffled_neg_source_target, rqa_results)
  
}

# fill in NA determinism values with zero
rqa_shuffled_neg_source_target$DET[is.na(rqa_shuffled_neg_source_target$DET)] <- 0

# initialize data frame for saving percentiles
significance_neg_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(quantile(rqa_shuffled_neg_source_target[,c(i)], c(.01, .05, .95, .99), na.rm = TRUE))
  
  # bind to data frame
  significance_neg_source_target <- cbind(significance_neg_source_target, temp)
}

# rename variables
names(significance_neg_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

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
  temp <- data.frame(quantile(rqa_shuffled_all_target[,c(i)], c(.01, .05, .95, .99), na.rm = TRUE))
  
  # bind to data frame
  significance_all_target <- cbind(significance_all_target, temp)
}

# rename variables
names(significance_all_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

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
  temp <- data.frame(quantile(rqa_shuffled_pos_target[,c(i)], c(.01, .05, .95, .99), na.rm = TRUE))
  
  # bind to data frame
  significance_pos_target <- cbind(significance_pos_target, temp)
}

# rename variables
names(significance_pos_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

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
  temp <- data.frame(quantile(rqa_shuffled_neg_target[,c(i)], c(.01, .05, .95, .99), na.rm = TRUE))
  
  # bind to data frame
  significance_neg_target <- cbind(significance_neg_target, temp)
}

# rename variables
names(significance_neg_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

#### 4. Plot the results across windows ####

### source and target filtered data ###

## count of all events and social cohesion ##

# RR plot #

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/source_target-windowed_all_RR.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_all_df_source_target$RR,xlab='Window',ylab='RR',type='b', ylim = c(12, 27),
     main = "Windowed RQA for Social\nCohesion and Event Count")

# add trend line
fit <- lm(RR ~ window, data = wincrqa_all_df_source_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_all_source_target[1,1], col="orange")
abline(h = significance_all_source_target[2,1], col="red")
abline(h = significance_all_source_target[3,1], col="red")
abline(h = significance_all_source_target[4,1], col="orange")

# finish image processing
dev.off()

# DET plot #

# fill in NA determinism values with 0
wincrqa_all_df_source_target$DET[is.na(wincrqa_all_df_source_target$DET)] <- 0

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/source_target-windowed_all_DET.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_all_df_source_target$DET,xlab='Window',ylab='DET',type='b', ylim = c(8,62), 
     main = "Windowed RQA for Social\nCohesion and Event Count")

# add trend line
fit <- lm(DET ~ window, data = wincrqa_all_df_source_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_all_source_target[1,2], col="orange")
abline(h = significance_all_source_target[2,2], col="red")
abline(h = significance_all_source_target[3,2], col="red")
abline(h = significance_all_source_target[4,2], col="orange")

# finish image processing
dev.off()

## count of positive events and social cohesion ##

# RR plot #

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/source_target-windowed_pos_RR.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_pos_df_source_target$RR,xlab='Window',ylab='RR',type='b', ylim = c(10, 28),
     main = "Windowed RQA for Social\nCohesion and Positive Event Count")

# add trend line
fit <- lm(RR ~ window, data = wincrqa_pos_df_source_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_pos_source_target[1,1], col="orange")
abline(h = significance_pos_source_target[2,1], col="red")
abline(h = significance_pos_source_target[3,1], col="red")
abline(h = significance_pos_source_target[4,1], col="orange")

# finish image processing
dev.off()

# DET plot #

# fill in NA determinism values with 0
wincrqa_pos_df_source_target$DET[is.na(wincrqa_pos_df_source_target$DET)] <- 0

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/source_target-windowed_pos_DET.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_pos_df_source_target$DET,xlab='Window',ylab='DET',type='b', ylim = c(7, 60), 
     main = "Windowed RQA for Social\nCohesion and Positive Event Count")

# add trend line
fit <- lm(DET ~ window, data = wincrqa_pos_df_source_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_pos_source_target[1,2], col="orange")
abline(h = significance_pos_source_target[2,2], col="red")
abline(h = significance_pos_source_target[3,2], col="red")
abline(h = significance_pos_source_target[4,2], col="orange")

# finish image processing
dev.off()

## count of negative events and social cohesion ##

# RR plot #

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/source_target-windowed_neg_RR.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_neg_df_source_target$RR,xlab='Window',ylab='RR',type='b', ylim = c(9, 28),
     main = "Windowed RQA for Social\nCohesion and Negative Event Count")

# add trend line
fit <- lm(RR ~ window, data = wincrqa_neg_df_source_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_neg_source_target[1,1], col="orange")
abline(h = significance_neg_source_target[2,1], col="red")
abline(h = significance_neg_source_target[3,1], col="red")
abline(h = significance_neg_source_target[4,1], col="orange")

# finish image processing
dev.off()

# DET plot #

# fill in NA determinism values with 0
wincrqa_neg_df_source_target$DET[is.na(wincrqa_neg_df_source_target$DET)] <- 0

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/source_target-windowed_neg_DET.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_neg_df_source_target$DET,xlab='Window',ylab='DET',type='b', ylim = c(7, 60), 
     main = "Windowed RQA for Social\nCohesion and Negative Event Count")

# add trend line
fit <- lm(DET ~ window, data = wincrqa_neg_df_source_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_neg_source_target[1,2], col="orange")
abline(h = significance_neg_source_target[2,2], col="red")
abline(h = significance_neg_source_target[3,2], col="red")
abline(h = significance_neg_source_target[4,2], col="orange")

# finish image processing
dev.off()

### target filtered data ###

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/target-windowed_all_RR.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_all_df_target$RR,xlab='Window',ylab='RR',type='b', ylim = c(11, 28),
     main = "Windowed RQA for Social\nCohesion and Event Count")

# add trend line
fit <- lm(RR ~ window, data = wincrqa_all_df_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_all_target[1,1], col="orange")
abline(h = significance_all_target[2,1], col="red")
abline(h = significance_all_target[3,1], col="red")
abline(h = significance_all_target[4,1], col="orange")

# finish image processing
dev.off()

# DET plot #

# fill in NA determinism values with 0
wincrqa_all_df_target$DET[is.na(wincrqa_all_df_target$DET)] <- 0

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/target-windowed_all_DET.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_all_df_target$DET,xlab='Window',ylab='DET',type='b', ylim = c(5,60), 
     main = "Windowed RQA for Social\nCohesion and Event Count")

# add trend line
fit <- lm(DET ~ window, data = wincrqa_all_df_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_all_target[1,2], col="orange")
abline(h = significance_all_target[2,2], col="red")
abline(h = significance_all_target[3,2], col="red")
abline(h = significance_all_target[4,2], col="orange")

# finish image processing
dev.off()

## count of positive events and social cohesion ##

# RR plot #

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/target-windowed_pos_RR.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_pos_df_target$RR,xlab='Window',ylab='RR',type='b', ylim = c(11, 29),
     main = "Windowed RQA for Social\nCohesion and Positive Event Count")

# add trend line
fit <- lm(RR ~ window, data = wincrqa_pos_df_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_pos_target[1,1], col="orange")
abline(h = significance_pos_target[2,1], col="red")
abline(h = significance_pos_target[3,1], col="red")
abline(h = significance_pos_target[4,1], col="orange")

# finish image processing
dev.off()

# DET plot #

# fill in NA determinism values with 0
wincrqa_pos_df_target$DET[is.na(wincrqa_pos_df_target$DET)] <- 0

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/target-windowed_pos_DET.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_pos_df_target$DET,xlab='Window',ylab='DET',type='b', ylim = c(8, 58), 
     main = "Windowed RQA for Social\nCohesion and Positive Event Count")

# add trend line
fit <- lm(DET ~ window, data = wincrqa_pos_df_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_pos_target[1,2], col="orange")
abline(h = significance_pos_target[2,2], col="red")
abline(h = significance_pos_target[3,2], col="red")
abline(h = significance_pos_target[4,2], col="orange")

# finish image processing
dev.off()

## count of negative events and social cohesion ##

# RR plot #

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/target-windowed_neg_RR.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_neg_df_target$RR,xlab='Window',ylab='RR',type='b', ylim = c(10, 32),
     main = "Windowed RQA for Social\nCohesion and Negative Event Count")

# add trend line
fit <- lm(RR ~ window, data = wincrqa_neg_df_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_neg_target[1,1], col="orange")
abline(h = significance_neg_target[2,1], col="red")
abline(h = significance_neg_target[3,1], col="red")
abline(h = significance_neg_target[4,1], col="orange")

# finish image processing
dev.off()

# DET plot #

# fill in NA determinism values with 0
wincrqa_neg_df_target$DET[is.na(wincrqa_neg_df_target$DET)] <- 0

# plot the windows and RRs 
png("./results/secondary/phoenix/windowed-crqa/target-windowed_neg_DET.png", width = 4, height = 4, units = 'in', res = 300)
plot(wincrqa_neg_df_target$DET,xlab='Window',ylab='DET',type='b', ylim = c(0, 53), 
     main = "Windowed RQA for Social\nCohesion and Negative Event Count")

# add trend line
fit <- lm(DET ~ window, data = wincrqa_neg_df_target)
summary(fit)
abline(fit, col="blue")

# add upper and lower 95th and 99th percentile lines 
abline(h = significance_neg_target[1,2], col="orange")
abline(h = significance_neg_target[2,2], col="red")
abline(h = significance_neg_target[3,2], col="red")
abline(h = significance_neg_target[4,2], col="orange")

# finish image processing
dev.off()
