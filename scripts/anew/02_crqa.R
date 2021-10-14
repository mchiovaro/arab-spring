######## CRQA ########
#
# This script runs CRQA on the affect data from ANEW
# for the Arab Spring project.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2021_10_13

#### 1. Set up ####

# clear environment
rm(list=ls())
library(crqa)

# set working directory
#setwd("./Documents/github/arab-spring/")

# specify global plotting variables
all_event_color = "#CC79A7"
positive_event_color = "#0072B2"
negative_event_color = "#D55E00"

# read in event data
anew <- read.csv("./data/formatted/formatted_anew.csv")
anew_shuff_full <- read.csv("./data/formatted/shuffled_anew_full.csv") 
event <- read.csv("./data/formatted/primary/formatted_data.csv")
event_shuff_full <- read.csv("./data/formatted/primary/shuffled_data_full.csv")

# split shuffled data into their own data frames
shuffled_val <- anew_shuff_full[,c(1:1000)]
shuffled_aro <- anew_shuff_full[,c(1001:2000)]
shuffled_dom <- anew_shuff_full[,c(2001:3000)]
shuffled_pos_source_target <- event_shuff_full[,c(2001:3000)]
shuffled_neg_source_target <- event_shuff_full[,c(3001:4000)]
shuffled_pos_target <- event_shuff_full[,c(5001:6000)]
shuffled_neg_target <- event_shuff_full[,c(6001:7000)]

#### 2. Run CRQA for source-target filtered data  ####

## count of positive events and valence ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=anew$daily_val_dec,
                                 ts2=event$pos_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_pos_val <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_pos_val,'./results/anew/crqa/pos-val-crqa_results.csv',
            sep=",", row.names=FALSE)

RP = cross_recurrence_analysis$RP

# build the CRP using 'crqa' package function: plotRP
png("./results/anew/crqa/rp-pos-val.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Valence", 
           labely = "Count of positive events", 
           cols = positive_event_color, 
           pcex = .5,
           pch = 19,
           labax = seq(0, nrow(RP), 10),
           labay = seq(0, nrow(RP), 10),
           las = 1)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## count of positive events and arousal ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=anew$daily_aro_dec,
                                 ts2=event$pos_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_pos_aro <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_pos_aro,'./results/anew/crqa/pos-aro-crqa_results.csv',
            sep=",", row.names=FALSE)

RP = cross_recurrence_analysis$RP

# build the CRP using 'crqa' package function: plotRP
png("./results/anew/crqa/rp-pos-aro.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Arousal", 
           labely = "Count of positive events", 
           cols = positive_event_color, 
           pcex = .5,
           pch = 19,
           labax = seq(1, nrow(RP), 1),
           labay = seq(1, nrow(RP), 1),
           las = 1)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## count of positive events and dominance ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=anew$daily_dom_dec,
                                 ts2=event$pos_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_pos_dom <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_pos_dom,'./results/anew/crqa/pos-dom-crqa_results.csv',
            sep=",", row.names=FALSE)

RP = cross_recurrence_analysis$RP

# build the CRP using 'crqa' package function: plotRP
png("./results/anew/crqa/rp-pos-dom.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Dominance", 
           labely = "Count of positive events", 
           cols = positive_event_color, 
           pcex = .5,
           pch = 19,
           labax = seq(1, nrow(RP), 1),
           labay = seq(1, nrow(RP), 1),
           las = 1)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## join target plots ##

# read in CRPs
crp_1 <- readPNG('./results/anew/crqa/rp-pos-val.png')
crp_2 <- readPNG('./results/anew/crqa/rp-pos-aro.png')
crp_3 <- readPNG('./results/anew/crqa/rp-pos-dom.png')

# join and save
png("./results/anew/crqa/rp-pos-joined.png", width=1440,height=650)
grid.arrange(rasterGrob(crp_1), rasterGrob(crp_2), rasterGrob(crp_3), nrow=1)
dev.off()

## count of negative events and valence ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=anew$daily_val_dec,
                                 ts2=event$neg_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_neg_val <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_neg_val,'./results/anew/crqa/neg-val-crqa_results.csv',
            sep=",", row.names=FALSE)

RP = cross_recurrence_analysis$RP

# build the CRP using 'crqa' package function: plotRP
png("./results/anew/crqa/rp-neg-val.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Valence", 
           labely = "Count of negative events", 
           cols = negative_event_color, 
           pcex = .5,
           pch = 19,
           labax = seq(1, nrow(RP), 1),
           labay = seq(1, nrow(RP), 1),
           las = 1)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## count of negative events and arousal ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=anew$daily_aro_dec,
                                 ts2=event$neg_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_neg_aro <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_neg_aro,'./results/anew/crqa/neg-aro-crqa_results.csv',
            sep=",", row.names=FALSE)

RP = cross_recurrence_analysis$RP

# build the CRP using 'crqa' package function: plotRP
png("./results/anew/crqa/rp-neg-aro.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Arousal", 
           labely = "Count of negative events", 
           cols = negative_event_color, 
           pcex = .5,
           pch = 19,
           labax = seq(1, nrow(RP), 1),
           labay = seq(1, nrow(RP), 1),
           las = 1)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## count of negative events and dominance ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=anew$daily_dom_dec,
                                 ts2=event$neg_deciles_source_target,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=.001,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=0) 

# save the crqa results
rqa_results_neg_dom <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results_neg_dom,'./results/anew/crqa/neg-dom-crqa_results.csv',
            sep=",", row.names=FALSE)

RP = cross_recurrence_analysis$RP

# build the CRP using 'crqa' package function: plotRP
png("./results/anew/crqa/rp-neg-dom.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Dominance", 
           labely = "Count of negative events", 
           cols = negative_event_color, 
           pcex = .5,
           pch = 19,
           labax = seq(1, nrow(RP), 1),
           labay = seq(1, nrow(RP), 1),
           las = 1)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## join target plots ##

# read in CRPs
crp_1 <- readPNG('./results/anew/crqa/rp-neg-val.png')
crp_2 <- readPNG('./results/anew/crqa/rp-neg-aro.png')
crp_3 <- readPNG('./results/anew/crqa/rp-neg-dom.png')

# join and save
png("./results/anew/crqa/rp-neg-joined.png", width=1440,height=650)
grid.arrange(rasterGrob(crp_1), rasterGrob(crp_2), rasterGrob(crp_3), nrow=1)
dev.off()

#### 3. Conduct permutation tests ####

## count of positive events and valence ##

# initialize data frame for saving metrics
rqa_shuffled_pos_val = data.frame()

# calculate crqa metrics for each shuffled time series
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
  rqa_shuffled_pos_val <- rbind(rqa_shuffled_pos_val, rqa_results)
  
}

# initialize data frame for saving significance results
significance_pos_val <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_pos_val[,c(i)] > rqa_results_pos_val[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_pos_val <- cbind(significance_pos_val, temp)
}

# rename variables
names(significance_pos_val) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_pos_val,'./results/anew/crqa/pos_val-metric_signif.csv',
            sep=",", row.names=FALSE)

## count of positive events and arousal ##

# initialize data frame for saving metrics
rqa_shuffled_pos_aro = data.frame()

# calculate crqa metrics for each shuffled time series
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
  rqa_shuffled_pos_aro <- rbind(rqa_shuffled_pos_aro, rqa_results)
  
}

# initialize data frame for saving significance results
significance_pos_aro <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_pos_aro[,c(i)] > rqa_results_pos_aro[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_pos_aro <- cbind(significance_pos_aro, temp)
}

# rename variables
names(significance_pos_aro) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_pos_aro,'./results/anew/crqa/pos_aro-metric_signif.csv',
            sep=",", row.names=FALSE)

## count of positive events and dominance ##

# initialize data frame for saving metrics
rqa_shuffled_pos_dom = data.frame()

# calculate crqa metrics for each shuffled time series
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
  rqa_shuffled_pos_dom <- rbind(rqa_shuffled_pos_dom, rqa_results)
  
}

# initialize data frame for saving significance results
significance_pos_dom <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_pos_dom[,c(i)] > rqa_results_pos_dom[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_pos_dom <- cbind(significance_pos_dom, temp)
}

# rename variables
names(significance_pos_dom) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_pos_dom,'./results/anew/crqa/pos_dom-metric_signif.csv',
            sep=",", row.names=FALSE)

## count of negative events and valence ##

# initialize data frame for saving metrics
rqa_shuffled_neg_val = data.frame()

# calculate crqa metrics for each shuffled time series
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
  rqa_shuffled_neg_val <- rbind(rqa_shuffled_neg_val, rqa_results)
  
}

# initialize data frame for saving significance results
significance_neg_val <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_neg_val[,c(i)] > rqa_results_neg_val[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_neg_val <- cbind(significance_neg_val, temp)
}

# rename variables
names(significance_neg_val) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_neg_val,'./results/anew/crqa/neg_val-metric_signif.csv',
            sep=",", row.names=FALSE)

## count of negative events and arousal ##

# initialize data frame for saving metrics
rqa_shuffled_neg_aro = data.frame()

# calculate crqa metrics for each shuffled time series
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
  rqa_shuffled_neg_aro <- rbind(rqa_shuffled_neg_aro, rqa_results)
  
}

# initialize data frame for saving significance results
significance_neg_aro <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_neg_aro[,c(i)] > rqa_results_neg_aro[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_neg_aro <- cbind(significance_neg_aro, temp)
}

# rename variables
names(significance_neg_aro) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_neg_aro,'./results/anew/crqa/neg_aro-metric_signif.csv',
            sep=",", row.names=FALSE)

## count of negative events and dominance ##

# initialize data frame for saving metrics
rqa_shuffled_neg_dom = data.frame()

# calculate crqa metrics for each shuffled time series
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
  rqa_shuffled_neg_dom <- rbind(rqa_shuffled_neg_dom, rqa_results)
  
}

# initialize data frame for saving significance results
significance_neg_dom <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_neg_dom[,c(i)] > rqa_results_neg_dom[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_neg_dom <- cbind(significance_neg_dom, temp)
}

# rename variables
names(significance_neg_dom) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_neg_dom,'./results/anew/crqa/neg_dom-metric_signif.csv',
            sep=",", row.names=FALSE)
