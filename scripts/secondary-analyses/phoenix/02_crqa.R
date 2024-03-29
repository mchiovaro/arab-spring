######## Cross-Recurrence Quantification Analysis (Phoenix) ########
#
# This script runs cross-recurrence quantification analysis
# the real-world events and Twitter social cohesion of 
# Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_05_21

#### 1. Set up ####

# clear environment
rm(list=ls())

# specify global plotting variables
all_event_color = "#CC79A7"
positive_event_color = "#0072B2"
negative_event_color = "#D55E00"

# read in data 
data <- read.csv("./data/formatted/secondary/phoenix/formatted_data.csv")
shuffled_full <- read.csv("./data/formatted/secondary/phoenix/shuffled_data_full.csv")

# split shuffled data into their own dataframes
shuffled_coh <- shuffled_full[,c(1:1000)]
shuffled_all_source_target <- shuffled_full[,c(1001:2000)]
shuffled_pos_source_target <- shuffled_full[,c(2001:3000)]
shuffled_neg_source_target <- shuffled_full[,c(3001:4000)]
shuffled_all_target <- shuffled_full[,c(4001:5000)]
shuffled_pos_target <- shuffled_full[,c(5001:6000)]
shuffled_neg_target <- shuffled_full[,c(6001:7000)]

#### 2. Run CRQA ####

### source and target filtered data ###

## count of all events and social cohesion ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=data$coh_sextiles,
                                 ts2=data$all_sextiles_source_target,
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
write.table(rqa_results_all_source_target,'./results/secondary/phoenix/crqa/source_target-crqa_results-all_events.csv',
            sep=",", row.names=FALSE)

# build the CRP using 'crqa' package function: plotRP
png("./results/secondary/phoenix/crqa/source_target-rp-all_events.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Social cohesion", 
           labely = "Count of all events", 
           cols = all_event_color, 
           pcex = .5)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## count of positive events and social cohesion ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=data$coh_sextiles,
                                 ts2=data$pos_sextiles_source_target,
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
write.table(rqa_results_pos_source_target,'./results/secondary/phoenix/crqa/source_target-crqa_results-pos_events.csv',
            sep=",", row.names=FALSE)

# build the CRP using 'crqa' package function: plotRP
png("./results/secondary/phoenix/crqa/source_target-rp-pos_events.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Social cohesion", 
           labely = "Count of positive events", 
           cols = positive_event_color, 
           pcex = .5)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## count of negative events and social cohesion ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=data$coh_sextiles,
                                 ts2=data$neg_sextiles_source_target,
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
write.table(rqa_results_neg_source_target,'./results/secondary/phoenix/crqa/source_target-crqa_results-neg_events.csv',
            sep=",", row.names=FALSE)

# build the CRP using 'crqa' package function: plotRP
png("./results/secondary/phoenix/crqa/source_target-rp-neg_events.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Social cohesion", 
           labely = "Count of negative events", 
           cols = negative_event_color, 
           pcex = .5)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## join source-target plots ##

# read in CRPs
crp_1 <- readPNG('./results/secondary/phoenix/crqa/source_target-rp-all_events.png')
crp_2 <- readPNG('./results/secondary/phoenix/crqa/source_target-rp-pos_events.png')
crp_3 <- readPNG('./results/secondary/phoenix/crqa/source_target-rp-neg_events.png')

# join images and save
png("./results/secondary/phoenix/crqa/source_target-rp-joined.png", width=720, height=325)
grid.arrange(rasterGrob(crp_1),rasterGrob(crp_2), rasterGrob(crp_3), ncol=3)
dev.off()

### target filtered data ###

## count of all events and social cohesion ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=data$coh_sextiles,
                                 ts2=data$all_sextiles_target,
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
write.table(rqa_results_all_target,'./results/secondary/phoenix/crqa/target-crqa_results-all_events.csv',
            sep=",", row.names=FALSE)

# build the CRP using 'crqa' package function: plotRP
png("./results/secondary/phoenix/crqa/target-rp-all_events.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Social cohesion", 
           labely = "Count of all events", 
           cols = all_event_color, 
           pcex = .5)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## count of positive events and social cohesion ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=data$coh_sextiles,
                                 ts2=data$pos_sextiles_target,
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
write.table(rqa_results_pos_target,'./results/secondary/phoenix/crqa/target-crqa_results-pos_events.csv',
            sep=",", row.names=FALSE)

# build the CRP using 'crqa' package function: plotRP
png("./results/secondary/phoenix/crqa/target-rp-pos_events.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Social cohesion", 
           labely = "Count of positive events", 
           cols = positive_event_color, 
           pcex = .5)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## count of negative events and social cohesion ##

# run cross recurrence
cross_recurrence_analysis = crqa(ts1=data$coh_sextiles,
                                 ts2=data$neg_sextiles_target,
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
write.table(rqa_results_neg_target,'./results/secondary/phoenix/crqa/target-crqa_results-neg_events.csv',
            sep=",", row.names=FALSE)

# build the CRP using 'crqa' package function: plotRP
png("./results/secondary/phoenix/crqa/target-rp-neg_events.png", width = 4, height = 4, units = 'in', res = 300)
par = list(unit = 2, 
           labelx = "Social cohesion", 
           labely = "Count of negative events", 
           cols = negative_event_color, 
           pcex = .5)
plotRP(cross_recurrence_analysis$RP, par)
dev.off()

## join target plots ##

# read in CRPs
crp_1 <- readPNG('./results/secondary/phoenix/crqa/target-rp-all_events.png')
crp_2 <- readPNG('./results/secondary/phoenix/crqa/target-rp-pos_events.png')
crp_3 <- readPNG('./results/secondary/phoenix/crqa/target-rp-neg_events.png')

# join images and save
png("./results/secondary/phoenix/crqa/target-rp-joined.png", width=720, height=325)
grid.arrange(rasterGrob(crp_1),rasterGrob(crp_2), rasterGrob(crp_3), ncol=3)
dev.off()

#### 3. Conduct permutation tests ####

### source and target filtered data ###

## count of all events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_all_source_target = data.frame()

# calculate crqa metrics for each shuffled time series
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

# initialize data frame for saving significance results
significance_all_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_all_source_target[,c(i)] > rqa_results_all_source_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_all_source_target <- cbind(significance_all_source_target, temp)
}

# rename variables
names(significance_all_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_all_source_target,'./results/secondary/phoenix/crqa/source_target-metric_signif_all.csv',
            sep=",", row.names=FALSE)

## count of positive events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_pos_source_target = data.frame()

# calculate crqa metrics for each shuffled time series
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

# initialize data frame for saving significance results
significance_pos_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_pos_source_target[,c(i)] > rqa_results_pos_source_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_pos_source_target <- cbind(significance_pos_source_target, temp)
}

# rename variables
names(significance_pos_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_pos_source_target,'./results/secondary/phoenix/crqa/source_target-metric_signif_pos.csv',
            sep=",", row.names=FALSE)

## count of negative events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_neg_source_target = data.frame()

# calculate crqa metrics for each shuffled time series
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

# initialize data frame for saving significance results
significance_neg_source_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_neg_source_target[,c(i)] > rqa_results_neg_source_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_neg_source_target <- cbind(significance_neg_source_target, temp)
}

# rename variables
names(significance_neg_source_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_neg_source_target,'./results/secondary/phoenix/crqa/source_target-metric_signif_neg.csv',
            sep=",", row.names=FALSE)

### target filtered data ###

## count of all events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_all_target = data.frame()

# calculate crqa metrics for each shuffled time series
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

# initialize data frame for saving significance results
significance_all_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_all_target[,c(i)] > rqa_results_all_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_all_target <- cbind(significance_all_target, temp)
}

# rename variables
names(significance_all_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_all_target,'./results/secondary/phoenix/crqa/target-metric_signif_all.csv',
            sep=",", row.names=FALSE)

## count of positive events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_pos_target = data.frame()

# calculate crqa metrics for each shuffled time series
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

# initialize data frame for saving significance results
significance_pos_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_pos_target[,c(i)] > rqa_results_pos_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_pos_target <- cbind(significance_pos_target, temp)
}

# rename variables
names(significance_pos_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_pos_target,'./results/secondary/phoenix/crqa/target-metric_signif_pos.csv',
            sep=",", row.names=FALSE)

## count of negative events and social cohesion ##

# initialize data frame for saving metrics
rqa_shuffled_neg_target = data.frame()

# calculate crqa metrics for each shuffled time series
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

# initialize data frame for saving significance results
significance_neg_target <- data.frame(matrix(, nrow=1, ncol=0))

# loop through all 9 crqa metrics
for (i in 1:9){
  
  # calculate proportion of time shuffled metric is greater than actual metric
  temp <- data.frame(sum((rqa_shuffled_neg_target[,c(i)] > rqa_results_neg_target[,c(i)]), na.rm=TRUE)/1000)
  
  # bind to data frame
  significance_neg_target <- cbind(significance_neg_target, temp)
}

# rename variables
names(significance_neg_target) <- c("RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT")

# write to file
write.table(significance_neg_target,'./results/secondary/phoenix/crqa/target-metric_signif_neg.csv',
            sep=",", row.names=FALSE)
