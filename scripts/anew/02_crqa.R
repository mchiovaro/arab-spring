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
setwd("./Documents/github/arab-spring/")

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
shuffled_all_source_target <- event_shuff_full[,c(1001:2000)]
shuffled_pos_source_target <- event_shuff_full[,c(2001:3000)]
shuffled_neg_source_target <- event_shuff_full[,c(3001:4000)]
shuffled_all_target <- event_shuff_full[,c(4001:5000)]
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
rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results,'./results/anew/crqa/pos-val-crqa_results.csv',
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
rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results,'./results/anew/crqa/pos-aro-crqa_results.csv',
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
rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results,'./results/anew/crqa/pos-dom-crqa_results.csv',
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
rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results,'./results/anew/crqa/neg-val-crqa_results.csv',
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
rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results,'./results/anew/crqa/neg-aro-crqa_results.csv',
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
rqa_results <- data.frame(c(cross_recurrence_analysis[1:9]))
write.table(rqa_results,'./results/anew/crqa/neg-dom-crqa_results.csv',
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