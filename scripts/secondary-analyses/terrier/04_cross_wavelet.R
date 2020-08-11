######## Cross Wavelet Analysis (Terrier) ########
#
# This script runs cross wavelet analysis on the
# real-world events and Twitter social cohesion of 
# Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_08_11

#### 1. Set up ####

# clear environment
rm(list=ls())

# set working directory
setwd("./arab-spring/")

# read in truncated data 
data <- read.csv("./data/formatted/secondary/terrier/formatted_data.csv")

# make date into a number for the analysis
data$date_num <- seq(from =1, to = nrow(data))

# read in time series
coh_dec <- data[,c("date_num", "coh_deciles")]
all_dec_targ <- data[,c("date_num", "all_deciles_target")]
pos_dec_targ <- data[,c("date_num", "pos_deciles_target")]
neg_dec_targ <- data[,c("date_num", "neg_deciles_target")]
all_dec_source_targ <- data[,c("date_num", "all_deciles_source_target")]
pos_dec_source_targ <- data[,c("date_num", "pos_deciles_source_target")]
neg_dec_source_targ <- data[,c("date_num", "neg_deciles_source_target")]

#### Run Cross-Wavelet Analysis ####

# specify the number of iterations (>1000 is best)
nrands = 1000

### target filtered data ###

## count of all events and social cohesion ##

# compute coherence
wtc.AB = wtc(coh_dec, all_dec_targ, nrands = nrands)

# make room for the color bar on the right
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)

# plot the graph
png('./results/secondary/terrier/cross-wavelet/target-xw-all_events.png')
plot(wtc.AB, 
     plot.phase = TRUE, 
     lty.coi = 1, 
     col.coi = "grey", 
     lwd.coi = 2, 
     lwd.sig = 2, 
     arrow.lwd = 0.03, 
     arrow.len = 0.12, 
     ylab = "Scale", 
     xlab = "Period (days)", 
     plot.cb = FALSE, 
     main = "Wavelet Coherence\nTwitter social cohesion and count of all events (deciles)\nTarget filtered data")
dev.off()

## count of positive events and social cohesion ##

# compute coherence
wtc.AB = wtc(coh_dec, pos_dec_targ, nrands = nrands)

# make room for the color bar on the right
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)

# plot the graph
png('./results/secondary/terrier/cross-wavelet/target-xw-pos_events.png')
plot(wtc.AB, 
     plot.phase = TRUE, 
     lty.coi = 1, 
     col.coi = "grey", 
     lwd.coi = 2, 
     lwd.sig = 2, 
     arrow.lwd = 0.03, 
     arrow.len = 0.12, 
     ylab = "Scale", 
     xlab = "Period (days)", 
     plot.cb = FALSE, 
     main = "Wavelet Coherence\nTwitter social cohesion and count of positive events (deciles)\nTarget filtered data")
dev.off()

## count of negative events and social cohesion ##

# compute coherence
wtc.AB = wtc(coh_dec, neg_dec_targ, nrands = nrands)

# make room for the color bar on the right
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)

# plot the graph
png('./results/secondary/terrier/cross-wavelet/target-xw-neg_events.png')
plot(wtc.AB, 
     plot.phase = TRUE, 
     lty.coi = 1, 
     col.coi = "grey", 
     lwd.coi = 2, 
     lwd.sig = 2, 
     arrow.lwd = 0.03, 
     arrow.len = 0.12, 
     ylab = "Scale", 
     xlab = "Period (days)", 
     plot.cb = FALSE, 
     main = "Wavelet Coherence\nTwitter social cohesion and count of negative events (deciles)\nTarget filtered data")
dev.off()

### source and target filtered data ###

## count of all events and social cohesion ##

# compute coherence
wtc.AB = wtc(coh_dec, all_dec_source_targ, nrands = nrands)

# make room for the color bar on the right
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)

# plot the graph
png('./results/secondary/terrier/cross-wavelet/source_target-xw-all_events.png')
plot(wtc.AB, 
     plot.phase = TRUE, 
     lty.coi = 1, 
     col.coi = "grey", 
     lwd.coi = 2, 
     lwd.sig = 2, 
     arrow.lwd = 0.03, 
     arrow.len = 0.12, 
     ylab = "Scale", 
     xlab = "Period (days)", 
     plot.cb = FALSE, 
     main = "Wavelet Coherence\nTwitter social cohesion and count of all events (deciles)\nSource and Target filtered data")
dev.off()

## count of positive events and social cohesion ##

# compute coherence
wtc.AB = wtc(coh_dec, pos_dec_source_targ, nrands = nrands)

# make room for the color bar on the right
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)

# plot the graph
png('./results/secondary/terrier/cross-wavelet/source_target-xw-pos_events.png')
plot(wtc.AB, 
     plot.phase = TRUE, 
     lty.coi = 1, 
     col.coi = "grey", 
     lwd.coi = 2, 
     lwd.sig = 2, 
     arrow.lwd = 0.03, 
     arrow.len = 0.12, 
     ylab = "Scale", 
     xlab = "Period (days)", 
     plot.cb = FALSE, 
     main = "Wavelet Coherence\nTwitter social cohesion and count of positive events (deciles)\nSource and Target filtered data")
dev.off()

## count of negative events and social cohesion ##

# compute coherence
wtc.AB = wtc(coh_dec, neg_dec_source_targ, nrands = nrands)

# make room for the color bar on the right
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)

# plot the graph
png('./results/secondary/terrier/cross-wavelet/source_target-xw-neg_events.png')
plot(wtc.AB, 
     plot.phase = TRUE, 
     lty.coi = 1, 
     col.coi = "grey", 
     lwd.coi = 2, 
     lwd.sig = 2, 
     arrow.lwd = 0.03, 
     arrow.len = 0.12, 
     ylab = "Scale", 
     xlab = "Period (days)", 
     plot.cb = FALSE, 
     main = "Wavelet Coherence\nTwitter social cohesion and count of negative events (deciles)\nSource and Target filtered data")
dev.off()
