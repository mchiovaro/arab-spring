######## Cross Wavelet Analysis (ICEWS) ########
#
# This script runs cross wavelet analysis on the
# real-world events and Twitter social cohesion of 
# Arab Spring.
#
# Code by: M. Chiovaro (@mchiovaro)
# University of Connecticut
# Last updated: 2020_05_15

# clear environment
rm(list=ls())

# set working directory
setwd("./arab-spring/")

# load the required packages
library(dplyr)
library(biwavelet)

# read in truncated data 
data <- read.csv("./data/formatted/formatted_data.csv")

# make date into a number for the analysis
data$date_num <- seq(from =1, to = nrow(data))

t1 <- data[,c("date_num", "coh_dec")]
t2 <- data[,c("date_num", "all_dec")]

##### Coherence #####

# Specify the number of iterations. The more, the better (>1000)
nrands = 1000

# compute coherence
wtc.AB = wtc(t1, t2, nrands = nrands)

# make room for the color bar on the right
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)

# plot the graph
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
     plot.cb = TRUE, 
     main = "Wavelet Coherence\nMean social coherence and Count of events (deciles)")

# Adding grid lines
n = length(t1[, 1])
abline(v = seq(5, n, 5), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
#axis(side = 3, at = c(seq(5, n, 5)), labels = c(seq(5, 75, 5)))

##### Cross Wavelet #####

# Compute cross-wavelet
xwt <- xwt(t1, t2)

# make room for the color bar on the right
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1) 

# Plot cross wavelet power and phase difference (arrows)
plot(xwt, plot.cb=TRUE)
