#==============================================================================#
# Documentation: rollingAvg function example
#==============================================================================#
  
# Author:       Austin Putz
# 1st created:  Tuesday Aug 23, 2016
# Last updated: Tuesday Aug 24, 2016
# License:      GPLv2.0
  
# Like always, open source, no warranty. 
  
#==============================================================================#
# Description
#==============================================================================#
  
# remove everything before starting
  rm(list=ls())
  
# For more go to the actual pdf documentation
  
# This function will take the rolling average based on dates. 
# Most of the functions I found were based on single vectors, not
# based on dates. 
  
# This will take the moving/rolling average of a numeric variable based on 
# the date it was observed. 
  
#==============================================================================#
# Call function from where ever you saved it
#==============================================================================#
  
# Where is it saved????
  location_saved <- "~/Documents/Programming/R/Functions/rollingAvg.R"
  
# load the function
  source(location_saved)
  
# remove location 
  rm(location_saved)
  
#==============================================================================#
# Libraries
#==============================================================================#
  
# load libraries
  library(ggplot2)
  
#==============================================================================#
# Example Data
#==============================================================================#
  
# vector of dates
  vec_date <- seq.Date(as.Date("2016-01-01"), as.Date("2016-12-31"), by="day")
  
# set seed for random number generator
  set.seed(1234)
  
# figure out breaks
  vec1 <- round(rnorm(100, mean=10, sd=3), 0)
  vec2 <- round(rnorm(100, mean=5,  sd=2), 0)
  vec3 <- round(rnorm(100, mean=11, sd=3), 0)
  vec4 <- round(rnorm(length(vec_date)-length(vec1)-length(vec2)-length(vec3), mean=15, sd=3))
  
# combine 
  vec_numbers <- c(vec1, vec2, vec3, vec4)
  
# remove component vectors
  rm(list=c("vec1", "vec2", "vec3", "vec4"))
  
# combine dataset into a data frame
  data <- data.frame(Date=vec_date, Value=vec_numbers)
  
# remove vectors
  rm(list=c("vec_date", "vec_numbers"))
  
#==============================================================================#
# Get rolling average
#==============================================================================#
  
# do rolling average
  data.rollingAvg <- rollingAvg(data, value.col="Value", date.col="Date")
  
# plot it
  ggplot(data.rollingAvg, aes(x=vec.dates, y=vec.means)) +
  geom_point(shape=15, color="steelblue") +
  geom_line(color="steelblue") +
  geom_vline(xintercept=julian(as.Date("2016-04-10"))) +
  geom_vline(xintercept=julian(as.Date("2016-07-19"))) +
  geom_vline(xintercept=julian(as.Date("2016-10-27"))) +
  ggtitle("Rolling Average Plot") +
  xlab("Date") +
  ylab("Rolling Average")
  






































