#------------------------------------------------------------------------------#
# rollingAvg Function
#------------------------------------------------------------------------------#
  
# Author:       Austin Putz
# 1st created:  February, 2016
# Last updated: Tuesday Aug 24, 2016
# License:      GPLv2.0
  
# Like always, open source, no warranty. 

#------------------------------------------------------------------------------#
# Description
#------------------------------------------------------------------------------#
  
# This function is designed to take the rolling average from so many days
# within a date value. By default is 30 days, but can be modified.

# DO NOT use "verbose = TRUE" with large datasets, only to test if you want
# to see what is happening.

#------------------------------------------------------------------------------#
# Inputs
#------------------------------------------------------------------------------#
  
# Input ------------------------------------------------------------------------
# data        = data set being used
# value.col   = column of data you want rolling avg for
# date.col    = column of dates you want to use for averages
# ndays       = number of days covering the average (default = 30 days)
#                - centered meaning it goes both sides of that date (ndays / 2)
#                  on both sides
# verbose     = print data matrices to visualize
# ------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
# Function
#------------------------------------------------------------------------------#
  
# start function
  rollingAvg <- function(data, value.col, date.col, ndays=30, end2end=TRUE, verbose=FALSE){
    
  	#----------------------------------------#
  	# Set inputs correctly
  	#----------------------------------------#
  	
  	# check if data is missing
  	if (missing(data)) stop("\nMissing Data argument\n\n")
    
  	# find columns in data
  	if (!(value.col %in% names(data))) stop("\nValue column not found\n\n")
  	if (!(date.col  %in% names(data))) stop("\nDate column not found\n\n")
  	
  	# subset dataset
  	data.sub1 <- data[, c(value.col, date.col)]
  	
    # print column names
    if (verbose==TRUE) cat("The column with values to average is:", value.col, "\n")
    if (verbose==TRUE) cat("The column with date values is:", date.col, "\n")
    
  	# find oldest and newest dates to know the range
    begin.date <- min(data[, date.col], na.rm=TRUE)
    end.date   <- max(data[, date.col], na.rm=TRUE)
    
    # print dates of begin and end dates
    if (verbose==TRUE) cat("The oldest date is:", "\n")
    if (verbose==TRUE) print(begin.date)
    if (verbose==TRUE) cat("The most recent date is:","\n")
    if (verbose==TRUE) print(end.date)
    
    # set half-width for number of days (both sides of date)
    within.n.days <- ndays / 2
    
    # print half-width
    if (verbose==TRUE) cat("The number of days from date (half width):", within.n.days, "\n")
    
    # set dates that can capture the entire 30 day window
    if (end2end == TRUE){
      start.date <- begin.date
      stop.date  <- end.date
    } else {
    	  start.date <- begin.date + within.n.days
      stop.date  <- end.date   - within.n.days
    }
  	
    # print start and stop dates to be used
    if (verbose==TRUE) cat("First date to be used:", "\n")
    if (verbose==TRUE) print(start.date)
    if (verbose==TRUE) cat("Last date to be used:", "\n")
    if (verbose==TRUE) print(stop.date)
    
  	#----------------------------------------#
  	# Modify data
  	#----------------------------------------#
  	
    # convert date column into julian dates
    data$date_jul <- julian(data[, date.col])
    
    # print dataset
    if (verbose == TRUE) print(head(data))
  
    # convert dates to julian dates
    start.datej <- julian(start.date)
    stop.date.j <- julian(stop.date)
    
  	#----------------------------------------#
  	# Start main body
  	#----------------------------------------#
  	
    # create sequence of dates from begining to end
    vec.dates <- seq.Date(from=start.date, to=stop.date, by=1)
    
    # initialize vector of means
    vec.means <- as.numeric(vector(length=length(vec.dates)))
    
    # loop through every date and get the mean
    for (i in 1:length(vec.dates)){
    
    	# subtract off the current date
    	data$DaysDiff <- data[, date.col] - vec.dates[i]
    	
    	# get absolute difference
    	data$DaysDiff <- abs(data$DaysDiff)
      
    	# subset to within half-width date (ndays / 2)
    	data.sub <- data[data$DaysDiff <= within.n.days, ]
    	
    	# print 
    	if (verbose == TRUE) print(data.sub)
    	
    	# calculate that average
    	mean_of_period <- mean(data.sub[, value.col], na.rm=TRUE)
    	
    	# put the mean into the vector that will be concatenated with
    	vec.means[i] <- mean_of_period
      
    }
    
    # concatenate into data frame
    df.output <- data.frame(vec.dates, vec.means)
    
    # return data frame of dates and values
    return(df=df.output)
    
  }

  

  

  