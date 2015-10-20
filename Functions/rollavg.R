
#------------------------------------------------------------------------------#
# Calculate rolling average from date value
#------------------------------------------------------------------------------#

# This function is designed to take the rolling average from so many days
# within a date value. By default is 30 days, but can be modified.

# DO NOT use "verbose = TRUE" with large datasets, only to test if you want
# to see what is happening.

# Input ------------------------------------------------------------------------
# data        = data set being used
# value.col   = column of data you want rolling avg for
# date.col    = column of dates you want to use for averages
# ndays       = number of days covering the average (default = 30 days)
#                - centered meaning it goes both sides of that date (ndays / 2)
#                  on both sides
# verbose     = print data matrices to visualize
# ------------------------------------------------------------------------------

# start function
  rollavg <- function(data, value.col, date.col, ndays=30, verbose=FALSE){
  
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
    cat("The column with values to average is:", value.col, "\n")
    cat("The column with date values is:", date.col, "\n")
    
  	# find oldest and newest dates to know the range
    begin.date <- min(data[, date.col], na.rm=TRUE)
    end.date   <- max(data[, date.col], na.rm=TRUE)
    
    # print dates
    cat("The oldest date is:", "\n")
    print(begin.date)
    cat("The most recent date is:","\n")
    print(end.date)
    
    # set half-width for number of days (both sides of date)
    within.n.days <- ndays / 2
    
    # print half-width
    cat("The number of days from date (half width):", within.n.days, "\n")
    
    # set dates that can capture the entire
    start.date <- begin.date + within.n.days
    stop.date  <- end.date   - within.n.days
  	
    # print start and stop dates to be used
    cat("First date to be used:", "\n")
    print(start.date)
    cat("Last date to be used:", "\n")
    print(stop.date)
    
  	#----------------------------------------#
  	# Modify data
  	#----------------------------------------#
  	
    # convert date column into julian dates
    data$date_jul <- julian(data[, date.col])
    
    # print dataset
    if (verbose == TRUE) print(data)
  
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

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
