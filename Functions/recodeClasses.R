#==============================================================================#
# recodeClasses.R
#==============================================================================#

# This is a simple function to recode all the classes of the data
# frame you are working on. This may save you a lot of time if 
# you have dozens or hundreds of columns (without all those single
# lines for each column). 

# Creating a function to do this recoding
  recodeClasses <- function(data, classes, format="%m/%d/%Y") {
  	
  	# data          = data frame
  	# classes       = vector of classes for the variables in order
  	# format        = format for date values
  	
  	# check to see if the number of columns equals the number of classes given
  	  if (ncol(data) != length(classes)) stop("ERROR: length of classes and
                                                  number of rows are not the same")
  		
    # recode them by looping through the entire vector of classes (columns)
  	for (i in 1:length(classes)){
	
  	# Set each to their respective classes
      if (classes[[i]] == "numeric")   data[, i] <- as.numeric(data[, i])
      if (classes[[i]] == "factor")    data[, i] <- as.factor(data[, i])
      if (classes[[i]] == "character") data[, i] <- as.character(data[, i])
      if (classes[[i]] == "integer")   data[, i] <- as.integer(data[, i])
      if (classes[[i]] == "date")      data[, i] <- as.Date(data[, i], format=format)
      
      cat("Done with column: ", i, "\n")
  	
  	}
  	
  	return(data=data)
    
  }



