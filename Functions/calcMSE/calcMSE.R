#==============================================================================#
# calcMSE
#==============================================================================#

#------------------------------------------------------------------------------#
# Info
#------------------------------------------------------------------------------#

# Author:   Austin Putz
# Created:  Sept 19, 2017
# Modified: Nov 17, 2017
# License:  GPLv2

#------------------------------------------------------------------------------#
# NO Warranty
#------------------------------------------------------------------------------#

# This R code is provided "as is" with absolutely NO warranty. I do my
# best to assure there are no bugs, but it's open source software with no
# warranty. Please contact me with bug fixes or improvements/advancements. 

#------------------------------------------------------------------------------#
# Description
#------------------------------------------------------------------------------#

# This function was designed to extract the MSE WITHIN animal, no regard
# to other animals. 

# The following is an example of the data format. I have used age as the
# x variable and Feed Intake (FI) as the response (y) variable.

# Data Format:
# 
# AnimalID Age FI
# 0001 50 0.5
# 0001 51 0.6
# 0001 52 0.7
# ...
# 0001 180 4.1
# 0002 55 0.55
# 0002 56 0.60
# ...
# 0002 181 4.4

# This function is designed to take data and extract variables for each pig
# over a period of time. 

# Inputs
#   data       = [variable] the dataset to use, data frame
#   id.col     = [char] the ID column in the dataset
#   x.col   = [char] the date column to used to regress on
#   y.col     = [char] the weight column in the dataset
#   n          = [numeric] minimum number of days with records to consider
#   verbose    = [logical] do you want to see output? (only for small examples)
#   type       = [char] "lm", "rq"
#                   lm: for basic lm() model
#                   rq: quantile regression
#   tau        = [numeric] in (0,1) for quantile regression, default = 0.50 
#                 this should be changed

# Example:
# data.FI.MSE.lm <- calcMSE(data=data.fi.ra, 
#   id.col = "ID",          # name of my grouping varible (Animal ID)
#   x.col = "Age",          # name of my x-variable 
#   y.col = "FIimpAll",     # name of my y-variable (response) variable
#   n = 60,                 # number of data points needed to get a non-missing result
#   type="lm",              # do linear regression
#   order=1)                # just fit a first order regression (no polynomial)

#------------------------------------------------------------------------------#
# Function
#------------------------------------------------------------------------------#

# Begin function
calcMSE <- function(data, id.col, x.col, y.col, n=60, 
                        verbose=FALSE, order=1, type="lm", tau=0.50){
  
  # check the number of args and give error to show usage
  if (nargs() == 0){
    stop("Usage: calcMSE(data, id.col, x.col, y.col, n=60, order=1, type=\"lm\"), tau=0.50)")
  }
  
  # check for quantile regression option
  if (type == "rq") {
    library(quantreg)
    if (tau < 0 | tau > 1) {
      stop("Need to specify tau if doing quantile regression")
    }
  }
  
  # make column names character
  id.col  = as.character(id.col)
  x.col   = as.character(x.col)
  y.col   = as.character(y.col)
  
  # print the column names
  cat("Columns:\n")
  cat("\t", sprintf("%-20s: %s", "ID Column", id.col), "\n")
  cat("\t", sprintf("%-20s: %s", "X Variable Column", x.col), "\n")
  cat("\t", sprintf("%-20s: %s", "Y Variable Column", y.col), "\n\n")
  
  # echo inputs
  cat("Other Inputs:\n")
  cat("\t", sprintf("%-20s: %s", "Number of rows", as.character(n)), "\n")
  cat("\t", sprintf("%-20s: %s", "Order of model", as.character(order)), "\n\n")
  
  if (type=="rq"){
    
  cat("Set quantile regression option:\n")
  cat("\t", sprintf("%-20s: %s", "tau for rq()", as.character(tau)), "\n\n")
  
  }
  
  # find date and Value column
  if (!(id.col %in% names(data))){
    stop("\n\t ID column was not found within the dataset")
  }
  if (!(x.col %in% names(data))){
    stop("\n\t X variable column was not found within the dataset")
  }
  if (!(y.col %in% names(data))){
    stop("\n\t Y variable column was not found within the dataset")
  }
  
  # subset data to only the 3 columns needed
  data <- as.data.frame(data[, c(id.col, x.col, y.col)])
  
  # change column names for later in the function
  names(data) <- c("ID", "Date", "Value")
  
  # get the number of unique individuals
  n.animals <- length(unique(data[, id.col]))
  
  # print number of animals
  cat(sprintf("%-29s: %s", "Number of individuals", as.character(n.animals)), "\n")
  
  # unique animal IDs
  animals <- unique(data[, id.col])
  
  # create empty dataset
  data.MSE <- matrix(NA, ncol=1, nrow=n.animals)
  data.MSE <- as.data.frame(data.MSE)
  
  # add animal ID to column 1
  data.MSE[, 1] <- animals
  
  # change column name
  names(data.MSE)[1] <- "ID"
  
  # set NA's for column 2 (slope)
  data.MSE[, 2] <- NA
  
  # change column name
  names(data.MSE)[2] <- "MSE"
  
  # remove any NA's in ID column
  data.MSE <- data.MSE[!is.na(data.MSE$ID), ]
  
  #----------------------------------------#
  # Begin Loop (through all animals)
  #----------------------------------------#
  
  # pull out ID's column
  all.ids <- data.MSE[, 1]
  
  # begin loop through all the ID's
  for (i in 1:nrow(data.MSE)){

    # subset dataset
    current.id <- as.character(data.MSE[i, 1])
    if (verbose==TRUE){ cat("\tCurrent ID: ", current.id, "\n") }
    
    if (verbose==TRUE){ cat("Animal: ", current.id, "\n") }
    
    # subset dataset
    data.id <- data[data$ID==current.id, ]
    if (verbose==TRUE){ head(data.id) }
    
    # test to make sure that there are at least n time points
    if (nrow(data.id) < n) {
      next
    }
    
    if (type=="lm"){
      
      # fit linear model
      if (order==1){
        mod.lm <- lm(Value ~ Date, data=data.id)
      } else if (order==2){
        mod.lm <- lm(Value ~ Date + I(Date^2), data=data.id)
      } else if (order==3){
        mod.lm <- lm(Value ~ Date + I(Date^2) + I(Date^3), data=data.id)
      } else if (order==4){
        mod.lm <- lm(Value ~ Date + I(Date^2) + I(Date^3) + I(Date^4), data=data.id)
      } else if (order==5){
        mod.lm <- lm(Value ~ Date + I(Date^2) + I(Date^3) + I(Date^4) + I(Date^5), data=data.id)
      } else {
        stop("Only have up to a 5th order polynomial")
      }
      
      # remove the MSE (mean squared error)
      new.MSE <- mean(residuals(mod.lm)^2)
      
      # fill MSE dataset
      data.MSE[i, 2] <- new.MSE
      
    } else if (type=="rq"){
      
      # fit linear model
      if (order==1){
        mod.rq <- rq(Value ~ Date, data=data.id, tau=tau)
      } else if (order==2){
        mod.rq <- rq(Value ~ Date + I(Date^2), data=data.id, tau=tau)
      } else if (order==3){
        mod.rq <- rq(Value ~ Date + I(Date^2) + I(Date^3), data=data.id, tau=tau)
      } else if (order==4){
        mod.rq <- rq(Value ~ Date + I(Date^2) + I(Date^3) + I(Date^4), data=data.id, tau=tau)
      } else if (order==5){
        mod.rq <- rq(Value ~ Date + I(Date^2) + I(Date^3) + I(Date^4) + I(Date^5), data=data.id, tau=tau)
      } else {
        stop("Only have up to a 5th order polynomial")
      }
      
      # remove the MSE (average squared residual)
      new.MSE <- mean(residuals(mod.rq)^2)
      
      # fill MSE dataset
      data.MSE[i, 2] <- new.MSE
      
    } else {
      stop("\nDon't understand your type, please use \"lm\" or \"rq\"")
    }
    
  }
  
  #----------------------------------------#
  # return output
  #----------------------------------------#
  
  # print the dataset
  return(data.MSE)
  
}
































