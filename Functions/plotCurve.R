#==============================================================================#
# plotCurve Function to recreate curves based on coefficients from model
#==============================================================================#

#==============================================================================#
# Info
#==============================================================================#

# Author: Austin Putz (putz.austin@gmail.com or aputz@iastate.edu if still here..)
# Created: Sept 7, 2016
# Edited: Sept 7, 2016
# License: GPLv2.0

#==============================================================================#
# Description
#==============================================================================#

# This function will take coefficients, say from a linear model or 
# random regression model (why this was created) and recreate the fixed
# or random curves.

# returns a plot (if you want) and the y values needed to plot the final curve

#==============================================================================#
# Inputs
#==============================================================================#

# x = vector of values for which the model was created, probably should 
#         extrapolate. Legendre polynomials are usually from -1 to 1 so set up
#         a vector 
# coefs = vector of coefficients from the model, make sure to include 
#         the intercept term
# plot = [logical] do you want to plot the curve??? (TRUE/FALSE)

#==============================================================================#
# Function
#==============================================================================#

# plot curve function
  plotCurve <- function(x, coefs, plot=TRUE){
    
    # set inputs
    order  <- length(coefs) - 1
    n.vals <- length(x)
    
    # put x into a matrix
    X.mat <- matrix(c(NA), ncol=(order+1), nrow=n.vals)
    X.mat[, 1] <- rep(1, length(x))
    X.mat[, 2] <- x
    
    # loop through and fill the matrix
    for (i in 1:order){
      X.mat[, i+1] <- (X.mat[, 2])^i
    }
    
    # put coeficients in a matrix to multiply
    coefs.mat <- matrix(c(coefs), ncol=1)
    
    # multiply by coefs to get value for y
    y <- X.mat %*% coefs.mat
    
    # plot if turned on
    if (plot==TRUE){
    plot(x=x, y=y, type="l")
    }

    # return the output
    return(y)
    
  }
  
  
  
  
  
  
  
  
  