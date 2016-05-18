#==============================================================================#
# matMiss Function to calculate a matrix of missing values
#==============================================================================#

missingMatrix <- function(data, percent=TRUE, missing=TRUE, digits=2){
  
  # check if it's a data frame or matrix
  if (class(data)=="data.frame"){
    cat("Data Frame\n\n")
  } else if (class(data)=="matrix"){
    cat("Matrix\n\n")
  } else {
    stop("Data is not a matrix or a data frame, please change\n\n")
  }
  
  # find the number of columns
  n_col <- dim(data)[2]
  
  # create matrix
  my_mat <- matrix(NA, ncol=n_col, nrow=n_col)
  
  # fill diagonals of missing down column
  for (i in 1:n_col) {
    for (j in 1:n_col) {
    
    if (i == j) {
      # find sum of missing on diagonal (down a column)
      my_mat[i, i] = sum(!is.na(data[, i]))
    } else if (j > i){
      # add sum of both not missing to offdiag 
      my_mat[i, j] = sum(!is.na(data[, i]) & !is.na(data[, j]))
    } else if (j < i){
      my_mat[i, j] = sum(!is.na(data[, i]) | !is.na(data[, j]))
    }
      
    }
  }
  
  # change column and row names
  rownames(my_mat) <- names(data)
  colnames(my_mat) <- names(data)
  
  # if missing then subtract the matrix from the total
  # right now it's counted the ones that are not NA
  if (missing == TRUE){
    my_mat <- (dim(data)[1] - my_mat)
  }
  
  # change to percentage if option is on
  if (percent == TRUE){
    my_mat <- (my_mat / dim(data)[1])*100
  }
  
  # round digits
  my_mat <- round(my_mat, digits)
  
  # return the matrix of missing values
  return(my_mat)
  
}

my_df <- data.frame(col1=c(1,2,3,4,5,6,9),
                    col2=c(NA,NA,5,7,NA,NA,NA),
                    col3=c(1,NA,NA,5,1,3,4))

print(my_df)

missingMatrix(my_df, percent=TRUE, missing=TRUE)
missingMatrix(my_df, percent=TRUE, missing=FALSE)
missingMatrix(my_df, percent=FALSE, missing=TRUE)
missingMatrix(my_df, percent=FALSE, missing=FALSE)


























































































