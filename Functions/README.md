# R

Basic R functions for common tasks.

## rollingAvg.R

Used to calculate rolling average given a date value. It will calculate the average of X number of days (default 30). The results are centered on that date meaning it goes out both sides of the date given in the result.

## recodeClasses.R

Used to recode all the columns in a data frame without listing each one individually. 

## missingMatrix.R

Please see the documentation in pdf form posted here. 

Used to calculate a matrix of values for a data frame or matrix. Diagonals represent missingness down a column, upper triangular and lower triangular values represent missingness between columns. 

## plotCurve.R

Use this function to recreate curves from coefficients you obtain from models. I created it to recreate a lactation curve from Mrode's Linear Model book (chapter 9). You can recreate the lactation curve using the coefficients obtained from a random regression model, but it will work for any coefficients. Let me know if you have questions and I can get documentation. 

