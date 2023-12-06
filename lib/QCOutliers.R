# ========================================================================================
# Find nutrient outliers in totals and remove if desired.
# Copied from load_clean_ASA24.R.
# Version 1
# Created on 11/30/2022 by Rie Sadohara
# ========================================================================================

# Cut-points are based on the 5th and 95th percentile of intakes from NHANES data.

# Flag outliers in the target column and min max values, and pop out a prompt asking whether to delete 
# the outliers or not.
# Whether the outliers are removed or not, the output will be a df called 'QCtotals'.
QCOutliers <- function(input.data, target.colname, min, max){
  temp <- input.data
  
  # the nth column has the variable of interest.
  nth_column <- which(colnames(temp) == target.colname )
  
  # Extact rows that are NOT in the range of min-max.
  Outlier_rows <<- temp[ temp[, nth_column] < min | temp[, nth_column] > max  , ]
  
  # Report how many rows are outside the min-max range.
  cat("There are", nrow(Outlier_rows), "observations with <", min, "or >", max, ". \n", sep = " ") 
  
  if(nrow(Outlier_rows) == 0){ 
    QCtotals <<- temp
    cat("There are no outlier rows, but the input data was renamed as QCtotals.\n",
        nrow(QCtotals), "rows remained.\n")}
  else{
    answer <- askYesNo("Remove?")
    if(answer==T){
      # Save rows that are within the range of min-max as QCtotals.
      QCtotals <<- temp[ temp[, nth_column] >= min & temp[, nth_column] <= max , ]
      cat("Outlier rows were removed; the cleaned data is saved as an object called \"QCtotals\".\n",
          nrow(QCtotals), "rows remained.\n")
    }else{
      QCtotals <<- temp
      cat("Outlier rows were not removed, but the input data was renamed as QCtotals.\n",
          nrow(QCtotals), "rows remained.\n")}
  }
}
# ---------------------------------------------------------------------------------------------------------------
