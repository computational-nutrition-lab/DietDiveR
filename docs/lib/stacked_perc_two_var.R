# ===============================================================================================================
# Calculate percentages of each level of var.y for each var.x in order to generate a stacked
# barchart.
# Version 1
# Created on 12/01/2022 by Rie Sadohara
# ===============================================================================================================

# ---------------------------------------------------------------------------------------------------------------

StackedPercTwoVar <- function(input.df, var.x, var.y, by){
  
  subsetted <- input.df[, c(by, var.x, var.y)]
  
  # longtable <- aggregate(SEQN           ~ AgeGroup       + GLU_index,                                    
  longtable <- aggregate(subsetted[, 1] ~ subsetted[, 2] + subsetted[, 3],
                         data = subsetted, FUN = length)
  
  # Rename the columns.
  colnames(longtable) <- c("VarX", "VarY", "By")
  
  # Make a short table with GLU_index levels in each column in order to compute their proportions. 
  # shorttable <- reshape2::dcast(longtable, AgeGroup ~ GLU_index, sum)
  shorttable <- reshape2::dcast(longtable, 
                                longtable[, "VarX"] ~ longtable[, "VarY"], sum)
  
  # Define the number of levels of VarY here.
  nlevels_VarY <- ncol(shorttable)-1
  
  print(paste("There are ", nlevels_VarY, " levels in var.y.", sep=""))
  
  # Calculate the sum of all the levels of VarX.
  shorttable$sum <- rowSums(shorttable[, 2: ncol(shorttable)])
  
  # Create an empty data frame to save calculated percentages.
  var.y_pr <- data.frame(matrix(NA, nrow = nrow(shorttable),
                                ncol = nlevels_VarY))
  
  # Create a vector of proportions calculated for each level of VarY, and 
  # add it to each column of var.y_pr.
  for(i in 1: nlevels_VarY){
    
    pr_vector <- shorttable[, i+1] / shorttable$sum 
    
    var.y_pr[, i] <- pr_vector
    
    # Rename each column as "LEVELNAME_pr". 
    colnames(var.y_pr)[i] <- paste(colnames(shorttable)[i+1], "_pr", sep="")
    
  }
  
  # Add sum of the percentages. Should add to 1.
  var.y_pr$sum_pr <- rowSums(var.y_pr[, 1:nlevels_VarY])
  
  # Join the ID (first column in shorttable) and percentage table var.y_pr. 
  shorttable_pr <- cbind(shorttable[, 1], var.y_pr) 
  
  # Rename the first column same as var.x. 
  colnames(shorttable_pr)[1] <- paste(var.x)
  
  # Take the proportions and make it into a long table again for plotting.
  
  longtable_pr <<- reshape2::melt(shorttable_pr[, 1:ncol(shorttable_pr)-1 ]) # Take all but the last column (which is all 1)
  
  print(paste("The percentages of each level of var.y are saved in the dataframe 'longtable_pr'."))
  
}

# ---------------------------------------------------------------------------------------------------------------

# The result will be something like this: AgeGroup = var.x, variable = var.y (GLU_index levels).

# AgeGroup       variable      value
# 1     18_19      Normal_pr 0.54347826
# 2       20s      Normal_pr 0.56043956
# 3       30s      Normal_pr 0.34166667
# 4       40s      Normal_pr 0.36697248
# 5       50s      Normal_pr 0.25000000
# 6       60s      Normal_pr 0.20567376
# 7       70s      Normal_pr 0.18750000
# 8    80plus      Normal_pr 0.25581395
# 9     18_19 Prediabetic_pr 0.43478261
# 10      20s Prediabetic_pr 0.41758242
# 11      30s Prediabetic_pr 0.58333333
# 12      40s Prediabetic_pr 0.53211009
# 13      50s Prediabetic_pr 0.54687500
# 14      60s Prediabetic_pr 0.51773050
# 15      70s Prediabetic_pr 0.56250000
# 16   80plus Prediabetic_pr 0.46511628
# 17    18_19    Diabetic_pr 0.02173913
# 18      20s    Diabetic_pr 0.02197802
# 19      30s    Diabetic_pr 0.07500000
# 20      40s    Diabetic_pr 0.10091743
# 21      50s    Diabetic_pr 0.20312500
# 22      60s    Diabetic_pr 0.27659574
# 23      70s    Diabetic_pr 0.25000000
# 24   80plus    Diabetic_pr 0.27906977
#   
