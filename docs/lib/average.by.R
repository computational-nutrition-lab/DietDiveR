# ===============================================================================================================
# Calculate average of totals.
# Copied from prep_data_for_clustering.R.
# Version 1
# Created on 11/10/2022 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Average each participant: average by the by argument ("UserName" for ASA24 and "SEQN" for NHANES.
# (Need to take average per user in the totals because subsetted_non0var does not have UserName.)
# ===============================================================================================================

# Function to take average by a specified column.

AverageBy <- function(data, by, start.col, end.col, outfn){
  
  # make By a factor --> sort the dataset by the By_f --> use unique().
  
  # Column Variables of "totals" as a dataframe.
  colvars <- names(data)
  
  # Get the column index number of the first variable (start.col)
  start_col_number <- match(start.col, colvars)
  
  # Get the column index number of the last variable (end.col)
  end_col_number <- match(end.col, colvars)
  
  # Subset the 'by' column and the columns in the start-end range.
  by_start_end <- data[, c(which(colnames(data)==by), start_col_number:end_col_number)]
  
  # Make the by argument as a factor.
  # data$by_f <- as.factor(data[, paste(by)])
  by_start_end[, paste(by)] <- as.factor(by_start_end[, paste(by)])

  # Sort the dataset by the by argument. (will be in alphabetical order)
  by_start_end_s <- by_start_end[ order(by_start_end[, paste(by)]), ]
  
  # Define which variables to take means. and a list to save results
  myvar <- colnames(by_start_end_s)[-1]
  
  # Define the category entries (username in this case) to calculate means for.
  category_by <- unique(by_start_end_s[, 1])
  
  # Create a dataframe with the rownames corresponding to the category entries to calculate means for. 
  meansbycategorydf_1 <- data.frame(row.names = category_by)
  
  # Calculate means for each colname in myvar. 
  for(i in 1:length(myvar)){
    
    # mymean <- tapply(X = df[, i+1], INDEX = as.factor(df[, 1]), FUN = mean, na.rm=T)
    resarray <- tapply(X = by_start_end_s[, i+1], INDEX = as.factor(by_start_end_s[, 1]), FUN=mean, na.rm=T)
    
    resdf <- data.frame(resarray)   # save the results (array) as a dataframe.
    
    colnames(resdf) <- myvar[i]     # name the column with the variable.
    
    meansbycategorydf_1 <- cbind(meansbycategorydf_1, resdf)   # add the new resdf to the existing result meansbycategorydf_1.
  
  }
  
  # Save the "By" argument.
  by_arg <- paste(by)
  
  # Create a column that has the By argument (UserName for ASA24).
  meansbycategorydf_1[, paste(by_arg)] <- rownames(meansbycategorydf_1)
  
  # Bring the rownames to the first column.
  # Take out the rownames (By), which is in the last column.  
  meansbycategorydf_1_by <- meansbycategorydf_1[by_arg]
  
  # Take out the rest of the columns: i.e. take all the columns except the last one (By).  
  meansbycategorydf_1_var <- meansbycategorydf_1[, -ncol(meansbycategorydf_1)]
  
  # Combine meansbycategorydf_by and meansbycategorydf_var.
  meansbycategorydf <- cbind(meansbycategorydf_1_by, meansbycategorydf_1_var)
  
  # Save meansbycategorydf as a .txt file.
  write.table(x=meansbycategorydf, outfn, sep="\t", row.names=F, quote=F)
  
}
# ---------------------------------------------------------------------------------------------------------------

# # Test
# data(mtcars)
# mtcars[1,3] <- NA
# head(mtcars)
# table(mtcars$cyl)
# AverageBy(data=mtcars, by="cyl", start.col = 'disp', end.col = 'wt', outfn = "mtcars_by_cyl.txt")
# read.delim("mtcars_by_cyl.txt")
# 
# cyl4 = subset(mtcars, cyl=="4")
# cyl6 = subset(mtcars, cyl=="6")
# colMeans(cyl4)
# colMeans(cyl6, na.rm = T)
# mean(cyl6$disp)
# unique(mtcars$cyl) # Unique just lists factor levels as they appear in the data! 
