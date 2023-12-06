# ===============================================================================================================
# SubsetByFirstChaInCol subsets columns starting with a specific character(s) from input dataframe.
# Version 1
# Created on 02/01/2023 by Rie Sadohara
# ===============================================================================================================

# Define a function to subset columns by their first character(s) 
  SubsetByFirstChaInCol <- function(input.df, starting.str){
    cols <- grepl(paste0("^", starting.str), colnames(input.df), ignore.case=F)
    subset(input.df, select = colnames(input.df)[cols])
  }

