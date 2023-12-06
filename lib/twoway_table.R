# ===============================================================================================================
# Generate two-way tables with totals of rows and columns 
# Version 1
# Created on 10/27/2022 by Rie Sadohara
# ===============================================================================================================


TwoWayTableWithTotals <- function(data, var1, var2, out.csv.fn, sort.by.var2.total=c(T,F)){
  
  var1_n <- which(colnames(data)==var1)
  
  var2_n <- which(colnames(data)==var2)
  
  twoway <- table(data[, var1_n], data[, var2_n])
  
  ttwoway_2 <- as.data.frame.matrix(t(twoway))
  
  Total <- colSums(t(ttwoway_2)) # Column totals
  
  cc <- cbind(ttwoway_2, Total)  # Add column totals
  
  ccrr <- rbind(cc, colSums(cc))  # Add row totals 
  
  rownames(ccrr)[length(rownames(ccrr))] <- "Total"  # Change the last rowname to "Total"
  
  ccrr$var_2 <- rownames(ccrr)
  
  # Bring the rownames column to the first column.
  ccrr_2 <- ccrr[, c( ncol(ccrr), 1:ncol(ccrr)-1 )]
  
  # Change "var_2" to the real variable name. 
  colnames(ccrr_2)[1] <- paste(var2) 

  if(sort.by.var2.total==F){
    
    # save as a .csv file without sorting.
    write.csv(ccrr_2, out.csv.fn,  row.names=F, quote=F)
    
    twoway_table <<- ccrr_2
    
    cat("The two-way frequency table has been saved as a dataframe 'twoway_table'.\nThe levels of Variable 2 are in the first column." )
    }else{
    
    # Sort by the rows' totals.
    ccrr_2_s <- ccrr_2[ order(ccrr_2$Total, decreasing=T ), ]
    
    # Bring the total row to the bottom.
    ccrr_2_s_b <- ccrr_2_s[ c(2:nrow(ccrr_2_s), 1), ] 

    twoway_table <<- ccrr_2_s_b
    
    cat("The two-way frequency table has been saved as a dataframe 'twoway_table'.\nThe levels of Variable 2 are in the first column." )
    
    # save as a .csv file.
    write.csv(ccrr_2_s_b, out.csv.fn, row.names=F, quote=F)
    
  }  
  
}

# ---------------------------------------------------------------------------------------------------------------

# # Example dataframe 
# df = data.frame(Users= c("VVKAJ102", "VVKAJ102", "VVKAJ103", "VVKAJ103", "VVKAJ103", "VVKAJ104", "VVKAJ104", "VVKAJ104", "VVKAJ105", "VVKAJ105"), 
#                 KCAL = c(1440, 1834, 1345, 2541, 1735, 2772, 1883, 2675, 2544, 1323),
#                 Day = c("1", "3", "1", "2", "3", "1", "2", "3", "2", "3"))  
# df

# TwoWayTableWithTotals(data=df, var1 = "Users", var2="Day", out.csv.fn = "~/GitHub/R_Toolbox/out_FALSE.csv", sort.by.var2.total = FALSE)

# twoway_table

# ---------------------------------------------------------------------------------------------------------------
# Save results as .txt file. 

TwoWayTableWithTotalsTxt <- function(data, var1, var2, out.txt.fn, sort.by.var2.total=c(T,F)){
  
  var1_n <- which(colnames(data)==var1)
  
  var2_n <- which(colnames(data)==var2)
  
  twoway <- table(data[, var1_n], data[, var2_n])
  
  ttwoway_2 <- as.data.frame.matrix(t(twoway))
  
  Total <- colSums(t(ttwoway_2)) # Column totals
  
  cc <- cbind(ttwoway_2, Total)  # Add column totals
  
  ccrr <- rbind(cc, colSums(cc))  # Add row totals 
  
  rownames(ccrr)[length(rownames(ccrr))] <- "Total"  # Change the last rowname to "Total"
  
  ccrr$var_2 <- rownames(ccrr)
  
  # Bring the rownames column to the first column.
  ccrr_2 <- ccrr[, c( ncol(ccrr), 1:ncol(ccrr)-1 )]
  
  # Change "var_2" to the real variable name. 
  colnames(ccrr_2)[1] <- paste(var2) 
  
  if(sort.by.var2.total==F){
    
    # save as a .txt file without sorting.
    write.table(ccrr_2, out.txt.fn, sep="\t", row.names=F, quote=F)
    
    twoway_table <<- ccrr_2
    
    cat("The two-way frequency table has been saved as a dataframe 'twoway_table'.\nThe levels of Variable 2 are in the first column." )
  }else{
    
    # Sort by the rows' totals.
    ccrr_2_s <- ccrr_2[ order(ccrr_2$Total, decreasing=T ), ]
    
    # Bring the total row to the bottom.
    ccrr_2_s_b <- ccrr_2_s[ c(2:nrow(ccrr_2_s), 1), ] 
    
    twoway_table <<- ccrr_2_s_b
    
    cat("The two-way frequency table has been saved as a dataframe 'twoway_table'.\nThe levels of Variable 2 are in the first column." )
    
    # save as a .txt file.
    write.table(ccrr_2_s_b, out.txt.fn, sep="\t", row.names=F, quote=F)
    
  }  
  
}
