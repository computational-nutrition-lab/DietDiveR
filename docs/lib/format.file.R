# ========================================================================================
# Replace special characters that will interfere data loading.
# Code from: knights-lab/Food_Tree/R/archived/lib/format_input_files.R
# https://github.com/knights-lab/Food_Tree/blob/master/R/archived/lib/format_input_files.R
# Version 1
# Created on 06/21/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Replace special characters with "_".
# ========================================================================================
# 
# - should only have to run this once on the starting file
# - Input is a text file that contains columns with extra characters that need to be newick friendly
# - Output is a text file with newick friendly text strings for food descriptions
# - columns is the name of the columns that need to be changed
# - Output directory must include the .txt file name

format.file <- function(filename, columns, outfn)
  {
  data0 <- read.table(filename, 
                      sep = "\t", 
                      header = TRUE)
  
  for(i in 1:length(columns))
    {
    col <- columns[i]
    # Formatting column text string to work with newick tree
    
    # Remove any leading or trailing spaces in the food descriptions
    data0[,col]<- gsub("^\\s+|\\s+$", "", data0[,col])
    
    # Remove non-text characters
    data0[,col]<- gsub('[():;,/-]+', "", data0[,col])
    
    # Remove pesky apostropies
    data0[,col]<- gsub("'", '', data0[,col])
    
    # Remove pesky percent symbols
    data0[,col]<-gsub("%", '', data0[,col])
    
    # Replace spaces with underscores
    data0[,col]<- gsub(' ','_', data0[,col])
    
    } 
  
  # If want to overwrite the data, write out to file
  write.table(data0, file=outfn, sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  # # If do not want to overwrite data...
  # data1 <- data0[,c(foodcodecolname, columns)]
  # 
  # # write out to file
  # write.table(data1, file=outfn, sep = "\t", quote = FALSE, row.names = FALSE)
  
  }

# ---------------------------------------------------------------------------------------------------------------
  
