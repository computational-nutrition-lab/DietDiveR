# ===============================================================================================================
# Sort OTU table rows (individuals) in alphanumeric order while leaving the taxonomy column intact at the end.
# This food is to be used in ordination.
# Version 1
# Created on 02/14/2023 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Define function.
# ===============================================================================================================

  SortOTUByID <- function(otu.input, outfn.for.corr.axis){
    
    # Load input food OTU table - this is our food OTU data
    food_raw <- read.delim(otu.input, row.names = 1)
    
    # Define the taxonomy column number, which is at the end of food.
    taxonomycolumn <- length(colnames(food_raw))
    
    # Order the column names of food (which is SEQN), except the last column which has taxonomy.
    sortedcolnames_order <- order(colnames(food_raw)[ 1 : taxonomycolumn-1 ])
    
    # Sort the SEQNs, but do not touch the taxonomy column at the end. 
    food <<- food_raw[, c(sortedcolnames_order, taxonomycolumn) ] 
    
    # Save this as .txt for corr.axis.foods.
    write.table(x = food, outfn.for.corr.axis,
                sep="\t", row.names=T, quote=F)
  }

# ---------------------------------------------------------------------------------------------------------------
# Example:
  # # Sort the columnnames of the userID, leaving the last column (taxonomy) intact.
  # # Then, save that dataframe as "food".
  # # Also, save "food" as a .txt file to be used in "correlation between Axes and foods" section.  
  # SortOTUByID(otu.input = "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.otu.txt",
  #             outfn.for.corr.axis = "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.otu_sortedbysample.txt")
  

  
