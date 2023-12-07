# ===============================================================================================================
# Version 1
# Created on 02/16/2023 by Rie Sadohara
# ===============================================================================================================

# ---------------------------------------------------------------------------------------------------------------
# From sorted food IFC table, generate a table of total amount of food consumed by all the individuals,
# and a table with correlation coefficients, p-values, and q-values with desired threshold between
# food items and Axes that were saved in the ordination section.
# Be careful about not to confuse WEIGHTED and UNweighted unifrac distances

# This function uses SubsetByFirstChaInCol and create_corr_frame functions. Load them in advance.
  source("lib/SubsetByFirstChaInCol.R") 
  source("lib/create_corr_frame.R") 

CorrAxesFood <- function(food_ifc_soted,
                         AmountSums_out_fn,
                         meta_users,
                         qval_threshold = 0.05,
                         corr_axes_foods_outfn  ){
  
  # Read in the food_ifc_soted.
  food1 <- read.delim(food_ifc_soted)
  
  # remove "taxonomy" column at the end of food1.
  food2 <- food1[, !colnames(food1) == "taxonomy"] 
  
  # transpose food2 so that rows will be the individuals (for which a distance matrix is calculated)     
  food3 <- as.data.frame(t(food2))
  
  # sort individuals (rows) in order.
  food3_s <- food3[order(rownames(food3)), ]
  
  x <- food3_s
  
  # Save the total amount consumed by all the individuals. 
  write.table(x=as.data.frame(colSums(x)), 
              file = AmountSums_out_fn, sep="\t", row.names=T, quote=F )
  
  # Load the meta_users, which has userID and Axis values.
  loaded_leg <- read.table(meta_users, sep="\t", header=T)  
  
  # Add rownames: X83732 etc. This will stay even after selecting only Axis. columns. 
  # rownames(loaded_leg_u) <- paste("X", loaded_leg_u$SEQN, sep="")
  rownames(loaded_leg) <- loaded_leg$Row.names
  
  # pick up only columns whose names start with "Axis.".
  loaded_leg_Axisonly <- SubsetByFirstChaInCol(input.df = loaded_leg, starting.str = "Axis.")
  
  y <- as.data.frame(loaded_leg_Axisonly)  # food group values.
  
  # make sure the samples are the same.
  if( identical(rownames(x), rownames(y)) == F){
    
    return("The columnnames of X and Y are different. Ensure your food_ifc_soted and\n 
                       meta_users have the same set of individuals.")
    
  }else{
    
    # Now test correlation of each of the columns in x with columns in y. This will take several minutes.
    # The variables (food items) that have been tested will be printed out in the console.
    dat <- create_corr_frame(x, y)
    
    # Change column names of x and y to more meaningful ones.
    colnames(dat)[1:2] <- c("Food","Axis")
    
    # Mark rows that have qvalues < 0.25 with an asterisk in a column called "Significance".
    dat$Significance <- cut(dat$qval, breaks=c(-Inf, qval_threshold, Inf), label=c("*", ""))
    
    # Sort dat by qval (small to large). 
    dat_s <- dat[order(dat$qval), ]
    
    # Save.
    write.table(x=dat_s, file = corr_axes_foods_outfn, sep="\t", row.names=F, quote=F)
    
  } 
  
}

# ---------------------------------------------------------------------------------------------------------------
# test
#   SpecifyDataDirectory("eg_data/VVKAJ/Ordination/")
#   
# # From sorted food IFC table, generate a table of total amount of food consumed by all the individuals, 
# # and a table with correlation coefficients, p-values, and q-values with desired threshold between 
# # food items and Axes that were saved in the ordination section. 
# # Be careful about not to confuse WEIGHTED and UNweighted unifrac distances   
#   CorrAxesFood(food_ifc_soted = "../Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted.txt", 
#                AmountSums_out_fn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_AmountSums.txt",
#                qval_threshold = 0.05,
#                meta_users =            "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt",
#                corr_axes_foods_outfn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")
# 
#   # food_ifc_soted:     xxx.food.ifc.sorted.txt file, saved in the ordination section.
#   # AmountSums_out_fn:  output filename to be saved which has the total consunption amount of each food.
#   # qval_threshold:     q-value threshold to call a correlation significant.
#   # meta_users:         xxx.meta_users.txt file, waved in the ordination section.
#   # corr_axes_foods_outfn: output filename to be saved which has the correlation between foods and Axees.
# 

  

  
