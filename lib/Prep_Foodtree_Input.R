# ===============================================================================================================
# just take out the five columns that are needed to generate a food tree. no filtering of food items.
# Version 1
# Created on 02/07/2023 by Rie Sadohara
# ===============================================================================================================

# Make a function to just take out the five columns that are needed.
  PrepFoodtreeInput <- function(food_records_fn, out_fn){
  
    inputfooddf <- read.delim(food_records_fn, quote="", colClasses="character")
    
    inputfooddf_sixcol <-inputfooddf[, c("DRXFCSD", "Main.food.description", 
                                         "Old.Main.food.description", "ModCode", "FoodID")] 
    
    write.table(x=inputfooddf_sixcol, file=out_fn, sep="\t", quote=F, row.names=F)
     
  }

# ---------------------------------------------------------------------------------------------------------------
# # how to use the function:
# # ftc means food tree columns.
# PrepFoodtreeInput(food_records_fn = "Food_D12_FC_QC_demo_QCed_leg.txt",
#                   out_fn          = "Food_D12_FC_QC_demo_QCed_leg_ftc.txt")
# 

