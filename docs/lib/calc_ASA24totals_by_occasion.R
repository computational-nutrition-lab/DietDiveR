# ========================================================================================
# Calculate totals by occasion, user, and day. 
# useful for analyzing dietary intake per occasion (breakfast, lunch, etc..).
# Version 1
# Created on 03/16/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Header 1 -- explain the purpose of this section.
# ========================================================================================
# 
# This produces a dataframe called Items_by_User_Occ.
SumByOccasion <- function(items.data, User.Name, 
                          Recall.No, Occ.No){
  
  # Get the index number of "FoodAmt" in dataframe Items_raw.
  FoodAmt_Index_No  <- grep("FoodAmt", colnames(items.data)) 
  A_DRINKS_Index_No <- grep("A_DRINKS", colnames(items.data)) 
  
  # Subset necessary columns.
  Items_raw2 <<- items.data[, c(User.Name, 
                                Recall.No, 
                                Occ.No, 
                                names(items.data)[FoodAmt_Index_No:A_DRINKS_Index_No] )]
  
  # Change column names so that it will be recognized in the for loop below.
  colnames(Items_raw2)[1:3] <<- c('UserName','RecallNo', 'Occ_No')
  
  # Define variables to calculate means for.
  myvar <- names(Items_raw2[, -c(1,2,3)])
  myvar
  # Create an empty list to store results.
  results <- list()
  
  # Calculate totals of each variable for each combination of User x Date x Occasion.
  for(i in 1:length(myvar)){
    if(i==1){
      subsetted <- Items_raw2[, c('UserName','RecallNo', 'Occ_No', myvar[i])]
      restable <- aggregate(subsetted[, 4] ~ subsetted[, 1] + subsetted[, 2] + subsetted[, 3],
                            data=subsetted, FUN = sum)
      colnames(restable) <- c('UserName', 'RecallNo', 'Occ_No', paste(myvar[i]))
      restable$User_Day_OccNo <- paste(restable$UserName, restable$RecallNo, restable$Occ_No, sep = "_")
      results[[i]] <- restable
      Items_by_User_Occ <<- restable
    }else if(i>1){
      subsetted <- Items_raw2[, c('UserName','RecallNo', 'Occ_No', myvar[i])]
      restable <- aggregate(subsetted[, 4] ~ subsetted[, 1] + subsetted[, 2] + subsetted[, 3],
                            data=subsetted, FUN = sum)
      colnames(restable) <- c('UserName', 'RecallNo', 'Occ_No', paste(myvar[i]))
      restable$User_Day_OccNo <- paste(restable$UserName, restable$RecallNo, restable$Occ_No, sep = "_")
      restable_sub <- restable[, c(5, 4)]  # take only User_Day_OccNo and means.
      results[[i]] <- restable_sub
      Items_by_User_Occ <<- merge(Items_by_User_Occ, results[[i]], by="User_Day_OccNo", all=T)
      # all=T takes care of missing data ... inserts NA for combinations not found
    }
  }
}


# -----------------------------------------------------------------------------------------  
#  A separate function to pick up UserName, Day, Occ_No, Occ_Name to match Occ No. and Occ_names_in_word.

# Get the type of occasion (breakfast, just a drink etc.) by user & by occasion.
AddOccNames <- function(items.data, User.Name='UserName', 
                        Recall.No='RecallNo', Occ.No='Occ_No', Occ.Name='Occ_Name'){
  
  subsetted2 <<- items.data[, c(User.Name,  Recall.No,  Occ.No,  Occ.Name)]
  
  # Get Occ_Name (which is numeric) for each occasion. Taking mean because Occ_Name is the same 
  # for the same occasion.
  Occ_Names <<- aggregate(subsetted2[, 4] ~ subsetted2[, 1] + subsetted2[, 2] +  subsetted2[, 3],
                          data=items.data,
                          FUN=mean)
  
  colnames(Occ_Names) <<- c('UserName', 'RecallNo', 'Occ_No', 'Occ_Name')
  
  # Add a unique ID for each line for merging.
  Occ_Names$User_Day_OccNo <<- paste0(Occ_Names$UserName, "_",
                                      Occ_Names$RecallNo, "_",
                                      Occ_Names$Occ_No)
  
  # Take only User_Day_OccNo and Occ_Name.
  Occ_Names_2 <<- Occ_Names[, c("User_Day_OccNo", "Occ_Name")]
  
  # Make a reference table that has Occ_Name and corresponding types.
  Occ_Names_ref <<- data.frame(Occ_Name=seq(1:8),
                               Occ_In_Words=c("Breakfast", "Brunch",
                                              "Lunch",     "Dinner",
                                              "Supper",    "Snack",
                                              "Just a drink", "Just a supplement" ))
  
  # Combine!
  # Match the Occ_Names with the Occ_Names_ref (~VLOOKUP)
  Occ_Names_and_Words <<- merge(x=Occ_Names_2, y=Occ_Names_ref, by="Occ_Name", all.x=T)
  
  # Combine the 2 tables so that the sums of each occasion, occ numbers, and occ names in word
  #  will be in one table.
  Sum_by_User_Day_Occ <<- merge(x=Items_by_User_Occ, y=Occ_Names_and_Words, by='User_Day_OccNo', all.x=T)
}    
# -----------------------------------------------------------------------------------------    
