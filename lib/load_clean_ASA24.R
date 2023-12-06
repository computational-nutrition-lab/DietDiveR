# ===============================================================================================================
# Functions to load and clean ASA24 data.
# Version 1
# Created on 02/04/2022 by Rie Sadohara
# ===============================================================================================================

# Abby's code: https://github.com/knights-lab/dietstudy_analyses/blob/master/data/diet/raw_and_preprocessed_ASA24_data/lib/Clean_diet_data.R

# ===============================================================================================================
# Add SampleID to the items file.
# ===============================================================================================================

#### To load the input.fn correctly, added quote="" to the 2nd line, read.table. So that quotation marks (whether 
# single-quotation or double-quotation marks) will NOT be used as a separator. 
# Also added colClasses = "character".
AddSampleIDtoItems <- function(input.fn, user.name="UserName", recall.no="RecallNo", 
                                 prefix, out.fn){
  
  # Load your raw items data.
  items_raw <- read.table(input.fn, sep = "\t", header=T, quote="", colClasses = "character") # quote="", colClasses="character" was added here. 
  
  # Create a combination of user and day for merging. 
  items_raw$userxday <- paste(items_raw[, user.name], items_raw[, recall.no], sep="_")
  
  specified_username <- user.name
  specified_recallno <- recall.no
  
  # Create a dataframe that has UserName and RecallNo (Day).
  user_recallno <- items_raw[, c(specified_username, specified_recallno, "userxday")]
  
  # print(head(user_recallno))
  
  # Change the colnames for easier processing. 
  colnames(user_recallno)[1:2] <- c("UserName", "RecallNo")
  
  # Remove duplicates 
  sampleIDtable <- user_recallno[!duplicated(user_recallno), ]
  
  # Sort by Username then day. 
  sampleIDtable_s <- sampleIDtable[order(sampleIDtable$UserName, sampleIDtable$RecallNo) , ]
  
  # Create a SampleID column that shows the combination of user x day.
  sampleIDtable_s$SampleID <- paste(prefix, 
                                    formatC(seq(from=1, to=nrow(sampleIDtable)), width=5, flag="0" ),
                                    sep="")
  # Add SampleID to Items data.
  items_raw_ID_1 <- merge(x=items_raw, y=sampleIDtable_s[, c("SampleID", "userxday")], 
                          all.x=T, by="userxday" )
  
  # Remove the "userxday" column (which is in the first column) as not necessary.   
  items_raw_ID_2 <- items_raw_ID_1[, 2: ncol(items_raw_ID_1)]
  
  # Bring SampleID (currently in the last column) to the first column. 
  items_raw_ID_3 <- items_raw_ID_2[, c(ncol(items_raw_ID_2), 1: (ncol(items_raw_ID_2)-1) ) ]
  
  # print(head(items_raw_ID_3, 2))
  
  # Save it as a .txt file for further processing.
  write.table(items_raw_ID_3, out.fn, sep="\t", row.names=F, quote=F)  
  
}

# Old version of AddSampleIDtoItems, just for record-keeping.
# AddSampleIDtoItems_Old <- function(input.fn, user.name="UserName", recall.no="RecallNo", 
#                                prefix, out.fn){
#   
#   # Load your raw items data.
#   items_raw <- read.table(input.fn, sep = "\t", header=T) 
#   
#   # Create a combination of user and day for merging. 
#   items_raw$userxday <- paste(items_raw[, user.name], items_raw[, recall.no], sep="_")
#   
#   specified_username <- user.name
#   specified_recallno <- recall.no
#   
#   # Create a dataframe that has UserName and RecallNo (Day).
#   user_recallno <- items_raw[, c(specified_username, specified_recallno, "userxday")]
#   
#   # print(head(user_recallno))
#   
#   # Change the colnames for easier processing. 
#   colnames(user_recallno)[1:2] <- c("UserName", "RecallNo")
#   
#   # Remove duplicates 
#   sampleIDtable <- user_recallno[!duplicated(user_recallno), ]
#   
#   # Sort by Username then day. 
#   sampleIDtable_s <- sampleIDtable[order(sampleIDtable$UserName, sampleIDtable$RecallNo) , ]
#   
#   # Create a SampleID column that shows the combination of user x day.
#   sampleIDtable_s$SampleID <- paste(prefix, 
#                                     formatC(seq(from=1, to=nrow(sampleIDtable)), width=5, flag="0" ),
#                                     sep="")
#   # Add SampleID to Items data.
#   items_raw_ID_1 <- merge(x=items_raw, y=sampleIDtable_s[, c("SampleID", "userxday")], 
#                           all.x=T, by="userxday" )
#   
#   # Remove the "userxday" column (which is in the first column) as not necessary.   
#   items_raw_ID_2 <- items_raw_ID_1[, 2: ncol(items_raw_ID_1)]
#   
#   # Bring SampleID (currently in the last column) to the first column. 
#   items_raw_ID_3 <- items_raw_ID_2[, c(ncol(items_raw_ID_2), 1: (ncol(items_raw_ID_2)-1) ) ]
#   
#   # print(head(items_raw_ID_3, 2))
#   
#   # Save it as a .txt file for further processing.
#   write.table(items_raw_ID_3, out.fn, sep="\t", row.names=F, quote=F)  
#   
# }


# ===============================================================================================================
# Calculate totals by hand if any correction was made in Items.
# ===============================================================================================================

  GenerateTotals <- function(inputfn, User.Name='UserName', 
                             Recall.No='RecallNo', outfn){
    
    # Load the input
    items_data <- read.table(inputfn, sep="\t", header=T, quote="")  #### quote="" added.
    #### Here I don't think I need colClasses="character" because columns need to be numeric in order to calculate totals.

    # Get the index number of "FoodAmt" and "A_DRINKS" in dataframe items_data.
    FoodAmt_Index_No  <- grep("FoodAmt", colnames(items_data)) 
    A_DRINKS_Index_No <- grep("A_DRINKS", colnames(items_data)) 
    
    # Subset the necessary columns.
    Items3 <- items_data[, c(User.Name, 
                              Recall.No,
                              names(items_data)[FoodAmt_Index_No:A_DRINKS_Index_No])]
    
    # Change column names to be recognized by the loop below. 
    colnames(Items3)[1:2] <- c('UserName', 'RecallNo') 
    
    # Define variables to calculate Totals for.
    myvar <- names(Items3[, -c(1,2)])
    
    # Create an empty list to store results.
    results <- list()
  
    # Calculate totals of each variable for each combination of User x Date x Occasion. 
    for(i in 1:length(myvar)){
     
      # Subset the three columns - UserName, RecallNo, and the ith variable to calculate totals. 
      subsetted <- Items3[, c('UserName','RecallNo', myvar[i])]
      
      # Compute the sum of ith variable for each combination of "UserName" and "RecallNo". 
      restable <- aggregate(subsetted[, 3] ~ subsetted[, 1] + subsetted[, 2], 
                            data=subsetted, FUN = sum, na.rm=T)
      
      # Rename the columns.
      colnames(restable) <- c('UserName', 'RecallNo', paste(myvar[i]))     
      
      # Create a new variable "User_Day" that has the combinations of UserName_RecallNo.
      restable$User_Day <- paste(restable$UserName, restable$RecallNo, sep = "_")
      
      if(i==1){
        
        # Define the first result as the first item of "results".
        results[[i]] <- restable
        
        # Save it also as "New_totals".
        New_Totals <- restable
        
      }else if(i > 1){
        
        # Take only User_Day and means and save it as the ith item in the results.
        results[[i]]  <- restable[, c(4, 3)]
        
        # Add it to New_Totals.
        New_Totals <- merge(New_Totals, results[[i]], by="User_Day", all=T) 
        # all=T takes care of missing data ... inserts NA for combinations not found
      }
    }
    
    # Save New_Totals as a txt file specified in outfn.
    write.table(New_Totals, file = outfn, sep = "\t", quote = FALSE, row.names = FALSE)
    
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to QC rows of 'totals' by Metadata
# Show which has "yes" in the "Remove" column, and remove them. 
  RemoveRows <- function(data, metadata.file, output.name){
    toberemoved <- subset(metadata.file, Remove=="yes")
    
    cat(nrow(toberemoved), "row(s) below are to be removed:", "\n")
    print(toberemoved) 
    
    # Merge the data and metadata.
    merged <- merge(x=data, y=metadata.file, by="UserName", all.x=T)
    
    # Remove the rows that have "yes" in the "Remove" column.
    selected_data <- subset(merged, Remove!="yes")
    
    # Now can omit the "Remove" column.
    selected_data_2 <- selected_data[, -which(names(selected_data) %in% "Remove")] 
    
    # Save externally.
    write.table(selected_data_2, output.name, sep="\t", row.names=F, quote=F)
  }
# ---------------------------------------------------------------------------------------------------------------
