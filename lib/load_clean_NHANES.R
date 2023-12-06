# ========================================================================================
# Functions to load NHANES 2015-16 data and take random samples.
# Version 1
# Created on 04/13/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import NHANES 2015-16 data. 
# ========================================================================================
# 
# Import NHANES data using the SASexport package.
# install.packages("SASxport")
  library(SASxport)
  library(foreign)


# ========================================================================================
# Load and prepare food code table. 
# ========================================================================================

# Load food items and bring the food description to the first column.

  PrepareFoodCodeTable <- function(raw.food.code.table, out.fn){
  
    codetable <- read.xport(raw.food.code.table)
    
    # Replace symbols (/, \, ', #, &) in DRXFCSD column that cannot be loaded correctly 
    # codetable[, "DRXFCSD"] <- gsub("\"", "_", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("'", "_", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("#",  "_", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("&",  "and", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("%",  "_", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("/",  "_", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("\"", "_", codetable[, "DRXFCSD"])
  
    # Replace symbols (/, \, ', #, &) in DRXFCLD column that cannot be loaded correctly 
    # codetable[, "DRXFCLD"] <- gsub("\"", "_", codetable[, "DRXFCLD"])
    codetable[, "DRXFCLD"] <- gsub("'", "_", codetable[, "DRXFCLD"]) 
    codetable[, "DRXFCLD"] <- gsub("#",  "_", codetable[, "DRXFCLD"])
    codetable[, "DRXFCLD"] <- gsub("&",  "and", codetable[, "DRXFCLD"])
    codetable[, "DRXFCLD"] <- gsub("/",  "_", codetable[, "DRXFCLD"])
    codetable[, "DRXFCLD"] <- gsub("%",  "_", codetable[, "DRXFCLD"])
    codetable[, "DRXFCLD"] <- gsub("\"", "_", codetable[, "DRXFCLD"])
  
    # Save as a txt file.   
    write.table(codetable, out.fn, sep="\t", row.names=F)
  
  }

# ========================================================================================
# Load Food items data and add food descriptions.  
# ========================================================================================

# Load food items and bring the food description to the first column.
 
  ImportNHANESFoodItems <- function(data.name, food.code.column, food.code.table, out.fn){
    
    nhanes1516_items <- read.xport(data.name)
    
    # make the rownames as a column called originalrownames
    nhanes1516_items$originalrownames <- rownames(nhanes1516_items)
    
    # Add a sequential number to sort the rows after merging.
    nhanes1516_items$id <- 1:nrow(nhanes1516_items)
    
    # Make a copy of the food code column as integer at the end of nhanes1516_items 
    nhanes1516_items$Food_code <- as.integer(nhanes1516_items[, food.code.column])
    
    # Bring the food code to the first column for merging. 
    nhanes1516_items_s <- nhanes1516_items[, c(length(colnames(nhanes1516_items)), 
                                               1:length(colnames(nhanes1516_items))-1)]
    
    # Load the text file with food code and descriptions. 
    codetable <- food.code.table
    
    # Change the first column name (DRXFDCD) to "Food_code" for merging.
    colnames(codetable)[1] <- "Food_code"
    
    # Make the Food code as integer here, too. 
    codetable$Food_codeint <- as.integer(codetable$Food_code)  
    
    # Merge the NHANES data and codetable.
    nhanes1516 <- merge(x=nhanes1516_items_s, y=codetable, 
                        by="Food_code", all.x=T)        # all.x=T matches all the rows in the 1st dataframe.
    
    # Merging sorts the dataframe by Food_code, so   
    # sort back by the id (the original order of nhanes1516_raw)
    nhanes1516_s <- nhanes1516[order(nhanes1516$id), ]  
    # Food description added! DRXFCSD is short descriptions, and DRXFCLD are long descriptions.
    
    # Save it as a txt file with the specified name. 
    write.table(nhanes1516_s, out.fn, sep="\t", row.names=F, quote=F)
    
  }
  
# ========================================================================================
# Add food category and quantity based on FPED and nutrition info on food data.   
# ========================================================================================
  
  # Matrix Mutiplication function to add food category and multiply the weight and serving for each row.
  AddFoodCat <- function(input.food, fped, grams, out.fn){     # grams are "DR1IGRMS" or "DR2IGRMS" etc..
    
    # Pick up only the foodcode and grams.
    columnstopick <- c("Food_code", grams)
    FPED <- fped
    
    Fdcd_GRMS <- input.food[, columnstopick]
    n_food_cat <- length(colnames(FPED))-1  # Define the number of food categories.
    
    # Create temporary dataframes for procesing.
    dfA <- Fdcd_GRMS                                      # keep all because all match in FPED
    dfB <- FPED[FPED$Food_code %in% Fdcd_GRMS$Food_code,] # keep only the matching codes
    # rearrange dataframes to match on food_code
    dfA2 <- cbind(dfA, rep(dfA[2], n_food_cat-1))         # repeat the DR2IGRMS values in table A
    tabA <- as.matrix(dfA2[, -1])                           # convert to matrix, leave out food code
    rownames(tabA) <- paste0(dfA2$Food_code, '_', dfA2[,2])  # rownames will be FOODCODE_GRAMS.
    colnames(tabA) <- colnames(dfB)[2:(n_food_cat+1)]        # colnames will be food category names. 
    
    dfB2 <- dfB[match(dfA$Food_code, dfB$Food_code),]      # match table B to table A
    tabB <- as.matrix(dfB2[,-1])                           # convert to matrix, leave out food code
    rownames(tabB) <- dfB2$Food_code
    
    # safety check: food codes match - un-commnt if editing.
    # print(identical(dim(tabA), dim(tabB)))
    # print(identical(gsub("_.*", "", rownames(tabA)), rownames(tabB)))
    # print(identical(colnames(tabA), colnames(tabB)))
    
    # Then we can perform matrix multiplication (element-wise)
    #   I thought this would be advantageous as you just want to multiply one value
    #   by a matching row in another table
    out <- tabA * tabB/100
    
    # reformatting to get a clean table
    foodcode_grams_df <- data.frame(Food_code = gsub("_.*", "", rownames(out)),
                                    GRMS = as.numeric(gsub(".*_","", rownames(out))))  # use a temporary column name
    
    # Change the temporary columnname "GRMS" to the specified 'grams' argument.
    colnames(foodcode_grams_df)[2] <- paste(grams) 
    
    # Convert to a df.
    out <- data.frame(out)
    # Number the rows.
    rownames(out) <- 1:nrow(out)
    
    # Join 
    out_df <- cbind(foodcode_grams_df, out)
    
    # Exclude the first 2 columns.
    dropcol = c("Food_code", grams)
    result_1 <- out_df[, !(names(out_df) %in% dropcol)]
    
    # Join Food_Dx and result_v.
    Food_DX_FC = cbind(input.food, result_1)
    
    # Save as a txt file.
    write.table(Food_DX_FC, out.fn, sep="\t", row.names=F, quote=F)
    
  }

# ========================================================================================
# Calculate total for day 1 and day 2, then merge those two dataframes.
# ========================================================================================

# Input - food12d, that you produced in the previous section. 
# This function requires food12d has the "SEQN", "Day" variables.

  TotalNHANES <- function(food12d= food12d, 
                          first.val= "GRMS", last.val= "A_DRINKS", 
                          outfn ){
    
    # Calculate total for day 1. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    food12d_d1 <- subset(food12d, Day==1) 
    
    # Sum nutrients and food categories.
    start_col_num <- match(first.val, names(food12d_d1))  # The number of column that matches the first variable specified.
    end_col_num <-   match(last.val, names(food12d_d1))   # The number of column that matches the last variable specified.
    
    # Sum food items by SEQN from start through end columns.
    total1 <- aggregate(food12d_d1[, start_col_num:end_col_num], 
                        by=list(food12d_d1$SEQN), 
                        FUN=sum)
    
    # Add a Day variable to total1.
    total1$Day <- 1
    
    # Rename the first column.
    colnames(total1)[1] <- "SEQN"
    
    # Create a vector of number of food items reported by each participant.
    n_items1 <- as.data.frame(table(food12d_d1$SEQN))
    colnames(n_items1) <- c("SEQN", "NoOfItems")
    
    # Add it to total1
    total1b <- merge(x=total1, y=n_items1, by="SEQN", all.x=T)
    
    # Calculate total for day 2. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    food12d_d2 <- subset(food12d, Day==2)
    
    # Sum food items and nutrients by SEQN from start through end columns.
    start_col_num <- match(first.val, names(food12d_d2))  # The number of column that matches the first variable specified.
    end_col_num <-   match(last.val, names(food12d_d2))   # The number of column that matches the last variable specified.
    
    total2 <- aggregate(food12d_d2[, start_col_num:end_col_num],
                        by= list(food12d_d2$SEQN),
                        FUN= sum)
    
    total2$Day <- 2
    colnames(total2)[1] <- "SEQN"
    
    # Create a vector of number of food items reported by each participant.
    n_items2 <- as.data.frame(table(food12d_d2$SEQN))
    colnames(n_items2) <- c("SEQN", "NoOfItems")
    
    # Add it to total1
    total2b <- merge(x=total2, y=n_items2, by="SEQN", all.x=T)
    
    # Check if all the columnnames match.
    identicalYN <- identical(colnames(total1b), colnames(total2b))
    
    # print(identicalYN)
    
    if(identicalYN == "TRUE"){
      # Merge totals day 1 and day 2
      total12c <- rbind(total1b, total2b)
      
      # Save the calculated totals of day 1 and 2 as a txt file.
      write.table(total12c, outfn, sep="\t", row.names=F, quote=F)
      
    }else{
      
      print("Cannot merge Day1 total and Day2 total because they do not have identical columns.")      
      
    }
  }

  
# ========================================================================================
# Take average of Day 1 and Day 2.
# ========================================================================================
# [NOTE] Now the average of No of Items can be calculated, too.
  
  AverageTotalNHANES <- function(total12d = total12d, 
                                 first.val= "GRMS", last.val= "NoOfItems", 
                                 outfn){
    
    start_col_num <- match(first.val, names(total12d))  # The number of column that matches the first variable specified.
    end_col_num <-   match(last.val,  names(total12d))  # The number of column that matches the last variable specified.
    
    # Take average by SEQN from start through end columns.
    meantotal12a <- aggregate(total12d[, start_col_num:end_col_num], 
                              by=list(total12d$SEQN), 
                              FUN=mean) 
    
    # Remove the day, which is now all 1.5 (the average of 1 and 2.)
    meantotal12 <- meantotal12a[, !names(meantotal12a) %in% "Day"]
    
    # Change "Group.1" to "SEQN".
    colnames(meantotal12)[1] <- "SEQN"
    
    # Save meantotals as a txt file.
    write.table(meantotal12, outfn, sep="\t", row.names=F, quote=F)
    
  }    


# ========================================================================================
# Take a random subsample.   
# ========================================================================================

RandomSample <- function(data, n, out.fn){
  
  # Define your whole dataset. 
  wholedata <- data
  
  if(n > length(unique(wholedata$SEQN))){
    cat("Error: n is larger than the number of original data (", 
        length(unique(Food_D1$SEQN)),").\nPlease specify a smaller n to sample.", sep="")
  }else{
    # Choose n random samples of participant ID
    subsetusers <- sample(unique(wholedata$SEQN), n)
    
    # Subset only those found in the chosen ID list.
    nhanes_sub1 <<- wholedata[wholedata$SEQN %in% subsetusers, ]
    
    # Confirm the desired number of participants were selected.
    cat( length(unique(nhanes_sub1$SEQN)), " participants were subsampled.", sep = "") 
    
    # Save the samples as a txt file with the specified name. 
    write.table(nhanes_sub1, out.fn, sep="\t", row.names=F, quote=F)
    
  }
}

