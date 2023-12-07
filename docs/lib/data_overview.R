# ========================================================================================
# Functions used in data overview.
# Version 1
# Created on 06/23/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Generate a summary of each variable in a dataframe 
# ========================================================================================

# Calculate minimum, 1st quantile, median, mean, 3rd quantile, max, and standard deviation
# for each variable in the input dataframe.   

  SummaryStats <- function(inputdf, outfn){
    # Create an empty table to save results.
    summarybox <- data.frame(Variables=names(inputdf), 
                             Min=NA, FirstQu=NA, Median=NA, Mean=NA, ThirdQu=NA, Max=NA, SD=NA)
    # Define input.
    input <- inputdf
    
    # For each column (variable), calculate summary statistics. If not numeric, indicate so.
    for(i in 1:ncol(input)){
      
      ith_col <- input[, i]
      
      # if numeric, calculate summary stats.
      if(is.numeric(ith_col)){
        summarybox[i, 2] <- min(     ith_col, na.rm=TRUE)
        summarybox[i, 3] <- quantile(ith_col, 0.25, na.rm=TRUE)
        summarybox[i, 4] <- median(  ith_col, na.rm=TRUE)
        summarybox[i, 5] <- mean(    ith_col, na.rm=TRUE)
        summarybox[i, 6] <- quantile(ith_col, 0.75, na.rm=TRUE)
        summarybox[i, 7] <- max(     ith_col, na.rm=TRUE)
        summarybox[i, 8] <- sd(      ith_col, na.rm=TRUE)
        
      }else{
        
        # if not numeric, say not numeric.
        summarybox[i, 2:8] <- "not_numeric"
      }  
    }
    # Save the summary information.
    write.table(summarybox, file=outfn, sep="\t", row.names=F, quote=F)
  }

# ---------------------------------------------------------------------------------------------------------------
# Generate a line plot with only individuals with all days of data are connected with dots.
# Or do not connect where there is missing data.
# merge out table and extracted variable table

  PrepLinePlot <- function(inputdf, day, username, all.fn, full.days.only.fn, partial.days.only.fn){
    
    df <- inputdf
    
    # Define users and days in your df
    users <- unique(df[, username])
    days  <- order(unique(df[, day]), decreasing = F)
    
    # Make day as a factor.
    df[, day]  <-  factor(df[, day])
    
    # Create a character vector to be "users"
    usersvector = rep(users, length(days))
    
    # Create a character vector to be "days"
    daysvector <- character(0)
    
    k = length(users) 
    
    # Repeat each day as many as there are users.  
    for(i in 1:length(days)){
      if(i==1){daysvector = rep(days[i], k)}else{daysvector = c(daysvector, rep(days[i], k) ) }
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a dataframe with UserName and RecallNo.
    full_user_day <-  data.frame(UserName = usersvector, RecallNo = daysvector)
    # Add a column of user_day
    full_user_day$User_Day = paste(full_user_day$UserName, full_user_day$RecallNo, sep="_")
    # Change the colnames slightly so that .x or .y won't be inserted after merging.
    # Leave "User_Day" as is, because this is needed for merging.
    colnames(full_user_day) <- c("UserName_a", "RecallNo_a", "User_Day")
    
    # Also add a column of user_day to df.
    # df may already have User_Day, but it won't hurt to make it again.
    # df$User_Day = paste(df$UserName, df$RecallNo, sep="_")
    df$User_Day = paste(df[, username], df[, day], sep="_")
    
    # Merge, so that the rows absent in full_user_day will be NA.
    df_w_NA <<- merge(x=full_user_day, y=df, by="User_Day", all.x=TRUE)
    
    # all data with NA inserted
    write.table( x=df_w_NA, file = all.fn, sep="\t", row.names = F, quote=F)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # pick up UserName that has NA values
    NAonlyrows <- df_w_NA[ is.na(df_w_NA[, username]), ]   
    
    # vector of partial users
    partial_users <- unique(NAonlyrows$UserName_a) # UserName_a has actual UserNames.

    # pick up df records of users who are not partial.
    # First, get T or F for each row of df
    partialTF <- df[, username] %in% partial_users
    # Then, take only rows that are marked as T.
    df_partial <- df[partialTF, ]

    # Save users' rows with partial data only
    write.table( x=df_partial, file = partial.days.only.fn, sep="\t", row.names = F, quote=F)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Define "Not in"
    `%!in%` <- Negate(`%in%`)
    
    # First, get T or F for each row of df
    fullonlyTF <-  df[, username] %!in% partial_users
    # Then, take only rows that are marked as T.
    df_fullonly <- df[fullonlyTF, ]
    
    # Save users' rows with full data only
    write.table( x=df_fullonly, file = full.days.only.fn, sep="\t", row.names = F, quote=F)
  
  } 

# ---------------------------------------------------------------------------------------------------------------
  