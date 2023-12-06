# FUNCTIONS ==============================================================================

# ========================================================================================
# Prep data for PCA and other cluster analysis.
# Version 1
# Created on 01.13.2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Subset the columns to be used and calculate means by a category if desired 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Function to subset specific data from the totals.

  SubsetColumns <- function(data, start.col, end.col){
    # Column Variables of "totals" dataframe.
    colvars <- names(data)
    # Get the first ID
    start.col <- match(start.col, colvars)
    # Get the second ID
    end.col <- match(end.col, colvars)
    # Subset range
    subsetted <<- data[, start.col:end.col]
    # Print what was loaded.
    cat("'subsetted' contains the following", length(colnames(subsetted)), "columns.", "\n")
    print(colnames(subsetted))
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Keep only the columns with non-zero variance in order to perform PCA.
  KeepNonZeroVarColumns <- function(data){
    
    subsetted_non0var <<- data[, which(apply(data, 2, var) != 0)] 
    
    # Print which column(s) were removed.
    
    if(ncol(data) == ncol(subsetted_non0var)){
      
      cat("No columns had zero variance.", "\n")
      cat("The numeric data without ID has", nrow(subsetted_non0var), "rows and", ncol(subsetted_non0var), "variables.\n")
      
      
    }
    if(ncol(data) != ncol(subsetted_non0var)){
      
      cat("The following column(s) in ", deparse(substitute(data)), " had zero variance and were removed.", "\n")
      cat("The numeric data without ID now has", nrow(subsetted_non0var), "rows and", ncol(subsetted_non0var), "variables.\n")
      
      print(which(apply(data, 2, var) == 0))
      
      
    }
  }
# ---------------------------------------------------------------------------------------------------------------


# ========================================================================================
# Collapse variables by correlation: take only 1 variable if 2 ore more are highly 
# correlated with each other. Code from:
# https://github.com/knights-lab/MLRepo/blob/master/example/lib/collapse-features.r
# ========================================================================================
  
# ---------------------------------------------------------------------------------------------------------------
# CLUSTER
# ClusterByCorrelation returns a vector of cluster ids for clusters with internal
  # complete-linkage correlation of min.cor
  # This is to be used in CollapseByCorrelation.
  "ClusterByCorrelation" <- function(x, min.cor=0.75){
    #     library('fastcluster')
    cc <<- cor(x, use='pairwise.complete.obs', method='pear')
    # if(ncol(x) == 379) browser()
    cc_1 <- as.dist(1-cc)
    hc <- hclust(cc_1)
    res <- cutree(hc, h=1-min.cor)
    names(res) <- colnames(x)
    return(res)
  }  

# COLLAPSE  
# CollapseByCorrelation returns a vector of cluster ids for clusters with internal  
# complete-linkage correlation of min.cor
#
# By default, chooses cluster reps as highest-variance member if select.rep.fcn=mean
  "CollapseByCorrelation" <- function(x, min.cor=0.75, 
                                        select.rep.fcn=c('var','mean','lowest.mean',
                                                         'longest.name', 'shortest.name')[2],
                                        verbose=FALSE){
    if(verbose) cat('Clustering', ncol(x), 'features...')
    gr <- ClusterByCorrelation(x, min.cor=min.cor)
    if(verbose) cat('getting means...')
    if(select.rep.fcn == 'mean'){
      v <- apply(x,2,function(xx) mean(xx, na.rm=TRUE))
    } else if(select.rep.fcn == 'lowest.mean'){
      v <- apply(x,2,function(xx) -mean(xx, na.rm=TRUE))
    } else if(select.rep.fcn == 'longest.name'){
      v <- nchar(colnames(x))
    } else if(select.rep.fcn == 'shortest.name'){
      v <- -nchar(colnames(x))
    } else {
      v <- apply(x,2,function(xx) var(xx,use='complete.obs'))
    }
    if(verbose) cat('choosing reps...')
    reps <- sapply(split(1:ncol(x),gr),function(xx) xx[which.max(v[xx])])
    if(verbose)
      cat(sprintf('collapsed from %d to %d.\n',ncol(x), length(reps)))
    return(list(reps=reps, groups=gr))
  }
# ---------------------------------------------------------------------------------------------------------------  

# ---------------------------------------------------------------------------------------------------------------  
# Function to save the correlation matrix as a txt file.
# The correlation matrix (cc) is produced by ClusterByCorrelation().
  SaveCorrMatrix <- function(x=cc, out.fn){
    
    write.table(as.data.frame(as.matrix(x)), out.fn, sep = "\t", 
                row.names=T, col.names=NA, quote=F)
    # col.names=NA inserts a blank colname to the rownames coloumn so that the order of colnames
    # will not be off by one. 
    
  } 
# ---------------------------------------------------------------------------------------------------------------  

# ---------------------------------------------------------------------------------------------------------------  
# Use the functions defined above and prepare input data (totals) for clustering.
# The PrepForClustering function below does:
  # 1: take complete cases in your variables of interest, 
  # 2: save the original totals of the complete cases individuals as a .txt, 
  # 3: keep non-zero columns, 
  # 4: remove the userID,
  # 5: identify correlated variables and remove them,
  # 6: save with uncorrelated variables as a .txt,
  # 7: save correlation matrix as a .txt.  
  
  PrepForClustering <- function(
    input_df,
    original_totals_df,
    userID,
    complete_cases_fn,
    clustering_input_fn,
    corr_matrix_fn){
    
    cat(deparse(substitute(input_df)), "has", nrow(input_df), "rows and", ncol(input_df), "variables.\n")
    
    # Save a vector that contains the number of missing data in each column of the input_df.
    missing.vec <- colSums(is.na(input_df))[order(colSums(is.na(input_df)), decreasing=T)]
    
    # Show only the columns with any missing data.
    cat("The following column(s) in", deparse(substitute(input_df)), "have missing data shown below.\nRows (samples) containing those missing data will be removed.\n ")
    print(missing.vec[missing.vec > 0])
    
    # Take only the rows with no missing data. 
    input_df_c <- input_df[complete.cases(input_df), ]
    
    # Take the rows of the original totals that are also present in "input_df_c".
    original_totals_df_c <- original_totals_df[original_totals_df[, userID] %in% input_df_c[, userID], ]
    
    # Check that those two have exactly the same individuals (with complete data). (for script writers)
    # print(identical(original_totals_df_c[, userID],  input_df_c[, userID])) 
    
    # Save the new selected totals to be used as the original input later in the PCA script.
    write.table(x= original_totals_df_c, file= complete_cases_fn, 
                sep="\t", row.names=F, quote=F)
    
    # Remove the userID column because it is the participants' ID and not appropriate to include in a PCA input.
    input_df_c_wo_ID = input_df_c[, !names(input_df_c) %in% userID]
    
    # This dataset, input_df_c_wo_ID, with no missing data and no userID column, will be the input for the
    # following preparation for clustering.
    
    # ---------------------
    # Pick up only the columns with non-zero variance, in order to run cluster analyses.
    # The removed columns will be shown if any.
    KeepNonZeroVarColumns(data= input_df_c_wo_ID)
    # The output is a df called "subsetted_non0var".

    # ---------------------
    # Collapse variables by correlation: take only one variable if they are highly correlated.
    # Identify correlated variables.
    cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
                                     min.cor = 0.75,
                                     select.rep.fcn = 'mean', verbose = T)
    
    # Save the correlation matrix for record in the results folder.
    # cc is the correlation matrix produced when variables are collapsed by correlation.
    SaveCorrMatrix(x=cc, out.fn= corr_matrix_fn)
    
    # Filter out highly correlated variables from the original dataset.
    selected_variables <- subsetted_non0var[, cbc_res$reps]
    
    cat("After removing correlated variables,", nrow(selected_variables), "rows and", ncol(selected_variables), "variables remained.\n")
    
    # ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
    
    # Save the variables after removing correlated variables, to be used as the input.
    write.table(x= selected_variables, file= clustering_input_fn,
                sep="\t", row.names=F, quote=F)
    
  }
  
# ---------------------------------------------------------------------------------------------------------------  
    
