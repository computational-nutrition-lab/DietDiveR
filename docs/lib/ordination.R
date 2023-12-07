# ========================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 1
# Created on 03/08/2022 by Rie Sadohara
# 06/26/2023 replaced "OTU" with "IFC".
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Prep food data.

  PrepFood <- function(data=food){
    
    # remove the taxonomy column
    food2 <- data[, !colnames(data) == "taxonomy"] 
    
    # transform to matrix, then a phyloseq object
    food_mat <- as.matrix(food2)
    IFC <<- phyloseq::otu_table(food_mat, taxa_are_rows = TRUE)
  } 
  
# ---------------------------------------------------------------------------------------------------------------
# Prep taxonomy (food names) data

  PrepTax <- function(data=tax){  # split taxonomy into 5 levels by default.  need to check if it works with other dataset than
    
    # Make the food description as row names
    row.names(data) <- data$Main.food.description 
    
    # remove the FoodID column
    woFoodID <- data[, !colnames(data) == "FoodID"]

    # Split taxonomy L1, L2, L3 etc. by a semicolon, in lieu of tidyr::separate
    splittax <- strsplit(as.character(tax$taxonomy), split=";")
    # How many levels were created after splitting? 
    max_n_levels <- max(lengths(splittax))
    
    tax1 <- woFoodID
    n <- 1
    
    if(max_n_levels==1){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        n <- n + 1
      }
    }else if(max_n_levels==2){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        n <- n + 1
      }
    }else if(max_n_levels==3){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        n <- n + 1
      }
    }else if(max_n_levels==4){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        tax1[n, 'L4'] <- i[[4]]
        n <- n + 1
      }
    }else if(max_n_levels==5){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        tax1[n, 'L4'] <- i[[4]]
        tax1[n, 'L5'] <- i[[5]]
        n <- n + 1
      }
    }else if(max_n_levels==6){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        tax1[n, 'L4'] <- i[[4]]
        tax1[n, 'L5'] <- i[[5]]
        tax1[n, 'L6'] <- i[[6]]
        n <- n + 1
      }
    }else if(max_n_levels==7){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        tax1[n, 'L4'] <- i[[4]]
        tax1[n, 'L5'] <- i[[5]]
        tax1[n, 'L6'] <- i[[6]]
        tax1[n, 'L7'] <- i[[7]]
        n <- n + 1
      }
    }else{
      cat("The number of levels are beyond the range of 1-6. Please check your input taxonomy file.")
    }
        
    # remove Main.food.description column, because it's already made into row names.
    woFoodID2 <- tax1[, !colnames(tax1) == "Main.food.description"]   

    # Transform to matrix, then to a tax_table object.
    tax_mat <- as.matrix(woFoodID2)
    TAX <<- phyloseq::tax_table(tax_mat)
  }
  
# ---------------------------------------------------------------------------------------------------------------
# Prep metadata.

  PrepMeta <- function(data=meta){
    
    # make UserName as rownames of meta.
    rownames(data) <- data[, "SampleID"]

    # subset metadata to the correct samples.
    # colnames(food) has users.  
    meta2 <- data[colnames(food), ]
    
    # Transform meta2 to sample_data object.
    SAMPLES <<- phyloseq::sample_data(meta2)
  }
  
# ---------------------------------------------------------------------------------------------------------------
# For NHANES data.
  PrepMeta_NHANES <- function(data=meta){

    # subset metadata to the correct samples.
    # colnames(food) has users.  
    meta2 <- data[colnames(food), ]
    
    # Transform meta2 to sample_data object.
    SAMPLES <<- phyloseq::sample_data(meta2)
  }
    
# ---------------------------------------------------------------------------------------------------------------
# prep foodtree.
  PrepTree <- function(data=foodtree){
    
    TREE <<- data
    
    # Replace '_' with spaces in the tree object.
    taxa_names(TREE) <<- gsub('_', ' ', taxa_names(data))
  }

# ---------------------------------------------------------------------------------------------------------------
# Save the percent variance explained as a txt file.
  
  Eigen <- function(eigen.input = eigen_percent, output.fn){
    
    eigen_percentdf <- data.frame(Axis=1:length(eigen.input), Eigen= eigen.input)
    
    write.table(x=eigen_percentdf, file= output.fn, sep="\t", row.names=F)
  }
  
  
# ---------------------------------------------------------------------------------------------------------------
# Merge metadata and Axis values.
  
  MergeAxesAndMetadata <- function(ord.object, number.of.axes, meta.data, output.fn){
    
    # extract all the Axis vectors
    allvectors <- as.data.frame(ord.object["vectors"])
    
    # Remove the suffix 'vectors.' in the column names of 'allvectors'
    colnames(allvectors) <- sub(pattern='vectors.', replacement='', x=colnames(allvectors))
    
    # Extract Axes 1 through the specified axis
    vectors <- allvectors[, 1:number.of.axes]
    
    # make SampleID as rownames of meta. (to use SampleID in rownames for merging)
    rownames(meta.data) <- meta.data[, "SampleID"]
    
    # merge by the rownames.
    meta_usersdf <- merge(x=meta.data, y=vectors, all.y=T, by="row.names", sort=FALSE)
    
    # Save as a txt file.
    write.table(x = meta_usersdf, file= output.fn, sep="\t", row.names= F)
  }

# ---------------------------------------------------------------------------------------------------------------
# For NHANES data. 
  
  MergeAxesAndMetadata_NHANES <- function(ord.object, number.of.axes, meta.data, output.fn){
    
    # extract all the Axis vectors
    allvectors <- as.data.frame(ord.object["vectors"])
    
    # Remove the suffix 'vectors.' in the column names of 'allvectors'
    colnames(allvectors) <- sub(pattern='vectors.', replacement='', x=colnames(allvectors))
    
    # Extract Axes 1 through the specified axis
    vectors <- allvectors[, 1:number.of.axes]
    
    # Merge by the rownames (X89125 etc.).
    meta_usersdf <- merge(x=meta.data, y=vectors, all.y=T, by="row.names", sort=FALSE)
    
    # Save as a txt file.
    write.table(x = meta_usersdf, file= output.fn, sep="\t", row.names= F)
  }
  
 
# ---------------------------------------------------------------------------------------------------------------
# Generate an UNweighted unifrac distance matrix and save it as a txt file.
  
  UnweightedUnifracDis <- function(input.phyloseq.obj = phyfoods, output.fn){
    
    # Calculate unweighted unifrac distance and save as a matrix.
    unweighted_uni_dis1 <- as.matrix(phyloseq::distance(phyfoods, method="unifrac"))
    
    # Convert it to a data frame. (to edit rownames) 
    unweighted_uni_dis2 <- as.data.frame(unweighted_uni_dis1)
    
    # Create a new column with a columnname of "Sample" and put rownames (UserName) on that column 
    unweighted_uni_dis3 <- data.frame("Sample" = rownames(unweighted_uni_dis2), unweighted_uni_dis2)
    
    # Save as a txt file.
    write.table(x = unweighted_uni_dis3, file = output.fn, sep="\t", row.names = F)
  }
  
  
# ---------------------------------------------------------------------------------------------------------------
  # Generate a WEIGHTED unifrac distance matrix and save it as a txt file.
  
  WeightedUnifracDis <- function(input.phyloseq.obj = phyfoods, output.fn){
    
    # Calculate unweighted unifrac distance and save as a matrix.
    weighted_uni_dis4 <- as.matrix(phyloseq::distance(phyfoods, method="wunifrac"))
    
    # Convert it to a data frame. (to edit rownames) 
    weighted_uni_dis5 <- as.data.frame(weighted_uni_dis4)
    
    # Create a new column with a columnname of "Sample" and put rownames (UserName) on that column 
    weighted_uni_dis6 <- data.frame("Sample" = rownames(weighted_uni_dis5), weighted_uni_dis5)
    
    # Save as a txt file.
    write.table(x = weighted_uni_dis6, file = output.fn, sep="\t", row.names = F)
  }
  
  
# ---------------------------------------------------------------------------------------------------------------
# 

  
  
  