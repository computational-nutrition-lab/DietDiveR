# ===============================================================================================================
# Prepare data for PCA and other cluster analysis.
# Version 1
# Created on 01/13/2022 by Rie Sadohara
# ===============================================================================================================

# Here, we will prepare ASA24 totals data for PCA and clustering analyses.
# We will need to calculate average dietary data per person across all days (if desired),
# remove variables that have zero variance, and collapse variables by correlation
# (i.e. remove redundancy of variables that are highly correlated).

# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")
  
# Name your main directory for future use.
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")
  source("lib/prep_data_for_clustering.R")

# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Import data and prepare them for analyses.
# ===============================================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name= "eg_data/VVKAJ/")

# There may be some variables that you would like to omit before performing PCA.
# Define which columns to drop.
  drops <- c("FoodAmt", "KCAL", "MOIS")
    
# Load the totals data (for each day, not averaged, but with the individuals in the QC-ed mean totals)
# "_m" stands for "metadata added", not "means".
  totals <- read.table("VVKAJ_Tot_m_QCed.txt", sep= "\t", header= T)
  
# Take only the columns whose names are NOT in the drops vector.
  totals_2 <- totals[ , !(names(totals) %in% drops)]

# Load the averaged and QC-ed totals that has one data per participant with metadata.
  totals_mean <- read.table("VVKAJ_Tot_mean_m_QCed.txt", sep="\t", header=T)  
  
# Take only the columns whose names are NOT in the drops vector. 
  totals_mean_2 <- totals_mean[ , !(names(totals_mean) %in% drops)]
  
  
# ===============================================================================================================
# NUTRIENTS: Use data as is.
# ===============================================================================================================

# Obtain the column numbers for UserName, BMI, start.col="PROT" through end.col="B12_ADD".
  userID_col <- match("UserName", names(totals_2))
  BMI_col   <-  match("BMI"     , names(totals_2))
  start_col <-  match("PROT"    , names(totals_2))
  end_col   <-  match("B12_ADD" , names(totals_2))

# Select the BMI, body weight, and the nutrient variables.
  user_BMI_nut <- totals_2[ , c(userID_col, BMI_col, start_col:end_col)]
  
# Ensure user_BMI_nut has only the selected columns (variables).   
  colnames(user_BMI_nut)
  
# Process this input, user_BMI_nut, for clustering analysis as follows. 
  # 1: Take complete cases in your variables of interest, 
  # 2: Save the original totals of the complete cases individuals as a .txt, 
  # 3: Keep non-zero columns, 
  # 4: Remove the userID,
  # 5: Identify correlated variables and remove them,
  # 6: Save with uncorrelated variables as a .txt,
  # 7: Save correlation matrix as a .txt.  
  
  PrepForClustering(input_df = user_BMI_nut,
                    userID = "UserName",
                    original_totals_df= totals, 
                    complete_cases_fn=   "VVKAJ_Tot_m_QCed_Nut_asis_c.txt",
                    clustering_input_fn= "VVKAJ_Tot_m_QCed_Nut_asis_c_rv.txt",
                    corr_matrix_fn=      "VVKAJ_Tot_m_QCed_Nut_asis_c_corr_matrix.txt")

# ===============================================================================================================
# NUTRIENTS: Take average of each user across all days 
# ===============================================================================================================

# Obtain the column numbers for UserName, BMI, start.col="PROT" through end.col="B12_ADD"  in totals_mean_2.
  UserName_col <- match("UserName" , names(totals_mean_2)) 
  BMI_col   <-    match("BMI"      , names(totals_mean_2)) 
  start_col <-    match("PROT"     , names(totals_mean_2))  
  end_col   <-    match("B12_ADD"  , names(totals_mean_2)) 
  
# Select the BMI, body weight, and the nutrient variables.
  m_user_BMI_nut <- totals_mean_2[ , c(UserName_col, BMI_col, start_col:end_col)]

# Process this input for clustering analyses.
  PrepForClustering(input_df = m_user_BMI_nut,
                    userID = "UserName",
                    original_totals_df= totals_mean, 
                    complete_cases_fn=   "VVKAJ_Tot_mean_m_QCed_Nut_ave_c.txt",
                    clustering_input_fn= "VVKAJ_Tot_mean_m_QCed_Nut_ave_c_rv.txt",
                    corr_matrix_fn=      "VVKAJ_Tot_mean_m_QCed_Nut_ave_c_corr_matrix.txt")

# ===============================================================================================================
# FOOD CATEGORIES: Use data as is.
# ===============================================================================================================

# Obtain the column numbers for BMI, UserName, start.col="F_TOTAL" through end.col="A_DRINKS".
  userID_col <- match("UserName" , names(totals_2))
  BMI_col   <-  match("BMI"      , names(totals_2))
  start_col <-  match("F_TOTAL"  , names(totals_2))
  end_col   <-  match("A_DRINKS" , names(totals_2))
  
# Select the BMI, body weight, and the nutrient variables.
  user_BMI_cat <- totals_2[ , c(userID_col, BMI_col, start_col:end_col)]

# Process this input for clustering analyses.
  PrepForClustering(input_df = user_BMI_cat,
                    userID = "UserName",
                    original_totals_df= totals, 
                    complete_cases_fn=   "VVKAJ_Tot_m_QCed_Cat_asis_c.txt",
                    clustering_input_fn= "VVKAJ_Tot_m_QCed_Cat_asis_c_rv.txt",
                    corr_matrix_fn=      "VVKAJ_Tot_m_QCed_Cat_asis_c_corr_matrix.txt")
  
  
# ===============================================================================================================
# FOOD CATEGORIES: Take average of each user across all days
# ===============================================================================================================

# Obtain the column numbers for UserName, BMI, start.col="F_TOTAL" through end.col="A_DRINKS" in totals_mean_2.
  UserName_col <- match("UserName" , names(totals_mean_2)) 
  BMI_col   <-    match("BMI"      , names(totals_mean_2)) 
  start_col <-    match("F_TOTAL"     , names(totals_mean_2))  
  end_col   <-    match("A_DRINKS"  , names(totals_mean_2)) 
  
# Pick up the BMI, body weight, and the nutrient variables.
  m_user_BMI_cat <- totals_mean_2[ , c(UserName_col, BMI_col, start_col:end_col)]

# Process this input for clustering analyses.
  PrepForClustering(input_df = m_user_BMI_cat,
                    userID = "UserName",
                    original_totals_df= totals_mean, 
                    complete_cases_fn=   "VVKAJ_Tot_mean_m_QCed_Cat_ave_c.txt",
                    clustering_input_fn= "VVKAJ_Tot_mean_m_QCed_Cat_ave_c_rv.txt",
                    corr_matrix_fn=      "VVKAJ_Tot_mean_m_QCed_Cat_ave_c_corr_matrix.txt")
  
# ===============================================================================================================
# Come back to the main directory
  setwd(main_wd) 
  
