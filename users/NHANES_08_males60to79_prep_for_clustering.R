# ===============================================================================================================
# Prepare NHANES males 60-79 years old data for PCA and other cluster analysis.
# Version 1
# Created on 12/01/2022 by Rie Sadohara
# ===============================================================================================================

# Here, we will prepare the subsetted NHANES totals with males 60-79 years old for PCA and clustering analyses.  
# Unlike ASA24, where we had a choice of using averages across days or treating each day separately, we will 
# have 2-day averages of totals data with NHANES because the NHANES data was collected for only two days. 
# 
# In this script, we will prepare the totals data by removing variables that have zero variance and 
# collapsing variables by correlation (i.e. removing redundancy of variables that are highly correlated).

# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Load the necessary functions 
  source("lib/specify_data_dir.R")
  source("lib/prep_data_for_clustering.R")

# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Load the data and omit some variables if desired.
# ===============================================================================================================
  
# Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data/")

# Load the totals data that have males 60-79 years old.   
  totals_males60to79 <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79.txt", 
                                   sep="\t", header=T)

# It should have 237 individuals (rows) and 266 variables (columns).
  dim(totals_males60to79)

# There may be some variables that you would like to omit before performing PCA.
# Define which columns to drop.
  drops <- c("KCAL","GRMS", "MOIS", "NoOfItems")

# Take only the columns whose names are NOT in the drops vector. 
  totals_males60to79_2 <- totals_males60to79[ , !(names(totals_males60to79) %in% drops)]
 
# ===============================================================================================================
# NUT: Nutrients and body weight
# ===============================================================================================================
# Prepare input dataset for clustering with nutrients data and BMI of the participants.
# We are interested in BMI because the GLU_index groups had different BMI. Add any other variables of interest
# that you would like to include in the PCA.
  
# For NHANES, the last nutrient variables is P226, not B12_ADDED as in ASA24. 

# Obtain the column numbers for start.col="PROT" through end.col="P226" plus "BMXBMI" and "SEQN".
  SEQN_col  <- match("SEQN"  , names(totals_males60to79_2)) 
  BMI_col   <- match("BMXBMI" , names(totals_males60to79_2)) 
  start_col <- match("PROT"   , names(totals_males60to79_2))  
  end_col   <- match("P226"   , names(totals_males60to79_2)) 
  
# Select the BMI, body weight, and the nutrient variables.
  user_BMI_nut <- totals_males60to79_2[ , c(SEQN_col, BMI_col, start_col:end_col)]

# Process this input, user_BMI_nut, for clustering analysis as follows. 
  # 1: Take complete cases in your variables of interest, 
  # 2: Save the original totals of the complete cases individuals as a .txt, 
  # 3: Keep non-zero columns, 
  # 4: Remove the userID,
  # 5: Identify correlated variables and remove them,
  # 6: Save with uncorrelated variables as a .txt,
  # 7: Save correlation matrix as a .txt.  

  PrepForClustering(input_df = user_BMI_nut,
                  userID = "SEQN",
                  original_totals_df= totals_males60to79, 
                  complete_cases_fn=   "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut.txt",
                  clustering_input_fn= "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut_rv.txt",
                  corr_matrix_fn=      "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut_corr_mat.txt")

 
# ===============================================================================================================
# CAT: Food category and body weight
# ===============================================================================================================
# Do the same preparation for the food category data.

# Obtain the column numbers for start.col="F_CITMLB" through end.col="A_DRINKS" plus "BMXBMI" and "SEQN".
  SEQN_col  <- match("SEQN"     , names(totals_males60to79_2)) 
  BMI_col   <- match("BMXBMI"   , names(totals_males60to79_2)) 
  start_col <- match("F_CITMLB" , names(totals_males60to79_2))  
  end_col   <- match("A_DRINKS" , names(totals_males60to79_2)) 
  
# Select the BMI, body weight, and the nutrient variables.
  user_BMI_cat <- totals_males60to79_2[ , c(SEQN_col, BMI_col, start_col:end_col)]

# Prep. 
  PrepForClustering(input_df = user_BMI_cat,
                    userID = "SEQN",
                    original_totals_df= totals_males60to79, 
                    complete_cases_fn=   "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat.txt",
                    clustering_input_fn= "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat_rv.txt",
                    corr_matrix_fn=      "QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat_corr_mat.txt")
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)   
  
  