# ===============================================================================================================
# k-means clustering.
# Version 1
# Created on 08/29/2022 by Rie Sadohara
# ===============================================================================================================

# Here, we will run k-means analysis with the NHANES 2-day average totals, subsetted for males, 60-79 years old.

# Create two folders named "males60to79_Nut_k-means" and "males60to79_Cat_k-means" inside "Laboratory_data" 
# to save k-means results of Nutrients and Food Category data, respectively.

# Set your working directory as to the main directory.
  Session --> Set working direHctory --> Choose directory.
  setwd("~/GitHub/DietDiveR")

# Name your main directory for future use.  
  main_wd <- file.path(getwd())

# Load necessary packages and source scripts
  library(ggplot2)
  library(factoextra)
  library(gridExtra)
  library(cluster)

# Set your ggplot2 theme.
  theme_set(theme_bw(base_size = 14))

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")
  source("lib/k-means.R")

# You can come back to the main directory by:
  setwd(main_wd) 
  
# ---------------------------------------------------------------------------------------------------------------
# Specify the directory where your data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/Laboratory_data/")
  
# ===============================================================================================================
# Nutrient data, processed for clustering analyses.
# ===============================================================================================================

# Your input data should be a data frame with uncorrelated variables with non-zero variance and with no 
# missing data.
  nut_kmeansinput <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut_rv.txt",
                                sep="\t", header=T)
  
# Ensure your input file has the correct number of rows and columns.
  dim(nut_kmeansinput)
  
# Scale your input file and name it as k-means_input.
  kmeans_input <- scale(nut_kmeansinput)

# Specify the directory (folder) to save the results. (create a folder named as this if not done so)
  res_dir_nut = "males60to79_Nut_k-means"

# Specify the prefix of filenames to be saved. 
  res_prefix_nut = "males60to79_Nut"

# Run the elbow, silhouette, and gap methods to find an optimum K (number of clusters). 
# Do not alter the name of the input file: kmeans_input. This function below assumes that
# the input is named as "kmeans_input". 
# You can only run those three methods for K = 1 through {number of observations - 1}. 
# The gap method output will be printed on the Console. The gap values are plotted in 
# xxx_gapmethod.pdf.
  ChooseK(out.dir= res_dir_nut, out.prefix= res_prefix_nut)

# The Gap method output on the console may say "Warning message: did not converge in 10 iterations." 
# If this happens, the optimum K suggested may not be as conclusive, but we can proceed for now while
# keeping that in mind.
  
# Look at the three figures generated by the ChooseK function above. The elbow and gap method did not give a 
# distinct peak (though the gap method indicates K=2 might be optimal), but the silhouette methods 
# gave a peak at K=2. 
  
# With specific K values in mind, perform k-means analysis with one specified K.
# Also, change the file name to be saved as a PDF. 
# This uses the factoextra package.  
  OneK(myK= 2, out.dir= res_dir_nut, out.fn = "males60to79_Nut_K2")
  oneKplot

# Or try multiple Ks and print respective biplots in one panel.
# Likewise, change the file name to be saved as a PDF as necessary. 
# This uses the factoextra and gridExtra packages.  
  MultipleK(myKs = c(2,3,4,5), out.dir = res_dir_nut, out.fn = "males60to79_Nut_K2-5")

# ===============================================================================================================
# Food category data, processed for clustering analyses.
# ===============================================================================================================

# Your input data should be a data frame with uncorrelated variables with non-zero variance and with no 
# missing data.
  cat_kmeansinput <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat_rv.txt", 
                                sep="\t", header=T)

# Ensure your input file has the correct number of rows and columns.
  dim(cat_kmeansinput)

# Scale your input file and name it as k-means_input.
  kmeans_input <- scale(cat_kmeansinput)

# Specify the directory (folder) to save the results.
  res_dir_cat = "males60to79_Cat_k-means"

# Specify the prefix of filenames to be saved. 
  res_prefix_cat = "males60to79_Cat"

# Run the elbow, silhouette, and gap methods to find an optimum K (number of clusters). 
# Do not alter the name of the input file: kmeans_input. This function below assumes that
# the input is named as "kmeans_input". 
# You can only run those three methods for K = 1 through {number of observations - 1}. 
# The gap method output will be printed on the Console. The gap values are plotted in 
# xxx_gapmethod.pdf.
  ChooseK(out.dir= res_dir_cat, out.prefix= res_prefix_cat)

# Look at the three figures generated by the ChooseK function above. The elbow and gap method did not give a 
# distinct peak (though the gap method indicates K=2 might be optimal), but the silhouette method 
# gave a peak at K=3. 

# With specific K values in mind, perform k-means analysis with one specified K.
# Also, change the file name to be saved as a PDF. 
  OneK(myK= 3, out.dir= res_dir_cat, out.fn = "males60to79_Cat_K3")
  oneKplot
  
# Or try multiple Ks and print respective biplots in one panel.
# Likewise, change the file name to be saved as a PDF as necessary. 
# This uses the factoextra and gridExtra packages.  
  MultipleK(myKs = c(2,3,4,5), out.dir = res_dir_cat, out.fn = "males60to79_Cat_K2-5")
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)  
  
