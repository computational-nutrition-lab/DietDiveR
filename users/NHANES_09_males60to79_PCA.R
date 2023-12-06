# ===============================================================================================================
# Perform PCA analysis with NHANES males 60-79 years old.
# Version 1
# Created on 12/01/2022 by Rie Sadohara
# ===============================================================================================================

# In this script, we will perform Principal Component Analysis (PCA) with the NHANES 2-day average totals,
# subsetted for males, 60-79 years old.

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

  # Load necessary packages and source scripts
  library(ggplot2)
  library(ggfortify)
  
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")
  source("lib/PCA.R")
  
# You can come back to the main directory by:
  setwd(main_wd)

# Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data")
  
# Create two folders named "males60to79_Nut_PCA" and "males60to79_Cat_PCA" inside "Laboratory_data" 
# to save PCA results of Nutrients and Food Category data, respectively.
  
# ===============================================================================================================
# PCA with nutrients and BMI
# ===============================================================================================================

# Your input data should be a dataframe with uncorrelated variables with non-zero variance and with no missing data. 
  pca_input <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut_rv.txt", 
                          sep="\t", header=T)

# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  colnames(pca_input)
  
# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x= pca_input, scale= TRUE)   

# Specify the directory (folder that you created in "Laboratory_data") to save the results.
  res_dir_Nut = "males60to79_Nut_PCA" 

# Specify the prefix of filenames to be saved. 
  res_prefix_Nut = "males60to79_Nut_fn"

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
  OutputPCA(pca.data= pca_input, pca.result= scaled_pca, 
             out.dir= res_dir_Nut, out.prefix= res_prefix_Nut)
  
# Now, output .pdf and .txt files should be found in your folder specified as in res_prefix_Nut.

# Combine the input with all the variables and the PC results. 
# "input" is your items/totals input file with no missing data in your variables, but including the ones that 
# are highly correlated and therefore were removed when processed into clustering input; however, these 
# variables are going to be included here so that we will be able to take a look at all the variables and 
# see which of them are correlated with the PCA axes and metadata etc.
  SaveInputAndPCs(input="QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut.txt", 
                  pca.results = scaled_pca, 
                  out.dir= res_dir_Nut, out.prefix= res_prefix_Nut)

# [NOTE] Even though the input file has both nutrients (Nut) and food categories (Cat) data,  
# PCA was done with only either Nut or Cat, not both.

# ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - GLU_index, in this case. 
  
# Load the totals_males60to79 data with no missing data in your variables of interest.
  totals_males60to79 <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Nut.txt", 
                                   sep="\t", header=T)
  
# Change GLU_index to a factor so that factors will be displayed in order.
  totals_males60to79$GLU_index <- factor(totals_males60to79$GLU_index, 
                                         levels= c("Normal", "Prediabetic", "Diabetic"))

# Use the autoplot function. Specify which PC to display in the x and y arguments.
# The 'data' argument needs the original input for PCA, not after selecting uncorrelated variables.
  food_Nut_PCA <- autoplot(scaled_pca, x=1, y=2,    
                           loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
                           data = totals_males60to79,  size= 3 ) +     
    geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= GLU_index)) +
    theme_bw(base_size = 12) + theme(aspect.ratio = 1) +  
    theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual( values= c("steelblue3", "yellow", "hotpink")) 
  food_Nut_PCA

  ggsave("males60to79_Nut_PCA/males60to79_c_Nut_PCA_by_GLU_index_PC12.pdf", 
         food_Nut_PCA, device="pdf", width=7, height=6.5)

 
# ===============================================================================================================
# PCA with food categories and BMI
# ===============================================================================================================

# Your input data should be a dataframe with uncorrelated variables with non-zero variance and with no missing data. 
  pca_input <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat_rv.txt",
                          sep="\t", header=T)

# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)

# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x= pca_input, scale= TRUE)   

# Specify the directory (folder that you created in "Laboratory_data") to save the results.
  res_dir_Cat = "males60to79_Cat_PCA" 

# Specify the prefix of filenames to be saved. 
  res_prefix_Cat = "males60to79_Cat"

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
  OutputPCA(pca.data=pca_input, pca.result=scaled_pca, 
             out.dir= res_dir_Cat, out.prefix= res_prefix_Cat)

# Combine the input with all the variables and the PC results. 
  SaveInputAndPCs(input="QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat.txt", 
                  pca.results = scaled_pca, 
                  out.dir= res_dir_Cat, out.prefix= res_prefix_Cat)

# [NOTE] Even though the input file has both nutrients (Nut) and food categories (Cat) data,  
# PCA was done with only either Nut or Cat, not both.

# ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - GLU_index, in this case. 
  
# Load the totals_males60to79 data with no missing data in your variables of interest.
  totals_males60to79 <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79_c_Cat.txt", 
                                   sep="\t", header=T)
  
# Change GLU_index to a factor so that factors will be displayed in order.
  totals_males60to79$GLU_index <- factor(totals_males60to79$GLU_index, 
                                     levels= c("Normal", "Prediabetic", "Diabetic"))  
  
# Use the autoplot function. Specify which PC to display in the x and y arguments.
  # The 'data' argument needs the original input for PCA, not after selecting uncorrelated variables.
  food_Cat_PCA <- autoplot(scaled_pca, x=1, y=2,  
                           loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
                           data = totals_males60to79,  size= 3 ) +          
    geom_point(size = 3, alpha = 1, na.rm = T, shape = 21,  aes(fill= GLU_index)) +
    theme_bw(base_size = 12) + theme(aspect.ratio = 1) +  
    theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual( values= c("steelblue3", "yellow", "hotpink")) 
  food_Cat_PCA

  ggsave("males60to79_Cat_PCA/males60to79_c_Cat_PCA_by_GLU_index_PC12.pdf", 
         food_Cat_PCA, device="pdf", width=7, height=6.5)

# ---------------------------------------------------------------------------------------------------------------
  # Come back to the main directory.
  setwd(main_wd)  
  
  