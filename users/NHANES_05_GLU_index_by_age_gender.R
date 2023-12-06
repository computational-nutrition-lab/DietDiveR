# ===============================================================================================================
# Look at the proportion of Normal, Prediabetic, and Diabetic in each age group. 
# Version 1
# Created on 11/21/2022 by Rie Sadohara
# ===============================================================================================================

# Here, we will look at the percentages of normal, prediabetic, and diabetic participants in our dataset by 
# age and gender groups.

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR/")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())  

# Load necessary packages.
  library(ggplot2)
  library(reshape2)
  
# Load necessary functions.
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
  source("lib/add_gender_and_age.R")   
  source("lib/stacked_perc_two_var.R")   

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# ---------------------------------------------------------------------------------------------------------------
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/Laboratory_data")  

  
# ===============================================================================================================
# Load the mean totals and add "Gender", "AgeGroup", and "Age_Gender".
# ===============================================================================================================
  
# Load the data of those to be used in the diabetes status analysis. 
  glu_2 <- read.table( file="QCtotal_d_ga_body_meta_glu_comp_2.txt", sep= "\t", header=T)
  
# Make GLU_index as a factor for plotting.
  glu_2$GLU_index <- factor(glu_2$GLU_index, levels = c("Normal", "Prediabetic", "Diabetic"))

# Add gender and age_groups to glu_2. The output is named "totals_out".
  AddGenderAgeGroups(input= glu_2, age.col="RIDAGEYR", gender.col="RIAGENDR")
  
# Rename the output.
  glu_2 <- totals_out
    
# Ensure that glu_2 now has Gender, AgeGroup, and Gender_Age columns.
  head(glu_2[, c("Gender", "AgeGroup", "Gender_Age")])

# ===============================================================================================================
# Build a stacked bar chart of diabetics by age and gender - FEMALE
# ===============================================================================================================

# Select females 
  glu_2_females <- subset(glu_2, Gender == "F") 
  
# Check the dimension of the selected data - 845 rows.
  nrow(glu_2_females)

# Calculate percentages of each level of GLU_index for each AgeGroup in order to generate a stacked barchart.
  StackedPercTwoVar(input.df=glu_2_females, var.x="AgeGroup", var.y="GLU_index", by="SEQN")
  # The percentages of each level of var.y are saved in the dataframe 'longtable_pr'.
  
# Generate a stacked barchart for females: GLU_index_pr_F. 
  GLU_index_pr_F <- ggplot(longtable_pr, aes(x= AgeGroup, y= value, fill= variable)) +
    geom_bar(position = "fill", stat = "identity",color='black',width=0.9) + 
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("steelblue2", "lightgoldenrod1", "lightpink1") ) +
    geom_text(aes(label = paste0( round(value*100, 0),"%")), 
              position = position_stack(vjust = 0.5), size = 5) +
    rotate_X_labels + space_axes + no_grid +
    labs(x="Age", y="")
  GLU_index_pr_F
  
# Save the plot.
  ggsave("QCtotal_d_ga_body_meta_glu_comp_2_AgeGroup_GLU_index_female.pdf", 
         GLU_index_pr_F, device="pdf", width=7, height=4.5, unit="in", dpi=300)

# ===============================================================================================================
# Build a stacked bar chart of diabetics by age and gender - MALE
# ===============================================================================================================
# Repeat the same operation and tables for females will be created. 
  
# Select males 
  glu_2_males <- subset(glu_2, Gender == "M") 
  
# Check the number of rows of the selected data - 765 rows.
  nrow(glu_2_males)
  
# Calculate percentages of each level of GLU_index for each AgeGroup in order to generate a stacked
# barchart.
  StackedPercTwoVar(input.df = glu_2_males, var.x = "AgeGroup", var.y="GLU_index", by="SEQN")
  # The percentages of each level of var.y are saved in the dataframe 'longtable_pr'.
  
# Plot it with percentage labels.
  GLU_index_pr_M <- ggplot(longtable_pr, aes(x= AgeGroup, y= value, fill= variable)) +
    geom_bar(position = "fill", stat = "identity",color='black',width=0.9) + 
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("steelblue2", "lightgoldenrod1", "lightpink1") ) +
    geom_text(aes(label = paste0( round(value*100, 0),"%")), 
              position = position_stack(vjust = 0.5), size = 5) +
    rotate_X_labels + space_axes + no_grid +
    labs(x="Age Group", y="")
  GLU_index_pr_M
  
# Save the plot.
  ggsave("QCtotal_d_ga_body_meta_glu_comp_2_AgeGroup_GLU_index_male.pdf", 
         GLU_index_pr_M, device="pdf", width=7, height=4.5, unit="in", dpi=300)

# By looking at the distribution, we see that males in their 60s and older have the highest percentages of 
# Diabetic individuals. We will use this gender-age group to explore their diets further. 

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory before you start running another script.  
  setwd(main_wd)
  