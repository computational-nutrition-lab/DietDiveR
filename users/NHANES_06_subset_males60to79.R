# ===============================================================================================================
# Select only males 60-79 years old, and check the difference between diabetic status groups.
# Version 1
# Created on 10/28/2022 by Rie Sadohara
# ===============================================================================================================

# In the previous script, we identified males in their 60s and older seem to be an appropriate subsample to 
# analyze their diabetic statuses and diet. We will remove people who are 80 years old or potentially above 
# from our analyses because they may be using meal assistance and thus may not be eating freely.

# ===============================================================================================================
# Load necessary functions and packages
# ===============================================================================================================
# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR/")
  
# Name your main directory for future use. 
  main_wd <- file.path(getwd())  

# Load necessary functions.
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 

# ===============================================================================================================
# Load NHANES15-16 totals data
# ===============================================================================================================
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/Laboratory_data")  
  
# Load the data of those to be used in the diabetes status analysis. 
  glu_2 <- read.delim(file="QCtotal_d_ga_body_meta_glu_comp_2.txt", sep="\t", header=T)
  
# Make GLU_index as a factor for plotting.
  glu_2$GLU_index <- factor(glu_2$GLU_index, levels = c("Normal", "Prediabetic", "Diabetic"))
    
# Check the sample size of each category.
  table(glu_2$GLU_index, useNA = "always")
  
  # Normal Prediabetic    Diabetic        <NA> 
  #   679         723         208           0 

# Age - no missing data, and spread pretty evenly. 
  summary(glu_2$RIDAGEYR)     
  hist(glu_2$RIDAGEYR)
  
# Gender - no missing data. 1: male, 2: female.
  table(glu_2$RIAGENDR, useNA="always")     
  
  
# ===============================================================================================================
# Select only males 60-79 years old, so that our samples are more uniform.
# ===============================================================================================================
  
# Select those who are males and whose ages fall between 60-79.
  glu_2_males60to79 <- subset(glu_2, RIAGENDR == 1 & RIDAGEYR >= 60 & RIDAGEYR <= 79) 
  
# Check the dimension of the selected data - 236 rows.
  dim(glu_2_males60to79)
  
# Ensure the ages of the selected subpopulation are between 60-79.  
  table(glu_2_males60to79$RIDAGEYR)
  
# Look at the distribution of GLU_index among the selected subpopulation.
  table(glu_2_males60to79$GLU_index, useNA="always")
  
# Save the glu_2_males60to79 as a txt file.
  write.table(glu_2_males60to79, "QCtotal_d_ga_body_meta_glu_comp_2_males60to79.txt", 
              sep="\t", row.names = F, quote = F)
  
# ===============================================================================================================
# Look at the frequency of body measure by diabetic status and test difference by ANOVA.
# ===============================================================================================================
  
# BMI
# Look at the BMI frequency of each group.
# It is OK to see an error saying it removed rows containing non-finite values or missing values,
# as long as the charts are produced.
  males60to79_BMIfreq <- 
    ggplot(data=glu_2_males60to79, aes(x=BMXBMI, group=GLU_index, fill=GLU_index)) +
    geom_density(adjust=1.5, alpha=0.4) + space_axes + no_grid +
    scale_fill_manual(values= c("aquamarine2", "lightgoldenrod1", "lightpink1") ) +
    labs(x="BMI", y="Density")
  males60to79_BMIfreq
  
  # Save the chart as .pdf.
  ggsave("males60to79_BMI_by_GLU_index.pdf", 
         males60to79_BMIfreq, device="pdf", width=5.3, height=4.5)
  
# ----------------------------------------------------------------------------------------------------------------  
# Create a boxplot of BMI of each GLU_index group.
  males60to79_BMIbox <- 
    ggplot(glu_2_males60to79, aes(x=GLU_index, y=BMXBMI, fill=GLU_index)) +
    geom_boxplot(outlier.shape = NA) + no_grid + space_axes +
    scale_fill_manual(values= c("aquamarine2", "lightgoldenrod1", "lightpink1") ) +
    geom_jitter(width=0.3)
  males60to79_BMIbox
  
  ggsave("males60to79_BMI_by_GLU_index_box.pdf", 
         males60to79_BMIbox, device="pdf", width=5.3, height=4.5)
  
# ----------------------------------------------------------------------------------------------------------------  
# ANOVA between groups
  
# The three GLU_index groups appear to have different BMI means. 
# Test the difference between groups by ANOVA.
  
# Remove samples (rows) that have missing data in the target variable.
  df <- glu_2_males60to79[complete.cases(glu_2_males60to79$BMXBMI), ]
  
# Run ANOVA
  myanova <- aov(BMXBMI ~ GLU_index, data=df)
  summary(myanova)

# The ANOVA results indicate that the groups are different (p<0.05).
  
# But first, test the assumptions for ANOVA.

  ## Create a new dataset of residuals of the model.
  res1 <- residuals(myanova)
  
  ## Check the normality assumption.
  hist(res1)
  qqnorm(res1, plot.it=TRUE)
  qqline(res1)
  
  ## The histogram and QQplot indicate the residuals have an approximately normal distribution. 
  
  ## Check equal variance of residuals with a side-by-side boxplots of the residuals.
  boxplot(res1 ~ df$GLU_index)
  
  ## This boxplot also indicates the variance of the residuals are approximately equal across the factor levels.
  
  ##  Create a new variable of squared residuals.
  ## If Pr>0.05, the residuals of the groups have equal variance.
  res1sq <- res1*res1
  ## Levene's test -- Run ANOVA for the squared residuals as the response.
  anova(lm(res1sq ~ df$GLU_index))
  ## The p-value (Pr) of the Levene's test also supported equal variance.
  
  ## When the assumptions are satisfied, then you can run ANOVA.
  summary(aov(BMXBMI ~ GLU_index, data=df))
  
# If ANOVA is significant, you can do a pairwise t-test.
# "holm" (default) is less conservative than Bonferroni p-value adjustment method for multiple comparisons.
# Other methods that can be used: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".
  pairwise.t.test(df$BMXBMI, df$GLU_index, p.adjust.method = "holm") 
  
# Based on the p-values, Normal and Prediabetic have the same mean BMI (p=0.12765 > 0.05). Normal and Diabetic 
# have different mean BMI (p=0.00037 < 0.05), as do Prediabetic and Diabetic (p=0.00324 < 0.05).    
  
# ----------------------------------------------------------------------------------------------------------------  
# KCAL
# Look at the KCAL frequency of each group.   
  males60to79_KCALfreq <- 
    ggplot(data=glu_2_males60to79, aes(x=KCAL, group=GLU_index, color=GLU_index)) +
    geom_density(adjust=1.5, alpha=0.4, linewidth=1.2, linetype="longdash") + space_axes + no_grid +
    scale_color_manual(values= c("aquamarine3", "lightgoldenrod3", "lightpink1")) +
    labs(x="KCAL", y="Density") +
    scale_y_continuous(labels= function(x) format(x, scientific = FALSE))
  males60to79_KCALfreq
  
  # Save the chart as .pdf.
  ggsave("males60to79_KCAL_by_GLU_index.pdf", 
         males60to79_KCALfreq, device="pdf", width=5.3, height=4.5)
  
# ----------------------------------------------------------------------------------------------------------------  
# Create a boxplot of KCAL of each GLU_index group.
  males60to79_KCALbox <- 
    ggplot(glu_2_males60to79, aes(x=GLU_index, y=KCAL, fill=GLU_index)) +
    geom_boxplot(outlier.shape = NA) + no_grid + space_axes +
    scale_fill_manual(values= c("aquamarine2", "lightgoldenrod1", "lightpink1") ) +
    geom_jitter(width=0.3)
  males60to79_KCALbox
  
  ggsave("males60to79_KCAL_by_GLU_index_box.pdf", 
         males60to79_KCALbox, device="pdf", width=5.3, height=4.5)
  
# ----------------------------------------------------------------------------------------------------------------  
# Test the difference of KCAL between groups by ANOVA.
# Remove samples (rows) that have missing data in the target variable.
  df <- glu_2_males60to79[complete.cases(glu_2_males60to79$KCAL), ]
  
# Run ANOVA
  summary(aov(KCAL ~ GLU_index, data=df))
  
# Based on the p-values for ANOVA, the energy intake in KCAL is not different between the GLU_index groups 
# are not different.    
  
  
# --------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)
  
  