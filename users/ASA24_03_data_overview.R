# ===============================================================================================================
# Take an overview of ASA24 items and totals data.
# Version 2 without the line plot.
# Created on 11/10/2022 by Rie Sadohara
# Updated on 03/08/2023 by Rie Sadohara
# ===============================================================================================================

# We can view summary statistics of either the individual food data or the totals data. First, let us take 
# a look at theitems data. This section is not intended to be a complete guide to analysis, but rather to give
# you some ideas for how to explore this data. 

# ===============================================================================================================
# Set working directory 
# ===============================================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")
  
# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow and generate plots.
  source("lib/specify_data_dir.R")  
  source("lib/data_overview.R")  
  source("lib/ggplot2themes.R")  

# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# Make a vector of colors for each factor of Diet so that plots will have consistent colors.
  # Take the first five colors from distinct100colors. 
  diet_colors <- distinct100colors[1:5]
  # Name each color.
  names(diet_colors) <- c("Vegetarian", "Vegan", "Keto", "American", "Japanese") 
  # Specific colors are assigned to be used for each diet.
  diet_colors

# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Load and analyze (QC-ed) ASA24 food items data
# ===============================================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ")  

# Load your items data to be analyzed.
# "_f_id_s_m" stands for: "food names formatted", "SampleID added", "selected individuals", 
# "metadata merged", and "the individuals that passed the QC of averaged totals".  
  items_f_id_s_m <- read.delim("VVKAJ_Items_f_id_s_m_QCed.txt")
  
# ---------------------------------------------------------------------------------------------------------------
# Summary statistics
  
# Summary statistics of one variable can be obtained by using R's summary() function. 

# View min, quantiles, mean, etc. for a variable in your dataset. 
  summary(items_f_id_s_m$KCAL)

# To calculate these summary statistics for multiple variables, use the SummaryStats() function.
# Calculate the minimum, 1st quantile, median, mean, 3rd quantile, max, and standard deviation
# for each variable in the input dataframe and save as a .txt file.
  SummaryStats(inputdf = items_f_id_s_m, 
               outfn = "VVKAJ_Items_f_id_s_m_summ.txt")

# [NOTE] These are individual items, not by user or day. 

# ---------------------------------------------------------------------------------------------------------------
# Boxplot
# Generate a boxplot to view data distribution.
  
# Boxplot of KCAL by users. 
  users_kcal <- ggplot(items_f_id_s_m, aes(x=UserName, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes + rotate_X_labels
  users_kcal
  
# Save it as a .pdf file.
  ggsave("VVKAJ_Items_f_id_s_m_users_kcal.pdf", users_kcal, 
         device="pdf", width=6, height=4.6, units="in")
  
# Similarly, generate a boxplot of KCAL by gender.
  gender_kcal <- ggplot(items_f_id_s_m, aes(x=Gender, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes 
  gender_kcal

# Save it as a .pdf file.
  ggsave("VVKAJ_Items_f_id_s_m_gender_kcal.pdf", gender_kcal, 
         device="pdf", width=3.5, height=4.6, units="in")
  
  
# ---------------------------------------------------------------------------------------------------------------
# Scatterplot
  
# Scatterplots can be generated to look at the relationship between two numeric variables. Here we look at 
# total fat and kilocalories. We would expect these values to be related because fat contributes a high 
# number of calories in foods.
  
# Scatterplot of two numeric variables: TFAT and KCAL. 
  TFAT_KCAL <- ggplot(items_f_id_s_m, aes(x=TFAT, y=KCAL)) +
    geom_point() + no_grid + space_axes + theme(aspect.ratio = 1)
  TFAT_KCAL

# Save it as a .pdf file.
  ggsave("VVKAJ_Items_f_id_s_m_TFAT_KCAL.png", TFAT_KCAL, 
         device="png", width=4.6, height=4.1, units="in")
  
# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient
  cor.test(x=items_f_id_s_m$TFAT, y=items_f_id_s_m$KCAL, method="pearson")

  
# ===============================================================================================================
# Load and analyze (QC-ed) ASA24 mean totals data
# ===============================================================================================================
  
# Load your QC-ed mean totals data to be analyzed.
  tot_mean_m_QCed <- read.delim("VVKAJ_Tot_mean_m_QCed.txt")
  
# Note that each row is the mean of total dietary intake of each user. 
  tot_mean_m_QCed[1:4, 1:4]

# ---------------------------------------------------------------------------------------------------------------
# Summary statistics
  
# Summary statistics of one variable
  summary(tot_mean_m_QCed$KCAL)
  
# Calculate the min, quantiles, mean, etc. for a variable in your dataset
# in the same way we did with the items.   
  SummaryStats(inputdf = tot_mean_m_QCed, 
               outfn = "VVKAJ_tot_mean_m_QCed_summ.txt")

# ---------------------------------------------------------------------------------------------------------------
# Boxplot
# Generate a boxplot to view data distribution.

# Create a vector named "Diet_by_median" containing the Diet in a desired order (by median in this case).
  Diet_by_median <- with(tot_mean_m_QCed, 
                    reorder(Diet, KCAL, median, na.rm=T))
  
# Diet_by_median is a factor, which contains the diets of all the 15 participants and the median values of 
# each Diet group. "Levels" show the order of them. 
  Diet_by_median
  
# Show the levels of this factor. This will be useful in plotting a factor in a desired order.
  levels(Diet_by_median)

# Generate a boxplot of KCAL by diet of the participants.  
  diet_KCAL_t <- ggplot(tot_mean_m_QCed, aes(x=Diet_by_median, y=KCAL, fill=Diet_by_median)) +
    geom_boxplot() + labs(x="Diet") +
    theme(legend.position = "none") + # hide legend  
    scale_fill_manual(values=diet_colors) +
    no_grid + space_axes + rotate_X_labels
  diet_KCAL_t

# Save it as a .pdf file.
  ggsave("VVKAJ_tot_mean_m_QCed_diet_KCAL.pdf", diet_KCAL_t, 
         device="pdf", width=5, height=4.5)
  
# Boxplot of KCAL by Diet, with each datapoint. 
# [NOTE] geom_boxplot must have outlier.shape = NA when plotted with geom_jitter. Otherwise, outlier points 
# will be duplicated and will be misleading.
  diet_KCAL_t_dots <- ggplot(tot_mean_m_QCed, aes(x=Diet_by_median, y=KCAL, fill=Diet_by_median)) +
    geom_boxplot(outlier.shape = NA) + labs(x="Diet") +    
    geom_jitter(width=0.3) +
    theme(legend.position = "none") + # hide legend  
    scale_fill_manual(values=diet_colors) +
    no_grid + space_axes + rotate_X_labels
  diet_KCAL_t_dots

# Save it as a .pdf file.
  ggsave("VVKAJ_tot_mean_m_QCed_diet_KCAL_dots.pdf", diet_KCAL_t_dots, 
         device="pdf", width=5, height=4.5)
  
# ---------------------------------------------------------------------------------------------------------------
# Scatterplot
  
# Generate a scatterplot of two variables, color-coded by Diet.
# Show the diets in the order of median, based on the boxplot that was generated above.
  TFAT_KCAL_t <- ggplot(tot_mean_m_QCed, aes(x=TFAT, y=KCAL, fill=Diet_by_median)) +
    geom_point(shape=21, size=3, color="black") + no_grid + space_axes +
    # order "diet_colors" in the same way as the level order of Diet_by_median.
    scale_fill_manual(values= diet_colors[levels(Diet_by_median)]) +
    labs(fill="Diet") + theme(aspect.ratio = 1) 
  TFAT_KCAL_t
  
# Save it as a .pdf file.
  ggsave("VVKAJ_tot_mean_m_QCed_TFAT_KCAL.pdf", TFAT_KCAL_t, device="pdf",
         width=5.5, height=4)

# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient.
  cor.test(x=tot_mean_m_QCed$TFAT, y=tot_mean_m_QCed$KCAL, method="pearson")

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory before you start running another script.
  setwd(main_wd)
  
