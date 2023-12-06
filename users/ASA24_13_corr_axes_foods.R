# ===============================================================================================================
# Generate a heatmap of correlation between food categories and ordination Axes.  
# Version 2
# Created on 02/16/2023 by Rie Sadohara
# 06/26/2023 replaced "OTU" with "IFC".
# The create_corr_frame function credit: Mo Hutti. 
# ===============================================================================================================

# In this script, we will analyze correlation between ordination axes values and foods.

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR/")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# load the necessary packages and the source code.
  library(ggplot2)
  source("lib/specify_data_dir.R")
  source("lib/corr.axes.foods.R")
  source("lib/ggplot2themes.R")

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# You can come back to the main directory by:
  setwd(main_wd)

# Specify the directory where the data is.
  SpecifyDataDirectory("eg_data/VVKAJ/Ordination/")

# ===============================================================================================================
# Weighted unifrac distance ordination results
# ===============================================================================================================

# From sorted IFC table, generate a table of total amount of food consumed by all the individuals, 
# and a table with correlation coefficients, p-values, and q-values with desired threshold between 
# food items and Axes that were saved in the ordination section. 
# Be careful about not to confuse WEIGHTED and UNweighted unifrac distances.

# WEIGHTED unifrac distance results.
  CorrAxesFood(food_ifc_soted = "../Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted.txt", 
               AmountSums_out_fn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_AmountSums.txt",
               qval_threshold = 0.05,
               meta_users =            "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt",
               corr_axes_foods_outfn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")

  # food_ifc_soted:     xxx.food.ifc.sorted.txt file, saved in the ordination section.
  # AmountSums_out_fn:  output filename to be saved which has the total consumption amount of each food.
  # qval_threshold:     q-value threshold to call a correlation significant.
  # meta_users:         xxx.meta_users.txt file, waved in the ordination section.
  # corr_axes_foods_outfn: output filename to be saved which has the correlation between foods and Axes.

# ---------------------------------------------------------------------------------------------------------------
# Load and analyze the output.

  dat <- read.delim("VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")

# Check the number of food items with significant q-values.
  nrow(subset(dat, Significance=="*"))
    
# There are 8 food items significantly correlated with one of the axes. 
# Show only food items that are significantly correlated with one of the axes.
  subset(dat, Significance=="*")
  
# Axis.1 is positively correlated with miso soup, green tea, white rice, and soy sauce.
# Those are commonly used in Japanese cuisine. So, it makes sense that Japanese-diet consumers are 
# clustered on the right side of the biplot (Axis.1 x Axis.2 plot). Similarly, Axis.2 is negatively 
# correlated with Avocado and Nutritional powder mix, whey based, and they are commonly found in
# keto (high protein, high fat, low carb) diet. And keto-diet consuming individuals tend to have lower 
# Axis.2 values in the biplot. 
# Correlation between axes and foods can be useful in identifying characteristics of the diet based on 
# clustered individuals on the ordination biplot.
  
# It is also possible to view each axis separately.
  # Select Axis 1 rows
  dat_1 <- subset(dat, Axis=="Axis.1")
  head(dat_1[order(dat_1$qval), ], 10)
  
  # Select Axis 2 rows and sort by qval.  
  dat_2 <- subset(dat, Axis=="Axis.2")
  head(dat_2[order(dat_2$qval), ], 10)
  
  # Select Axis 3 rows and sort by qval.  
  dat_3 <- subset(dat, Axis=="Axis.3")
  head(dat_3[order(dat_3$qval), ], 10)
  
  # Select Axis 4 rows and sort by qval.  
  dat_4 <- subset(dat, Axis=="Axis.4")
  head(dat_4[order(dat_4$qval), ], 10)
  
  
# ===============================================================================================================
# UNweighted unifrac distance ordination results.
# ===============================================================================================================

# xxx_AmountSums.txt will be generated again, but its content will be the same regardless of which distance method
# (weighted or unweighted unifrac or else) was used, as long as the food.ifc_sorted is the same.
  
  CorrAxesFood(food_ifc_soted = "../Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted.txt",
               AmountSums_out_fn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_AmountSums.txt",
               qval_threshold = 0.05,
               meta_users =            "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_meta_users.txt",
               corr_axes_foods_outfn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt")
  
# ---------------------------------------------------------------------------------------------------------------
# Load and analyze the output.
    
# UNweighted can be viewed in the same way.         
  dat <- read.delim("VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt")
  
# Check the number of food items with significant q-values.
  nrow(subset(dat, Significance=="*"))

# There are 6 food items significantly correlated with one of the axes. 
# Show only food items that are significantly correlated with one of the axes.
  subset(dat, Significance=="*")
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)  
  