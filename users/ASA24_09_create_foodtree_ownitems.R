# ===============================================================================================================
# Generate a foodtree from ASA24 data without using FilterDBByDiet, a clean version.
# Version 2
# Created on 02/16/2023 by Rie Sadohara
# 06/26/2023 replaced "OTU" with "IFC".
# ===============================================================================================================

# This brief script is to serve as an example of generating foodtrees and IFC tables with your own dataset. 
# IFC tables contain participants and food items, showing the consumption amount of each food item by each 
# participant. IFC tables also contain the taxonomy (food group information) for each food items and will be 
# used in ordination (grouping) analyses.

# This script demonstrates how to:
# 1. Build a foodtree with food items reported by VVKAJ study participants.
# 2. Generate IFC tables from the taxonomy information of reported food items.

# As explained in the previous script, the functions in Food_tree_scripts folder expects that the input files are 
# tab-delimited txt file with no special characters that impede correct loading such as:
#   "
#   '
#   #
#   &

# The use of the FormatFoods function in 02_load_clean_ASA24.R script has already dealt with special characters in 
# the VVKAJ items data, but it is helpful to know the assumptions which the functions you are going to use were built on.

# Before proceeding, create a new folder called "Foodtree" in the "VVKAJ" folder, in which you will save the output.

# ---------------------------------------------------------------------------------------------------------------

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# ---------------------------------------------------------------------------------------------------------------
# Load the packages/scripts necessary for tree building.
  if (!require("reshape2", quietly = TRUE))install.packages("reshape2")
# Load the data.tree package necessary for newick.tree.r, and if it is not installed, install it. 
  if (!require("data.tree", quietly = TRUE))install.packages("data.tree")
  
# Load source scripts
  source("lib/specify_data_dir.R")
  source("lib/Food_tree_scripts/newick.tree.r")
  source("lib/Food_tree_scripts/make.food.tree.r") # This needs 'newick.tree.r' already loaded.
  source("lib/Food_tree_scripts/make.food.ifc.r")
  source("lib/Food_tree_scripts/make.fiber.ifc.r")
  source("lib/Food_tree_scripts/make.dhydrt.ifc.r")

# You can come back to the main directory by:
  setwd(main_wd)   


# ===============================================================================================================
# Generate a foodtree from food items reported in your study.
# ===============================================================================================================
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")

# Create foodtree with the reduced dataset (only reported foods) classified at 
# a desired level of classification.

  MakeFoodTree(nodes_fn="../Food_tree_eg/NodeLabelsMCT.txt", 
               food_database_fn =            "VVKAJ_Items_f_id_s_m_QCed.txt",  
               addl_foods_fn = NULL,
               num_levels = 4,
               output_tree_fn =     "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tree.nwk", 
               output_taxonomy_fn = "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tax.txt")  

  # nodes_fn:           the food level (node) information for each food item.
  # food_database_fn:   whole ASA24 database to use.
  # addl_foods_fn:      additional foods that are not in ASA24 database but you would like to add; soylent_codes 
  #                     could be added in this case. If none, enter "NULL" instead.
  # num_levels:         number of food levels (1 - 5) to save.
  # output_tree_fn:     output tree file name. Should end with ".nwk"
  # output_taxonomy_fn: output taxonomy file (to be used later) name.
  

# ===============================================================================================================
# Generate standard, grams of fiber, and dehydrated grams per kcal IFC tables to be used later.
# ===============================================================================================================
# Make the standard ifc table with data in gram weights of food.
# For the food_records_fn argument, you need to supply the items data which contains 'FoodAmt' column.

# It is OK to see see a warning message: 
# In write.table(dhydrt.ifc, output_fn, sep = "\t", quote = F, append = TRUE) :
#   appending column names to file
  MakeFoodIfc(food_records_fn=  "VVKAJ_Items_f_id_s_m_QCed.txt",   
              food_record_id =  "SampleID",               
              food_taxonomy_fn= "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tax.txt",  
              output_fn =       "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.ifc.txt")  
  
    # food_records_fn:  Same as food_records_fn in MakeFoodTree.
    # food_record_id:   Your SampleID (User x Day)
    # food_taxonomy_fn: Taxonomy file produced by MakeFoodTree.
    # output_fn:        Name output ifc file.
  
# Make an ifc table with data in grams of fiber per food
  MakeFiberIfc(food_records_fn=  "VVKAJ_Items_f_id_s_m_QCed.txt", 
               food_record_id=   "SampleID", 
               food_taxonomy_fn= "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tax.txt", 
               output_fn=        "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.fiber.ifc.txt")
  
# Make an ifc table as dehydrated grams per kcal.
  MakeDhydrtIfc(food_records_fn=  "VVKAJ_Items_f_id_s_m_QCed.txt", 
                food_record_id =  "SampleID", 
                food_taxonomy_fn= "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tax.txt", 
                output_fn =       "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.dhydrt.ifc.txt")

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd)  
  