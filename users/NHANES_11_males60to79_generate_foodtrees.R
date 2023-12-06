# ===============================================================================================================
# Generate foodtree out of GLU - males in their 60-79, with 55 subsamples of Prediabetic.
# Do not use format.foods or CheckDB or FilterDBByDiet functions. Clean version. 
# Version 3
# Created on 02/16/2023 by Rie Sadohara
# 06/26/2023 replaced "OTU" with "IFC".
# ===============================================================================================================

# In this script, we will build a foodtree with the NHANES 2-day average totals, subsetted for males, 
# 60-79 years old. 

# First, we will load the QC-ed averaged totals of males 60-79 years old (n=236) and all food records (n=4,164). 
# Then, we will keep the food records of only the individuals in males 60-79 years old and generate a foodtree
# with the filtered food data. 

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# Load the packages/scripts necessary for tree building, and if it is not installed, install it.
  if (!require("reshape2", quietly = TRUE))install.packages("reshape2")
# Load the data.tree package necessary for newick.tree.r, and if it is not installed, install it. 
  if (!require("data.tree", quietly = TRUE))install.packages("data.tree")
  
# ---------------------------------------------------------------------------------------------------------------
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
# Load and prep data for generating foodtrees 
# ===============================================================================================================
  
# Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data")

# Load the males60to79 people. Note this is a total data (1 row/person).
  totals_males60to79 <- read.table("QCtotal_d_ga_body_meta_glu_comp_2_males60to79.txt", 
                                   sep="\t", header=T)

# Make the individuals as a vector.
  selectedind <- totals_males60to79$SEQN

# Load the input file (all food record data) to be filtered.
  all.food.record <- read.table("../Food_D12_FC_QC_demo_QCed.txt", sep="\t", header=T)
               
# Select only the individuals listed in 'selectedind'.
  sel.food.record <- all.food.record[all.food.record$SEQN %in% selectedind, ]

# Confirm the two contains the same set of individuals. 
  identical(unique(sel.food.record$SEQN), selectedind)

# Save. This will be the input for the following procedures.
  write.table(sel.food.record, "Food_D12_FC_QC_demo_QCed_males60to79.txt", 
              sep="\t", row.names=F, quote=F) 

# ===============================================================================================================
# Create foodtrees
# ===============================================================================================================

# Create foodtree with the foods classified at a desired level of classification (Lv. 1-5).
# "NodeLabelsMCT.txt" has a list of food levels and names, which comes with the DietR package.
  MakeFoodTree(nodes_fn= "../../Food_tree_eg/NodeLabelsMCT.txt", 
               addl_foods_fn = NULL,
               num_levels = 3,
               food_database_fn =            "Food_D12_FC_QC_demo_QCed_males60to79.txt",  
               output_tree_fn =     "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.nwk", 
               output_taxonomy_fn = "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.tax.txt"
  )

  # nodes_fn:           The food level (node) information for each food item.
  # num_levels:         Number of food levels (1 - 5) to save.
  # food_database_fn:   Your food item data.
  # addl_foods_fn:      List of foods not present in the list can be added. NULL by default.
  # output_tree_fn:     Output tree file name. Should end with ".nwk"
  # output_taxonomy_fn: Output taxonomy file (to be used later) name.  
  
# The xxx.nwk is the foodtree, and xxx.tax.txt is the taxonomy file that is to be used to IFC tables next. 
    
# --------------------------------------------------------------------------------------------------------------
# Generate IFC tables for downstream analyses; IT MAY TAKE SOME TIME.
# It is OK to see the following warning message:
  # In write.table(fiber.ifc, output_fn, sep = "\t", quote = F, append = TRUE) :
  # appending column names to file.
  
# Make the standard IFC table with data in gram weights of food.
# For the food_records_fn argument, you need to supply 'sel.food.records' file that have 'FoodAmt' column.     
  MakeFoodIfc(food_records_fn=  "Food_D12_FC_QC_demo_QCed_males60to79.txt",   
              food_record_id =  "SEQN",                              
              food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.tax.txt",       
              output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.food.ifc.txt")  
  
  # food_records_fn:   Same as food_records_fn in MakeFoodTree.
  # food_record_id:    The colunmname of your participants' ID.
  # food_taxonomy_fn:  Taxonomy file produced by MakeFoodTree.
  # output_fn:         Name output ifc file to be saved.
  
# Make a IFC table with data in grams of fiber per food.
  MakeFiberIfc(food_records_fn=  "Food_D12_FC_QC_demo_QCed_males60to79.txt", 
               food_record_id=   "SEQN", 
               food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.tax.txt", 
               output_fn=        "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.fiber.ifc.txt")
  
# Make a IFC table as dehydrated grams per kcal.
  MakeDhydrtIfc(food_records_fn=  "Food_D12_FC_QC_demo_QCed_males60to79.txt", 
                food_record_id =  "SEQN", 
                food_taxonomy_fn= "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.tax.txt", 
                output_fn =       "Foodtree/Food_D12_FC_QC_demo_QCed_males60to79_3Lv.dhydrt.ifc.txt")  
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)
  

  