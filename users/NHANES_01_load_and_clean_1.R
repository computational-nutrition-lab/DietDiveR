# ===============================================================================================================
# Load and clean NHANES - 1 - format food data and add food description. 
# Version 2
# Created on 05/18/2022 by Rie Sadohara and Suzie Hoops
# ===============================================================================================================

# In this tutorial, we aim to demonstrate the utility of the DietR package by applying it to the dietary data 
# available from the National Health and Nutrition Examination Survey (NHANES). 

# NHANES is a national survey conducted with some thousands of participants across the US over two days. NHANES 
# data is published every other year and consists of various kinds of data including dietary intake, body 
# measurements, laboratory-taken data. There are numerous questions that can be asked with these data to 
# connect dietary intake with various biomarkers. Here, as an example, we will explore the dietary intake
# (food items) of participants and look at the relationship between dietary patterns and diabetic status in 
# NHANES 2015-2016.

# Refer to the NHANES data and documentation (https://wwwn.cdc.gov/nchs/nhanes/Default.aspx) for more information 
# about each release (version) of NHANES and the variables measured.

# Sample weight is an important tool to use if you aim for population-level estimates because the composition of 
# demographics of NHANES participants differ slightly from that of the actual US population (i.e., certain demographic
# subgroups were oversampled). Weights are applied to adjust NHANES data to reflect the proportions of demographic 
# subgroups in the actual population. However, this part is omitted in this tutorial as our main goal here is to 
# demonstrate how to analyze NHANES data as is, not to make population-wise inferences.  For more detailed information,
# please refer to the sample weight tutorial by NHANES (https://wwwn.cdc.gov/nchs/nhanes/tutorials/Weighting.aspx). 

# Data from NHANES are in .XPT format, and you will need a specific R package, SASxport, to import it into R. 
# We will download the example data files from the NHANES 2015-2016 page 
# (https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015) 
# and save them in the "Raw_data" folder in the "NHANES" folder.

# Open "02_load_clean_NHANES_food_1.R" script (this script) in the "users" folder and load the packages 
# needed as per the directions.

# In this tutorial, you will learn how to download datasets and associated other databases from NHANES, 
# how to process that data with DietR, and the food tree to generate dietary patterns, and how to integrate these 
# dietary patterns with other biomarker data from NHANES to explore specific hypotheses.

# Folder structure 
# 
#               |----- eg_data -- NHANES -- Raw_data -- has Food Items and Totals files 
#               |                                       downloaded from NHANES 15-16.
#               |
#  Main --------|----- lib -- functions
#  (DietR)      |
#               |----- users -- where this script is located
#

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR/")

# Name your main directory for future use.
  main_wd <- file.path(getwd())
  
# Install the SASxport package if it is not installed yet.
  if (!require("SASxport", quietly = TRUE)) install.packages("SASxport")

# Load SASeport, necessary to import NHANES data.
  library(SASxport)

# Load necessary functions.
  source("lib/specify_data_dir.R")
  source("lib/load_clean_NHANES.R")
  source("lib/Food_tree_scripts/format.foods_2.r")
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  

# ===============================================================================================================
# Prepare food codes, FPED, and food items files
# ===============================================================================================================

# Load and prepare food codes  
# Download food code data from the NHANES website and save it in "Raw_data" folder. 
  # Name the file and destination. mod="wb" is needed for Windows OS.
  # Other OS users may need to delete it.
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DRXFCD_I.XPT", 
                destfile= "Raw_data/FoodCodes_DRXFCD_I.XPT", mode="wb")
  
# Prepare the food code table - replace special characters with "_" or "and".
  
  # Format the food table and save it as a .txt file.
  PrepareFoodCodeTable(raw.food.code.table = "Raw_data/FoodCodes_DRXFCD_I.XPT", 
                       out.fn =              "FoodCodes_DRXFCD_I_f.txt")  
  
  # Load the formatted foodcode table.
  foodcodetable_f <- read.table("FoodCodes_DRXFCD_I_f.txt", sep="\t", header=T)
  
  # Show the first 10 rows of the output and ensure special characters are gone; e.g., 
  # MILK, REDUCED FAT (2%) is now MILK, REDUCED FAT (2_).
  foodcodetable_f[1:10, ]
  
# ---------------------------------------------------------------------------------------------------------------
# Load and prepare Food Patterns Equivalent Database for use
  
# The Food Patterns Equivalents Database (FPED) has the composition of nutrients and food categories of 
# each food item coded by food codes. FPED can be downloaded from USDA -ARS FPED databases 
# (https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fped-databases/)

# You need to use the version of FPED that corresponds with the NHANES version that you explore. When using NHANES,
# you will need to use the FPED with the same release year as the year of NHANES you are analyzing. 
# For this tutorial, FPED was downloaded from NHANES the FPED table is formatted by renaming the variables with
# R-loadable ones (e.g., "F_CITMLB (cup eq.)" -> "F_CITMLB" and saved as "FPED_1516_forR.txt".
                    
# Load FPED15-16, needed for the AddFoodCat function.
  FPED <- read.table("FPED/FPED_1516_forR.txt", sep="\t", header=T)
  
  # Check the first 2 rows of FPED. 
  head(FPED, 2)
  
  # Important! Change the food code column name as Food_code.
  colnames(FPED)[1] <- "Food_code"

# ---------------------------------------------------------------------------------------------------------------
# Load "food items" data and add food descriptions
# Download food items data

# Food data can be downloaded from the NHANES website. For each cycle of NHANES there are two days of dietary 
# records available for many participants. The food-level data is available for each of these days of 
# dietary records. These are stored separately as Day 1 and Day 2. Here we will download both days and 
# combine them to obtain better estimates of diet.

  # Download day 1 data.
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1IFF_I.XPT", 
                destfile= "Raw_data/DR1IFF_I.XPT", mode="wb")
  
  # Download day 2 data.
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR2IFF_I.XPT",
                destfile= "Raw_data/DR2IFF_I.XPT", mode="wb")
  
# [NOTE] Different alphabets are used on the variables' names in different release of NHANES 
# data.  Therefore, you will need to change the alphabet (and potentially the other parts of the 
# variable names) in order to run this script with other releases of NHANES. For example, 
# DR1IFDCD is a column name for the food code used in the NHANES 2015-2016, and the 
# alphabet for this release is "I". The alphabet for NHANES 2017-2018 is "J", so the food code 
# column in that dataset is DR1JFDCD, and the other columns in it have DR1J or DR2J as prefixes.  

# Whilst we will only use one dataset (2015-16) in this tutorial, you may want to combine several releases to 
# analyze long-term trends. It is recommended that you check the variable names and food item names as the food 
# databases are updated regularly, and they may not perfectly match across versions.
  
# ---------------------------------------------------------------------------------------------------------------
# Load and prepare Day 1 food items data
  
# Import items data Day 1, add food item descriptions, and save it as a txt file.
# OUTPUT WILL LIKELY BE A VERY LARGE FILE.
  ImportNHANESFoodItems(data.name="Raw_data/DR1IFF_I.XPT", 
                        food.code.column = "DR1IFDCD", 
                        food.code.table = foodcodetable_f,
                        out.fn = "DR1IFF_I_d.txt") # 'd' stands for food descriptions

# Load the saved food items file.
  Food_D1 <- read.table("DR1IFF_I_d.txt", sep="\t", header=T)

# Count the number of participants - should be 8,505 people.
  length(unique(Food_D1$SEQN)) 

# Add the food category info and serving for each item. #### WILL TAKE A FEW MOMENTS. ####
  # It is OK to see a message saying "NAs introduced by coercion."
  # NAs will be removed later in the filtering process.  
  AddFoodCat(input.food= Food_D1,
             fped= FPED,
             grams= "DR1IGRMS", 
             out.fn= "Food_D1_FC.txt")

# ---------------------------------------------------------------------------------------------------------------
# Load and prepare Day 2 food items data
  
# Import items data Day 2, add food item descriptions, and save it as a txt file.
  ImportNHANESFoodItems(data.name="Raw_data/DR2IFF_I.XPT",
                        food.code.column = "DR2IFDCD",
                        food.code.table = foodcodetable_f,
                        out.fn = "DR2IFF_I_d.txt")

# Add food item description and save it as a txt file.
  Food_D2 <- read.table("DR2IFF_I_d.txt", sep="\t", header=T)

# Count the number of participants - should be 7,027 people.
  length(unique(Food_D2$SEQN)) 

# Do the same for Day 2. Add the food items info and serving for each item. 
#### WILL TAKE A FEW MOMENTS. ####
  # It is OK to see a message saying "NAs introduced by coercion."
  # NAs will be removed later in the filtering process.  
  AddFoodCat(input.food= Food_D2, 
             fped= FPED, 
             grams= "DR2IGRMS", 
             out.fn= "Food_D2_FC.txt")


# ===============================================================================================================
# Load the Food_Dx_FC which has the food category data 
# ===============================================================================================================

# Change column names to more intuitive ones
  
# Food Day 1 with Food Category *** WILL BE A VERY LARGE TABLE. ***
  Food_D1_FC <- read.table("Food_D1_FC.txt", sep="\t", header=T)
  
# Food Day 2 with Food Category *** WILL BE A VERY LARGE TABLE. ***
  Food_D2_FC <- read.table("Food_D2_FC.txt", sep="\t", header=T)
  
# Change the colnames for downstream analyses
  names(Food_D1_FC)[names(Food_D1_FC) == "DR1IFDCD"] <- "FoodCode"
  names(Food_D1_FC)[names(Food_D1_FC) == "DR1IGRMS"] <- "FoodAmt"
  names(Food_D1_FC)[names(Food_D1_FC) == "DRXFCLD"] <- "Main.food.description"
  
  names(Food_D2_FC)[names(Food_D2_FC) == "DR2IFDCD"] <- "FoodCode"
  names(Food_D2_FC)[names(Food_D2_FC) == "DR2IGRMS"] <- "FoodAmt"
  names(Food_D2_FC)[names(Food_D2_FC) == "DRXFCLD"] <- "Main.food.description"
  
# Ensure the column names are changed. 
# Among the colnames of Food_D1_FC, show one that matches with "DR1IFDCD", if it exists.
# If it returns the specified colname, "DR1IFDCD", a column with that name exists.
# If it returns "character(0)", no colname matched the specified characters, which means 
# colnames of our interest do not exist.
  names(Food_D1_FC)[names(Food_D1_FC) == "FoodCode"]
  names(Food_D2_FC)[names(Food_D2_FC) == "Main.food.description"]
  
# Save after changing the column names. "cc" stands for column names changed.
  write.table(Food_D1_FC, "Food_D1_FC_cc.txt", sep="\t", row.names=F, quote=F)
  write.table(Food_D2_FC, "Food_D2_FC_cc.txt", sep="\t", row.names=F, quote=F)
  
# Replace special characters with "_" using FormatFoods.
  
# FotmatFoods() function adds "Main.Food.Description" where special characters are removed/replaced, the previous
# Main.Food.Description as Old.Main.Food.Description, ModCode, and FoodID. $FoodID is a cha vector, but has .0 at the end. 
  FormatFoods(input_fn="Food_D1_FC_cc.txt", output_fn= "Food_D1_FC_cc_f.txt")
  FormatFoods(input_fn="Food_D2_FC_cc.txt", output_fn= "Food_D2_FC_cc_f.txt")

  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)  
  
  