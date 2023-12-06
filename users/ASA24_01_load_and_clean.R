# ===============================================================================================================
# Load and clean ASA24 data. After FormatFoods has been fixed.
# Version 1
# Created on 01/19/2023 by Rie Sadohara
# QC for males and females separately, 06/28/2023.
# ===============================================================================================================

# In this tutorial, we will use mock data from the VVKAJ dataset that was created with ASA24 
# (https://epi.grants.cancer.gov/asa24/). VVKAJ stands for Vegetarian, Vegan, Keto, American, Japanese and 
# was designed because these different eating patterns reflect differences that are often seen in real data. 
# CThis mock dataset contains dietary for 15 mock participants who report dietary data while following the 5
# different dietary patterns (VVKAJ) for three days. There are a total of XX dietary records in this dataset.

# ASA24 data includes the following files: _Items.csv, _INS.csv, _Responses.csv, _TNS.csv, Totals.csv, and 
# TS.csv. Refer to the ASA24 Researchers' website for specific explanations for each file, but for the purpose of 
# this tutorial, we focus on using the Items.csv, which has all the food items reported by the participants.

# In this script, you will:
# 1. Use Metadata 1 to filter out individuals. 
# 2. Remove users that has only a small number of totals (days of record). - if you know which one to remove.  
# 3. Look for outliers in your totals by nutrient consumed on each day. 

# Calculate totals by occasion. - extra dataset. 

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory where input files are pulled.
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")  
  source("lib/load_clean_ASA24.R")
  source("lib/average.by.R") 
  source("lib/QCOutliers.R")
  source("lib/Food_tree_scripts/format.foods_2.r")
  
# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Load ASA24 data
# ===============================================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name= "eg_data/VVKAJ/")
  
# Load your unprocessed (raw) food items-level data (as downloaded from the ASA24 study website).
# The csv file will be loaded as a dataframe in R and be named as items_raw. 
  items_raw <- read.csv("Raw_data/VVKAJ_Items.csv", sep = ",", header=T) 
  
# items_raw has a column called "Food_Description", but this needs to be changed to "Main.food.description".
# Change the column name.
  names(items_raw)[names(items_raw) == "Food_Description"] <- "Main.food.description"
  
# Check if any column names match with "Main.food.description". If there is a match, it will be printed.  
  names(items_raw)[names(items_raw) == "Main.food.description"]

  # [Note] The numbers in the square brackets of the output indicate the sequential number of each element to 
  # help count the number of elements. 
  
# Save the items file as a .txt file. This command saves the object "items_raw" as a .txt file with 
# the specified filename using the write.table function. 
  write.table(items_raw, "VVKAJ_Items.txt", sep="\t", row.names=F) 
  
# Special characters common in food names in dietary data such as "'", ",", "%" may interfere correct 
# data loading in R; thus, we replace them with an underscore "_". 

# Format foods so that special characters will be replaced with "_". "_f" stands for "formatted".
  FormatFoods(input_fn =  "VVKAJ_Items.txt", 
              output_fn = "VVKAJ_Items_f.txt")

# [Note] It is best practice to avoid overwriting your raw data. Always save formatted/manipulated versions 
# as a new file as described above. 
  
# Load the Items_f.txt file to take a look at it.
# You need the "quote=""" and "colClasses="character"" arguments to ignore quotation marks (do not regard them 
# as a cell separator) and to load all the columns as characters so that FoodID will keep the trailing ".0".  
  items_f <- read.delim("VVKAJ_Items_f.txt", quote="", colClasses="character")
  
# All special characters in the items data should have been replaced with an underscore in the Main.foood.description 
# column, the 3rd from the last column of the items_f. We can confirm that by using the head function, which shows the 
# first six rows of the specified dataset by default. 
  head(items_f)
  
# Add a human-readable sample identifier (SampleID) with a desired prefix, and save it as a txt file. SampleIDs 
# are IDs unique to each combination of users and day and represent days of dietary intake in this dataset.   
  AddSampleIDtoItems(input.fn="VVKAJ_Items_f.txt", user.name="UserName", recall.no="RecallNo", 
                     prefix="vvkaj.", out.fn="VVKAJ_Items_f_id.txt")

# Load the formatted Items file with SampleID added.
  items_f_id <- read.delim("VVKAJ_Items_f_id.txt", quote="", colClasses="character")
  
# A combination of the specified prefix and sequential number (vvkaj.00001) should be added in the SampleID  
# column, the first column of the items_f_id dataframe. You will probably need to scroll up the output a
# little bit in the console to view the first column.     
  head(items_f_id)

# Ensure your items file has the expected dimensions (number of rows x number of columns, 
# shown as number of obs. and number of variables) in the environment window of R Studio. 
# Or by using dim(items_f_id) and dim(items_raw). Note that items_f_id has 3 more columns 
# than items_raw because new columns of FoodID, Old.Main.food.description, and SampleID have been added.
  dim(items_f_id)
  dim(items_raw)

# ===============================================================================================================
# Use individuals_to_remove.txt to filter out users marked as Remove = yes.  
# ===============================================================================================================
# Optionally, if you know a certain user(s) should be removed, you can supply a metadata with two columns: 
# UserName and "Remove", where the user(s) to be removed are marked "yes". 
  
# Load your metadata that has information about which UserName(s) to remove. 
  ind_to_rm <- read.delim("individuals_to_remove.txt")

  ind_to_rm
  # Metadata for this purpose (ind_to_rm) has UserName and which one to be removed:
  #     UserName Remove
  # 1   VVKAJ101       
  # 2   VVKAJ102    
  # ... ...        
  # ... ...        
  # 16  VVKAJ116   yes
  # 17  VVKAJ117

# Show which has "yes" in the "Remove" column. 
  subset(ind_to_rm, Remove == "yes")

# As shown in the console, the user named "VVKAJ116" is marked to be removed. VVKAJ116 has only 1 day of data, 
# which may not be complete, thus it is marked as an individual to remove.  However, be careful when 
# deleting a datapoint from your study and never remove individuals from the raw dataset, to ensure you 
# can always go back and include them if desired.

# Remove the specified individuals.  
# The output will be saved as a text file with the specified name. 
# This assumes the usernames are in UserName column, and will print which user(s) will be removed.   
  RemoveRows(data=items_f_id, metadata.file= ind_to_rm, 
             output.name= "VVKAJ_Items_f_id_s.txt")
  
# Load the output for further processing.
  items_f_id_s <- read.delim("VVKAJ_Items_f_id_s.txt", quote="", colClasses="character")

# Show unique usernames in items_f_id_s and confirm "VVKAJ116" has been removed.
  unique(items_f_id_s$UserName)  
  
# ===============================================================================================================
#  Merge individuals' metadata to items.   
# ===============================================================================================================
  
# ind_metadata has the participants' gender, age, height, weight, BMI, and Waist.Circumference, etc.
# If desired, this individual-specific information can be added to items data.
  
# Load ind_metadata.txt.
  ind_metadata <- read.table("ind_metadata.txt", sep="\t", header=T)
  
# Look at what the metadata has.
  head(ind_metadata)
# This includes information on the removed individual, VVKAJ116, but it will not be used 
# if VVKAJ116 is not in the items data.

# Add this metadata of each participant to totals or items.
# 'NA' will be inserted to UserNames which are not in ind_metadata.
  items_f_id_s_m <- merge(x=items_f_id_s, y=ind_metadata, by="UserName", all.x=T)
  
# Check that the items data and metadata are merged.
  head(items_f_id_s_m)

# Furthermore, as a quick way to look at the metadata of only the selected individuals, you can subset the 
# metadata to just the usernames present in the analysis dataset (items_f_id_s) using the "%in%" operator.
  ind_metadata_s <- ind_metadata[ind_metadata$UserName %in% items_f_id_s$UserName, ] 

# Use the tail function to show the last six rows of ind_metadata_s. You can see that the last individual
# in this metadata is now VVKAJ117, and that VVKAJ116, which was not in items_f_id_s, has been omitted. 
  tail(ind_metadata_s)
  
# Save the merged dataframe as a .txt file.
  write.table(items_f_id_s_m, "VVKAJ_Items_f_id_s_m.txt", sep="\t", row.names=F, quote=F)
  
  
# ===============================================================================================================
# Generate new totals file from the items file. 
# ===============================================================================================================

# Use one of the input files saved above as an input for calculating totals for.
# Specify which columns have usernames and Recall.No., which has the recorded days. 
  GenerateTotals(inputfn =   "VVKAJ_Items_f_id_s_m.txt", 
                 User.Name = "UserName", 
                 Recall.No = "RecallNo",
                 outfn =     "VVKAJ_Tot.txt")

# Load the total file generated above.
  new_totals <- read.table("VVKAJ_Tot.txt", header=T, sep="\t")

# The number of rows should be {No. of users x No. days}.
# For the example data, 16 users x 3 days = 48 rows (observations).
  nrow(new_totals) 

# View the new_totals.
  head(new_totals)

# ===============================================================================================================
#  Add the participants' metadata back to totals.
# ===============================================================================================================

# Load ind_metadata.txt if you have not done so.
  ind_metadata <- read.table("ind_metadata.txt", sep="\t", header=T)

# Add this metadata of each participant to totals.
# 'NA' will be inserted to UserNames which are not in ind_metadata. 
  new_totals_m <- merge(x=new_totals, y=ind_metadata, by="UserName", all.x=T)
  
# Check that the items data and metadata are merged.
  head(new_totals_m)
  
# Save the merged dataframe as a .txt file.
  write.table(new_totals_m, "VVKAJ_Tot_m.txt", sep="\t", row.names=F, quote=F)

# ===============================================================================================================
# Calculate the mean of totals/participant
# ===============================================================================================================
  
# Calculate the mean of the totals data across all the days for each participant.
  AverageBy(data= new_totals, by= "UserName", start.col= "FoodAmt", end.col= "A_DRINKS",
            outfn="VVKAJ_Tot_mean.txt")

# Load the output for further processing.  
  new_totals_mean <- read.table("VVKAJ_Tot_mean.txt", header=T, sep="\t")

# The number of rows should be equal to the number of users.
# This example data has 16 users, so there should be 16 rows of mean totals.
  nrow(new_totals_mean)     

# ===============================================================================================================
# Add the participants' metadata to the mean totals.
# ===============================================================================================================
  
# Load ind_metadata.txt if you have not done so.
  ind_metadata <- read.table("ind_metadata.txt", sep="\t", header=T)
  
# Add this metadata of each participant in the mean totals.
# 'NA' will be inserted to UserNames which are not in ind_metadata. 
  new_totals_mean_m <- merge(x=new_totals_mean, y=ind_metadata, by="UserName", all.x=T)
  
# Check that the mean totals and the users' metadata are merged.
  head(new_totals_mean_m, 1)
  
# Save the merged dataframe as a .txt file.
  write.table(new_totals_mean_m, "VVKAJ_Tot_mean_m.txt", sep="\t", row.names=F, quote=F)
  

# ===============================================================================================================
# Quality Control (QC) for the mean totals data
# ===============================================================================================================
# Totals data may contain outliers due to errors in dietary reporting. These errors may be due to omission or 
# inaccurate over- or under-estimation of portion size, leading to improbable nutrient totals. ASA24 provides 
# General Guidelines for Reviewing & Cleaning Data 
# (https://epi.grants.cancer.gov/asa24/resources/cleaning.html#guidelines) for identifying and removing 
# suspicious records. 
  
# Here, we will identify records that contain values that fall outside typically observed ranges of 
# kilocalories (KCAL), protein (PROT), total fat (TFAT), and vitamin C (VC). The ASA24 guide provides ranges 
# of beta carotene (BCAR), too, however, outlier checking for BCAR is omitted in this tutorial but can be 
# considered if you identify it as a nutrient that has a high variance in your study dataset.
  
# Please note that your input dataframe (QCtotals) will be overwritten after each outlier removal.

# Load your totals if necessary - to be used as input for QC.
  new_totals_mean_m <- read.table("VVKAJ_Tot_mean_m.txt", sep="\t", header=T)

# Split your dataset to males and females because different thresholds apply for males and females.
  new_totals_mean_m_M <- subset(new_totals_mean_m, Gender=="M")  
  new_totals_mean_m_F <- subset(new_totals_mean_m, Gender=="F")  

# ---------------------------------------------------------------------------------------------------------------  
# QC for males  
# Define your males totals dataset to be used as input.
  QCtotals <- new_totals_mean_m_M  

# Flag if KCAL is <650 or >5700 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 650, max = 5700)

# This function will print out rows that fall outside the specified min-max range, and a dialogue box 
# will appear outside the R Studio, asking whether to remove them. You should make sure to review these records 
# carefully to double-check if the removal is warranted. It is possible to have a valid record that could 
# meet the threshold for removal. Only you will know if you can trust the record when working with real data.

# If you find potential outlier(s) here, click "No", and view those 
# total(s) with their other nutrient intake information by running the following;
  KCAL_outliers <- subset(QCtotals, KCAL < 650 | KCAL > 5700)     
  # Sort the rows by KCAL and show only the specified variables.
  KCAL_outliers[order(KCAL_outliers$KCAL, decreasing = T),
              c('UserName', 'KCAL', 'FoodAmt', 'PROT', 'TFAT', 'CARB')]  
# If you think it is a true outlier, then run the QCOutliers command for KCAL again, and click "Yes" to 
# remove the outlier. Here for this tutorial, we will remove this individual.
  QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 650, max = 5700)

# Continue the QC process with other variables.
      
# Flag if PROT is <25 or >240 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "PROT", min = 25, max = 240)

# Flag if TFAT is <25 or >230 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "TFAT", min = 25, max = 230)

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "VC", min = 5, max = 400)

# Name the males totals after QC. 
  QCed_M <- QCtotals

# ---------------------------------------------------------------------------------------------------------------  
# QC for females  
  # Define your females totals dataset to be used as input.
  QCtotals <- new_totals_mean_m_F  
  
  # Flag if KCAL is <600 or >4400 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 600, max = 4400)
  
  # Flag if PROT is <10 or >180 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "PROT", min = 10, max = 180)
  
  # Flag if TFAT is <15 or >185 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "TFAT", min = 15, max = 185)
  
  # Flag if VC (Vitamin C) is <5 or >350 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "VC", min = 5, max = 350)
  
# Name the females totals after QC. 
  QCed_F <- QCtotals
  
# ---------------------------------------------------------------------------------------------------------------  
# Combine the rows of M and F. 
  QCtotals_MF <- rbind(QCed_M, QCed_F)
  
# Save as a .txt file.
  write.table(QCtotals_MF, "VVKAJ_Tot_mean_m_QCed.txt", sep="\t", quote=F, row.names=F)

# ===============================================================================================================
# Adjust items and totals after QC
# ===============================================================================================================
  
# ===============================================================================================================
# Remove the QC-ed individual(s) from the totals to be consistent
# ===============================================================================================================
  
# In the previous section, we have removed individual(s) that did not pass the QC from mean total data.
# We will remove those individual(s) from the totals (before taking means of days), so that we will have
# the same individuals in the mean_total and total. 
  
# Among the individuals in new_totals_m, retain only those in QCtotals_MF. 
  new_totals_m_QCed <- new_totals_m[ new_totals_m$UserName %in% QCtotals_MF$UserName, ]
  
# Save as a .txt file. This will be the total for each of the "QC-ed" individuals for each day, to be 
# used for clustering analyses. 
  write.table(new_totals_m_QCed, "VVKAJ_Tot_m_QCed.txt", sep="\t", quote=F, row.names=F)
  
# ===============================================================================================================
# Similarly, remove the QC-ed individual(s) from the items to be consistent with the QC-ed averaged totals
# ===============================================================================================================

# Among the individuals in new_totals_m, pick up only those in QCtotals_MF. 
  items_f_id_s_m_QCed <- items_f_id_s_m[ items_f_id_s_m$UserName %in% QCtotals_MF$UserName, ]
  
# Save as a .txt file. This will be the items for each of the "QC-ed" individuals for each day, to be 
# used for ordination etc. 
  write.table(items_f_id_s_m_QCed, "VVKAJ_Items_f_id_s_m_QCed.txt", sep="\t", quote=F, row.names=F)
    
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory before you start running another script.
  setwd(main_wd)
  
  