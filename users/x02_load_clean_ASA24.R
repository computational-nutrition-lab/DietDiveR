# ===============================================================================================================
# Load and clean ASA24 data.
# Version 1
# Created on 02/04/2022 by Rie Sadohara
# ===============================================================================================================

# Use Metadata 1 to filter out individuals. 
# Remove users that has only a small number of totals (days of record). - if you know which one to remove.  
# Look for outliers in your totals by nutrient consumed on each day. 

# Calculate totals by occasion. - extra dataset. 

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use.
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")  
  source("lib/load_clean_ASA24.R")
  source("lib/format.file.R")
  
# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Load ASA24 data
# ===============================================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name= "eg_data/VVKAJ/")

# Load your raw items data (as downloaded from the ASA24 study website).
# The csv file will be loaded as a dataframe in R and be named as items_raw. 
  items_raw <-  read.csv("VVKAJ_Items.csv", sep = ",", header=T) 
  head(items_raw)
  
# Save it as a .txt file. 
  write.table(items_raw, "VVKAJ_Items.txt", sep="\t", row.names=F) 

# Special characters such as "'", ",", "%" may interfere correct data loading; thus,
# we replace them with an underscore "_".  Takes only .txt files as input.
# Specify column(s) to be processed in the "columns" argument.
# Specify the output file name in the outfn argument; "_f" stands for "formatted".  
  format.file(filename = "VVKAJ_Items.txt",
              columns  = "Food_Description", 
              outfn    = "VVKAJ_Items_f.txt")  

# Add SampleID with a desired prefix, and save it as a txt file. SampleIDs are IDs unique 
# to each combination of users and day.  
  AddSampleIDtoItems(input.fn="VVKAJ_Items_f.txt", user.name="UserName", recall.no="RecallNo", 
                     prefix="vvkaj.", out.fn="VVKAJ_Items_f_id.txt")

# Load the formatted Items file.
  items_f_id <- read.table("VVKAJ_Items_f_id.txt", sep="\t", header=T)
  
# A combination of the specified prefix and sequential number should be added.    
  head(items_f_id)

# Ensure your items file has the expected dimensions (number of rows x number of columns,
# shown as number of obs. and number of variables) in the environment window of R Studio, or
# you can also check the dimension of items_f by using the dim() function.
  dim(items_f_id)

# ===============================================================================================================
# <Optional> Use individuals_to_remove.txt to filter out users marked as Remove = yes.  
# ===============================================================================================================
# Load your metadata that has information about which UserName(s) to remove. 
  ind_to_rm <- read.table("individuals_to_remove.txt", sep="\t", header=T)

  ind_to_rm
  # Metadata for this purpose (ind_to_rm) has UserName and which one to be removed:
  #     UserName Remove
  # 1   VVKAJ101       
  # 2   VVKAJ102    
  # ... ...        
  # ... ...        
  # 16  VVKAJ116   yes 

# Show which has "yes" in the "Remove" column. 
  subset(ind_to_rm, Remove == "yes")

# Remove the specified individuals.  
# The output will be saved as a text file with the specified name. 
# This assumes the usernames are in UserName column, and will print which user(s) will be removed.   
  RemoveRows(data=items_f_id, metadata.file= ind_to_rm, 
             output.name= "VVKAJ_Items_f_id_s.txt")
  
# Load the output for further processing.
  items_f_id_s <- read.table("VVKAJ_Items_f_id_s.txt", header=T, sep="\t")
  
# Show unique usernames in items_f_id_s and confirm "VVKAJ116" has been removed.
  unique(items_f_id_s$UserName)  
  
# ===============================================================================================================
# <Optional> Merge individuals' metadata to items.   
# ===============================================================================================================
  
# ind_metadata has the participants' gender, age, height, weight, BMI, and Waist.Circumference, etc.
# If desired, this individual-specific information can be added to items data.
  
# Load ind_metadata.txt.
  ind_metadata <- read.table("ind_metadata.txt", sep="\t", header=T)
  
# Look at what the metadata has.
  head(ind_metadata)
  
# Add this metadata of each participant in totals or items.
# 'NA' will be inserted to UserNames which are not in ind_metadata.
  items_f_id_s_m <- merge(x=items_f_id_s, y=ind_metadata, by="UserName", all.x=T)
  
# Check that the items data and metadata are merged.
  head(items_f_id_s_m)
  
# Save the merged dataframe as a .txt file.
  write.table(items_f_id_s_m, "VVKAJ_Items_f_id_s_m.txt", sep="\t", row.names=F, quote=F)

  
# ===============================================================================================================
# Generate new totals file if any edits were made to the items file. 
# ===============================================================================================================

# Use one of the input files saved above as an input for calculating totals for.
# Specify which columns have usernames and Recall.No., which is the number of recorded days. 
  GenerateTotals(inputfn = "VVKAJ_Items_f_id_s_m.txt", 
                 User.Name = 'UserName', 
                 Recall.No = 'RecallNo',
                 outfn = "VVKAJ_Tot.txt")

# Load the total file generated above.
  new_totals <- read.table("VVKAJ_Tot.txt", header=T, sep="\t")

# The number of rows should be {No. of users x No. days}.
# For the example data, 15 users x 3 days = 45 rows (observations).
  nrow(new_totals) 

# View the new_totals
  head(new_totals)

# ===============================================================================================================
# <Optional> Add the participants' metadata back to totals.
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
# QC totals data
# ===============================================================================================================
# Look for outliers in your totals. 
# Note that input dataframe (QCtotals) will be overwritten after outlier removal.
  
# Load your totals if necessary - to be used as input for QC.
  new_totals <- read.table("VVKAJ_Tot_m.txt", sep="\t", header=T)
    
# Define your totals dataset to be used as input.
  QCtotals <- new_totals  

# Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 600, max = 5700)

# Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "PROT", min = 10, max = 240)

# Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "TFAT", min = 15, max = 230)

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "VC", min = 5, max = 400)
    
      # You may find numerous potential outliers here. Then, click "No", and view those 
      # outliers with their other nutrient intake information by running the following;
      VC_outliers <- subset(new_totals, VC < 5 | VC > 400)    
      # sort in the order of VC and show only the specified variables.
      VC_outliers[order(VC_outliers$BCAR, decreasing = T),
                  c('UserName', 'KCAL', 'VC', 'V_TOTAL', 'V_DRKGR', 'F_TOTAL')]  # F is fruits.

# Save as "Totals_m_QCed.txt"
  write.table(QCtotals, "VVKAJ_Tot_m_QCed.txt", sep="\t", quote=F, row.names=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory before you start running another script.
  setwd(main_wd)
  