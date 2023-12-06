# ===============================================================================================================
# Add metadata, Age and Gender, and GLU_index (Normal, Prediabetic, and Diabetic) to the mean totals. 
# Version 1
# Created on 11/21/2022 by Rie Sadohara
# ===============================================================================================================

# In this script, we will add participants' metadata, and GLU_index variable based on their blood glucose level. 

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR/")
  
# Name your main directory for future use. 
  main_wd <- file.path(getwd())  

# Load necessary packages.
  library(SASxport)
  
# Load necessary functions.
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R") 
  source("lib/add_gender_and_age.R") # to use AddGenderAgeGroups function.  
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  

# Create a folder called "Laboratory_data" under "NHANES", so that our results with laboratory data (glucose 
# tolerance test) can be saved.   

# ===============================================================================================================
# Load NHANES15-16totals with demographic data
# ===============================================================================================================
 
# Load the QC-ed total (with food categories), filtered for KCAL, PROT, TFAT, VC. 4,164 people.
  QCtotal_d <- read.table("Total_D12_FC_QC_mean_QC_demo.txt", sep="\t", header=T)
                           
# Check the number of participants in the QCtotals - should be 4,614 people.
  length(unique(QCtotal_d$SEQN))

    
# ===============================================================================================================
# Add Gender and Age, body measure, and metadata.
# ===============================================================================================================

# We are going to use the following columns:
# RIAGENDR = gender
# RIDAGEYR = age
  
# Add gender and age_groups to QCtotal_d_glu_body_meta. The output is named "totals_out".
  AddGenderAgeGroups(input=QCtotal_d, age.col="RIDAGEYR", gender.col="RIAGENDR")

# Rename the output as QCtotal_d_ga. 
  QCtotal_d_ga <- totals_out  
    
# Ensure grouping has been done correctly. 
  head(QCtotal_d_ga[, c("RIAGENDR", "Gender", "RIDAGEYR", "AgeGroup", "Gender_Age")])
  
# Check the frequency of the groups. As expected, people aged 18-19 are less frequent.
  table(QCtotal_d_ga$Gender_Age)  
  table(QCtotal_d_ga$AgeGroup)    

# ---------------------------------------------------------------------------------------------------------------
# Add body measure data.
  
# Download the body measure data from NHANES website and save it in "Raw_data" folder. 
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT", 
                destfile= "Raw_data/BMX_I.XPT", mode="wb")  
  
# Load the body measure data.
  bodymea <- read.xport("Raw_data/BMX_I.XPT")

# Explanation of variables can be found from NHANES: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.htm
# Relevant variables here include:
  # BMDSTATS - Body Measures Component Status Code: 
  #            1 == Complete data for age group.
  #            2 ==	Partial: Only height and weight obtained
  # BMXHT - Standing Height (cm)
  # BMIHT - Standing Height Comment
  # BMXBMI - Body Mass Index (kg/m**2)
  # BMXWAIST - Waist Circumference (cm)

# Add body measure to QCtotal_d.
  QCtotal_d_ga_body <- merge(x=QCtotal_d_ga, y=bodymea, by="SEQN")

# ---------------------------------------------------------------------------------------------------------------
# Download the metadata for people, who are in Total Day 1 from the NHANES website, and save it in "Raw_data" folder. 
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.XPT", 
                destfile= "Raw_data/DR1TOT_I.XPT", mode="wb")  
  
# Load the metadata in Total Day 1.
  metadata_raw <- read.xport("Raw_data/DR1TOT_I.XPT")

# Total Day 1 has "dietary data for day 1" and "metadata", but we only need the metadata; thus, take out
# only the metadata columns (variables) and exclude the day 1 data.
# Column names' descriptions can be found here: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.htm
  
# First, specify the first and the last column names to select.
# Look for the column number that matches the first and last variable specified.
  sta_col_num_a <- match("DBQ095Z"  , names(metadata_raw))  # Salt-related questions
  end_col_num_a <- match("DRQSPREP" , names(metadata_raw))
  sta_col_num_b <- match("DRQSDIET" , names(metadata_raw))  # Diet-related questions
  end_col_num_b <- match("DRQSDT91" , names(metadata_raw))
  sta_col_num_c <- match("DRD340"   , names(metadata_raw))  # Fish-related questions
  end_col_num_c <- match("DRD370V"  , names(metadata_raw))

# Only select the metadata variables and SEQN, which is in column 1.
  metadata_only <- metadata_raw[, c(1,
                                    sta_col_num_a:end_col_num_a,
                                    sta_col_num_b:end_col_num_b,
                                    sta_col_num_c:end_col_num_c
                                    )]

# Check that this has only the SEQN and metadata columns.
  colnames(metadata_only)

# Add meatadata to QCtotal_d_glu_body
  QCtotal_d_ga_body_meta <- merge(x=QCtotal_d_ga_body, y=metadata_only, by="SEQN")

# Save as a .txt file. This can be used for answering research questions other than glycaemic index. 
  write.table(QCtotal_d_ga_body_meta, "Total_D12_FC_QC_mean_QC_demo_ga_body_meta.txt",
              sep="\t", row.names=F, quote=F)

# ===============================================================================================================
# Load the blood glucose data, add GLU_index, and filter out rows containing missing data
# ===============================================================================================================

# Download the blood glucose data from NHANES website, and save it in "Raw_data" folder. 
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/GLU_I.XPT", 
                destfile= "Raw_data/GLU_I.XPT", mode="wb")  
    
# Load the blood glucose data and see.
  glu <- read.xport("Raw_data/GLU_I.XPT")

# glu has LBXGLU - Fasting Glucose (mg/dL).
  head(glu)

# Check its distribution.
  hist(glu$LBXGLU)
  
# Add glu to QCtotal_d_ga_body_meta.
  QCtotal_d_ga_body_meta_glu <- merge(x=QCtotal_d_ga_body_meta, y=glu, by="SEQN")
  
# ---------------------------------------------------------------------------------------------------------------
# Add GLU_index according to their glucose level: Normal, Prediabetic, and Diabetic.
  # Normal     : 99 mg/dL or lower
  # Prediabetic: 100 to 125 mg/dL
  # Diabetic   : 126 mg/dL or higher

# If LBXGLU is missing, put "NA"; if it has a value, add GLU_index category in "GLU_index" column.
  QCtotal_d_ga_body_meta_glu$GLU_index <- ifelse(
    # test sentence
    is.na(QCtotal_d_ga_body_meta_glu$LBXGLU) == TRUE,
    
    # if LBXGLU is NA, put NA to GLU_index column. 
    NA,
    
    # Otherwise, put "Normal", "Prediabetic" or "Diabetic" to GLU_index column. 
    ifelse(QCtotal_d_ga_body_meta_glu$LBXGLU < 100,
           "Normal", 
           ifelse(QCtotal_d_ga_body_meta_glu$LBXGLU < 126,
                  "Prediabetic",
                  "Diabetic"))
  )
  
  # Check the first 30 rows of glucose and GLU_index columns in QCtotal_d_glu_body_meta.
  QCtotal_d_ga_body_meta_glu[1:30, c("LBXGLU", "GLU_index")]
  
  # ---------------------------------------------------------------------------------------------------------------
  # There are some missing data, so check the frequency with useNA argument to show NAs.
  table(QCtotal_d_ga_body_meta_glu$GLU_index, useNA="always")
  
  # Select individuals that have no missing data in the LBXGLU column.
  QCtotal_d_ga_body_meta_glu_comp <- QCtotal_d_ga_body_meta_glu[!is.na(QCtotal_d_ga_body_meta_glu$LBXGLU), ]
  
  # Check the number of rows - should have 1943 rows.   
  nrow(QCtotal_d_ga_body_meta_glu_comp)
  
  # Double-check there is no missing data in GLU_index.
  table(QCtotal_d_ga_body_meta_glu_comp$GLU_index, useNA="always")
  
  
  # ===============================================================================================================
  # Exclude individuals who are following special diets
  # ===============================================================================================================
  
  # There may be some participants following special diets such as low-sodium or gluten-free. Detailed explanation 
  # about the special diet question can be found on the documentation
  # [https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Dietary&Cycle=2015-2016].
  # For this demonstration, we will select only those who are eating freely without following any diet.  
  
  # Check the number of individuals who are following any specific diet (DRQSDIET==1).
  table(QCtotal_d_ga_body_meta_glu_comp$DRQSDIET, useNA="always")
  
  # DRQSDIET==1 is following a special diet, so select only rows with DRQSDIET==2. 
  QCtotal_d_ga_body_meta_glu_comp_2 <- subset(QCtotal_d_ga_body_meta_glu_comp, DRQSDIET == 2)
  
  # How many people remained? -- 1610 remained.
  table(QCtotal_d_ga_body_meta_glu_comp_2$DRQSDIET)
  
  # Check the sample size of each category.
  table(QCtotal_d_ga_body_meta_glu_comp_2$GLU_index, useNA="always")
  
  # Diabetic      Normal Prediabetic        <NA> 
  #   208         679         723           0 
  
  # Save the dataset as a .txt file in the folder called "Laboratory_data".
  write.table(QCtotal_d_ga_body_meta_glu_comp_2, file="Laboratory_data/QCtotal_d_ga_body_meta_glu_comp_2.txt",
              sep= "\t", row.names=F, quote= F)
  
  # ---------------------------------------------------------------------------------------------------------------
  # Come back to the main directory before you start running another script.  
  setwd(main_wd)
  
  