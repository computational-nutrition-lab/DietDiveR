# ====================================================================================================================
# Add Gender and Age variables in NHANES dataset.
# Copied from percent_kcal.R.
# Version 1
# Created on 12/01/2022 by Rie Sadohara
# ====================================================================================================================

# ====================================================================================================================
# Function to add gender and age groups to NHANES totals data.
# ====================================================================================================================

  AddGenderAgeGroups <- function(input=totals, age.col="RIDAGEYR", gender.col="RIAGENDR"){
    
    # Rename input. 
    totals2 <- input
    
    # column number of gender 
    gender.col.number <- which(colnames(totals2)==gender.col)
    
    # Add a new column of Gender.
    totals2$Gender = NA
    
    # Add gender index to totals.
    for(i in 1:nrow(totals2)){
      if(     totals2[i, gender.col.number]==1){totals2$Gender[i] <- "M"}
      else if(totals2[i, gender.col.number]==2){totals2$Gender[i] <- "F"}
    }  
    
    # column number of age 
    age.col.number <- which(colnames(totals2)==age.col)
    
    # Add a new column of Age group.
    totals2$AgeGroup = NA
    
    # Add age group. 
    for(i in 1:nrow(totals2)){
      if(     totals2[i, age.col.number] < 20){totals2$AgeGroup[i] <- "18_19"}
      else if(totals2[i, age.col.number] < 30){totals2$AgeGroup[i] <- "20s"}
      else if(totals2[i, age.col.number] < 40){totals2$AgeGroup[i] <- "30s"}
      else if(totals2[i, age.col.number] < 50){totals2$AgeGroup[i] <- "40s"}
      else if(totals2[i, age.col.number] < 60){totals2$AgeGroup[i] <- "50s"}
      else if(totals2[i, age.col.number] < 70){totals2$AgeGroup[i] <- "60s"}
      else if(totals2[i, age.col.number] < 80){totals2$AgeGroup[i] <- "70s"}
      else                                    {totals2$AgeGroup[i] <- "80plus"}
    }
    
    # Combine Age_Group and Gender as a new factor. e.g. "M_40s".
    totals2$Gender_Age <- paste(totals2$Gender, totals2$AgeGroup, sep="_")
    
    # Name the output and make it usable outside the function. 
    totals_out <<- totals2
    
  }

# ---------------------------------------------------------------------------------------------------------------
