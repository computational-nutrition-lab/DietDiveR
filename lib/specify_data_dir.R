# ========================================================================================
# Move to the working directory where your data is, within "main_wd". 
# Version 1
# Created on 12.11.2021 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory 
# ========================================================================================

# You must have named the main directory ("dietary_patterns") as "main_wd". 
#  main_wd <- file.path(getwd())
#  main_wd

# Specify the name of the folder containing the data, ------------------------------------ 
# and make it as a current directory.
  SpecifyDataDirectory <- function(directory.name){
      data.folder <- paste(main_wd, directory.name,  sep = .Platform$file.sep )  
      setwd(data.folder)
      print("The data directory has been set as")
      return(data.folder)
  }

