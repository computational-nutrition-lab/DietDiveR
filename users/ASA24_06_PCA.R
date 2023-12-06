# ===============================================================================================================
# PCA analysis with ASA24 data.
# Version 1
# Created on 12/16/2021 by Rie Sadohara
# ===============================================================================================================

# Set your working directory as to the main directory.
  Session --> Set working direHctory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Load necessary pacages.
  library(ggplot2)
  library(ggfortify)
  
# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")
  source("lib/PCA.R")

# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# You can come back to the main directory by:
  setwd(main_wd) 

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")
  
# Before you proceed to perform PCA, create folders called "PCA_Nut_asis", "PCA_Nut_ave", "PCA_Cat_asis", 
# "PCA_Cat_asis" in your VVKAJ directory to save output.
  
# ===============================================================================================================
# Nutrient data as is, processed for clustering analyses.
# ===============================================================================================================

# Load the Nut_asis data.
  Tot_m_QCed_Nut_asis <- read.table(file="VVKAJ_Tot_m_QCed_Nut_asis_c_rv.txt", sep="\t", header=T)
    
# Name your input data.
  pca_input <- Tot_m_QCed_Nut_asis

# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)

# Scale the data and perform PCA.
  scaled_pca <- prcomp(x=pca_input, scale=TRUE)   
  
# Specify the directory (folder) to save the results.
  res_dir_nut_asis = "PCA_Nut_asis"

# Specify the prefix of filenames to be saved. 
  res_prefix_nut_asis = "VVKAJ_Nut_asis"
  
# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
  OutputPCA(pca.data=pca_input, pca.result=scaled_pca, 
            out.dir= res_dir_nut_asis, out.prefix= res_prefix_nut_asis )
  
# Combine the input (Nut before processing) with all the variables and the PC results. 
  SaveInputAndPCs(input="VVKAJ_Tot_m_QCed_Nut_asis_c.txt", pca.results = scaled_pca, 
                  out.dir= res_dir_nut_asis, out.prefix= res_prefix_nut_asis)

# [Note] Even though the input file has both nutrients (Nut) and food categories (Cat) data,  
# PCA was done with only either Nut or Cat, not both.

# ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - Diet, in this case. 
  
# Load the complete Nutrients data. (before filtering variables)
  Nut_asis_c <- read.table("VVKAJ_Tot_m_QCed_Nut_asis_c.txt", sep="\t", header=T)
  
  # Change Diet to a factor so that factor levels will be displayed in order.
  Nut_asis_c$Diet <- factor(Nut_asis_c$Diet,
                        levels= c("Vegetarian", "Vegan", "Keto", "American", "Japanese"))
  
  # Use the autoplot function. Specify which PC in the x and y arguments.
  # The 'data' argument needs the original input for PCA, not after selecting specific variables.
  Nut_asis_PC12_diet <- 
    autoplot(scaled_pca, x=1, y=2,    
             loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
             data = Nut_asis_c,  
             size= 3 ) +     
             geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Diet)) +
    theme_bw(base_size = 12) + theme(aspect.ratio = 1) +  
    no_grid + space_axes +
    scale_fill_manual(values= distinct100colors) 
  Nut_asis_PC12_diet
  
  ggsave("PCA_NUt_asis/VVKAJ_Nut_asis_PC12_diet5.pdf", 
         Nut_asis_PC12_diet, device="pdf", width=7, height=6.5)  
  
# You can do this operation for the other three datasets: Nut_ave, Cat_asis, Cat_ave, by
# changing the input names as necessary.
    
# ===============================================================================================================
# Nutrient data averaged and processed for clustering analyses.
# ===============================================================================================================
  
# Load Nut_ave data.
  Tot_m_QCed_Nut_ave <- read.table(file="VVKAJ_Tot_mean_m_QCed_Nut_ave_c_rv.txt", sep="\t", header=T)
  
# Name your input data.
  pca_input <- Tot_m_QCed_Nut_ave
  
# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
# Scale the data and perform PCA.
  scaled_pca <- prcomp(x=pca_input, scale = TRUE)   
  
# Specify the directory (folder) to save the results.
  res_dir_nut_ave = "PCA_Nut_ave" 
  
# Specify the prefix of filenames to be saved. 
  res_prefix_nut_ave = "VVKAJ_Nut_ave"
  
# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
# Input is your items/Nut input file before any prep for clustering, from which you derived the input for the PCA.
  OutputPCA(pca.data=pca_input, pca.result=scaled_pca, 
            out.dir= res_dir_nut_ave, out.prefix= res_prefix_nut_ave)
  
# Combine the input (before processing) with all the variables and the PC results.
# In the case of averaged data / user, the input file used here is xxx_ave_c.txt, which 
# has all the variables before filtering out by correlation or zero variance.
  SaveInputAndPCs(input="VVKAJ_Tot_mean_m_QCed_Nut_ave_c.txt", pca.results = scaled_pca,
                  out.dir= res_dir_nut_ave, out.prefix= res_prefix_nut_ave)
  
  # ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - Diet, in this case.   
  
# Load the complete Nut average data. (before filtering variables)
  Nut_ave_c <- read.table("VVKAJ_Tot_mean_m_QCed_Nut_ave_c.txt", sep="\t", header=T)
  
  # Change Diet to a factor so that factor levels will be displayed in order.
  Nut_ave_c$Diet <- factor(Nut_ave_c$Diet,
                           levels= c("Vegetarian", "Vegan", "Keto", "American", "Japanese"))
  
  # Use the autoplot function. Specify which PC in the x and y arguments.
  # The 'data' argument needs the original input for PCA, not after selecting specific variables.
  Nut_ave_PC12_diet <- 
    autoplot(scaled_pca, x=1, y=2,    
             loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
             data = Nut_ave_c,  
             size= 3 ) +     
      geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Diet)) +
      theme_bw(base_size = 12) + theme(aspect.ratio = 1) +  
      no_grid + space_axes +
      scale_fill_manual( values= distinct100colors) 
  Nut_ave_PC12_diet
  
  ggsave("PCA_Nut_ave/VVKAJ_Nut_ave_PC12_diet.pdf", 
         Nut_ave_PC12_diet, device="pdf", width=7, height=6.5)  
    
# ===============================================================================================================
# Food Category data as is, processed for clustering analyses.
# ===============================================================================================================
  
# Load Cat_asis data.
  Tot_m_QCed_Cat_asis <- read.table(file="VVKAJ_Tot_m_QCed_Cat_asis_c_rv.txt", sep="\t", header=T)
  
  # Name your input data.
  pca_input <- Tot_m_QCed_Cat_asis
  
  # Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
  # Scale the data and perform PCA.
  scaled_pca <- prcomp(x=pca_input, scale=TRUE)   
  
  # Specify the directory (folder) to save the results.
  res_dir_cat_asis = "PCA_Cat_asis" 
  
  # Specify the prefix of filenames to be saved. 
  res_prefix_cat_asis = "VVKAJ_Cat_asis"
  
  # Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
  OutputPCA(pca.data=pca_input, pca.result=scaled_pca, 
             out.dir= res_dir_cat_asis, out.prefix= res_prefix_cat_asis )
  
  # Combine the input (Cat before processing) with all the variables and the PC results. 
  SaveInputAndPCs(input="VVKAJ_Tot_m_QCed_Cat_asis_c.txt", pca.results = scaled_pca, 
                  out.dir= res_dir_cat_asis, out.prefix= res_prefix_cat_asis)
  
# ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - Diet, in this case. 
  # Load the complete Cat data. (before filtering variables)
  Cat_asis_c <- read.table("VVKAJ_Tot_m_QCed_Cat_asis_c.txt", sep="\t", header=T)
  
  # Change Diet to a factor so that factor levels will be displayed in order.
  Cat_asis_c$Diet <- factor(Cat_asis_c$Diet,
                        levels= c("Vegetarian", "Vegan", "Keto", "American", "Japanese"))
  
  # Use the autoplot function. Specify which PC in the x and y arguments.
  # The 'data' argument needs the original input for PCA, not after selecting specific variables.
  Cat_asis_PC12_diet <- 
    autoplot(scaled_pca, x=1, y=2,    
             loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
             data = Cat_asis_c,  size= 3) +     
      geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Diet)) +
      theme_bw(base_size = 12.5) + theme(aspect.ratio = 1) +  
      no_grid + space_axes +
      scale_fill_manual( values= distinct100colors) 
  Cat_asis_PC12_diet 
  
  ggsave("PCA_Cat_asis/VVKAJ_Cat_asis_PC12_diet.pdf", 
         Cat_asis_PC12_diet, device="pdf", width=7, height=6.5)  
  
# ===============================================================================================================
# Food category data averaged and processed for clustering analyses.
# ===============================================================================================================
  
# Load Cat_ave data.
  Tot_m_QCed_Cat_ave <- read.table(file="VVKAJ_Tot_mean_m_QCed_Cat_ave_c_rv.txt", sep="\t", header=T)
  
  # Name your input data.
  pca_input <- Tot_m_QCed_Cat_ave
  
  # Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
  # Scale the data and perform PCA.
  scaled_pca <- prcomp(x=pca_input, scale = TRUE)   
  
  # Specify the directory (folder) to save the results.
  res_dir_cat_ave = "PCA_Cat_ave" 
  
  # Specify the prefix of filenames to be saved. 
  res_prefix_cat_ave = "VVKAJ_Cat_ave"
  
  # Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
  # Input is your items/Nut input file before any prep for clustering, from which you derived the input for the PCA.
  OutputPCA(pca.data=pca_input, pca.result=scaled_pca, 
            out.dir= res_dir_cat_ave, out.prefix= res_prefix_cat_ave)
  
  # Combine the input (Nut before processing) with all the variables and the PC results. 
  # In the case of averaged Nut data / user, the input file used here is xxx_ave_c.txt, which 
  # has all the variables before filtering out by correlation or zero variance.
  SaveInputAndPCs(input="VVKAJ_Tot_mean_m_QCed_Cat_ave_c.txt", pca.results= scaled_pca, 
                  out.dir= res_dir_cat_ave, out.prefix= res_prefix_cat_ave)  

# ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - Diet, in this case. 
  
  # Load the complete Cat average data. (before filtering variables)
  Cat_ave_c <- read.table("VVKAJ_Tot_mean_m_QCed_Cat_ave_c.txt", sep="\t", header=T)
  
  # Change Diet to a factor so that factor levels will be displayed in order.
  Cat_ave_c$Diet <- factor(Cat_ave_c$Diet,
                            levels= c("Vegetarian", "Vegan", "Keto", "American", "Japanese"))
  
  # Use the autoplot function. Specify which PC in the x and y arguments.
  # The 'data' argument needs the original input for PCA, not after selecting specific variables.
  Cat_ave_PC12_diet <- 
    autoplot(scaled_pca, x=1, y=2,    
             loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
             data = Cat_ave_c,  
             size= 3 ) +
      geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Diet)) +
      theme_bw(base_size = 12) + theme(aspect.ratio = 1) +  
      no_grid + space_axes +
      scale_fill_manual( values= distinct100colors) 
  Cat_ave_PC12_diet
  
  ggsave("PCA_Cat_ave/VVKAJ_Cat_ave_PC12_diet.pdf", 
         Cat_ave_PC12_diet, device="pdf", width=7, height=6.5)  
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory
  setwd(main_wd) 


# ===============================================================================================================
# Code to create and make adjustments to each plot/file, if desired.
# ===============================================================================================================

# You can specify different directory and prefix to avoid overwriting files 
# produced by the OutputPCA function. 
  
  res_dir =    "PCA_Nut_asis_2" 
  res_prefix = "VVKAJ_Nut_asis_2"
  
# Create a scree plot.
  screep <- LineScreePlot(pca.data = pca_input, pca.result = scaled_pca)
  screep
  ggsave( paste(res_dir, paste(res_prefix, "_scree.pdf"), sep= .Platform$file.sep), 
          screep, device="pdf", width=5, height=5, units="in") 
  
# Create a biplot.
  # A biplot with the individuals as black dots and variables labelled.
  biplotdots <- BiplotDots(pca.result = scaled_pca, pca.data = pca_input, alpha = 0.5)
  biplotdots
  ggsave( paste(res_dir, paste(res_prefix, "_biplotdots.pdf"), sep= .Platform$file.sep),
          biplotdots, device="pdf", width=5, height=5, units="in")

# A biplot with the individuals labeled.
  biplotlabeled <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=T)
  biplotlabeled
  ggsave( paste(res_dir, paste(res_prefix, "_biplotlabeled.pdf"), sep= .Platform$file.sep),
          biplotlabeled, device="pdf", width=5, height=5, units="in")
  
# A biplot with the individuals labeled without the variables' arrows.
  biplotlabeledwoarrows <- BiplotLabeledwoArrows(pca.result=scaled_pca, pca.data=pca_input, 
                                                 individuals.label=T)
  biplotlabeledwoarrows 
  # Zoom in to a particular area of interest in the plot 
  biplotlabeledwoarrows + coord_cartesian(xlim=c(-0.1, 0.1), ylim=c(0.05, 0.1))
  
  ggsave( paste(res_dir, paste(res_prefix, "_biplotlabeledwoarrows.pdf"), sep= .Platform$file.sep),
          biplotlabeledwoarrows, device="pdf", width=5, height=5, units="in")
  
# Plot the directions of the variables.
  directions <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=F)
  directions
  ggsave( paste(res_dir, paste(res_prefix, "_directions.pdf"), sep= .Platform$file.sep),
          directions, device="pdf", width=5, height=5, units="in")

# Plot the contribution of the variables to a given PC: Change the PC and the file name as desired.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1", 
               positive.color="green2", negative.color="grey70", sort.variables = T)
  loadings_plot
  ggsave( paste(res_dir, paste(res_prefix, "_loadings_PC1.pdf"), sep= .Platform$file.sep),
          loadings_plot, device="pdf", width=8, height=4.8, units="in")

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)  
  