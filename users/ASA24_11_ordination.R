# ===============================================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Use foodtrees gengerated Without using FilterDBByDiets. 
# After running formatfoods first and keeping ".0" in FoodIDs. 
# Version 2 
# Created on 02/16/2022 by Rie Sadohara
# 06/26/2023 replaced "OTU" with "IFC".
# ===============================================================================================================

# In this section, we will take the phylogeny of food items into account in clustering individuals according to 
# their dietary data. In order to do so, we will use the phyloseq package.(https://joey711.github.io/phyloseq/index.html), 
# which uses phylogeny of microbes and their abundance. We will replace microbes with food items consumed by 
# our dietary study participants.

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/DietR")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# If you have not downloaded and installed the phyloseq package yet: 
# You can do so by first installing BiocManager (if you have not done so):
  if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
  
# Then download and install the phyloseq package.
  # BiocManager::install("phyloseq")

# Install the devtools package necessary for installing the pairwiseAdonis package. 
  if (!require("devtools",    quietly = TRUE))install.packages("devtools")

# Install pairwise adonis function from Github. (https://github.com/pmartinezarbizu/pairwiseAdonis)
  # devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")  
  
# ---------------------------------------------------------------------------------------------------------------
# load the necessary packages.
  library(phyloseq)
  library(ggtree)
  library(pairwiseAdonis)

# Load necessary functions and ggplot formatting themes
  source("lib/specify_data_dir.R")
  source("lib/ordination.R")
  source("lib/ggplot2themes.R")
  source("lib/sort_IFC_by_ID.R")
  source("lib/plot.axis.1to4.by.factor.R")

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("lib/distinct100colors.rda")  

# You can come back to the main directory by:
  setwd(main_wd)

# ===============================================================================================================
# Create a phyloseq object for ordination.
# ===============================================================================================================
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")
  
# Load the necessary files for creating a phyloseq object.  
  
# Food
# Load IFC table, and sort the columnnames (userID), leaving the last column (taxonomy) intact.
# This dataframe will be saved as "food".
# Also, save "food" as a .txt file to be used in the "correlation between Axes and foods" section.  
  SortIFCByID(ifc.input =           "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.ifc.txt",
              outfn.for.corr.axis = "Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted.txt")
  
  # "food" is a matrix of Food descriptions (rows) x SampleID (columns).
  head(food)[1:6, 1:4]
  
  # Format the food object and create an ifc_table called IFC.
  PrepFood(data= food)
  
# Taxonomy (tax)
  tax <- read.delim("Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tax.txt")
  
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data= tax)
  
# Sample
  meta <- read.table( "ind_metadata_UserxDay.txt", sep="\t", header=T)
  
  # Format the metadata file and save it as 'SAMPLES'. 
  PrepMeta(data= meta)

# Foodtree
  foodtree <- read_tree("Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.tree.nwk")
  # It is OK to see a message saying that
    # "Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
    # Also defined by 'tidytree'".
  
  # Format foodtree and save it as 'TREE'. 
  PrepTree(data= foodtree)
  # Again, it is OK to see the same message as the previous line. 

# ---------------------------------------------------------------------------------------------------------------
# Make a phyloseq object with IFC, TAX, SAMPLES, and TREE.
  phyfoods <- phyloseq(IFC, TAX, SAMPLES, TREE)
  # It is OK to see a message (or multiple of them) saying that
    # Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
    # Also defined by 'tidytree'.

# Check your metadata
  # Show the sample names and ensure they are vvkaj.00xxx. 
  sample_names(phyfoods)
  
# Show metadata. 
  head(sample_data(phyfoods), n=3)
  
# Check the level 1 foods in your food tree.
  L1s = tax_table(phyfoods)[, "L1"]
  as.vector(unique(L1s))

# ===============================================================================================================
# Use your phyloseq object and perform ordination 
# ===============================================================================================================

# Change to the folder called "Ordination" in your "VVKAJ" folder.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Ordination/")
  
# Perform Principal Coordinate Analysis (PCoA) with weighted unifrac distance of your food data.
# Ordination by UNweighted unifrac distances can be done by having the "weighted" argument as FALSE. 
# This may take a few minutes depending on your data size.
# e.g. a large phyloseq object (7.9 MB) takes ~ 1 min. 
  ordinated <- phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=TRUE) 

# Save the percent variance explained as a txt file.
  Eigen(eigen.input = ordinated$values$Relative_eig, 
        output.fn="VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_eigen_percent.txt")

# Merge the first n axes to the metadata and save it as a txt file. 
# This will be used for plotting ordination results.
  MergeAxesAndMetadata(ord.object=ordinated, number.of.axes=10, meta.data= meta, 
                       output.fn= "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt")
  
# ===============================================================================================================
# Plot your ordination results 
# ===============================================================================================================
  
# Read in the eigenvalues for axis labels of biplots.
  eigen_loaded <- read.table("VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_eigen_percent.txt", header=T)
  
# Make a vector that contains the variance explained.
  eigen_loaded_vec <- eigen_loaded[, 2]
  
# Read in the metadata and users' Axis values.
  meta_usersdf <- read.table("VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt", header=T)    

# Change Diet to a factor so that factor levels will be displayed in order.
  meta_usersdf$Diet <- factor(meta_usersdf$Diet,
                              levels= c("Vegetarian", "Vegan", "Keto", "American", "Japanese"))  
    
# Take a look at meta_usersdf that has been loaded. 
  head(meta_usersdf, 3)
  
# ---------------------------------------------------------------------------------------------------------------
# Save Axes 1 & 2, 1 & 3, 2 & 3, 3 & 4, 2 & 4 biplots with and without ellipses with specified confidence interval.
# The results are saved with filenames with the specified "prefix_AxisXY.pdf" or "prefix_AxisXY_ellipses.pdf".
# You need to supply the same number of colors in the order of the factor level to be used. 
# dot.colors are for datapoints, and ellipses.colors are for ellipses outlines. 
  PlotAxis1to4ByFactor(axis.meta.df    = meta_usersdf, 
                       factor.to.color = "Diet", 
                       eigen.vector    = eigen_loaded_vec,
                       dot.colors      = distinct100colors, 
                       ellipses.colors = distinct100colors,  
                       ellipses.cflevel = 0.95,
                       out.prefix = "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_diet"
  )
  
# ---------------------------------------------------------------------------------------------------------------
# Permanova tests  
  
# Some of the Diet groups seem to form distinct clusters. Use beta-diversity and adonis (permanova) tests 
# to see if they are actually distinct from one another.
  
# Generate a weighted unifrac distance matrix.
  dist_matrix <- phyloseq::distance(phyfoods, method = "wunifrac")
  
# Perform dispersion test.
# vegan::betadisper computes centeroids and distance of each datapoint from it. 
  dispr <- vegan::betadisper(d=dist_matrix, phyloseq::sample_data(phyfoods)$Diet)
  
# Show the centroids and dispersion of each group. 
  plot(dispr)
  
# Use dispr to do a permutation test for homogeneity of multivariate dispersion.  
# The set.seed function ensures the same permutation results will be obtained every time; 
# otherwise, the p-values will slightly differ each run, as it is a permutation test.

  set.seed(123)
  vegan::permutest(dispr, perm=5000)
  # If p>0.05, the dispersion of each group are not different, and the assumption for adonis is met.
  # The results here indicate that the dispersion of each group may be different, so we should consider 
  # this information in discussion. Nevertheless, we will proceed for demonstration purposes. 
  
# Use adonis to test whether there is a difference between groups' composition. 
# i.e., composition among groups (food they consumed) is similar or not.
  set.seed(123)
  vegan::adonis(dist_matrix ~ phyloseq::sample_data(phyfoods)$Diet, permutations = 5000)
  
# If overall adonis is significant, which is true in this case,  
# you can run pairwise adonis to see which group pairs are different.
  pairwise.adonis(dist_matrix, phyloseq::sample_data(phyfoods)$Diet, perm = 5000,
                  p.adjust.m = "none")    

  
# ===============================================================================================================
# Save weighted unifrac distance matrix
# ===============================================================================================================
# Generate and save a weighted unifrac distance matrix of "Samples". 
  WeightedUnifracDis(input.phyloseq.obj = phyfoods, 
                     output.fn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_WEIGHTED_uni_dis.txt")        
  

# ===============================================================================================================
# Use unweighted unifrac distance.  
# ===============================================================================================================
# You can perform Principal Coordinate Analysis (PCoA) with UNweighted unifrac distance of your food data.
  ordinated_u = phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=FALSE)  
  
# Use the same code above for creating plots, but now with ordinated_u for the ord.object argument, and 
# change WEIGHTED to UNweighted, or an appropriate name for the method you selected.
  
  # Save the percent variance explained as a txt file.
  Eigen(eigen.input = ordinated_u$values$Relative_eig, 
        output.fn="VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_eigen_percent.txt")
  
  # Merge the first n axes to the metadata and save it as a txt file. 
  # This will be used for plotting ordination results.
  MergeAxesAndMetadata(ord.object=ordinated_u, number.of.axes=10, meta.data= meta, 
                       output.fn= "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_meta_users.txt")
  
# ===============================================================================================================
# Plot your ordination results 
# ===============================================================================================================
  
# Read in the eigenvalues for axis labels of biplots.
  eigen_loaded <- read.table("VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_eigen_percent.txt", header=T)
  
# Make a vector that contains the variance explained.
  eigen_loaded_vec <- eigen_loaded[, 2]
  
# Read in the metadata and users' Axis values. 
  meta_usersdf <- read.table("VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_meta_users.txt", header=T)    

# Change Diet to a factor so that factor levels will be displayed in order.
  meta_usersdf$Diet <- factor(meta_usersdf$Diet,
                              levels= c("Vegetarian", "Vegan", "Keto", "American", "Japanese"))  
    
# Take a look at meta_usersdf that has been loaded. 
  head(meta_usersdf, 3)
  
# ---------------------------------------------------------------------------------------------------------------
# Save Axes 1 & 2, 1 & 3, 2 & 3, 3 & 4, 2 & 4 biplots with and without ellipses with specified confidence interval.
# The results are saved with filenames with the specified "prefix_AxisXY.pdf" or "prefix_AxisXY_ellipses.pdf".
# You need to supply the same number of colors in the order of the factor level to be used. 
# dot.colors are for datapoints, and ellipses.colors are for ellipses outlines. 
  PlotAxis1to4ByFactor(axis.meta.df    = meta_usersdf, 
                       factor.to.color = "Diet", 
                       eigen.vector    = eigen_loaded_vec,
                       dot.colors      = distinct100colors, 
                       ellipses.colors = distinct100colors,  
                       ellipses.cflevel = 0.95,
                       out.prefix = "VVKAJ_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_diet"
  )    
  
# ---------------------------------------------------------------------------------------------------------------
# Permanova tests
  
# Some of the Diet groups seem to form distinct clusters. Use beta-diversity and adonis tests 
# to see if they are actually distinct from one another.

# Generate a weighted unifrac distance matrix.
  dist_matrix <- phyloseq::distance(phyfoods, method = "unifrac")  # UNweighted
  
# Dispersion test and plot
# vegan::betadisper computes centeroids and distance of each datapoint from it. 
  dispr <- vegan::betadisper(d=dist_matrix, phyloseq::sample_data(phyfoods)$Diet)
  
  # Show the centroids and dispersion of each group. 
  plot(dispr)
  
  # Use dispr to do a permutation test for homogeneity of multivariate dispersion.
  set.seed(123)
  vegan::permutest(dispr, perm=5000)

  # If p>0.05, the dispersion of each group are not different, and the assumption for adonis is met.
  # The results here indicate that the dispersion of each group may be different, so we should consider 
  # this information in discussion. Nevertheless, we will proceed for demonstration purposes. 
  
  # Use adonis to test whether there is a difference between groups' composition. 
  # i.e., composition among groups (food they consumed) is similar or not.
  set.seed(123)
  vegan::adonis(dist_matrix ~ phyloseq::sample_data(phyfoods)$Diet, permutations = 5000)
  
  # If overall adonis is significant, which is true in this case,  
  # you can run pairwise adonis to see which group pairs are different.
  pairwise.adonis(dist_matrix, phyloseq::sample_data(phyfoods)$Diet, perm = 5000,
                  p.adjust.m = "none")    
  
# ===============================================================================================================
# Save unweighted unifrac distance matrix
# ===============================================================================================================

# Generate and save an unweighted unifrac distance matrix of "Samples". 
  UnweightedUnifracDis(input.phyloseq.obj = phyfoods, 
                       output.fn = "VVKAJ_Items_f_id_s_m_QCed_4Lv_UNweighted_uni_dis.txt")        

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)  
  
