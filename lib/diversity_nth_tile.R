# ===============================================================================================================
# Define function to group Div (1,2,3,...,n) as specified, based on a diversity measure.
# Version 1
# Created on 04/12/2023 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Define function.
# ===============================================================================================================

DivNthTile <- function(input, div.var, nth.tile){
  
  # Select only those who have diversity > 0 first.
  input_largerthanzero <- input[ which(input[, div.var] > 0), ] 
  # print(dim(input_largerthanzero))
  
  percentage <- (nrow(input_largerthanzero)/nrow(input)*100)
  
  print(paste(round(percentage) , "% of the ", nrow(input), " individuals have > 0 in ", div.var, sep=""))   
  
  # Define breaks to split Shannon into halves.   
  breaks <- quantile(input_largerthanzero[, div.var], probs= seq(0, 1, 1/nth.tile))
  print(breaks)
  
  # Divide individuals in the totals based on their Shannon measure with specified breaks. 
  input_largerthanzero$Div_cut <- cut(input_largerthanzero[, div.var], breaks=breaks)
  
  # The individuals were equally split, with one data assigned with "NA". 
  # print(table(input_largerthanzero$Div_cut, useNA="always"))
  
  # Make levels and number table for adding Div numbers (1,2,...). 
  divlevtable <- data.frame(Div_cut = levels(input_largerthanzero$Div_cut),
                            Div = 1:(length(breaks)-1))
  # print(divlevtable)
  
  # # Add Div numbers to the totals.
  input_2 <- merge(input_largerthanzero, divlevtable, all.x=T, by="Div_cut")
  
  # Now, the intervals are expressed as integers.   
  # table(totals_div_4$Div, useNA = "always")
  
  # There is one row that contains NA, that is the minimum value of diversity measure and was not 
  # included in the intervals. Since it is the minimum value, it belongs to the smallest group, "1".
  input_2[is.na(input_2$Div_cut), "Div"] <- 1
  # 
  # input_2[812:813, c("Shannon", "Shannon_cut", "Div")]
  # colnames(input_2)
  # 
  # # Remove the "Shannon_cut" column.
  input_3 <- input_2[, -which(colnames(input_2) == "Div_cut")]
  # print(colnames(input_3))
  
  out <<- input_3 
  
  print(paste("Div has been grouped into ", nth.tile, " groups based on ", div.var, ". Output is named as 'out'.", sep=""))
  
  print("Table of out$Div:")
  print(table(out$Div, useNA="always"))
  
  print("Summary of each level of the Div groups is as follows:")
  
  for(i in 1: nth.tile){
    print(i)
    selectedi <- subset(input_3, Div == i)
    print(summary(selectedi[, div.var]))
  }
  
} 

# # ---------------------------------------------------------------------------------------------------------------
# # How to use it with the iris dataset, and important NOTES.
#   data(iris)
# 
#   DivNthTile (input= iris, div.var="Sepal.Width", nth.tile= 3)
#   
# # DivNthTile puts samples with the same variable values into the same subgroups, so the number of samples in each
# # subgroup may not be the same where there are the same values in the target variable.  
#   table(out$Div, useNA = "always")
#   
# # NOTE that this result is different from dplyr::ntile because ntile splits samples into subgroups with an equal number of
# # samples; whereas DivNthTile (which uses baseR::break and cut) splits samples into subgroups such that samples with the 
# # same diversity measure value will be in the same subgroup. Therefore, the number of samples in each of the subgroups may
# # not be equal because some samples had the same diversity values.
# 
# # So, in the iris e.g., ntile divided 150 samples into three subgroups, and each of them contains 50 subsamples.
# # However, some samples with Sepal.Width=2.9 are in Group 1, while some others with Sepal.Width=2.9 are in Group 2, since
# # dplyr::ntile prioritized having an equal number of samples in all the subgroups.
# 
#   iris_2 <- iris %>% mutate(threetile = ntile(Sepal.Width, 3))
#   table(iris_2$threetile, useNA = "always")
#   
#   for(i in 1: 3){
#     print(i)
#     selectedi <- subset(iris_2, threetile == i)
#     print(summary(selectedi$Sepal.Width))
#   }
#   
#   
#   
#   
# # ---------------------------------------------------------------------------------------------------------------
# # How to use it with real dietary data (IFC table): need a long prep....
# 
# # Load the generated IFC table.
#   ifc <- read.delim("Foodtree/Food_D12_FC_QC_demo_QCed_4s_3Lv.food.ifc.txt")
# 
# 
# # Take out the foodID (description) and taxonomy from ifc.
#   ifc2 <- ifc[, 2: (ncol(ifc)-1) ]
# 
# # transpose so that the SEQN will come to rows.   
#   ifc2t <- as.data.frame(t(ifc2)) 
# 
# # Add taxonomy as the column names of ifc2t. 
#   colnames(ifc2t) <- ifc$X.FOODID
# 
# 
# # Make a table to save results. 
#   SEQNdiv <- as.data.frame(matrix(nrow = nrow(ifc2t) , ncol = 4))
#   colnames(SEQNdiv) <- c("SEQN", "Shannon", "Simpson", "Invsimpson")
#   head(SEQNdiv)
# 
# # Do a loop to calculate Shannon's, Simpson, and inverse-Simpson diversity  for all SEQNs (in rows).
# # This may take a few minutes.
# 
#   for( i in 1: nrow(ifc2t) ){
#     
#     SEQNdiv[i, 1] <- rownames(ifc2t)[i]
#     SEQNdiv[i, 2] <- diversity(ifc2t[i, ], 'shannon')
#     SEQNdiv[i, 3] <- diversity(ifc2t[i, ], 'simpson')
#     SEQNdiv[i, 4] <- diversity(ifc2t[i, ], 'invsimpson')
#   } 
# 
# # some have 0 diversity --> If a row has only one non-zero values, then diversity will be zero. e.g. c(20,0,0,0,0,0,0) 
# # i.e., those only consumed one nuts/seeds/legumes food have diversity of zero.
# # None of them are normally distributed because of a lot of zero diversity values.
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Group individuals based on their diversity values.
# # Remove the "X" in the SEQNdiv$SEQN for merging. 
#   SEQNdiv$SEQN_noX <- gsub(SEQNdiv$SEQN, pattern = "X", replacement = "") 
# # Bring SEQN_noX to the first column. 
#   SEQNdiv_2 <- SEQNdiv[, c(5,2,3,4,1)] 
# # Rename the columns.
#   colnames(SEQNdiv_2)[5] <- "XSEQN"   
#   colnames(SEQNdiv_2)[1] <- "SEQN"  # numbers only; to be used for merging.   
# 
# # First, need to add the diversity values to totals. Only take the rows present in both datasets.
#   totals_div <- merge(totals, SEQNdiv_2, by='SEQN')
# 
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Finally, split people with diversity > 0 into 2 groups based on diversity measure.
# # The output has "Div" 
#   DivNthTile (input= totals_div, div.var="Shannon", nth.tile=2)
#   
#   table(out$Div)
# 

  
