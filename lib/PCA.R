# ===============================================================================================================
# Principal Component Analysis (PCA)    
# Version 1
# Created on 12.17.2021 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# PCA
# ===============================================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Function to create a scree plot.
# If there are 10 or less PCs in total, plot all, and else plot the first 10 PCs. 
  LineScreePlot <- function(pca.data = pca_input, pca.result = scaled_pca, out.dir, out.prefix){

    # Extract the importance of the PCs
    pca_summary <- summary(pca.result)
    
    # # Extract the Proportion of Variance
    var_explained_values <- pca_summary[["importance"]][2, ]
    
    # Create a dataframe that has the PCs and their importance (var explained by each PC)
    var_explained_df <<- data.frame(PC = seq(1:length(var_explained_values)),
                                    var_explained = var_explained_values)

    # if there are only 9 or fewer variables, plot them all; otherwise plot the first 10 PCs.
    if(length(colnames(pca.data))<10){
      myPCs <<- var_explained_df
    }else{
      myPCs <<- var_explained_df[1:10, ]    # Subset the first 10 PCs
    }      
    # Create a scree plot.
    require(ggplot2)
    screep <- ggplot(myPCs, aes(x = PC, y = var_explained*100)) + 
      geom_line() + 
      geom_point() +
      scale_x_continuous(breaks = 1:nrow(myPCs)) +
      labs(x = "Number of PCs",
           y = "Variance explained by PCs (%)") +
      theme_bw(base_size = 13) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 0.9)
    
  }

# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to create a biplot with the individuals as black dots.
  BiplotDots <- function(pca.result = scaled_pca, pca.data = pca_input, alpha=1){
    require(ggfortify) # Need ggfortify packge to use 'autoplot'.
    autoplot(object = pca.result, data = pca.data,
             loadings = T, loadings.label = T, loadings.colour = 'pink',
             loadings.label.size=3, alpha=alpha) +
      scale_x_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of X (to fit text).
      scale_y_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of Y (to fit text).
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 1)
  }

# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to create a biplot with the individuals labeled and without the variables' arrows.
  BiplotLabeledwoArrows <- function(pca.result = scaled_pca, pca.data = pca_input, 
                        individuals.label = T){
    require(ggfortify)
    autoplot(object = pca.result, data = pca.data, 
             label = individuals.label, label.size = 3, shape =FALSE,  
             loadings = F, loadings.label = F, alpha=alpha) +
      scale_x_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of X (to fit text).
      scale_y_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of Y (to fit text).
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 1)
  }
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Function to create a biplot with the individuals labeled.
  BiplotLabeled <- function(pca.result = scaled_pca, pca.data = pca_input, individuals.label = TRUE){
    require(ggfortify)
    
    if(individuals.label == TRUE){
      autoplot(object = pca.result, data = pca.data, 
               label = individuals.label, label.size = 3, shape =FALSE,  
               loadings = T, loadings.label = T, loadings.colour = 'pink',
               loadings.label.size=3) +
        scale_x_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of X (to fit text).
        scale_y_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of Y (to fit text).
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
        theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
        theme(aspect.ratio = 1)
      
    }else if(individuals.label == FALSE){
      # variables only, so needs more space around the plot, to fit long-named variables within the chart. 
      autoplot(object = pca.result, data = pca.data, 
               label = individuals.label, label.size = 3, shape =FALSE,
               loadings = T, loadings.label = T, loadings.colour = 'pink',
               loadings.label.size=3) +
        scale_x_continuous(expand = expansion(mult=c(0.3, 0.3))) + # give some space on the lower and the upper limits of X (to fit text).
        scale_y_continuous(expand = expansion(mult=c(0.3, 0.3))) + # give some space on the lower and the upper limits of Y (to fit text).
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor = element_blank()) +
        theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
        theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
        theme(aspect.ratio = 1)
    }
  } 
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Plot the contribution of each variable to PC1.  
  LoadingsPlot <- function(pca.result,  whichPC, positive.color="green2", negative.color="grey70", sort.variables=c(TRUE, FALSE)){
    
    # Save the rotation (contribution of the Variables to each PC) as a dataframe.  
    Rota <- pca.result[["rotation"]]
    Rotadf = as.data.frame(Rota)
    
    # Make a column that contains the variable names (rownames).
    Rotadf$Variables <- rownames(Rotadf)
    
    # IF keeping the original order of the Variables --------------------------------------------
    if(sort.variables==FALSE){

      # Select the Variables and only the specified PC to plot.
      aaa <- data.frame(Variables=Rotadf$Variables, Ytoplot= Rotadf[, whichPC])
      
      # Make Variables as an ordered factor (the order of levels will be preserved). 
      aaa$Variables <- factor(aaa$Variables, levels=aaa$Variables)
      
      # Calculate the position at which labels are placed for each bar. 
      n.PCx <- ifelse(aaa[, "Ytoplot"] > 0, yes= -0.01, no= aaa[, "Ytoplot"]-0.01)
      
      # Assign each value a positive color and negative color. 
      c.PCx <- ifelse(aaa[, "Ytoplot"] > 0, yes=positive.color, no=negative.color)  
      
      # Make a plot with the original order of Variables.
      loadings_plot <<- ggplot(data= aaa, aes(x=Variables, y=Ytoplot)) +
        geom_bar(stat="identity", fill=c.PCx) + 
        theme_bw() +
        labs(y=paste0(whichPC)) +
        # theme(axis.text.x = element_text(angle = 45, hjust = 1) ) + # to check if the variable orders are correct.
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ),
              axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
        # geom_text(aes(label=Variables), position=position_dodge(width = 0.5), hjust=1.1, angle=90)+
        geom_text(aes(label=Variables, y=n.PCx), position=position_dodge(width = 0.5), hjust=1, angle=90)+
        scale_y_continuous(expand = expansion(mult=c(0.5, 0.1))) # give 0.5 space at the lower limit of Y (to fit text). 
      
      # IF sorting the Variables by their contributions ---------------------------------------
    }else if(sort.variables==TRUE){
      
      # Select the Variables and only the specified PC to plot.
      bbb <- data.frame(Variables=Rotadf$Variables, Ytoplot= Rotadf[, whichPC])
      
      # Sort the Variables in the order of Y (the contribution to the specified PC.)
      bbb_s <- bbb[ order(bbb$Ytoplot, decreasing=T),  ]
      
      # Make Variables as an ordered factor.
      bbb_s$Variables <- factor(bbb_s$Variables, levels=bbb_s$Variables)   
      
      # Calculate the position at which labels are placed for each bar. 
      n.PCx <- ifelse(bbb_s[, "Ytoplot"] > 0, yes= -0.01, no= bbb_s[, "Ytoplot"]-0.01)
      
      # Assign each value a positive color and negative color. 
      c.PCx <- ifelse(bbb_s[, "Ytoplot"] > 0, yes=positive.color, no=negative.color)  
      
      # Make a plot with the original order of the Variables.
      loadings_plot <<- ggplot(data= bbb_s, aes(x=Variables, y=Ytoplot)) +
        geom_bar(stat='identity', fill=c.PCx) +
        theme_bw() +
        labs(y=paste0(whichPC)) +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ),
              axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
        geom_text(aes(label=Variables, y=n.PCx), position=position_dodge(width = 0.5), hjust=1, angle=90)+
        scale_y_continuous(expand = expansion(mult=c(0.5, 0.1))) # give 0.5 space at the lower limit of Y (to fit text). 
    }
  }
  
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to save the variance explained by each PC in the result folder.

  SaveVarExplained <- function(pca.data=pca_input, pca.result=scaled_pca, out.fn){
    # Extract the importance of the PCs
    pca_summary <- summary(pca.result)
    
    # # Extract the Proportion of Variance
    var_explained_values <- pca_summary[["importance"]][2, ]
    
    # Create a dataframe that has the PCs and their importance (var explained by each PC)
    var_explained_df <<- data.frame(PC = seq(1:length(var_explained_values)),
                                   var_explained = var_explained_values)
    
    write.table(var_explained_df, out.fn, sep = "\t", row.names = F, quote = F)
  }   
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to calculate loadings of each PC to the variables and save it as a txt file
  
  SaveLoadings <- function(pca.result = scaled_pca, out.fn){
    
    p <- pca.result[["rotation"]]
    p <- as.data.frame(scaled_pca[["rotation"]])
    
    # make a variable column. 
    variables <- rownames(p)
    p$Var <- variables

    # Sort the columns so that the rownames (variable names) come first
    sortedp <- p[, c(length(colnames(p)), 1:length(colnames(p))-1)]
    
    write.table(sortedp, out.fn, sep = "\t", row.names = F, quote = F)
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to obtain PC values and save as a txt file
  SaveInputAndPCs <- function(input, pca.results, out.dir, out.prefix){
    
    # Define your food input file from which you derived the input for the PCA. 
    pca_input <- read.table(input, sep="\t", header=T)
    # This has food codes and food names.
    
    # extract the PCs
    PCs <- as.data.frame(pca.results[["x"]]) 
    
    # These should have the same number of rows, so their difference should be zero.  
    diff <- nrow(pca_input) - nrow(PCs)

    # Gives an error message if the input and pca.result have a different number of rows.
    if(diff != 0){
      cat("Error: The input and the PCA results should have the same number of samples.")
    }else{
      
      # Add columns
      Input_PCs <<-  cbind(pca_input, PCs)
      
      # Save as a txt file.
      write.table(Input_PCs, 
                  paste(out.dir, paste(out.prefix, '_PCs.txt', sep=""), sep= .Platform$file.sep),
                  sep="\t", row.names = F, quote = F)
      
    }
  }
  
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# OutputPCA function to create and save PCA plots and outputs all at once.
  OutputPCA <- function(pca.data=pca_input, pca.result=scaled_pca, out.dir, out.prefix){

  # Create a scree plot.
    screep <<- LineScreePlot(pca.data = pca_input, pca.result = scaled_pca)
    # Save your plot 
    ggsave( paste(out.dir, paste(out.prefix, "_scree.pdf", sep=""),   sep= .Platform$file.sep), 
            screep, device="pdf", width=5, height=5, units="in") 

  # Create a biplot.
    # A biplot with the individuals as black dots and variables labelled.
    biplotdots <<- BiplotDots(pca.result = scaled_pca, pca.data = pca_input, alpha = 0.5)
    # Save your plot
    ggsave( paste(out.dir, paste(out.prefix, "_biplotdots.pdf", sep=""), sep= .Platform$file.sep),
            biplotdots, device="pdf", width=5, height=5, units="in")
    
  # Create a biplot with the individuals labeled.
    biplotlabeled <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=T)
    ggsave( paste(out.dir, paste(out.prefix, "_biplotlabeled.pdf", sep=""), sep= .Platform$file.sep),
            biplotlabeled, device="pdf", width=5, height=5, units="in")
    
  # Create a biplot with the individuals labeled without the variables' arrows.
    biplotlabeledwoarrows <<- BiplotLabeledwoArrows(pca.result=scaled_pca, pca.data=pca_input, individuals.label=T)
    ggsave( paste(out.dir, paste(out.prefix, "_biplotlabeledwoarrows.pdf", sep=""), sep= .Platform$file.sep),
           biplotlabeledwoarrows, device="pdf", width=5, height=5, units="in")

  # Plot the directions of the variables.
    directions <<- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=F)
    ggsave( paste(out.dir, paste(out.prefix, "_directions.pdf", sep=""), sep= .Platform$file.sep),
            directions, device="pdf", width=5, height=5, units="in")

  # Plot the contribution of the variables to a given PC: PC1 here.
    loadings_plot_PC1 <<- LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1",
                 positive.color="green2", negative.color="grey70", sort.variables = T)
    ggsave( paste(out.dir, paste(out.prefix, "_loadings_PC1.pdf", sep=""), sep= .Platform$file.sep),
            loadings_plot_PC1, device="pdf", width=8, height=4.8, units="in")

  # Plot the contribution of the variables to a given PC: PC2 here.
    loadings_plot_PC2 <<- LoadingsPlot(pca.result=scaled_pca,  whichPC="PC2",
                                      positive.color="green2", negative.color="grey70", sort.variables = T)
    ggsave( paste(out.dir, paste(out.prefix, "_loadings_PC2.pdf", sep=""), sep= .Platform$file.sep),
            loadings_plot_PC2, device="pdf", width=8, height=4.8, units="in")

  # ---------------------------------------------------------------------------------------------------------------
  # Save the variance explained by each PC as a .txt file.
      # Extract the importance of the PCs
      pca_summary <- summary(pca.result)

      # # Extract the Proportion of Variance
      var_explained_values <- pca_summary[["importance"]][2, ]

      # Create a dataframe that has the PCs and their importance (var explained by each PC)
      var_explained_df <- data.frame(PC = seq(1:length(var_explained_values)),
                                      var_explained = var_explained_values)

      write.table(var_explained_df, 
                  paste(out.dir, paste(out.prefix, '_PC_var_explained.txt', sep=""), sep= .Platform$file.sep),
                  sep = "\t", row.names=F, quote=F)

  # ---------------------------------------------------------------------------------------------------------------
  # Calculate loadings of each PC to the variables and save it as a txt file. 

      p <- pca.result[["rotation"]]
      p <- as.data.frame(scaled_pca[["rotation"]])
      
      # make a variable column. 
      variables <- rownames(p)
      p$Var <- variables
      
      # Sort the columns so that the rownames (variable names) come first
      sortedp <- p[, c(length(colnames(p)), 1:length(colnames(p))-1)]
      
      write.table(sortedp, 
                  paste(out.dir, paste(out.prefix, '_PC_loadings.txt', sep=""), sep= .Platform$file.sep),
                  sep = "\t", row.names=F, quote=F)
      
  # ---------------------------------------------------------------------------------------------------------------
 

  }

  
  
