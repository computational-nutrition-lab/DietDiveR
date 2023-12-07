# FUNCTIONS ==============================================================================

# ========================================================================================
# k-means clustering 
# Version 1
# Created on 01/06/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Find the ideal k
#  Modified code from https://uc-r.github.io/kmeans_clustering
# ========================================================================================
# ---------------------------------------------------------------------------------------------------------------
# Function to do the Elbow method.
  ElbowMethod <- function(k.values=1:15){
    # set.seed(123)
    
    # Define a function to compute total within-cluster sum of square 
    wss <- function(k, data) {
      kmeans(data, k, nstart = 25)$tot.withinss
    }

    # extract wss for 2-15 clusters
    wsstable <- data.frame(K=k.values, WithinClusterSS=NA)
    for(i in k.values){
      wssvalue <- wss(k.values[i], data = kmeans_input)
      wsstable[i, 2] <- wssvalue
    }
    
    # create a wss value plot 
    ggplot(wsstable, aes(x = K, y = WithinClusterSS)) + 
      geom_line() + 
      geom_point() +
      scale_x_continuous(breaks = 1:nrow(wsstable)) +
      labs(x = "Number of clusters K",
           y = "Total within-clusters sum of squares") +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 0.9)
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Find the ideal k: the Silhouette method # need the cluster package
 
 SilhouetteMethod <- function(k.values = 2:15){
   
   # Define avg_sil function first.
     avg_sil <- function(number){
       km.res <- kmeans(kmeans_input, centers=number, nstart=25)
       ss <<- silhouette(km.res$cluster, dist(kmeans_input))
       mean(ss[, 3])
     }
     
   # Create a dataframe with k values.
     siltable3  <- data.frame(K=k.values)
   # Apply avg_sil function to each of the K and save results as a vector. 
     resultvec <- apply(siltable3, MARGIN=1, FUN = avg_sil)
   # Save the result vector as a new column of siltable.
     siltable3$Avg_Silhouette <- resultvec
  
   # Plot K and the Silhouette values.    
     ggplot(siltable3, aes(x = K, y = Avg_Silhouette)) + 
       geom_line() + 
       geom_point() +
       scale_x_continuous(breaks = siltable3$K) +
       labs(x = "Number of clusters K",
            y = "Average Silhouettes") +
       theme(panel.grid.major = element_blank()) +
       theme(panel.grid.minor = element_blank()) +
       theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
       theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
       theme(aspect.ratio = 0.9)
     # The K with the max average Silhouette value is the ideal K. 
   } 

# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Find the ideal k: Gap statistic method. use the cluster package.

  GapMethod <- function(k.values=1:15){

    # Calculate the gap statistic.
    gap_stat <- clusGap(kmeans_input, FUN = kmeans, nstart = 25,
                        K.max = k.values[length(k.values)], 
                        B=50) # B is the number of bootstrapping
    # Print the result.
    print(gap_stat, method = "firstmax")
    
    # Convert the table to a dataframe first.
    gap_stat_df <- as.data.frame(gap_stat[1])
    # Add the number of clusters as a new column.
    gap_stat_df$NumberofK <- k.values 
    
    # Plot the gap statistic with ggplot2
    ggplot(gap_stat_df, aes(x=NumberofK, y=Tab.gap)) + 
      geom_line() + 
      geom_point() +
      geom_errorbar(aes(ymin=Tab.gap-Tab.SE.sim, 
                        ymax=Tab.gap+Tab.SE.sim),  
                    width=0.2, 
                    position=position_dodge(0.05)) +
      scale_x_continuous(breaks = 1:nrow(gap_stat_df)) +
      labs(x = "Number of clusters K",
           y = "Gap stastistic") +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 0.9)
    # The highest K is the optimum K. 
  }

  # Or use the factoextra package.
  
  FactoextraGapMethod <- function(k.values = 1:15){
    
  
    # Calculate Gap statistics first.
    gap_stat <- clusGap(kmeans_input, FUN = kmeans, nstart = 25,
                        K.max = k.values[length(k.values)], 
                        B=50) # B is the number of bootstrapping

    # Print the result.
    print(gap_stat, method = "firstmax")
    # Visualize. The best K is marked with a dotted line. 
    factoextra::fviz_gap_stat(gap_stat)
  }
# ---------------------------------------------------------------------------------------------------------------
  
# ========================================================================================
# Run 3 methods to find the best K and save the output in specified folder. 
# The 3 methods are: elbow, silhouette, and gap methods. 
# ========================================================================================
  
  ChooseK <- function(out.dir= res_dir, out.prefix= res_prefix){
 
    # Set seed for consistent results.
    set.seed(123)
    
    # Detemine max K to try. If there are 15 or more observations, go with 15;
    # if there are less than 15 observations, go with the number of observations. 
    if(nrow(kmeans_input)<15){
      maxK = nrow(kmeans_input)
    }else{
      maxK = 15
    }
    
    # ---------------------------------------------------------------------------------------------------------------
    # Use the elbow method to find the ideal K. K cannot be larger than the number of datapoints (rows) in input. 
    elbowmethod <- ElbowMethod(k.values = 1 : (maxK-1) )
    
    # Save the elbowmethod graphic (K vs total within-clusters sum of squares) as a pdf.
    ggsave( paste(out.dir, paste(out.prefix, "_elbowmethod.pdf", sep=""), sep= .Platform$file.sep),
            elbowmethod, device="pdf", width=5, height=5, units="in")

    # ---------------------------------------------------------------------------------------------------------------
    # Use the Silhouette method to find the ideal K. This uses the cluster and factoextra package.
 
    silhouettechart <- factoextra::fviz_nbclust(kmeans_input, kmeans, k.max= maxK-1, method="silhouette")

    # Save the silhouette method graphic as a pdf.
    ggsave( paste(out.dir, paste(out.prefix, "_silhouettemethod.pdf", sep=""), sep= .Platform$file.sep),
            silhouettechart, device="pdf", width=5, height=5, units="in")

    # ---------------------------------------------------------------------------------------------------------------
    # Use the factoextra package to use the Gap statistic method.
    gapchart <- FactoextraGapMethod(k.values = 1: (maxK-1) )

    # Save the silhouette method graphic as a pdf.
    ggsave( paste(out.dir, paste(out.prefix, "_gapmethod.pdf", sep=""), sep= .Platform$file.sep),
            gapchart, device="pdf", width=5, height=5, units="in")
   
  }
  
  
  
# ========================================================================================
# The optimum k should have been identified by now.
#  Do the k-means analysis with your optimum k. 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Perform k-means analysis with one specified number and plot it.
  OneK <- function(myK, out.dir, out.fn){
    # k-means analysis
    km_results_one <- kmeans(x=kmeans_input, centers = myK, nstart = 25)
    # Define your plot title
    plot_title_one <- paste("K=", myK, sep = "")
    oneKplot <<- factoextra::fviz_cluster(km_results_one, 
                             data = kmeans_input, 
                             ellipse = T, ellipse.alpha = 0.1,
                             show.clust.cent = F,
                             ggtheme = theme_bw(base_size = 10),
                             repel = F, labelsize = 10,
                             main = plot_title_one) + 
      theme(aspect.ratio = 1) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) 
    
    # Save the plot as a pdf file.
    ggsave(paste(out.dir, paste(out.fn, ".pdf", sep=""), sep= .Platform$file.sep), 
           oneKplot, device="pdf", width=4, height=4.05, units="in")  
    
  }
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Loop through multiple Ks
  MultipleK <- function(myKs, out.dir, out.fn){
    
    plots <- list()
    km_results_mult <- list()

      # Perform the k-means analysis, with the optimum number you found above as the 'centers'. 
      for(i in 1:length(myKs)){
        # k-means analysis
        km_results_mult[[i]] <- kmeans(x=kmeans_input, centers = myKs[i], nstart = 25)
        
        # Define title for each K
        plot_title <- paste("K=", myKs[i], sep = "")
        
        # Plot
        plots[[i]] <- factoextra::fviz_cluster(km_results_mult[[i]],
                                              data = kmeans_input,
                                              ellipse = T, ellipse.alpha = 0.1,
                                              show.clust.cent = F,
                                              ggtheme = theme_bw(base_size = 10),
                                              repel = F, labelsize = 10,
                                              main = plot_title ) +
          theme(aspect.ratio = 1) +
          theme(panel.grid.major = element_blank()) +
          theme(panel.grid.minor = element_blank()) 
      }
    

    # Arrange the plots in the same panel.
    if(length(myKs)==2){
      panel <- gridExtra::grid.arrange(plots[[1]], plots[[2]], nrow = round(length(myKs)/2))
    }
    else if(length(myKs)==3){
      panel <- gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], nrow = round(length(myKs)/2))
    }
    else if(length(myKs)==4){
      panel <- gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow = round(length(myKs)/2))
    }
    else{
      return(paste("Only 2-4 plots can be created at one time.", "\n",
          "Please enter 2-4 K values and run again."))
    }
    
    # Save the plot as a pdf file.
    ggsave(paste(out.dir, paste(out.fn, ".pdf", sep=""), sep= .Platform$file.sep), 
           panel, device="pdf", width=8, height=8.1, units="in")  
    
  }
# ---------------------------------------------------------------------------------------------------------------


