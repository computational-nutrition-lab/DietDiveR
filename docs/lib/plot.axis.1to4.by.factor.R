# ===============================================================================================================
# Plot an ordination results - Axis.x vs Axis.y colored by a factor level.  
# Needs a dataset that contains the factor and Axis1 through at least Axis.4.
# Version 1
# Created on 12/02/2022 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Function to save Axes 1 & 2, 1 & 3, 2 & 3, 3 & 4, 2 & 4.
# ===============================================================================================================

PlotAxis1to4ByFactor <- function(axis.meta.df, 
                                 factor.to.color, 
                                 eigen.vector,
                                 dot.colors, 
                                 ellipses.colors, 
                                 ellipses.cflevel=0.95,
                                 out.prefix){
  
  # Define which column is the factor to color (index No.).
  factor_nrow <- match(factor.to.color, colnames(axis.meta.df))
  
  # Take columns by specified index No. and specific names.   
  axis.meta.df.factor_Axis1to4 <- axis.meta.df[, c(names(axis.meta.df)[factor_nrow], "Axis.1", "Axis.2", "Axis.3", "Axis.4")]
  
  # Rename the factor as Factor_to_color, so that it can be used for plotting.
  colnames(axis.meta.df.factor_Axis1to4)[1] <- "Factor_to_color" 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot Axis.1 and Axis.2 datapoints colored by the specified factor.
  # The real factor name should appear in the legend of the graphic because of the 'labs' argument.  
  p12 <- ggplot(axis.meta.df.factor_Axis1to4, aes(x=Axis.1, y=Axis.2, color=Factor_to_color )) +
    geom_point(shape=21, aes(fill= Factor_to_color), size=3, color="black") + 
    scale_fill_manual( values= dot.colors) +
    xlab( paste("Axis.1 (", paste(round(eigen.vector[1]*100, 1)), "%)", sep="") ) +
    ylab( paste("Axis.2 (", paste(round(eigen.vector[2]*100, 1)), "%)", sep="") ) +
    labs(fill = paste(factor.to.color)) + 
    no_grid + space_axes + theme(aspect.ratio = 1)
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis12.pdf", sep=""), 
          p12, device="pdf", width=7, height=5, units="in") 
  
  # Draw ellipses lines. 
  p12_ellipses <- p12 + 
    stat_ellipse(data= axis.meta.df.factor_Axis1to4, aes(color= Factor_to_color), level= ellipses.cflevel) +
    scale_color_manual(values = ellipses.colors) + # colors for ellipsesstat_ellipse 
    labs(color = paste(factor.to.color))  
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis12_ellipses.pdf", sep=""), 
          p12_ellipses, device="pdf", width=7, height=5, units="in") 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot Axis.1 and Axis.3 datapoints colored by the specified factor.
  # The real factor name should appear in the legend of the graphic because of the 'labs' argument.  
  p13 <- ggplot(axis.meta.df.factor_Axis1to4, aes(x=Axis.1, y=Axis.3, color=Factor_to_color )) +
    geom_point(shape=21, aes(fill= Factor_to_color), size=3, color="black") + 
    scale_fill_manual( values= dot.colors) +
    xlab( paste("Axis.1 (", paste(round(eigen.vector[1]*100, 1)), "%)", sep="") ) +
    ylab( paste("Axis.3 (", paste(round(eigen.vector[3]*100, 1)), "%)", sep="") ) +
    labs(fill = paste(factor.to.color)) + 
    no_grid + space_axes + theme(aspect.ratio = 1)
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis13.pdf", sep=""), 
          p13, device="pdf", width=7, height=5, units="in") 
  
  # Draw ellipses lines. 
  p13_ellipses <- p13 + 
    stat_ellipse(data= axis.meta.df.factor_Axis1to4, aes(color= Factor_to_color), level= ellipses.cflevel) +
    scale_color_manual(values = ellipses.colors) + # colors for ellipsesstat_ellipse 
    labs(color = paste(factor.to.color))  
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis13_ellipses.pdf", sep=""), 
          p13_ellipses, device="pdf", width=7, height=5, units="in") 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot Axis.2 and Axis.3 datapoints colored by the specified factor.
  # The real factor name should appear in the legend of the graphic because of the 'labs' argument.  
  p23 <- ggplot(axis.meta.df.factor_Axis1to4, aes(x=Axis.2, y=Axis.3, color=Factor_to_color )) +
    geom_point(shape=21, aes(fill= Factor_to_color), size=3, color="black") + 
    scale_fill_manual( values= dot.colors) +
    xlab( paste("Axis.2 (", paste(round(eigen.vector[2]*100, 1)), "%)", sep="") ) +
    ylab( paste("Axis.3 (", paste(round(eigen.vector[3]*100, 1)), "%)", sep="") ) +
    labs(fill = paste(factor.to.color)) + 
    no_grid + space_axes + theme(aspect.ratio = 1)
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis23.pdf", sep=""), 
          p23, device="pdf", width=7, height=5, units="in") 
  
  # Draw ellipses lines. 
  p23_ellipses <- p23 + 
    stat_ellipse(data= axis.meta.df.factor_Axis1to4, aes(color= Factor_to_color), level= ellipses.cflevel) +
    scale_color_manual(values = ellipses.colors) + # colors for ellipsesstat_ellipse 
    labs(color = paste(factor.to.color))  
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis23_ellipses.pdf", sep=""), 
          p23_ellipses, device="pdf", width=7, height=5, units="in") 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot Axis.3 and Axis.4 datapoints colored by the specified factor.
  # The real factor name should appear in the legend of the graphic because of the 'labs' argument.  
  p34 <- ggplot(axis.meta.df.factor_Axis1to4, aes(x=Axis.3, y=Axis.4, color=Factor_to_color )) +
    geom_point(shape=21, aes(fill= Factor_to_color), size=3, color="black") + 
    scale_fill_manual( values= dot.colors) +
    xlab( paste("Axis.3 (", paste(round(eigen.vector[3]*100, 1)), "%)", sep="") ) +
    ylab( paste("Axis.4 (", paste(round(eigen.vector[4]*100, 1)), "%)", sep="") ) +
    labs(fill = paste(factor.to.color)) + 
    no_grid + space_axes + theme(aspect.ratio = 1)
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis34.pdf", sep=""), 
          p34, device="pdf", width=7, height=5, units="in") 
  
  # Draw ellipses lines. 
  p34_ellipses <- p34 + 
    stat_ellipse(data= axis.meta.df.factor_Axis1to4, aes(color= Factor_to_color), level= ellipses.cflevel) +
    scale_color_manual(values = ellipses.colors) + # colors for ellipsesstat_ellipse 
    labs(color = paste(factor.to.color))  
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis34_ellipses.pdf", sep=""), 
          p34_ellipses, device="pdf", width=7, height=5, units="in") 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot Axis.2 and Axis.4 datapoints colored by the specified factor.
  # The real factor name should appear in the legend of the graphic because of the 'labs' argument.  
  p24 <- ggplot(axis.meta.df.factor_Axis1to4, aes(x=Axis.2, y=Axis.4, color=Factor_to_color )) +
    geom_point(shape=21, aes(fill= Factor_to_color), size=3, color="black") + 
    scale_fill_manual( values= dot.colors) +
    xlab( paste("Axis.2 (", paste(round(eigen.vector[2]*100, 1)), "%)", sep="") ) +
    ylab( paste("Axis.4 (", paste(round(eigen.vector[4]*100, 1)), "%)", sep="") ) +
    labs(fill = paste(factor.to.color)) + 
    no_grid + space_axes + theme(aspect.ratio = 1)
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis24.pdf", sep=""), 
          p24, device="pdf", width=7, height=5, units="in") 
  
  # Draw ellipses lines. 
  p24_ellipses <- p24 + 
    stat_ellipse(data= axis.meta.df.factor_Axis1to4, aes(color= Factor_to_color), level= ellipses.cflevel) +
    scale_color_manual(values = ellipses.colors) + # colors for ellipsesstat_ellipse 
    labs(color = paste(factor.to.color))  
  
  # Save your plot. 
  ggsave( paste(out.prefix, "_Axis24_ellipses.pdf", sep=""), 
          p24_ellipses, device="pdf", width=7, height=5, units="in") 
  
}

# ---------------------------------------------------------------------------------------------------------------

# # ---------------------------------------------------------------------------------------------------------------
# # Before making into a function - by hand. - If want to plot Axis 5 and onward.

# # Plot Axis 1 and Axis 2 to show the separation of samples colored by Groups as in the metadata.
# p1_w <- ggplot(loaded_glu_w, aes(x=Axis.1, y=Axis.2, color= GLU_index)) +
#   geom_point(shape=21, aes(fill= GLU_index), size=3, color="black") + 
#   scale_fill_manual( values= c("steelblue3", "yellow", "hotpink")) +
#   xlab( paste("Axis.1 (", paste(round(eigen_percent_w[1]*100, 1)), "%)", sep="") ) +
#   ylab( paste("Axis.2 (", paste(round(eigen_percent_w[2]*100, 1)), "%)", sep="") ) +
#   no_grid + space_axes + theme(aspect.ratio = 1)
# p1_w
# 
# # Save p1 as a pdf. 
# ggsave("Food_D12_FC_cc_f_males60to79_red_Lv3_ord_WEIGHTED_Axis12_byhand.pdf", 
#        p1_w, device="pdf", width=7, height=5.5, unit="in", dpi=300)
# 
# # You can add ellipses at a desired confidence level; but with this 
# # example data, there are too few samples per user to draw them. 
# ellipses_w <- p1_w + 
#   stat_ellipse(data= loaded_glu_w, aes(color= GLU_index), level= 0.95) +
#   scale_color_manual(values = c("steelblue3", "gold3", "hotpink"))   # colors for ellipsesstat_ellipse 
# ellipses_w
# 
# # Save ellipses as a pdf. 
# ggsave("Food_D12_FC_cc_f_males60to79_red_Lv3_ord_WEIGHTED_Axis12_ellipses_byhand.pdf", 
#        ellipses_w, device="pdf", width=7, height=5.5, unit="in", dpi=300)
