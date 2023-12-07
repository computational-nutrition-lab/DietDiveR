# ========================================================================================
# Set theme of ggplot2.
# Version 1
# Created on 07/25/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Set ggplot2 theme with black and white, no grid, and space between axis and axis titles. 
# ========================================================================================
# 
# Define ggplot2 themes
  library(ggplot2)

# Theme black and white, with the base font size 14: change if necessary.
  theme_set(theme_bw(base_size = 14))

# No gridlines inside charts
  no_grid <- theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank())

# Insert some space between axes and axes labels. 
  space_axes <- theme(axis.title.x = element_text(margin=margin(t = 8, r = 0, b = 0, l = 0) ),
                      axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) 
  
# Rotate the X axis labels 45 degrees for visibility. 
  rotate_X_labels <- theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1) )