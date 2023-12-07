# ===============================================================================================================
# Check ANOVA assumptions.
# Version 1
# Created on 03/15/2023 by Rie Sadohara
# ===============================================================================================================

# ---------------------------------------------------------------------------------------------------------------
# The ANOVA_assumption function checks assumptions about residuals by the following steps:
# 1. Calculate residuals (res) from a supplied model.
# 2. Plot the following in a one-panel view:
#    - Histogram of res, 
#    - QQplot of res
#    - Boxplot of res by factor
#    - Res vs. fitted values plot
# 3. Conduct Levene's test to test homogeneity of variance for each factor level

# ---------------------------------------------------------------------------------------------------------------
# A version that shows the residual's hitogram, QQplot, boxplot, and fitted vs. residuals plot.

ANOVA_assumption <- function(input.anova, input.factor, df){
  
  par(mfrow = c(2, 2)) # Generate a 2x2 plot field. 
  
  #  Compute residuals.
  res <- residuals(input.anova) 
  
  # Fig 1. Histogram of res.
  hist(res, main="Hist of residuals")
  
  # Fig 2. QQ plot of res. 
  qqnorm(res, plot.it=TRUE, main="QQplot of residuals")
  qqline(res)
  
  # Fig 3. Boxplot of residuals.
  boxplot(res ~ df[, input.factor])
  title("Boxplot of residuals")
  
  # Fig 4. produce residual vs. fitted plot
  plot(fitted(input.anova), res)
  #add a horizontal line at 0 
  abline(0,0)
  title("Fitted vs. Residuals plot")
  
  # Create a new variable of squared residuals.
  res_sq <- res*res
  
  # Revert to one-plot-per-field 
  par(mfrow = c(1, 1))
  
  # Run Levene's test (ANOVA for the squared residuals as the response).
  Levenes_test <- anova(lm(res_sq ~ df[, input.factor]))
  
  print("Levene's test")
  print(Levenes_test)
  
}

# Example.
# source("~/GitHub/R_Toolbox/anova_assumption_plots.R")
# data(iris)
# myanova <- aov(Sepal.Length ~ Species, data=iris)
# ANOVA_assumption(input.anova=myanova, 
#                 input.factor="Species", 
#                 df= iris)

# ---------------------------------------------------------------------------------------------------------------
# # Sepal.Length in iris by hand
# 
# myanova <- aov(Sepal.Length ~ Species, data=iris)
# 
#   res1 <- residuals(myanova)
#   # Generate a 2x2 plot field. 
#   par(mfrow = c(2, 2))
#   # Histogram of res1.
#   hist(res1)
#   # QQ plot of res1. 
#   qqnorm(res1, plot.it=TRUE)
#   qqline(res1)
#   # Boxplot
#   boxplot(res1 ~ iris$Species)
#   title("Boxplot of res1")
#   # produce residual vs. fitted plot
#   plot(fitted(myanova), res1)
#   #add a horizontal line at 0 
#   abline(0,0)
#   title("Fitted vs. Res1 plot")
#   # Revert to one-plot-per-field 
#   par(mfrow = c(1, 1))
#   
#   # Create a new variable of squared residuals.
#   res1sq <- res1*res1
#   # Run Levene's test (ANOVA for the squared residuals as the response).
#   Levenes_test <- anova(lm(res1sq ~ iris$Species))
#   Levenes_test
 

# ---------------------------------------------------------------------------------------------------------------
# A version that shows the trait distribution as well. Too many images maybe...

ANOVA_assumption2 <- function(input.anova, input.factor, input.response, df){
  
  par(mfrow = c(2, 2)) # Generate a 2x2 plot field. 
  
  # Fig 1. histogram of data.
  hist( df[, input.response], main= paste("Hist of residuals", input.response) )
  
  # Fig 2. QQplot of data.
  qqnorm(df[, input.response], plot.it=TRUE, main= paste("QQplot of", input.response) )
  qqline(df[, input.response])
  
  #  Compute residuals.
  res <- residuals(input.anova) 
  
  # Fig 3. Histogram of res.
  hist(res, main="Hist of residuals")
  
  # Fig 4. QQ plot of res1. 
  qqnorm(res, plot.it=TRUE, main="QQplot of residuals")
  qqline(res)

  par(mfrow = c(2, 2)) # Generate a 2x2 plot field again.
  
  # Fig 5. Boxplot of residuals.
  boxplot(res ~ df[, input.factor])
  title("Boxplot of residuals")
  
  # Fig 6. produce residual vs. fitted plot
  plot(fitted(input.anova), res)
  #add a horizontal line at 0 
  abline(0,0)
  title("Fitted vs. Residuals plot")
  
  # Create a new variable of squared residuals.
  res_sq <- res*res

  # Fig 7. plot squared residuals for each factor Levene's test will be testing
  #        if they are actually different.
  boxplot(res_sq ~ df[, input.factor], main="Squared residuals by factor")
  
  # Revert to one-plot-per-field 
  par(mfrow = c(1, 1))

  # Run Levene's test (ANOVA for the squared residuals as the response).
  Levenes_test <- anova(lm(res_sq ~ df[, input.factor]))

  print("Levene's test")
  print(Levenes_test)
  
}

# myanova <- aov(Sepal.Length ~ Species, data=iris)
# ANOVA_assumption2(input.anova=myanova, 
#                    input.factor="Species", 
#                    input.response = "Sepal.Length",  
#                    df= iris)
# # Output spans over 2 pages of plots.



# # ---------------------------------------------------------------------------------------------------------------
  
