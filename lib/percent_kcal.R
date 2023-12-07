# ====================================================================================================================
# Analyze the kcal from carbohydrate, protein, and fat in the totals file of  
# ASA24 output (data) 
# Version 1
# Created on 12.16.2021 by Rie Sadohara
# ====================================================================================================================


# ====================================================================================================================
# Function to calculate the mean and SD of CARB, PROT, and TFAT. with UserName in ASA24.
# ====================================================================================================================

  # CPTgramsPerUser <- function(inputfn, user.name='UserName', 
  #                             recall.no='RecallNo', outfn){
  #   
  #   # Get index numbers for username, recallno, "CARB","PROT","TFAT", "KCAL"
  #   indexno_username <- which(names(inputfn)== user.name)
  #   indexno_recallno <- which(names(inputfn)== recall.no)
  #   indexno_carb <-     which(names(inputfn)== "CARB")
  #   indexno_prot <-     which(names(inputfn)== "PROT")
  #   indexno_tfat <-     which(names(inputfn)== "TFAT")
  #   
  #   # Take only the relevant columns from inputfn.
  #   totalssub <- inputfn[, c(indexno_username, 
  #                            indexno_recallno, 
  #                            indexno_carb,
  #                            indexno_prot,
  #                            indexno_tfat)]
  #   
  #   # Change the column names for the following process
  #   colnames(totalssub)[1:2] <- c("UserName", "RecallNo")
  #   
  #   # Calc grams of macronutrients.
  #   CARBmean <- aggregate(totalssub$CARB, by = list(totalssub$UserName), FUN = mean)
  #   colnames(CARBmean) <- c("UserName", "CARB_mean")
  #   CARBsd <- aggregate(totalssub$CARB, by = list(totalssub$UserName), FUN = sd)
  #   colnames(CARBsd) <- c("UserName", "CARB_sd")
  #   CARBlength <- aggregate(totalssub$CARB, by = list(totalssub$UserName), FUN = length)
  #   colnames(CARBlength) <- c("UserName", "CARB_n")
  #   C_length_mean <-   merge(x=CARBlength, y=CARBmean, all.x=T)
  #   C_length_mean_sd <- merge(x=C_length_mean, y=CARBsd, all.x=T)
  #   C_length_mean_sd$macronutrient <- "CARB"
  #   
  #   PROTmean <- aggregate(totalssub$PROT, by = list(totalssub$UserName), FUN = mean)
  #   colnames(PROTmean) <- c("UserName", "PROT_mean")
  #   PROTsd <-   aggregate(totalssub$PROT, by = list(totalssub$UserName), FUN = sd)
  #   colnames(PROTsd) <- c("UserName", "PROT_sd")
  #   PROTlength <- aggregate(totalssub$PROT, by = list(totalssub$UserName), FUN = length)
  #   colnames(PROTlength) <- c("UserName", "PROT_n")
  #   P_length_mean <-   merge(x=PROTlength, y=PROTmean, all.x=T)
  #   P_length_mean_sd <- merge(x=P_length_mean, y=PROTsd, all.x=T)
  #   P_length_mean_sd$macronutrient <- "PROT"
  #   
  #   TFATmean <- aggregate(totalssub$TFAT, by = list(totalssub$UserName), FUN = mean)
  #   colnames(TFATmean) <- c("UserName", "TFAT_mean")
  #   TFATsd <- aggregate(totalssub$TFAT, by = list(totalssub$UserName), FUN = sd)
  #   colnames(TFATsd) <- c("UserName", "TFAT_sd")
  #   TFATlength <- aggregate(totalssub$TFAT, by = list(totalssub$UserName), FUN = length)
  #   colnames(TFATlength) <- c("UserName", "TFAT_n")
  #   T_length_mean <-    merge(x=TFATlength, y=TFATmean, all.x=T)
  #   T_length_mean_sd <- merge(x=T_length_mean, y=TFATsd, all.x=T)
  #   T_length_mean_sd$macronutrient <- "TFAT"
  #   T_length_mean_sd
  #   
  #   # Change column names for rbind
  #   colnames(C_length_mean_sd)[2:4] <- c("n", "mean", "sd")
  #   colnames(P_length_mean_sd)[2:4] <- c("n", "mean", "sd")
  #   colnames(T_length_mean_sd)[2:4] <- c("n", "mean", "sd")
  #   
  #   rbound <- rbind(C_length_mean_sd, P_length_mean_sd, T_length_mean_sd)
  #   CPT_g_fn <- rbound[, c(1,5,2,3,4)] # bring macronutrient to 2nd
  #   
  #   # Save CPT_g_fn. (fn stands for "function")
  #   write.table(x=CPT_g_fn, file=outfn, sep="\t", row.names=F, quote=F)
  # }


# ====================================================================================================================
# Function to calculate the mean % of energy intake (kcal) and SD of CARB, PROT, and TFAT.
# Use KCAL as the denominator. with UserName in ASA24.
# ====================================================================================================================
  # CPTpctKcalPerUser <- function(inputfn, user.name='UserName', 
  #                               recall.no='RecallNo', outfn){
  #   
  #   # Get index numbers for username, recallno, "CARB","PROT","TFAT", "KCAL"
  #   indexno_username <- which(names(inputfn)== user.name)
  #   indexno_recallno <- which(names(inputfn)== recall.no)
  #   indexno_carb <-     which(names(inputfn)== "CARB")
  #   indexno_prot <-     which(names(inputfn)== "PROT")
  #   indexno_tfat <-     which(names(inputfn)== "TFAT")
  #   indexno_kcal <-     which(names(inputfn)== "KCAL")
  #   
  #   # Take only the relevant columns from inputfn.
  #   totalssub2 <- inputfn[, c(indexno_username, 
  #                             indexno_recallno, 
  #                             indexno_carb,
  #                             indexno_prot,
  #                             indexno_tfat,
  #                             indexno_kcal)]
  #   
  #   # Change the column names for the following process
  #   colnames(totalssub2)[1:2] <- c("UserName", "RecallNo")
  #   
  #   # % KCAL
  #   # calculate calories
  #   totalssub2$CARB_kcal <- totalssub2$CARB * 4
  #   totalssub2$PROT_kcal <- totalssub2$PROT * 4
  #   totalssub2$TFAT_kcal <- totalssub2$TFAT * 9
  #   
  #   # calculate kcal of each macronutrient per engergy (%)
  #   totalssub2$CARB_kcal_pct <- totalssub2$CARB_kcal / totalssub2$KCAL * 100
  #   totalssub2$PROT_kcal_pct <- totalssub2$PROT_kcal / totalssub2$KCAL * 100
  #   totalssub2$TFAT_kcal_pct <- totalssub2$TFAT_kcal / totalssub2$KCAL * 100
  #   
  #   CARB_kcal_pctmean <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$UserName), FUN = mean)
  #   colnames(CARB_kcal_pctmean) <- c("UserName", "CARB_kcal_pct_mean")
  #   CARB_kcal_pctsd <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$UserName), FUN = sd)
  #   colnames(CARB_kcal_pctsd) <- c("UserName", "CARB_kcal_pct_sd")
  #   CARB_kcal_pctlength <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$UserName), FUN = length)
  #   colnames(CARB_kcal_pctlength) <- c("UserName", "CARB_kcal_pct_n")
  #   C_length_mean <-   merge(x=CARB_kcal_pctlength, y=CARB_kcal_pctmean, all.x=T)
  #   C_length_mean_sd <- merge(x=C_length_mean, y=CARB_kcal_pctsd, all.x=T)
  #   C_length_mean_sd$macronutrient <- "CARB_kcal_pct"
  #   
  #   PROT_kcal_pctmean <- aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$UserName), FUN = mean)
  #   colnames(PROT_kcal_pctmean) <- c("UserName", "PROT_kcal_pct_mean")
  #   PROT_kcal_pctsd <-   aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$UserName), FUN = sd)
  #   colnames(PROT_kcal_pctsd) <- c("UserName", "PROT_kcal_pct_sd")
  #   PROT_kcal_pctlength <- aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$UserName), FUN = length)
  #   colnames(PROT_kcal_pctlength) <- c("UserName", "PROT_kcal_pct_n")
  #   P_length_mean <-   merge(x=PROT_kcal_pctlength, y=PROT_kcal_pctmean, all.x=T)
  #   P_length_mean_sd <- merge(x=P_length_mean, y=PROT_kcal_pctsd, all.x=T)
  #   P_length_mean_sd$macronutrient <- "PROT_kcal_pct"
  #   
  #   TFAT_kcal_pctmean <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$UserName), FUN = mean)
  #   colnames(TFAT_kcal_pctmean) <- c("UserName", "TFAT_kcal_pct_mean")
  #   TFAT_kcal_pctsd <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$UserName), FUN = sd)
  #   colnames(TFAT_kcal_pctsd) <- c("UserName", "TFAT_kcal_pct_sd")
  #   TFAT_kcal_pctlength <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$UserName), FUN = length)
  #   colnames(TFAT_kcal_pctlength) <- c("UserName", "TFAT_kcal_pct_n")
  #   T_length_mean <-    merge(x=TFAT_kcal_pctlength, y=TFAT_kcal_pctmean, all.x=T)
  #   T_length_mean_sd <- merge(x=T_length_mean, y=TFAT_kcal_pctsd, all.x=T)
  #   T_length_mean_sd$macronutrient <- "TFAT_kcal_pct"
  #   
  #   # Change column names for rbind 
  #   colnames(C_length_mean_sd)[2:4] <- c("n", "mean", "sd")
  #   colnames(P_length_mean_sd)[2:4] <- c("n", "mean", "sd")
  #   colnames(T_length_mean_sd)[2:4] <- c("n", "mean", "sd")
  #   
  #   rbound <- rbind(C_length_mean_sd, P_length_mean_sd, T_length_mean_sd)
  #   CPT_kcal_fn <- rbound[, c(1,5,2,3,4)] # bring macronutrient to 2nd
  #   
  #   # Save CPT_kcal_fn. (fn stands for "function")
  #   write.table(x=CPT_kcal_fn, file=outfn, sep="\t", row.names=F, quote=F)
  #   
  # }

# ====================================================================================================================
# Function to calculate the mean % of energy intake (kcal) and SD of CARB, PROT, and TFAT.
# Use the sum of XXXX_pct as the denominator so that the sum of CARB, PROT, and TFAT will be 100. 
# ====================================================================================================================
  CPTpctKcalPerUser <- function(inputfn, group='Group', 
                                across='SEQN', outfn){
    
    # Get index numbers for Group, recallno, "CARB","PROT","TFAT", "KCAL"
    indexno_group <- which(names(inputfn)== group)
    indexno_across <- which(names(inputfn)== across)
    indexno_carb <-     which(names(inputfn)== "CARB")
    indexno_prot <-     which(names(inputfn)== "PROT")
    indexno_tfat <-     which(names(inputfn)== "TFAT")
    # indexno_kcal <-     which(names(inputfn)== "KCAL")
    
    # Take only the relevant columns from inputfn.
    totalssub2 <<- inputfn[, c(indexno_group, 
                              indexno_across, 
                              indexno_carb,
                              indexno_prot,
                              indexno_tfat
                              # indexno_kcal
                              )]
    
    # Change the column names for the following process
    colnames(totalssub2)[1:2] <- c("Group", "SEQN")
    
    # % KCAL
    # calculate calories
    totalssub2$CARB_kcal <- totalssub2$CARB * 4
    totalssub2$PROT_kcal <- totalssub2$PROT * 4
    totalssub2$TFAT_kcal <- totalssub2$TFAT * 9
    totalssub2$kcal_sum <-  totalssub2$CARB_kcal + totalssub2$PROT_kcal + totalssub2$TFAT_kcal
    
    # calculate kcal of each macronutrient per engergy (%)
    totalssub2$CARB_kcal_pct <- totalssub2$CARB_kcal / totalssub2$kcal_sum * 100
    totalssub2$PROT_kcal_pct <- totalssub2$PROT_kcal / totalssub2$kcal_sum * 100
    totalssub2$TFAT_kcal_pct <- totalssub2$TFAT_kcal / totalssub2$kcal_sum * 100
    
    CARB_kcal_pctmean <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$Group), FUN = mean)
    colnames(CARB_kcal_pctmean) <- c("Group", "CARB_kcal_pct_mean")
    CARB_kcal_pctsd <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$Group), FUN = sd)
    colnames(CARB_kcal_pctsd) <- c("Group", "CARB_kcal_pct_sd")
    CARB_kcal_pctlength <- aggregate(totalssub2$CARB_kcal_pct, by = list(totalssub2$Group), FUN = length)
    colnames(CARB_kcal_pctlength) <- c("Group", "CARB_kcal_pct_n")
    C_length_mean <-   merge(x=CARB_kcal_pctlength, y=CARB_kcal_pctmean, all.x=T)
    C_length_mean_sd <- merge(x=C_length_mean, y=CARB_kcal_pctsd, all.x=T)
    C_length_mean_sd$macronutrient <- "CARB_kcal_pct"
    
    PROT_kcal_pctmean <- aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$Group), FUN = mean)
    colnames(PROT_kcal_pctmean) <- c("Group", "PROT_kcal_pct_mean")
    PROT_kcal_pctsd <-   aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$Group), FUN = sd)
    colnames(PROT_kcal_pctsd) <- c("Group", "PROT_kcal_pct_sd")
    PROT_kcal_pctlength <- aggregate(totalssub2$PROT_kcal_pct, by = list(totalssub2$Group), FUN = length)
    colnames(PROT_kcal_pctlength) <- c("Group", "PROT_kcal_pct_n")
    P_length_mean <-   merge(x=PROT_kcal_pctlength, y=PROT_kcal_pctmean, all.x=T)
    P_length_mean_sd <- merge(x=P_length_mean, y=PROT_kcal_pctsd, all.x=T)
    P_length_mean_sd$macronutrient <- "PROT_kcal_pct"
    
    TFAT_kcal_pctmean <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$Group), FUN = mean)
    colnames(TFAT_kcal_pctmean) <- c("Group", "TFAT_kcal_pct_mean")
    TFAT_kcal_pctsd <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$Group), FUN = sd)
    colnames(TFAT_kcal_pctsd) <- c("Group", "TFAT_kcal_pct_sd")
    TFAT_kcal_pctlength <- aggregate(totalssub2$TFAT_kcal_pct, by = list(totalssub2$Group), FUN = length)
    colnames(TFAT_kcal_pctlength) <- c("Group", "TFAT_kcal_pct_n")
    T_length_mean <-    merge(x=TFAT_kcal_pctlength, y=TFAT_kcal_pctmean, all.x=T)
    T_length_mean_sd <- merge(x=T_length_mean, y=TFAT_kcal_pctsd, all.x=T)
    T_length_mean_sd$macronutrient <- "TFAT_kcal_pct"
    
    # Change column names for rbind 
    colnames(C_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    colnames(P_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    colnames(T_length_mean_sd)[2:4] <- c("n", "mean", "sd")
    
    rbound <- rbind(C_length_mean_sd, P_length_mean_sd, T_length_mean_sd)
    
    CPT_kcal_fn <- rbound[, c(1,5,2,3,4)] # bring macronutrient to 2nd
    
    # Replace "CARB_kcal_pct" with "CARB" etc.
    CPT_kcal_fn_2 <- CPT_kcal_fn # copy to avoid overwriting.
    CPT_kcal_fn_2$macronutrient <- sub(CPT_kcal_fn_2$macronutrient, pattern="CARB_kcal_pct", replacement="Carbohydrate") 
    CPT_kcal_fn_2$macronutrient <- sub(CPT_kcal_fn_2$macronutrient, pattern="PROT_kcal_pct", replacement="Protein") 
    CPT_kcal_fn_2$macronutrient <- sub(CPT_kcal_fn_2$macronutrient, pattern="TFAT_kcal_pct", replacement="Total Fat") 
    
    # Save CPT_kcal_fn. (fn stands for "function")
    write.table(x=CPT_kcal_fn_2, file=outfn, sep="\t", row.names=F, quote=F)
    
  }

# ====================================================================================================================
# Plot a stacked barchart without SD  ... with UserName of ASA24. ... to be deleted.
# ====================================================================================================================
  
  # StackedwoSD <- function(data){
  #   ggplot(data, aes(x = UserName, y = mean, fill = macronutrient)) + 
  #     geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
  #     # change colors and labels of legend. Ensure the factor order is correct. 
  #     scale_fill_manual(values = distinct100colors, 
  #                       labels=c( "Carbohydrates", "Protein", "Total fat")) +
  #     labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
  #     # Specify the font size and angle of the x axis label.  
  #     theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) + no_grid
  # }

# ====================================================================================================================
# Plot a stacked barchart without SD  ... with Group of NHANES.
# ====================================================================================================================
  # 
  # StackedwoSD_NHANES <- function(data){
  #   ggplot(data, aes(x = Group, y = mean, fill = macronutrient)) + 
  #     geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
  #     # change colors and labels of legend. Ensure the factor order is correct. 
  #     scale_fill_manual(values = distinct100colors, 
  #                       labels=c( "Carbohydrates", "Protein", "Total fat")) +
  #     labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
  #     # Specify the font size and angle of the x axis label.  
  #     theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) + no_grid
  # }
  # 
  # 
# ====================================================================================================================
# Plot individual bars for each macronutrients (3 bars in total) with SD ... with UserName of ASA24. .. to be deleted.
# ====================================================================================================================
  # DodgedBarchart  <- function(data){
  #   ggplot(data, aes(x = factor(UserName), y = mean, fill = macronutrient, colour = macronutrient)) + 
  #     geom_bar(stat = "identity", position = "dodge", color="black")  +
  #     geom_errorbar(aes(ymin= mean, ymax= mean + sd), position = position_dodge(0.9), width = 0.25,
  #                   color="black") +
  #     scale_fill_manual(values = distinct100colors,
  #                       labels=c( "Carbohydrates", "Protein", "Total fat")) +
  #     labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
  #     no_grid + space_axes +
  #     theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  # }
  
# ====================================================================================================================
# Plot individual bars for each macronutrients (3 bars in total) with SD ... NHANES. 
# ====================================================================================================================
  # DodgedBarchart_NHANES <- function(data){
  #   ggplot(data, aes(x = factor(Group), y = mean, fill = macronutrient, colour = macronutrient)) + 
  #     geom_bar(stat = "identity", position = "dodge", color="black")  +
  #     geom_errorbar(aes(ymin= mean, ymax= mean + sd), position = position_dodge(0.9), width = 0.25,
  #                   color="black") +
  #     scale_fill_manual(values = distinct100colors,
  #                       labels=c( "Carbohydrates", "Protein", "Total fat")) +
  #     labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
  #     no_grid + space_axes +
  #     theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  # }
  
# ====================================================================================================================
# Calculation for stacked barchart with SD ... with "UserName" of ASA24. Renamed as CalcStackedSD_ASA24. .. may not be necessary?
# ====================================================================================================================
# 
# # Function to calculate sd_base and sd_stacked for stacked barchart. 
# # This assumes all users (individuals) have CARB, PROT, and TFAT values.
# 
#   CalcStackedSD_ASA24 <- function(input.df, out.fn){
# 
#     # Generate a dataframe to save sd data.
#     CPT_kcal_forstacked <- data.frame(matrix(NA, nrow=length(individuals)*3, ncol=7)) 
#     
#     # Specify its column names.
#     colnames(CPT_kcal_forstacked) <- c("UserName", "macronutrient", "n", "mean", "sd", "sd_base", "sd_stacked")
#     
#     for(i in 1:length(individuals)){
#       
#       if(i == 1){
#         ith_user <- subset(input.df, UserName == individuals[i])
#         
#         # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
#         PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
#         TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
#         CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
#         PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
#         TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
#         
#         # sd values for stacked barchart. 
#         ith_user$sd_base <- c(TFATmeanval+PROTmeanval,  # carb, on top of the stacked barchart.
#                               TFATmeanval,              # prot, in the middle.
#                               0)                        # tfat, on the bottom.
#         ith_user$sd_stacked <-  c(CARBsdval+PROTmeanval+TFATmeanval,    # carb, on top of the stacked barchart.
#                                   PROTsdval+TFATmeanval,                # prot, in the middle.
#                                   TFATsdval)                            # tfat, on the bottom.
#         
#         # for i=1, make the first result dataframe. 
#         CPT_kcal_forstacked[c(i,i+1,i+2), ] <- ith_user
#         
#       }else{
#         
#         ith_user <- subset(input.df, UserName == individuals[i])
#         
#         # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
#         PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
#         TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
#         CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
#         PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
#         TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
#         
#         # sd values for stacked barchart. 
#         ith_user$sd_base <- c(TFATmeanval+PROTmeanval,  # carb, on top of the stacked barchart.
#                               TFATmeanval,              # prot, in the middle.
#                               0)                        # tfat, on the bottom.
#         ith_user$sd_stacked <-  c(CARBsdval+PROTmeanval+TFATmeanval,    # carb, on top of the stacked barchart.
#                                   PROTsdval+TFATmeanval,                # prot, in the middle.
#                                   TFATsdval)                            # tfat, on the bottom.
#         
#         # need another value k in order to specify the correct row.
#         k = i-2
#         
#         # for i = 2,3,4,..., combine rows with the previously made CPT_kcal_forstacked. 
#         CPT_kcal_forstacked[c(i+i+k, i+i+k+1, i+i+k+2), ] <- ith_user
#         
#       }
#     }
#     # Save the resultant file as .txt file.
#     write.table(x=CPT_kcal_forstacked, file = out.fn, sep="\t", row.names=F, quote=F)
#     
#   }

# ====================================================================================================================
# Calculation for stacked barchart with SD ... with "Group" of NHANES. .. may not be necessary?
# ====================================================================================================================
  
  # # Function to calculate sd_base and sd_stacked for stacked barchart. 
  # # This assumes all groups (Gender_Age groups) have CARB, PROT, and TFAT values.
  # 
  # CalcStackedSD_NHANES <- function(input.df, out.fn){
  #   
  #   # Replace "CARB" etc. back with "CARB_kcal_pct".
  #   copied.input.df <- input.df
  #   copied.input.df$macronutrient <- sub(copied.input.df$macronutrient, pattern="Carbohydrate", replacement="CARB_kcal_pct") 
  #   copied.input.df$macronutrient <- sub(copied.input.df$macronutrient, pattern="Protein",      replacement="PROT_kcal_pct") 
  #   copied.input.df$macronutrient <- sub(copied.input.df$macronutrient, pattern="Total Fat",    replacement="TFAT_kcal_pct") 
  # 
  #   # Generate a dataframe to save sd data.
  #   CPT_kcal_forstacked <- data.frame(matrix(NA, nrow=length(groups)*3, ncol=7)) 
  #   
  #   # Specify its column names.
  #   colnames(CPT_kcal_forstacked) <- c("Group", "macronutrient", "n", "mean", "sd", "sd_base", "sd_stacked")
  #   
  #   for(i in 1:length(groups)){
  #     
  #     if(i == 1){
  #       ith_user <- subset(copied.input.df, Group == groups[i])
  #       
  #       # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
  #       PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
  #       TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
  #       CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
  #       PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
  #       TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
  #       
  #       # sd values for stacked barchart. 
  #       ith_user$sd_base <- c(TFATmeanval+PROTmeanval,  # carb, on top of the stacked barchart.
  #                             TFATmeanval,              # prot, in the middle.
  #                             0)                        # tfat, on the bottom.
  #       ith_user$sd_stacked <-  c(CARBsdval+PROTmeanval+TFATmeanval,    # carb, on top of the stacked barchart.
  #                                 PROTsdval+TFATmeanval,                # prot, in the middle.
  #                                 TFATsdval)                            # tfat, on the bottom.
  #       
  #       # for i=1, make the first result dataframe. 
  #       CPT_kcal_forstacked[c(i,i+1,i+2), ] <- ith_user
  #       
  #     }else{
  #       
  #       ith_user <- subset(copied.input.df, Group == groups[i])
  #       
  #       # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
  #       PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
  #       TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
  #       CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
  #       PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
  #       TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
  #       
  #       # sd values for stacked barchart. 
  #       ith_user$sd_base <- c(TFATmeanval+PROTmeanval,  # carb, on top of the stacked barchart.
  #                             TFATmeanval,              # prot, in the middle.
  #                             0)                        # tfat, on the bottom.
  #       ith_user$sd_stacked <-  c(CARBsdval+PROTmeanval+TFATmeanval,    # carb, on top of the stacked barchart.
  #                                 PROTsdval+TFATmeanval,                # prot, in the middle.
  #                                 TFATsdval)                            # tfat, on the bottom.
  #       
  #       # need another value k in order to specify the correct row.
  #       k = i-2
  #       
  #       # for i = 2,3,4,..., combine rows with the previously made CPT_kcal_forstacked. 
  #       CPT_kcal_forstacked[c(i+i+k, i+i+k+1, i+i+k+2), ] <- ith_user
  #       
  #     }
  #   }
  #   
  #   # Replace "CARB_kcal_pct" etc. back with "CARB" for plotting.
  #   CPT_kcal_forstacked_2 <- CPT_kcal_forstacked
  #   CPT_kcal_forstacked_2$macronutrient <- sub(CPT_kcal_forstacked_2$macronutrient, pattern="CARB_kcal_pct", replacement="Carbohydrate") 
  #   CPT_kcal_forstacked_2$macronutrient <- sub(CPT_kcal_forstacked_2$macronutrient, pattern="PROT_kcal_pct", replacement="Protein") 
  #   CPT_kcal_forstacked_2$macronutrient <- sub(CPT_kcal_forstacked_2$macronutrient, pattern="TFAT_kcal_pct", replacement="Total Fat") 
  #   
  #   # Save the resultant file as .txt file.
  #   write.table(x=CPT_kcal_forstacked_2, file = out.fn, sep="\t", row.names=F, quote=F)
  #   
  # }

# ====================================================================================================================
# Calculation for stacked barchart with SD ... with "Group" of NHANES OR ASA24. With macronut.order agrument.
# ====================================================================================================================
  
  CalcStackedSD <- function(input.df, macronut.order){
    
    copied.input.df <- input.df
 
    # Generate a dataframe to save sd data.
    CPT_kcal_forstacked <- data.frame(matrix(NA, nrow=length(groups)*3, ncol=7)) 
    
    # Specify its column names.
    colnames(CPT_kcal_forstacked) <- c("Group", "macronutrient", "n", "mean", "sd", "sd_base", "sd_stacked")
    
    for(i in 1:length(groups)){
      ith_user_asis <- subset(copied.input.df, Group == groups[i])
      
      # Re-order rows in ith_user based on macronut.order.  
      ith_user <- ith_user_asis[match(macronut.order, ith_user_asis$macronutrient), ]
      
      Firstmeanval <-  subset(ith_user, macronutrient==macronut.order[1])[, "mean"]
      Secondmeanval <- subset(ith_user, macronutrient==macronut.order[2])[, "mean"]
      Thirdmeanval <-  subset(ith_user, macronutrient==macronut.order[3])[, "mean"]
      Firstsdval <-  subset(ith_user, macronutrient==macronut.order[1])[, "sd"]
      Secondsdval <- subset(ith_user, macronutrient==macronut.order[2])[, "sd"]
      Thirdsdval <-  subset(ith_user, macronutrient==macronut.order[3])[, "sd"]
      
      
      # sd values for stacked barchart. 
      ith_user$sd_base <- c(Thirdmeanval + Secondmeanval,     # starting point for the 1st macronut, to be plotted on top of the 3rd and the 2nd ones.
                            Thirdmeanval,                     # starting point for the 2nd macronut, to be plotted in the middle, on the 3rd one.
                            0)                                # starting point for the 3rd macronut, to be plotted on the bottom.
      ith_user$sd_stacked <-  c(Firstsdval + Thirdmeanval + Secondmeanval,  # SD value to be plotted for the 1st macronut    
                                Secondsdval + Thirdmeanval,                 # SD value to be plotted for the 2nd macronut.
                                Thirdsdval)                                 # SD value to be plotted for the 3rd macronut.
      
      if(i == 1){
        # ith_user_asis <- subset(copied.input.df, Group == groups[i])
        # 
        # # Re-order rows in ith_user based on macronut.order.  
        # ith_user <- ith_user_asis[match(macronut.order, ith_user_asis$macronutrient), ]
        # 
        # # # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
        # # PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
        # # TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
        # # CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
        # # PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
        # # TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
        # 
        # Firstmeanval <-  subset(ith_user, macronutrient==macronut.order[1])[, "mean"]
        # Secondmeanval <- subset(ith_user, macronutrient==macronut.order[2])[, "mean"]
        # Thirdmeanval <-  subset(ith_user, macronutrient==macronut.order[3])[, "mean"]
        # Firstsdval <-  subset(ith_user, macronutrient==macronut.order[1])[, "sd"]
        # Secondsdval <- subset(ith_user, macronutrient==macronut.order[2])[, "sd"]
        # Thirdsdval <-  subset(ith_user, macronutrient==macronut.order[3])[, "sd"]
        # 
        # 
        # # sd values for stacked barchart. 
        # ith_user$sd_base <- c(Thirdmeanval + Secondmeanval,     # starting point for the 1st macronut, to be plotted on top of the 3rd and the 2nd ones.
        #                       Thirdmeanval,                     # starting point for the 2nd macronut, to be plotted in the middle, on the 3rd one.
        #                       0)                                # starting point for the 3rd macronut, to be plotted on the bottom.
        # ith_user$sd_stacked <-  c(Firstsdval + Thirdmeanval + Secondmeanval,  # SD value to be plotted for the 1st macronut    
        #                           Secondsdval + Thirdmeanval,                 # SD value to be plotted for the 2nd macronut.
        #                           Thirdsdval)                                 # SD value to be plotted for the 3rd macronut.
        # 
        
        # for i=1, make the first result dataframe. 
        CPT_kcal_forstacked[c(i,i+1,i+2), ] <- ith_user
        
      }else{
        
        # ith_user_asis <- subset(copied.input.df, Group == groups[i])
        # 
        # # Re-order rows in ith_user based on macronut.order.  
        # ith_user <- ith_user_asis[match(macronut.order, ith_user_asis$macronutrient), ]
        # 
        # Firstmeanval <-  subset(ith_user, macronutrient==macronut.order[1])[, "mean"]
        # Secondmeanval <- subset(ith_user, macronutrient==macronut.order[2])[, "mean"]
        # Thirdmeanval <-  subset(ith_user, macronutrient==macronut.order[3])[, "mean"]
        # Firstsdval <-  subset(ith_user, macronutrient==macronut.order[1])[, "sd"]
        # Secondsdval <- subset(ith_user, macronutrient==macronut.order[2])[, "sd"]
        # Thirdsdval <-  subset(ith_user, macronutrient==macronut.order[3])[, "sd"]
        # 
        # # # CARBmeanval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "mean"]
        # # PROTmeanval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "mean"]
        # # TFATmeanval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "mean"]
        # # CARBsdval <- subset(ith_user, macronutrient=="CARB_kcal_pct")[, "sd"]
        # # PROTsdval <- subset(ith_user, macronutrient=="PROT_kcal_pct")[, "sd"]
        # # TFATsdval <- subset(ith_user, macronutrient=="TFAT_kcal_pct")[, "sd"]
        # 
        # # sd values for stacked barchart. 
        # ith_user$sd_base <- c(Thirdmeanval + Secondmeanval,  
        #                       Thirdmeanval,              
        #                       0)                       
        # ith_user$sd_stacked <-  c(Firstsdval  + Thirdmeanval + Secondmeanval,    
        #                           Secondsdval + Thirdmeanval,                
        #                           Thirdsdval)                            
        
        # need another value k in order to specify the correct row.
        k = i-2
        
        # for i = 2,3,4,..., combine rows with the previously made CPT_kcal_forstacked. 
        CPT_kcal_forstacked[c(i+i+k, i+i+k+1, i+i+k+2), ] <- ith_user
        
      }
    }
    
    # Save for use outside the function.
    CPT_kcal_forstackedwSD <<- CPT_kcal_forstacked
   
  }
  
# ====================================================================================================================
# function1 PlotStackedwoSD ...Reorder Diets, specify the order of macronutrients. 
# ====================================================================================================================

  PlotStackedwoSD <- function(data, order.by, macronut.order, group.order=NULL){
    
    if(order.by=="NULL"){
      
      # Create a factor 'macronutrient_f' with the specified macronutrient stacking order. 
      data[, 'macronutrient_f'] <- factor(data[, 'macronutrient'], levels= macronut.order) 
      
      # Create a factor 'group_f' in the default order or the specified order.
      if(is.null(group.order)){

        # Create a factor 'Group_f' with the default, alphabetical order, to be plotted on the X axis.
        data[, 'Group_f'] <- factor(data[, 'Group'])

      }else{

        # Create a factor 'Group_f' with the specified group order to be plotted on the X axis.
        data[, 'Group_f'] <- factor(data[, 'Group'], levels= group.order)

      }
      
      # Stacked barchart without SD
      stacked_wo_SD <<- ggplot(data, aes(x = Group_f, y = mean, fill = macronutrient_f)) + 
        geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
        scale_fill_manual(values = distinct100colors) + 
        labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
        # Specify the font size and angle of the x axis label.  
        theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) + no_grid
      
    }else{
      
      # Subset CPT_kcal by the desired macronutrient. 
      subsetted_by_macronut <- subset(data, macronutrient == order.by)
      
      # Order by the mean (small-large) of the subset.  
      subsetted_by_macronut_s <- subsetted_by_macronut[order(subsetted_by_macronut$mean), ]
      
      # Take only the "Group" as a vector, which is ordered by the desired macronutrient.
      Diet_by_macronut <- subsetted_by_macronut_s[["Group"]]
      
      # Specify the order of Diet as in Diet_by_macronut.
      # CPT_kcal$Group_f <- factor(CPT_kcal$Group, levels= Diet_by_macronut)
      data[, 'Group_f'] <- factor(data[, 'Group'], levels= Diet_by_macronut)
      
      # Also specify the order of plotting CARB, PROT, TFAT if you would like.
      # CPT_kcal$macronutrient_f <- factor(CPT_kcal$macronutrient, levels= c("Carbohydrate", "Total Fat", "Protein"))
      data[, 'macronutrient_f'] <- factor(data[, 'macronutrient'], levels= macronut.order) 
      
      # Stacked barchart without SD
      stacked_wo_SD <<- ggplot(data, aes(x = Group_f, y = mean, fill = macronutrient_f)) + 
        geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
        # change colors and labels of legend. Ensure the factor order is correct. 
        scale_fill_manual(values = distinct100colors) + 
        labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
        # Specify the font size and angle of the x axis label.  
        theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) + no_grid
      
    }
    
  }  
  
# ====================================================================================================================
# function2 PlotDodged ... Not stacked. can specify the order of group levels (optional) and the macronutrients. 
# ====================================================================================================================
  PlotDodged <- function(data, order.by, macronut.order, group.order=NULL){
    
    if(order.by=="NULL"){
      
      # Create a factor 'macronutrient_f' with the specified macronutrient stacking order. 
      data[, 'macronutrient_f'] <- factor(data[, 'macronutrient'], levels= macronut.order) 
      
      # Create a factor 'group_f' in the default order or the specified order.
      if(is.null(group.order)){
        
        # Create a factor 'Group_f' with the default, alphabetical order, to be plotted on the X axis.
        data[, 'Group_f'] <- factor(data[, 'Group'])
        
      }else{
        
        # Create a factor 'Group_f' with the specified group order to be plotted on the X axis.
        data[, 'Group_f'] <- factor(data[, 'Group'], levels= group.order)
        
      }
      
      # "dodge"-type barchart with SD.
      dodged_w_SD <<- ggplot(data, aes(x = Group_f, y = mean, fill = macronutrient_f, colour = macronutrient_f)) + 
        geom_bar(stat = "identity", position = "dodge", color="black")  +
        geom_errorbar(aes(ymin= mean, ymax= mean + sd), position = position_dodge(0.9), width = 0.25,
                      color="black") +
        scale_fill_manual(values = distinct100colors) +
        labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
        no_grid + space_axes +
        theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
      
    }else{
      
      # Subset CPT_kcal by the desired macronutrient. 
      subsetted_by_macronut <- subset(data, macronutrient == order.by)
      
      # Order by the mean (small-large) of the subset.  
      subsetted_by_macronut_s <- subsetted_by_macronut[order(subsetted_by_macronut$mean), ]
      
      # Take only the "Group" as a vector, which is ordered by the desired macronutrient.
      Diet_by_macronut <- subsetted_by_macronut_s[["Group"]]
      
      # Specify the order of Diet as in Diet_by_macronut.
      # CPT_kcal$Group_f <- factor(CPT_kcal$Group, levels= Diet_by_macronut)
      data[, 'Group_f'] <- factor(data[, 'Group'], levels= Diet_by_macronut)
      
      # Also specify the order of plotting CARB, PROT, TFAT if you would like.
      data[, 'macronutrient_f'] <- factor(data[, 'macronutrient'], levels= macronut.order) 
      
      # "dodge"-type barchart with SD.
      dodged_w_SD <<- ggplot(data, aes(x = Group_f, y = mean, fill = macronutrient_f, colour = macronutrient_f)) + 
        geom_bar(stat = "identity", position = "dodge", color="black")  +
        geom_errorbar(aes(ymin= mean, ymax= mean + sd), position = position_dodge(0.9), width = 0.25,
                      color="black") +
        scale_fill_manual(values = distinct100colors) +
        labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
        no_grid + space_axes +
        theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
      
    }
    
  }
  
  
# ====================================================================================================================
# function3 PlotStackedWithSD
# ====================================================================================================================
  PlotStackedWithSD <- function(data, order.by, macronut.order, group.order=NULL){
    
    # need to calculate SD_base etc. with the specified macronutrient order.----------------------
    
      CalcStackedSD(input.df = data, macronut.order = macronut.order)
      # output is saved as CPT_kcal_forstackedwSD.
    
    # Generate a plot with SD depending on whether to reorder Diet or not.------------------------- 
    
      if(order.by=="NULL"){
        
        # Create a factor 'macronutrient_f' with the specified macronutrient stacking order. 
        CPT_kcal_forstackedwSD[, 'macronutrient_f'] <- factor(CPT_kcal_forstackedwSD[, 'macronutrient'], levels= macronut.order) 
        
        # Create a factor 'group_f' in the default order or the specified order.
        if(is.null(group.order)){
          
          # Create a factor 'Group_f' with the default, alphabetical order, to be plotted on the X axis.
          CPT_kcal_forstackedwSD[, 'Group_f'] <- factor(CPT_kcal_forstackedwSD[, 'Group'])
          
        }else{
          
          # Create a factor 'Group_f' with the specified group order to be plotted on the X axis.
          CPT_kcal_forstackedwSD[, 'Group_f'] <- factor(CPT_kcal_forstackedwSD[, 'Group'], levels= group.order)
          
        }
        
        # Stacked barchart with SD
        stacked_with_SD <<- ggplot(CPT_kcal_forstackedwSD, aes(x=Group_f, y=mean, fill= macronutrient_f, colour=macronutrient_f)) + 
          geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.7)  +
          geom_errorbar(aes(ymin= mean+sd_base, ymax= mean+sd_stacked), width = 0.15, color="grey10") + 
          scale_fill_manual(values = distinct100colors) +
          labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
          no_grid + space_axes +
          theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
        
      }else{
        
        # Subset CPT_kcal_forstackedwSD by the desired macronutrient. 
        subsetted_by_macronut <- subset(CPT_kcal_forstackedwSD, macronutrient == order.by)
        
        # Order by the mean (small-large) of the subset.  
        subsetted_by_macronut_s <- subsetted_by_macronut[order(subsetted_by_macronut$mean), ]
        
        # Take only the "Group" as a vector, which is ordered by the desired macronutrient.
        Diet_by_macronut <- subsetted_by_macronut_s[["Group"]]
        
        # Specify the order of Diet as in Diet_by_macronut.
        CPT_kcal_forstackedwSD[, 'Group_f'] <- factor(CPT_kcal_forstackedwSD[, 'Group'], levels= Diet_by_macronut)
        
        # Create a factor 'macronutrient_f' with the specified macronutrient stacking order. 
        CPT_kcal_forstackedwSD[, 'macronutrient_f'] <- factor(CPT_kcal_forstackedwSD[, 'macronutrient'], levels= macronut.order) 
        
        # Stacked barchart with SD
        stacked_with_SD <<- ggplot(CPT_kcal_forstackedwSD, aes(x=Group_f, y=mean, fill= macronutrient_f, colour=macronutrient_f)) + 
          geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.7)  +
          geom_errorbar(aes(ymin= mean+sd_base, ymax= mean+sd_stacked), width = 0.15, color="grey10") + 
          scale_fill_manual(values = distinct100colors) +
          labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
          no_grid + space_axes +
          theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
        
      }
    
  }
  
