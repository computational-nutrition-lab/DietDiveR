# ========================================================================================
# Generate a food tree from dietary data.
# Version 1
# Created on 02/17/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Visualize a food tree using the ggtree package.
# ========================================================================================
 
# ---------------------------------------------------------------------------------------------------------------
# Function to do some prep for plotting L1 labels.
# Do some preparation to highlight and annotate.
# Save the tip labels as a vector for processing.
  PrepFoodTreePlots <- function(input.tree=tree){
    
    tiplabels <- tree[["tip.label"]]
    length(tiplabels)  # 134 food items = tips for VVKAJ.
    
    # Make the nodes (root, L1s and L2s and so on) into a dataframe.   
    nodelabels <- tree[["node.label"]]
    length(nodelabels) # 42 = 1 root + 41 L1s&L2s.
    nodelabelsdf <- data.frame(nodelabels= nodelabels, 
                                level= substr(nodelabels, 1,2), # Take the 1st and 2nd characters from nodelabels. 
                                seqnum= seq(1:length(nodelabels)),   
                                nodenum= seq(1:length(nodelabels))+length(tiplabels)  # This corresponds to the node numbers in the plot.
                                )
    
    # Replace 'fo' to 'root'
    nodelabelsdf[1, 2] <- 'root'
    
    # Take only the rows that are L1 and find their nodenumbers in order to annotate nodes.
    L1s <- subset(nodelabelsdf, level=='L1')  
    # This should have 9 rows if all L1 levels are present in your dataset.
    # if for some reason there are not 9, need to decrease the number of colors specified below. 
    
    # Make a reference table of the full node names and shorter node names.
    L1sref <- data.frame(nodelabels = L1s$nodelabels,
                          shortnodelabels=c("Milks",                  "Meats",
                                            "Eggs",                   "Legumes",
                                            "Grains",                 "Fruits", 
                                            "Vegetables",             "Fats", 
                                            "Sweets&\nBeverages"),
                          hilightcolors=  c('lightblue',              'firebrick1',        # Add 9 colors for highlighting each L1.
                                            'orange',                 'royalblue',
                                            'gold',                   'darkorchid',
                                            'seagreen',               'lightgreen',
                                            'rosybrown1'),
                          L1labelcolors=  c('lightblue4',             'firebrick',         # Add 9 colors for annotating each L1.
                                            'darkorange',            'darkblue',
                                            'gold4',                 'darkorchid',
                                            'seagreen',               'limegreen',
                                            'mediumvioletred'))
    
    # Merge the shortnodelabels to L1s
    merged1 <- merge(x=L1s, y=L1sref, all.x=T, by='nodelabels') # all.x=T ignores items in y that is missing in x. 
    
    # merge() sorts rows automatically, so need to re-sort it to the original order - by seqnum.
    # and sort the columns also so that nodelabels (column 1) and shortnodelabels (column 5) will be next to each other.
    L1s <- merged1[order(merged1$seqnum), c(1,5,2:4,6:7)]
    
    # Make vectors for plotting.
    L1nodenum       <<- L1s$nodenum
    L1nodelabels    <<- L1s$shortnodelabels
    L1hilightcolors <<- L1s$hilightcolors
    L1labelcolors   <<- L1s$L1labelcolors
  }

# ---------------------------------------------------------------------------------------------------------------
# highlight and annotate L1s using the nodenumbers. 
  
  VizFoodTree <- function(input.tree=tree, layout= c("radial", "circular")){
    tree_an_hi <- ggtree(input.tree, ladderize=F, layout = layout) +
      # geom_text(aes(label=node), hjust= -0.3) +
      geom_hilight(   node=L1nodenum[1],  fill=L1hilightcolors[1]) +  # Milk products
      geom_cladelabel(node=L1nodenum[1], color=  L1labelcolors[1], label=L1nodelabels[1], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[2],  fill=L1hilightcolors[2]) +  # Meat & fish
      geom_cladelabel(node=L1nodenum[2], color=  L1labelcolors[2], label=L1nodelabels[2], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[3],  fill=L1hilightcolors[3]) +  # Eggs
      geom_cladelabel(node=L1nodenum[3], color=  L1labelcolors[3], label=L1nodelabels[3], offset=0.5, geom="label", fill='white', hjust=0.5) +  
      geom_hilight(   node=L1nodenum[4],  fill=L1hilightcolors[4]) +  # Legumes, nuts & seeds
      geom_cladelabel(node=L1nodenum[4], color=  L1labelcolors[4], label=L1nodelabels[4], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[5],  fill=L1hilightcolors[5]) +  # Grain products
      geom_cladelabel(node=L1nodenum[5], color=  L1labelcolors[5], label=L1nodelabels[5], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[6],  fill=L1hilightcolors[6]) +  # Fruits
      geom_cladelabel(node=L1nodenum[6], color=  L1labelcolors[6], label=L1nodelabels[6], offset=0.5, geom="label", fill='white', hjust=0.5) +
      geom_hilight(   node=L1nodenum[7],  fill=L1hilightcolors[7]) +  # Vegetables
      geom_cladelabel(node=L1nodenum[7], color=  L1labelcolors[7], label=L1nodelabels[7], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[8],  fill=L1hilightcolors[8]) +  # Fats & oils
      geom_cladelabel(node=L1nodenum[8], color=  L1labelcolors[8], label=L1nodelabels[8], offset=0.5, geom="label", fill='white', hjust=0.5) + 
      geom_hilight(   node=L1nodenum[9],  fill=L1hilightcolors[9]) +  # Sweets & beverages
      geom_cladelabel(node=L1nodenum[9], color=  L1labelcolors[9], label=L1nodelabels[9], offset=0.5, geom="label", fill='white', hjust=0.5) 
    
    # Widen the opening of the tree  
    tree_an_hi_o <- open_tree(tree_an_hi, 10)
    
    # Rotate the tree so that the root (break) will come to the bottom
    tree_an_hi_o_rt <- rotate_tree(tree_an_hi_o, 275) # 270 + 10*0.5 
    
    # Rename the tree with a better name.
    annotated_tree <<- tree_an_hi_o_rt
    
    } 

# ---------------------------------------------------------------------------------------------------------------

