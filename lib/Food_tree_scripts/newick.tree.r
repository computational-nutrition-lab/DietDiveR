# Creates a Node Object (data.tree obj) 
# For information on data.tree, type vignette("data.tree") in console

# Output Newick tree file from tree
#
# Input
# Node-based tree, with path variables such as : exampleTree$pathString:root1/internal2/internal3/leaf4
# 
#
# Output
# Newick file formatted character vector
# To find without internal nodes, look at RecursiveNewickWrite2 below

# Add data.tree
# require("data.tree")

# Based on Input node, recursively generate Newick Tree
RecursiveNewickWrite <- function(node)
{
  # If Node has no children, just return its name and distance value
  if(length(node$children)==0) {
    dist<-distance()
    return (sprintf("%s:%.1f", node$name, dist))
  }else{
    # Set current node as "internal", Get Children of Node and apply fn RecursiveNewickWrite
    dist <- distance()
    # Result Format example : (  result from child nodes, separated by commas ) CurrentNode$name:1.0
    return (sprintf("(%s)%s:%.1f", paste(vapply(node$children, RecursiveNewickWrite, FUN.VALUE=character(1)), collapse=", "), node$name, dist))
    
  }
}

# Generate Newick Format WITHOUT internal nodes 
RecursiveNewickWrite2 <- function(node)
{
  # If Node has no children, just return its name and distance value
  if(length(node$children)==0) {
    dist <- distance()
    return (sprintf("%s:%.1f", node$name, dist))
  }else{
    # Set current node as "internal", Get Children of Node and apply fn RecursiveNewickWrite
    dist <- distance()
    # Result Format example : (  result from child nodes, separated by commas ) CurrentNode$name:1.0
    return (sprintf("(%s)", paste(vapply(node$children, RecursiveNewickWrite2, FUN.VALUE = character(1)), collapse=", ")))
    
  }
}

# Distance Function for Tree, tbd
distance <- function()
{
  return (1.0)
}

 

