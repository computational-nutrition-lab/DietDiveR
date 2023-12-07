# ===============================================================================================================
# Save a table in .txt format without having to specify a lot of arguments.
# Version 1
# Created on 02/07/2023 by Rie Sadohara
# ===============================================================================================================

WriteTableTabDelim <- function(x, file, row.names=F){
  
  write.table(x=x, file=file, sep="\t", quote=F, row.names=row.names)
  
}
  

# # Example
# data(iris)
# 
# WriteTableTabDelim(iris, "irisbyfn.txt")

