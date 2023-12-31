# food_records_fn = formatted file of all diet records, must contain FoodID and grams columns
# food_record_id = column name of identifier separating your diet records (e.g. subjectid, sampleid, etc.)
# food_taxonomy_fn = taxonomy file generated by make.food.tree
# output_fn = output file name of your ifc table
# 06/26/2023 replaced "OTU" with "IFC".

MakeFiberIfc <- function(food_records_fn, food_record_id, food_taxonomy_fn, output_fn)
{
    # read everything in as a character to preserve numeric food codes and IDs
    diet <- read.table(food_records_fn, header = TRUE, sep="\t", colClasses="character", quote="", strip.white=T)
    diet$FIBE <- as.numeric(diet$FIBE)

    # sum total grams of each food eaten within a record
    cdiet <- aggregate(diet$FIBE, by=list(diet[,food_record_id], diet$FoodID), FUN=sum)
    colnames(cdiet) <- c(food_record_id, "FoodID", "fiber.grams")

    cdiet.w <- reshape(cdiet, timevar = "FoodID", idvar = food_record_id, direction = "wide")
    cdiet.w[is.na(cdiet.w)] <- 0
    rownames(cdiet.w) <- cdiet.w[,1] # make record_ids the rownames
    cdiet.w <- cdiet.w[,-1]    
    colnames(cdiet.w) <- gsub("fiber.grams.", "", colnames(cdiet.w)) # rename column names to FoodIDs only
    t.cdiet.w <- t(cdiet.w)
    
    food.taxonomy <- read.table(food_taxonomy_fn, sep="\t", colClasses="character", quote="", header=T, row=1)
    
    fiber.ifc <- merge(t.cdiet.w, food.taxonomy, by=0)
    
    # get rid of the FoodIDs and replace it with the food tree leaf names
    rownames(fiber.ifc) <- fiber.ifc[,"Main.food.description"]
    remove.col.ix <- which(colnames(fiber.ifc) %in% c("Main.food.description", "Row.names"))
    fiber.ifc <- fiber.ifc[,-remove.col.ix]
    
    # Write "#FOODID\t" in a file specified. Creating the first row of output.  
    cat("#FOODID\t", file=output_fn)
    
    # Add fiber.ifc to the output. 
    write.table(fiber.ifc, output_fn, sep = "\t", quote = F, append=TRUE)
    
    invisible(fiber.ifc)
}

