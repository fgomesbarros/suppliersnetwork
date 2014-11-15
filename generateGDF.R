### Generate GDF file
### generateGDF.R
### This script generate a GDF file, based on payment tidy data.
###

generateGDF <- function(suppliersfile = "ICT_suppliers.csv",
                        tdirectory = "data/tidy",
                        adirectory = "data/analisys",
                        outputfile = "suppliers_network.gdf") {
    
    ## Create the GDF file
    outputfile  <- paste(adirectory, "/", outputfile, sep="")
    file.create(outputfile)
    
    ## Print the node section of the file
    cat("nodedef> name, label\n", file = outputfile)
    
    ## Print the contents of the node section
    suppliersfile  <- paste(tdirectory, "/", suppliersfile, sep="")
    suppliers <- read.csv(suppliersfile, colClasses = rep("character", 2))
    cat(suppliers, file = outputfile)
}