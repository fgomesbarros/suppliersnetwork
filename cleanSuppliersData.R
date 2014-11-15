### Clean suppliers data - cleanSuppliersData.R
### This script cleans the allSuppliers data and stores in the ICT_suppliers.csv
### file 

cleanSuppliersData <- function(rdataDirectory = "data/raw", 
                               tdataDirectory = "data/tidy",
                               suppliersFile = "ICT_suppliers.csv") {

    ## Getting the CNAE ID for ICT companies
    columnNames <- c("cod_secao", "desc_secao", "cod_subclasse", 
                     "desc_subclasse")
    filename <- paste(tdataDirectory, "/ICT_CNAE.csv", sep = "")
    cnae <- read.csv(file = filename, header = FALSE, sep = "\t", 
                     col.names = columnNames, colClasses = rep("character", 4), 
                     skip = 1)
    cod_cnae <- cnae$cod_subclasse
    
    ## Cleaning the CNPJ files 
    columnNames <- c("cnpj", "razaosocial", "nomefantasia", "cod_cnae", 
                     "cod_natjuridica")
    
    cnpjFiles <- list.files(path = rdataDirectory, pattern = "*_CNPJ.csv", 
                            full.names = TRUE)
    
    for(i in 1:length(cnpjFiles)) {
        # Read the CNPJ file
        suppliers <- read.csv(file = cnpjFiles[i], header = FALSE, sep = "\t", 
                                col.names = columnNames, 
                                colClasses = rep("character", 5), skip = 1, 
                                encoding = "latin1")
        
        # Filter the suppliers, passing only ICT ones 
        suppliers <- suppliers[suppliers$cod_cnae %in% cod_cnae, ]
        
        # Remove the last 6 digits of CNPJ ID (the suffix). The first eight 
        # (the root) will be used. 
        suppliers$cnpj <- substr(suppliers$cnpj, 1, 8) 
        
        # Remove the unused columns "nomefantasia", "cod_cnae" and 
        # "cod_natjuridica".
        suppliers <- suppliers[, c("cnpj", "razaosocial")]
        
        # Remove duplicated rows by CNPJ ID.
        suppliers <- suppliers[!duplicated(suppliers$cnpj), ]
        
        # Append data readed and processed in the allSuppliers dataset.
        if(i == 1) {
            allSuppliers <- suppliers    
        } else {
            allSuppliers <- rbind.data.frame(allSuppliers, suppliers)
            allSuppliers <- allSuppliers[!duplicated(allSuppliers$cnpj), ]
        }
    }
    ## Write the allSuppliers data in the suppliers file
    filename <- paste(tdataDirectory, "/", suppliersFile, sep = "")
    write.csv(allSuppliers, file = filename, row.names = FALSE)
}