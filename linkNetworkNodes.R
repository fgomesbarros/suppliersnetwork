### 03 - Link Network Nodes
### linkNetworkNodes.R
### This script checks the relationships between the ICT_suppliers and between 
### the agencies. It outputs the GDF files of two networks.

linkNetworkNodes <- function(quantileAgencies = 0,
                             quantileSuppliers = 0,
                             paymentsFile = "total_payments.csv",
                             suppliersFile = "ICT_suppliers.csv",
                             agenciesFile = "agencies.csv",
                             tDirectory = "data/tidy",
                             aDirectory = "data/analisys",
                             suppliersNetFile = "suppliersNetwork.gdf",
                             agenciesNetFile = "agenciesNetwork.gdf") {
    
    ## Reads libraries needed
    library(dplyr)
    
    ### STEP 1 - Refines the suppliers and agencies dataset according quantiles

    ## Reads the ICTsuppliers and agencies datasets
    columnClasses <- c("character", "character", "numeric")
    filename <- paste(tDirectory, "/", suppliersFile, sep="")
    suppliers <- read.csv(file = filename, colClasses = columnClasses)
    suppliers <- tbl_df(suppliers)
    
    filename <- paste(tDirectory, "/", agenciesFile, sep="")
    agencies <- read.csv(file = filename, colClasses = columnClasses)
    agencies <- tbl_df(agencies)
    
    # Calculates quantiles
    if(quantileSuppliers > 0) {
        quantileSuppliers <- quantile(suppliers$valor_receitas, 
                                      probs = quantileSuppliers/100)    
    }
    if(quantileAgencies > 0) {
        quantileAgencies <- quantile(agencies$valor_despesas, 
                                 probs = quantileAgencies/100)
    }
    
    ## Cleans the data, filtering data based on quantile and removing unused 
    ## colunms and commas from razaosocial and nome_orgao variables
    suppliers <- suppliers %>%
        filter(valor_receitas > quantileSuppliers) %>%
        select(-valor_receitas) %>%
        mutate(razaosocial = gsub(",", "", razaosocial))
    agencies <- agencies %>%
        filter(valor_despesas > quantileAgencies) %>%
        select(-valor_despesas) %>%
        mutate(nome_orgao = gsub(',', "", nome_orgao))
    
    ### STEP 2 - Filters payment data
    
    ## Reads payments data
    paymentsFile <- paste(tDirectory,"/", paymentsFile, sep ="")
    payments <- read.csv(file = paymentsFile, 
                         colClasses = c(rep("character", 2), "numeric"))
    payments <- tbl_df(payments)
    
    ## Merges payments with suppliers and agencies
    payments <- payments %>%
        select(codigo_orgao, cnpj) %>%
        inner_join(agencies) %>%
        inner_join(suppliers) %>%
        select(-nome_orgao, -razaosocial)
    
    ### STEP 3 - Calculates weights
    
    ## Converts payments to a matrix  
    paymentsTbl <- table(payments)
    paymentsMtx <- as.matrix(paymentsTbl)
    
    # Calculates weights
    agenciesNetwork <- t(paymentsMtx) %*% (paymentsMtx)
    suppliersNetwork <- paymentsMtx %*% t(paymentsMtx)
    
    # Removing redundant information, setting to zero values in the lower 
    # triangle of both matrixes 
    suppliersNetwork[lower.tri(suppliersNetwork, diag = TRUE)]  <- 0
    agenciesNetwork[lower.tri(agenciesNetwork, diag = TRUE)]  <- 0
        
    # Transforms to a local data frame
    suppliersNetwork <- as.data.frame(as.table(suppliersNetwork), 
                                      stringsAsFactors = TRUE)
    agenciesNetwork <- as.data.frame(as.table(agenciesNetwork), 
                                     stringsAsFactors = TRUE)
    colnames(suppliersNetwork)  <- c("node1", "node2", "weight")
    colnames(agenciesNetwork)  <- c("node1", "node2", "weight")
    suppliersNetwork <- tbl_df(suppliersNetwork)
    agenciesNetwork <- tbl_df(agenciesNetwork)
    
    # Removes rows with weigths equal to zero and equal nodes
    suppliersNetwork <- filter(suppliersNetwork, weight > 0 & node1 != node2) 
    agenciesNetwork <- filter(agenciesNetwork, weight > 0 & node1 != node2)
    
    # Order rows by weight
    suppliersNetwork <- arrange(suppliersNetwork, desc(weight))
    agenciesNetwork <- arrange(agenciesNetwork, desc(weight))
        
    ### STEP 4 - Generates the GDF Files
        
    ## Creates the GDF files
    suppliersNetFile  <- paste(aDirectory, "/", suppliersNetFile, sep="")
    file.create(suppliersNetFile)
    agenciesNetFile  <- paste(aDirectory, "/", agenciesNetFile, sep="")
    file.create(agenciesNetFile)
    
    ## Prints the node section of GDF files
    cat("nodedef>name VARCHAR,label VARCHAR\n", file = suppliersNetFile)
    cat("nodedef>name VARCHAR,label VARCHAR\n", file = agenciesNetFile)
    
    ## Prints the contents of node sections
    write.table(suppliers, file = suppliersNetFile, col.names = FALSE, 
                append = TRUE, row.names = FALSE, quote = FALSE, sep = ",")
    write.table(agencies, file = agenciesNetFile, col.names = FALSE, 
                append = TRUE, row.names = FALSE, quote = FALSE, sep = ",")
    
    ## Prints the edge section of files
    cat("edgedef>node1 VARCHAR, node2 VARCHAR, weight DOUBLE\n", 
        file = suppliersNetFile, append = TRUE)
    cat("edgedef>node1 VARCHAR, node2 VARCHAR, weight DOUBLE\n", 
        file = agenciesNetFile, append = TRUE)
    
    ## Prints the contents of the edge section
    write.table(suppliersNetwork, file = suppliersNetFile, col.names = FALSE, 
                append = TRUE, row.names = FALSE, quote = FALSE, sep = ",")
    write.table(agenciesNetwork, file = agenciesNetFile, col.names = FALSE, 
                append = TRUE, row.names = FALSE, quote = FALSE, sep = ",")
}