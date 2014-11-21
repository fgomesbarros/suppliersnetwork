### 02 - Clean Data
### cleanData.R
### This script gets all raw data, cleans and stores them in a set of files:
### ICT_suppliers.csv, agencies.csv, total_payments.csv
###

cleanData <- function(rDirectory = "data/raw",
                      tDirectory = "data/tidy",
                      suppliersFile = "ICT_suppliers.csv", 
                      agenciesFile = "agencies.csv",
                      paymentsFile = "total_payments.csv", 
                      cnaeFile = "ICT_CNAE.csv") { 

    ## Reads libraries
    library(dplyr)

    ### STEP 1 - Clean suppliers data files
    
    ## Gets the CNAE ID for ICT companies
    columnNames <- c("cod_secao", "desc_secao", "cod_subclasse", 
                     "desc_subclasse")
    filename <- paste(tDirectory, "/", cnaeFile, sep = "")
    cnae <- read.csv(file = filename, header = FALSE, sep = "\t", skip = 1,
                     col.names = columnNames, colClasses = rep("character", 4))
    cnaeSubclasse <- cnae$cod_subclasse
    
    ## Cleans the CNPJ files 
    columnNames <- c("cnpj", "razaosocial", "nomefantasia", "cod_cnae", 
                     "cod_natjuridica")
    
    cnpjFiles <- list.files(path = rDirectory, pattern = "*_CNPJ.csv", 
                            full.names = TRUE)
    
    # Starts loop for reading raw suppliers files
    for(i in 1:length(cnpjFiles)) {
        # Reads the CNPJ file
        suppliers <- read.csv(file = cnpjFiles[i], header = FALSE, sep = "\t", 
                              col.names = columnNames, 
                              colClasses = rep("character", 5), skip = 1, 
                              encoding = "latin1")
        suppliers <- tbl_df(suppliers)
        
        # Cleans the suppliers data
        suppliers <- suppliers %>%
            # Filters the suppliers, passing only ICT ones 
            filter(cod_cnae %in% cnaeSubclasse) %>%
            
            # Removes the unused columns "nomefantasia", "cod_cnae" and 
            # "cod_natjuridica".
            select(cnpj, razaosocial) %>%
            
            # Removes the last 6 digits of CNPJ ID (the suffix). The first eight 
            # (the root) will be used
            mutate(cnpj = substr(cnpj, 1, 8)) %>%
        
            # Removes duplicated rows by CNPJ ID.
            filter(!duplicated(cnpj))
                
        # Appends data readed and processed in the ICTSuppliers dataset.
        if(i == 1) {
            ICTSuppliers <- suppliers    
        } else {
            ICTSuppliers <- rbind_list(ICTSuppliers, suppliers)
            ICTSuppliers <- filter(ICTSuppliers, !duplicated(cnpj))
        }
    }
    
    ### STEP 2 - Cleans payment data
    
    ## Sets the column names
    columnNames <- c("codigo_orgao_superior", "nome_orgao_superior", 
                     "codigo_orgao", "nome_orgao", 
                     "codigo_unidade_gestora", "nome_unidade_gestora", 
                     "codigo_grupo_despesa", "nome_grupo_despesa", 
                     "codigo_elemento_despesa", "nome_elemento_despesa", 
                     "codigo_funcao", "nome_funcao", "codigo_subfuncao", 
                     "nome_subfuncao", "codigo_programa", "nome_programa", 
                     "codigo_acao", "nome_acao", "linguagem_cidada",
                     "cnpj", "nome_favorecido", 
                     "numero_documento_pagamento", "gestao_pagamento", 
                     "data_pagamento", "valor_pagamento")
    
    ## Reads all the names of raw payment files 
    paymentsFiles <- list.files(path = rDirectory, 
                               pattern = "*_GastosDiretos.csv", 
                               full.names = TRUE)
    
    ## Starts loop for reading raw payment files
    for(i in 1:length(paymentsFiles)) {
        
        ## Reads the raw payment file
        monthlyPayments <- read.csv(file = paymentsFiles[i], header = FALSE, 
                            col.names = columnNames, sep = "\t", 
                            colClasses = c(rep("character", 24), "numeric"), 
                            dec = ",", skip = 1, encoding = "latin1")
        monthlyPayments <- tbl_df(monthlyPayments)
        
        ## Filters columns 
        monthlyPayments <- monthlyPayments %>%
            select(codigo_orgao, nome_orgao, cnpj, valor_pagamento) %>%
            # Removes unapropriate cnpj content (e.g.: secret data, CPF)
            filter(!is.na(as.numeric(cnpj))) %>%
            # Transforms CNPJ-14-digits in CNPJ-first-8-digits
            mutate(cnpj = substr(cnpj, 1, 8))  %>%
            # Merges the payment dataset with the CNPJ of the ICT suppliers
            inner_join(ICTSuppliers) %>%
            # Reorder columns 
            select(codigo_orgao, nome_orgao, cnpj, razaosocial, valor_pagamento)
        
        ## Write the monthly resulting data frame in a file
        prefixFile <- substr(paymentsFiles[i], 10, 15)
        filename <- paste(tDirectory, "/", prefixFile, 
                          "_ICT_payments.csv", sep = "")
        write.csv(monthlyPayments, file = filename, row.names = FALSE)
        
        ## Appends data readed and processed in the ICTSuppliers dataset.
        if(i == 1) {
            ICTpayments <- monthlyPayments 
        } else {
            ICTpayments <- rbind_list(ICTpayments, monthlyPayments)
        }
    } 
    ## Consolidates all payments
    ICTpayments <- ICTpayments %>%
        group_by(codigo_orgao, nome_orgao, cnpj, razaosocial) %>%
        summarise(total = sum(valor_pagamento))
    ICTpayments <- ungroup(ICTpayments)
    
    ### STEP 3 - Consolidates agencies and suppliers datasets
    
    ## Agencies:
    ## Gets the IDs and names, removing the duplicates
    agenciesNames <- ICTpayments %>%
        select(codigo_orgao, nome_orgao) %>%
        filter(!duplicated(codigo_orgao))
    ## Gets the IDs and expense
    agenciesExpenses <- ICTpayments %>%
        select(codigo_orgao, total) %>%
        group_by(codigo_orgao) %>%
        summarise(valor_despesas = sum(total))
    ## Join all data
    agencies <- inner_join(agenciesNames, agenciesExpenses)
    
    ## Suppliers:
    ICTsuppliers <- ICTpayments %>%
        select(cnpj, razaosocial, total) %>%
        group_by(cnpj, razaosocial) %>%
        summarise(valor_receitas = sum(total))
    
    ### STEP 4 - Writes resulting datasets in a file
    
    ## Removes columns unused of ICTpayments
    ICTpayments <- select(ICTpayments, -nome_orgao, -razaosocial)
    
    filename <- paste(tDirectory, "/", agenciesFile, sep = "")
    write.csv(agencies, file = filename, row.names = FALSE)
    filename <- paste(tDirectory, "/", suppliersFile, sep = "")
    write.csv(ICTsuppliers, file = filename, row.names = FALSE)
    filename <- paste(tDirectory, "/", paymentsFile, sep = "")
    write.csv(ICTpayments, file = filename, row.names = FALSE)
}