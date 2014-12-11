### 02 - Adjust Agencies
### adjustAgencies.R
### This script correct all information with duplicated agencies.

adjustAgencies <- function(agenciesInput1 = "agencies.csv",
                           agenciesInput2 = "agencies_adjust.csv",
                           agenciesOutput = "agencies_old.csv",
                           paymentsInput = "total_payments.csv",
                           paymentsOutput = "total_payments_old.csv",
                           tDirectory = "data/tidy") {
    
    ## Load libraries needed
    library(dplyr)
    
    ## Sets the complete path of the files
    agenciesInput1 <- paste(tDirectory, "/", agenciesInput1, sep = "")
    agenciesInput2 <- paste(tDirectory, "/", agenciesInput2, sep = "")
    agenciesOutput <- paste(tDirectory, "/", agenciesOutput, sep = "")
    paymentsInput <- paste(tDirectory, "/", paymentsInput, sep = "")
    paymentsOutput <- paste(tDirectory, "/", paymentsOutput, sep = "")
    
    ## Reads agencies file
    agencies <- read.csv(file = agenciesInput1, colClasses = c("character",
                                                               "character",
                                                               "numeric"))
    agencies <- tbl_df(agencies)
    
    ## Reads agencies adjust file
    adjust <- read.csv(file = agenciesInput2, header = TRUE, quote = "",
                         colClasses = c("character", "character"))
    
    ## Reads the payments file
    ICTpayments <- read.csv(file = paymentsInput, 
                            colClasses = c(rep("character", 2), "numeric"))
    ICTpayments <- tbl_df(ICTpayments)
                           
    ## Creates a backup copy of the files
    file.copy(agenciesInput1, agenciesOutput)
    file.copy(paymentsInput, paymentsOutput)   
    
    ## Corrects the rows
    for(i in 1:dim(adjust)[1]) {
        ICTpayments <- ICTpayments %>%
            mutate(codigo_orgao = ifelse(codigo_orgao == adjust$ID[i],
                                         adjust$newID[i], codigo_orgao))
    }
    
    ## Summarizes payments again
    ICTpayments <- ICTpayments %>%
        group_by(codigo_orgao, cnpj) %>%
        summarise(total = sum(total))
    ICTpayments <- ungroup(ICTpayments)
            
    ## Summarizes the agencies expenses data
    agenciesExpenses <- ICTpayments %>%
        select(codigo_orgao, total) %>%
        group_by(codigo_orgao) %>%
        summarise(valor_despesas = sum(total))

    ## Gets the agencies names
    agenciesNames <- select(agencies, codigo_orgao, nome_orgao)
    
    ## Joins all data
    agencies <- inner_join(agenciesExpenses, agenciesNames) 
    
    ## Arrange data
    agencies <- agencies %>%
        select(codigo_orgao, nome_orgao, valor_despesas) %>%
        arrange(desc(valor_despesas))
    
    ## Writes data in the files
    write.csv(agencies, file = agenciesInput1, row.names = FALSE)
    write.csv(ICTpayments, file = paymentsInput, row.names = FALSE)
}