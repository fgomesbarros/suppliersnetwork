### Consolidate payment data
### consolidatePayments.R
### This script gets all payment data and stores in an unique file called 
### total_ICT_payments.csv
consolidatePayments <- function(tdataDirectory = "data/tidy") {
    
    ## Load the libraries needed
    library(plyr)
    
    columnClasses <- c(rep("character", 4), "numeric")
    
    paymentFiles <- list.files(path = tdataDirectory, 
                             pattern = "*_ICT_payments.csv", full.names = TRUE)
    
    # Read the first dataset file 
    allPayment <- read.csv(file = paymentFiles[1], colClasses = columnClasses)
    
    # Read the dataset files in a loop, starting from the second one
    for(i in 2: length(paymentFiles)) {
        payment <- read.csv(file = paymentFiles[i], colClasses = columnClasses)
        allPayment  <- rbind.data.frame(allPayment, payment)
    }
    
    ## Summarize the payment by cnpj and codigo_orgao_subordinado in 
    ## total_pago variable
    tdata <- ddply(allPayment, c("cnpj", "razaosocial", 
                              "codigo_orgao_subordinado",
                              "nome_orgao_subordinado"), 
                   summarise, total = sum(total_pagamento))
    
    ## Write the resulting dataset in a file
    filename <- paste(tdataDirectory, "/", "total_ICT_payments.csv", sep = "")
    write.csv(tdata, file = filename, row.names = FALSE)
} 