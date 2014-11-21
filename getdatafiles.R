### 01 - Get Data Files
### getDataFiles.R
### This script gets the payment and client files from Portal da Transparencia
### URL: http://arquivos.portaldatransparencia.gov.br/
### The files correspond the months from Jan 2011 to Sep 2014 (last update)

getDataFiles <- function(rdataDirectory = "data/raw") {
    
    urlbase  <- "http://arquivos.portaldatransparencia.gov.br/downloads.asp?"
    consult  <- c("FavorecidosGastosDiretos", "GastosDiretos")
            
    ## Get and unzip files from January 2011 to September 2014 
    for(year in c("2011", "2012", "2013", "2014")) {
        for(month in c("01", "02", "03", "04", "05", "06", "07", "08", "09", 
                "10", "11", "12")) {
            
            # Break loop to 2014 files
            if(year == "2014" & month == "10") {
                break
            }
            else {
                for(i in 1:2) {
                    filename  <- paste(rdataDirectory, "/", year, month, "_", 
                                       consult[i], ".zip", sep = "")
                    urlfile <- paste(urlbase, "a=", year, "&m=", month, 
                                     "&consulta=", consult[i], sep = "")
                    download.file(url = urlfile, destfile = filename, 
                                  method = "curl")
                    
                    if(!file.exists(filename)){
                        stop("Download error!")
                    } else {
                        unzip(zipfile = filename, exdir = rdataDirectory)
                    } 
                }
            }
        }
    }
}