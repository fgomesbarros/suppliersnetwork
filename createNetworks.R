### 04 - Create Networks
### createNetworks.R
### This script creates networks to be analysed by Gephi.

createNetworks <- function(){
    source("linkNetworkNodes.R")
    source("linkNetworkNodes2.R")
        
    for(quantile in c(0, 20, 50, 80, 85, 90, 95, 99)) {
        
        suffix <- 100 - quantile
        suppliersFile <- c("suppliersNetwork.gdf", "suppliersNetwork2.gdf")
        agenciesFile <- c("agenciesNetwork.gdf", "agenciesNetwork2.gdf")
        
        # Adjust file names
        if(suffix != 100){
            for(i in 1:2) {
                suppliersFile[i] <- substr(suppliersFile[i], 1, 
                                           (nchar(suppliersFile[i]) - 4))
                agenciesFile[i] <- substr(agenciesFile[i], 1, 
                                          (nchar(agenciesFile[i]) - 4))
                
                suppliersFile[i] <- paste(suppliersFile[i], "-", 
                                       as.character(suffix), "perc.gdf", sep = "")
                agenciesFile[i] <- paste(agenciesFile[i], "-",
                                      as.character(suffix), "perc.gdf", sep = "")
            }
        }
        
        # Create file
        linkNetworkNodes(quantileAgencies = quantile,
                         quantileSuppliers = quantile,
                         suppliersNetFile = suppliersFile[1],
                         agenciesNetFile = agenciesFile[1])
        
        linkNetworkNodes2(quantileAgencies = quantile,
                          quantileSuppliers = quantile,
                          suppliersNetFile = suppliersFile[2],
                          agenciesNetFile = agenciesFile[2])
    }
}
    