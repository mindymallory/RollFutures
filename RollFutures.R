install.packages("Quandl")
install.packages("plyr")
install.packages("tidyr")
install.packages("ggplot2")
library(Quandl)
library(plyr)
library(tidyr)
library(ggplot2)
Quandl.api_key("YourAPIKeyHere")

start <- 2010
end <- 2017
c_code <- 7
commodity_code <- c("C", "S", "W", "KW", "CL", "HO", "RB", "BO", "SM", "LN")
# #C
# contracts <- c('H', 'K', 'N', 'U', 'Z')

# #S
# contracts <- c( 'F', 'H', 'K', 'N', 'Q', 'U', 'X')

# #BO
# contracts <- c('F', 'H', 'K', 'N', 'Q', 'U', 'Z')  # For meal and oil I made a decision to skip V and get Z to match crush production
                                                    #(can't sell SO and SM before you buy the soybeans).
##SM
#contracts <- c('F', 'H', 'K', 'N', 'Q', 'U', 'Z')

# #W
# contracts <- c( 'H', 'K', 'N', 'U', 'Z')
# #KW
# contracts <- c( 'H', 'K', 'N', 'U', 'Z')

#CL
contracts <- c( 'F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')

# # LH
# contracts <- c('G', 'J', 'K', 'M', 'N', 'Q', 'V', 'Z')

years <- seq(start, end, by =1)
data <- list()
k <- 1

for (i in start:end){
  for (j in 1:length(contracts)){
    MyData = cbind(Quandl(paste0("CME/", commodity_code[c_code], contracts[j], years[i-start +1])), paste0(contracts[j], '-', years[i-start+1], '-Settle'), paste0(contracts[j], '-', years[i-start+1], '-Volume'))
    data[[k]] <- MyData
    k <- k+1
    # To keep raw data files uncomment this line. 
    # write.csv(MyData, file = paste0("data-download/", contracts[j], years[i-start +1], ".csv"))
  }
}

DATA <- ldply(data, rbind)
DATA <- DATA[, c(1,7,8,10, 11)]
DATA <- as.data.frame(DATA)
colnames(DATA) <- c('Date', 'Settle', 'Volume', 'Contract.Settle', 'Contract.Volume')

settle <- DATA[, c("Date", "Contract.Settle", "Settle")]
volume <- DATA[, c("Date", "Contract.Volume", "Volume")]
colnames(settle) <- c('Date', 'Contract', 'Value')
colnames(volume) <- c('Date', 'Contract', 'Value')
DATA <- rbind(settle, volume)
colnames(DATA) <- c('Date', 'Contract', 'Value')

DATA <- spread(DATA, Contract, Value)
DATA <- as.xts(DATA[, -1], order.by = DATA[, 1])
DATA <- DATA[paste0(start,'/',end)]

temp <- apply(DATA[, (length(contracts)*length(years)+1):dim(DATA)[2]], 1, which.max)

nearby <- vector(mode= "numeric", length = length(temp))  
for (i in 1:length(temp)){
nearby[i] <- DATA[i, temp[i]]
}
DATA$Nearby <- nearby

g <- autoplot(DATA$Nearby)
g

 DATA_Export_RB <- DATA$Nearby
 
 
DATA_Export <- merge(DATA_Export_CL, DATA_Export_HO, DATA_Export_RB)
 
colnames(DATA_Export) = c("CrudeOil", "ULSD", "RBOB")

write.zoo(DATA_Export, file = "P:/Github-Repos/PriceAnalysis/Excel-files/CrackSpread.csv", row.names=FALSE, na="",col.names=TRUE, sep=",")
