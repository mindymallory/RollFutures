install.packages("Quandl")
install.packages("plyr")
install.packages("tidyr")
library(Quandl)
library(plyr)
library(tidyr)
Quandl.api_key("otkcuWHb5hYFTXTsiMds")

start <- 2015
end <- 2016
years <- seq(start, end, by =1)

contracts <- c('H', 'K', 'N', 'U', 'Z')

data <- list()
k <- 1

for (i in start:end){
  for (j in 1:5){
    MyData = cbind(Quandl(paste0("CME/C", contracts[j], years[i-start +1])), paste0(contracts[j], '-', years[i-start+1]))
    data[[k]] <- MyData
    k <- k+1
    # To keep raw data files uncomment this line. 
    # write.csv(MyData, file = paste0("data-download/", contracts[j], years[i-start +1], ".csv"))
  }
}

DATA <- ldply(data, rbind)
DATA <- DATA[, c(1,7,8,10)]
colnames(DATA) <- c('Date', 'Settle', 'Volume', 'Contract')

DATA <- gather(DATA, Observation, Value, Settle:Contract)
DATA <- spread(DATA, Observation, Value)
