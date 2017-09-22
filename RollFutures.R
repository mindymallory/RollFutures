install.packages("Quandl")
install.packages("plyr")
library(Quandl)
library(plyr)
Quandl.api_key("otkcuWHb5hYFTXTsiMds")

start <- 2015
end <- 2016
years <- seq(start, end, by =1)

contracts <- c('H', 'K', 'N', 'U', 'Z')

data <- list()
k <- 1

for (i in start:end){
  for (j in 1:5){
    MyData = cbind(contracts[j], Quandl(paste0("CME/C", contracts[j], years[i-start +1])))
    data[[k]] <- MyData
    k <- k+1
    # To keep raw data files uncomment this line. 
    # write.csv(MyData, file = paste0("data-download/", contracts[j], years[i-start +1], ".csv"))
  }
}

DATA <- ldply(data, rbind)
DATA <- DATA[, c(1,2,8,9)]
colnames(DATA) <- c('Contract', 'Date', 'Settle', 'Volume')

# playing from here. 
DATA$Contract <- as.character(DATA$Contract)
filter(DATA, DATA$Contract == 'Z')
class(DATA)
