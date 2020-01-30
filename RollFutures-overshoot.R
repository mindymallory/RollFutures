#Download data

library(tidyverse)
library(profvis)



#apikey <- 'putyourapikeyhere'


tick_<- list('1983':'2012')[[1]] 


#C
c_contracts <- c('H', 'K', 'N', 'U', 'Z')
c_months    <- c('03', '05', '07', '09', '12')

#S
s_contracts <- c( 'F', 'H', 'K', 'N', 'Q', 'U', 'X')
s_months    <- c('01', '03', '05', '07', '09', '11')

#W
w_contracts <- c( 'H', 'K', 'N', 'U', 'Z')
w_months    <- c_months

# CL
cl_contracts <- c('F', 'H', 'K', 'N', 'Q', 'U', 'X', 'Z') # Chose contract months that match c and s
cl_months <- c('01', '03', '05', '07', '09', '11', '12')

#Years Loop
for(j in 1:length(tick_)){
  # 
  # # Corn Contracts
  # for(i in 1:length(c_contracts)){
  #   
  #   tick <- tick_[[j]]
  #   
  #   data <- paste0('http://ondemand.websol.barchart.com/getHistory.csv?apikey=', apikey, '&symbol=ZC', c_contracts[i], 
  #                  substr(as.character(tick), 3, 4), '&type=daily') %>%
  #     download.file(destfile =paste0('data/', 'ZC', c_contracts[i], tick, '.csv'), method = "libcurl")
  #   
  # }
  # 
  # # Soybeans
  # for(i in 1:length(s_contracts)){
  #   
  #   tick <- tick_[[j]]
  #   
  #   data <- paste0('http://ondemand.websol.barchart.com/getHistory.csv?apikey=', apikey, '&symbol=ZS', s_contracts[i], 
  #                  substr(as.character(tick), 3, 4), '&type=daily') %>%
  #     download.file(destfile =paste0('data/', 'ZS', s_contracts[i], tick, '.csv'), method = "libcurl")
  #   
  # }
  # 
  # # Chi Wheat
  # for(i in 1:length(w_contracts)){
  #   
  #   tick <- tick_[[j]]
  #   
  #   data <- paste0('http://ondemand.websol.barchart.com/getHistory.csv?apikey=', apikey, '&symbol=ZW', w_contracts[i], 
  #                  substr(as.character(tick), 3, 4), '&type=daily') %>%
  #     download.file(destfile =paste0('data/', 'ZW', w_contracts[i], tick, '.csv'), method = "libcurl")
  #   
  # }
  
  # WTI Crude
  for(i in 1:length(cl_contracts)){
    
    tick <- tick_[[j]]
    
    data <- paste0('http://ondemand.websol.barchart.com/getHistory.csv?apikey=', apikey, '&symbol=CL', cl_contracts[i], 
                   substr(as.character(tick), 3, 4), '&type=daily') %>%
      download.file(destfile =paste0('data/', 'CL', cl_contracts[i], tick, '.csv'), method = "libcurl")
    
  }
  
  
}

