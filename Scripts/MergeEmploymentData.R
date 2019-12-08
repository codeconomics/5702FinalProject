library(readxl)
library(tidyverse)
library(stringr)
data_list <- list()

for(year in c(11:18)){
  data_list<- c(data_list, list(read_excel(paste('UnemploymentData/laucnty',year,'.xlsx', sep=''),skip=4)[-1,]))
}
data <- do.call("rbind", data_list)

data$State <- str_sub(data$`County Name/State Abbreviation`,  start=-2)
data$State <- ifelse(data$State == 'ia', 'DC', data$State)
data <- data[!is.na(data$Year),]
data <- data %>% group_by(Year, State) %>% 
  summarise(Force = sum(Force), Unemployed=sum(Unemployed), Employed=sum(Employed))
data$Ratio <- data$Unemployed / data$Force

write.csv(data, file='Unemployment_By_States.csv', row.names = F)
