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

write.csv(data, file='./Data/Unemployment_By_States.csv', row.names = F)


unemploymenet_data <- read.csv('./Data/Unemployment_By_States.csv', stringsAsFactors=F)
tax_data <-read.csv('./Data/TobaccoTax.csv', stringsAsFactors = F)
use_data <-read.csv('./Data/TobaccoUse.csv',na.strings = "*", stringsAsFactors = F)

tax_data <- tax_data %>% transmute(Year = Year, State =  Location.Description, TaxRate =Data.Value)
use_data <- use_data %>% transmute(Year = Year, State =  Location.Description, SampleSize = Sample.Size, Gender = Gender, Age=Age, Race=Race, Education = Education,SmokeRate = Data.Value, LowConf = Low.Confidence.Limit, HighConf = High.Confidence.Limit)
unemploymenet_data <- transmute(unemploymenet_data, Year = Year, State =State, Unemployment = Ratio)


state_abbre  <- read.csv('./Data/state-abbrevs.csv', stringsAsFactors = F)
use_data <- use_data[use_data$State != 'Guam' & use_data$State != 'Puerto Rico',]


tax_data$State <- sapply(tax_data$State, function(x) state_abbre$abbreviation[state_abbre$state ==  x])
use_data$State <- sapply(use_data$State, function(x) state_abbre$abbreviation[state_abbre$state ==  x])

data <- merge(unemploymenet_data, tax_data)
data <- merge(data, use_data, all=T)

### Only data with different races are missing

library(extracat)
visna(data)

write.csv(data, file = './Data/DataCombined.csv', row.names = F)

smokerateonly <- data %>% filter(Gender == 'Overall' & Age == 'All Ages' & Race == 'All Races') %>% select(Year, State, SmokeRate)
write.csv(smokerateonly, file='./Data/SmokeRateData.csv', row.names = F)


# create the range of colors
for (year in c(2011:2018)){
   l = max(smokerateonly[smokerateonly$Year == year,]$SmokeRate) - min(smokerateonly[smokerateonly$Year == year,]$SmokeRate)
   m = min(smokerateonly[smokerateonly$Year == year,]$SmokeRate)
   print(paste(year, ':{','mean:',m,',','range',l,'}', sep=''))
}

max(smokerateonly$SmokeRate)
min(smokerateonly$SmokeRate)
mean(smokerateonly$SmokeRate)

smokerateonly <- data %>% filter(Gender == 'Overall' & Age == 'All Ages' & Race == 'All Races') %>% select(Year, State, SmokeRate, TaxRate)

# Create the table with data wanted: 
selected_data <- data %>% filter(Gender == 'Overall' & Age == 'All Ages' & Race == 'All Races') %>% select(Year, State, SmokeRate, TaxRate, Unemployment)
write.csv(selected_data, file='./Data/SelectedData.csv', row.names = F)





