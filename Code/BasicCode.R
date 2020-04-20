pacman::p_load(tidyverse, skimr)

#json_file <- "https://covidtracking.com/api/v1/states/current.json"
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))
#temp <- fromJSON(file=json_file)

#df = read.csv('https://covidtracking.com/api/v1/states/current.csv')
df = read.csv('https://covidtracking.com/api/states/daily.csv') %>% 
     arrange(state. date)
#df3 = read.csv('https://covidtracking.com/api/us/daily.csv')

