pacman::p_load(tidyverse, skimr, lubridate, ggthemes)

#json_file <- "https://covidtracking.com/api/v1/states/current.json"
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))
#temp <- fromJSON(file=json_file)

#df = read.csv('https://covidtracking.com/api/v1/states/current.csv')
df = read.csv('https://covidtracking.com/api/states/daily.csv') %>% 
     arrange(state, date) %>% 
     select(-c(hash, dateChecked)) %>% 
     mutate(fips = str_sub(paste0('0', fips), -2, -1), date = ymd(date))
#df3 = read.csv('https://covidtracking.com/api/us/daily.csv')

df %>% 
     filter(!is.na(positiveIncrease)) %>% 
     ggplot(aes(x = date, y = positiveIncrease)) +
     geom_bar(stat = 'identity', fill = 'steelblue') +
     facet_wrap(~ state, scales = 'free') +
     theme_minimal()

state.list = df %>% 
     group_by(state) %>% 
     mutate(MaxCase = max(positive)) %>% 
     ungroup() %>% 
     distinct(state, .keep_all = TRUE) %>% 
     filter(MaxCase > 5000) %>% 
     select(state)

df %>% 
     filter(state %in% state.list$state, positive > 500) %>% 
     filter(state %in% c('CA', 'NJ', 'MA', 'NY', 'TX', 'FL')) %>% 
     group_by(state) %>% 
     mutate(day = c(1:n())) %>% 
     ungroup() %>% 
     ggplot(aes(x = day, y = log(positive), color = state)) +
          geom_point() +
          geom_line() +
          #coord_trans(y="log2") +
          theme_fivethirtyeight()


## World
data = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')

data2 = data %>% 
     select(-c(Lat, Long)) %>% 
     group_by(Country.Region, Province.State) %>% 
     pivot_longer(-c(Country.Region, Province.State), names_to = "tempdate", values_to = "count") %>% 
     mutate(n = c(1:n()), date = as.Date(ifelse(tempdate == 'X1.22.20', as.Date('2020-01-22'), as.Date('2020-01-22') +n -1))) %>% 
     ungroup()

