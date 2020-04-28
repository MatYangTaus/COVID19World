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
     filter(MaxCase > 10000) %>% 
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
data = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')

data2 = data %>% 
     select(-c(Lat, Long)) %>% 
     group_by(Country.Region, Province.State) %>% 
     pivot_longer(-c(Country.Region, Province.State), names_to = "tempdate", values_to = "count") %>% 
     mutate(n = c(1:n())) %>% #, date =  as.Date(ifelse(n == 1, '2020-01-22', NA))) %>% 
     mutate(date = as.Date('2020-01-21') + n) %>% 
     ungroup() %>% 
     select(Country.Region, Province.State, date, count) %>% 
     group_by(Country.Region, date) %>% 
     summarize(Count = sum(count)) %>% 
     ungroup()
 
state.list2 = data2 %>% 
     group_by(Country.Region) %>% 
     mutate(MaxCase = max(Count)) %>% 
     ungroup() %>% 
     distinct(Country.Region, .keep_all = TRUE) %>% 
     filter(MaxCase > 5000) %>% 
     select(Country.Region)

data2 %>% 
     group_by(Country.Region) %>% 
     arrange(desc(date)) %>% 
     slice(1) %>% 
     filter(log(Count) < 9, log(Count) > 8.5)

data2 %>% 
     filter(Country.Region %in% state.list2$Country.Region, Count > 500) %>% 
     #filter(Country.Region %in% c('Brazil', 'Japan', 'Singapore', 'Sweden', 'Korea, South', 'Canada', 'Philippines', 'Malaysia', 'Qatar')) %>% 
     filter(Country.Region %in% c('Japan', 'Singapore', 'Sweden', 'Korea, South', 'Philippines', 'Malaysia', 'Qatar')) %>% 
     group_by(Country.Region) %>% 
     mutate(day = c(1:n())) %>% 
     ungroup() %>% 
     ggplot(aes(x = day, y = log(Count), color = Country.Region)) +
          geom_point() +
          geom_line() +  
          xlab('Days since case number exceeded 500') +
          ylab('log Case') +
          #coord_trans(y="log2") +
          theme_minimal()
     #     theme_fivethirtyeight()

## World Death
data = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')

data2 = data %>% 
        select(-c(Lat, Long)) %>% 
        group_by(Country.Region, Province.State) %>% 
        pivot_longer(-c(Country.Region, Province.State), names_to = "tempdate", values_to = "count") %>% 
        mutate(n = c(1:n())) %>% #, date =  as.Date(ifelse(n == 1, '2020-01-22', NA))) %>% 
        mutate(date = as.Date('2020-01-21') + n) %>% 
        ungroup() %>% 
        select(Country.Region, Province.State, date, count) %>% 
        group_by(Country.Region, date) %>% 
        summarize(Count = sum(count)) %>% 
        ungroup()

state.list2 = data2 %>% 
        group_by(Country.Region) %>% 
        mutate(MaxCase = max(Count)) %>% 
        ungroup() %>% 
        distinct(Country.Region, .keep_all = TRUE) %>% 
        filter(MaxCase > 100) %>% 
        select(Country.Region)

data2 %>% 
        group_by(Country.Region) %>% 
        arrange(desc(date)) %>% 
        slice(1) %>% 
        filter(log(Count) < 6.25, log(Count) > 5)

data2 %>% 
        filter(Country.Region %in% state.list2$Country.Region, Count > 50) %>% 
       # filter(Country.Region %in% c('Japan', 'Singapore', 'Sweden', 'Korea, South', 'Philippines', 'Malaysia', 'Qatar')) %>% 
        filter(Country.Region %in% c('Japan', 'Singapore', 'Pakistan', 'Korea, South', 'Dominican Republic','Israel', 'Norway')) %>% 
        group_by(Country.Region) %>% 
        mutate(day = c(1:n())) %>% 
        ungroup() %>% 
        ggplot(aes(x = day, y = log(Count), color = Country.Region)) +
        geom_point() +
        geom_line() +  
        xlab('Days since case number exceeded 50') +
        ylab('log Case') +
        #coord_trans(y="log2") +
        theme_minimal()
#     theme_fivethirtyeight()
