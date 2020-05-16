pacman::p_load(tidyverse, skimr, lubridate, ggthemes, nord)

#json_file <- "https://covidtracking.com/api/v1/states/current.json"
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))
#temp <- fromJSON(file=json_file)

#df = read.csv('https://covidtracking.com/api/v1/states/current.csv')
df = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv') %>% 
     arrange(state, date) %>% 
        group_by(state) %>% 
    # select(-c(hash, dateChecked)) %>% 
     mutate(fips = str_sub(paste0('0', fips), -2, -1), date = ymd(date), positiveIncrease = cases - lag(cases), Maxcase = max(cases)) %>% 
        ungroup()
#df3 = read.csv('https://covidtracking.com/api/us/daily.csv')

state.list0 = df %>% 
     filter(!is.na(positiveIncrease), Maxcase > 10000) 

state.list0 %>% 
     ggplot(aes(x = date, y = positiveIncrease)) +
             geom_bar(stat = 'identity', fill = 'steelblue') +
             facet_wrap(~ state, scales = 'free') +
             theme_minimal()

df %>% 
  #   filter(state %in% state.list$state, positive > 500) %>% 
     filter(fips %in% unique(state.list0$fips), (cases)> 50) %>%
        arrange(state,date) %>% 
     group_by(state) %>% 
     mutate(day = c(1:n())) %>% 
     ungroup() %>% 
     ggplot(aes(x = day, y = log(cases), color = state)) +
          geom_point() +
          geom_line() +
          #coord_trans(y="log2") +
          theme_fivethirtyeight()


## World
data = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')

pop = read.csv('https://raw.githubusercontent.com/datasets/population/master/data/population.csv') %>% 
    filter(Year == 2018) %>% 
    rename(Country.Region = Country.Name) %>% 
    mutate(Country.Region = replace(Country.Region, Country.Region =='Iran, Islamic Rep.', 'Iran'),
           Country.Region = replace(Country.Region, Country.Region =='Bahamas, The', 'Bahamas'),
           Country.Region = replace(Country.Region, Country.Region =='Brunei Darussalam', 'Brunei'),
           Country.Region = replace(Country.Region, Country.Region =='Myanmar', 'Burma'),
           Country.Region = replace(Country.Region, Country.Region =='Congo, Dem. Rep.', 'Congo (Brazzaville)'),
           Country.Region = replace(Country.Region, Country.Region =='Czech Republic', 'Czechia'),
           Country.Region = replace(Country.Region, Country.Region =='Egypt, Arab Rep.', 'Egypt'),                        Country.Region = replace(Country.Region, Country.Region =='Gambia, The', 'Gambia'),  
           Country.Region = replace(Country.Region, Country.Region =='Korea, Rep.', 'Korea, South'),  
           Country.Region = replace(Country.Region, Country.Region =='Kyrgyz Republic', 'Kyrgyzstan'),  
           Country.Region = replace(Country.Region, Country.Region =='Lao PDR', 'Laos'),  
           Country.Region = replace(Country.Region, Country.Region =='Russian Federation', 'Russia'),  
           Country.Region = replace(Country.Region, Country.Region =='Slovak Republic', 'Slovakia'),  
           Country.Region = replace(Country.Region, Country.Region =='Syrian Arab Republic', 'Syria'),  
           Country.Region = replace(Country.Region, Country.Region =='United States', 'US'),  
           Country.Region = replace(Country.Region, Country.Region =='Venezuela, RB', 'Venezuela'),  
           Country.Region = replace(Country.Region, Country.Region =='Yemen, Rep.', 'Yemen')
    )

data2 = data %>% 
     select(-c(Lat, Long)) %>% 
     group_by(Country.Region, Province.State) %>% 
     pivot_longer(-c(Country.Region, Province.State), names_to = "tempdate", values_to = "count") %>% 
     mutate(n = c(1:n())) %>% 
     mutate(date = as.Date('2020-01-21') + n) %>% 
     ungroup() %>% 
     select(Country.Region, Province.State, date, count) %>% 
     group_by(Country.Region, date) %>% 
     summarize(Count = sum(count)) %>% 
     ungroup() %>% 
     left_join(pop, by = 'Country.Region')
  
 
data2 %>% 
     group_by(Country.Region) %>%  
     arrange(date) %>% 
     mutate(New.Case.1wk = Count - lag(Count,7), K = 1- New.Case.1wk/Count) %>% 
     arrange(date) %>% 
     slice(n()) %>% 
     mutate(Rate = Count*10000/Value) %>% 
     filter(Count> 2000) %>% 
     arrange(K) %>% 
    # print(n = 20) %>%
     ggplot(aes(x = log(Count), y = K, label = Country.Region)) +
        geom_point() +
        geom_text(hjust = 0, nudge_x = 0.05) +
        theme_bw()

data2 %>% 
    group_by(Country.Region) %>%  
    arrange(date) %>% 
    mutate(New.Case.1wk = Count - lag(Count,7), K = 1- New.Case.1wk/Count) %>% 
    arrange(date) %>% 
    slice(n()) %>% 
    mutate(Rate = Count*10000/Value) %>% 
    filter(Count> 2000) %>% 
    arrange(K) %>% 
    # print(n = 20) %>%
    ggplot(aes(x = Rate, y = K, label = Country.Region)) +
    geom_point() +
    geom_text(hjust = 0, nudge_x = 0.05) +
    theme_bw()

data2 %>% 
    group_by(Country.Region) %>%  
    arrange(date) %>% 
    mutate(New.Case.1wk = Count - lag(Count,7), K = 1- New.Case.1wk/Count, Rate = Count*10000/Value, Max = max(Count)) %>% 
    arrange(Country.Region,date) %>% 
    filter(!is.na(K), Max>20000) %>% 
    ggplot(aes(x = Rate, y = K, color = Country.Region)) +
       geom_line() +
       theme_bw()
  
state.list2 = data2 %>% 
     group_by(Country.Region) %>% 
     mutate(MaxCase = max(Count)) %>% 
     ungroup() %>% 
     distinct(Country.Region, .keep_all = TRUE) %>% 
     filter(MaxCase > 5000) %>% 
     select(Country.Region)

(state.list3 = data2 %>% 
     group_by(Country.Region) %>% 
     arrange(desc(date)) %>% 
     slice(1) %>% 
#     filter(Count > 100000, Count < 200000) %>% 
     filter((Count) < 26000, (Count) > 15000) %>% 
    {.})

data2 %>% 
 #    filter(Country.Region %in% state.list2$Country.Region, Count > 500) %>% 
     filter(Country.Region %in% unique(state.list3$Country.Region), Count>500) %>% 
     #filter(Country.Region %in% c('Japan', 'Singapore', 'Sweden', 'Korea, South', 'Philippines', 'Malaysia', 'Qatar'), Count>500) %>% 
     group_by(Country.Region) %>% 
     mutate(day = c(1:n())) %>% 
     ungroup() %>% 
     ggplot(aes(x = day, y = (Count), col = Country.Region)) +
          geom_point() +
          geom_line() +  
        #  scale_color_nord("victory_bonds") +
          xlab('Days since case number exceeded 500') +
          ylab('log Case') +
          #coord_trans(y="log2") +
          theme_minimal() +
          theme_fivethirtyeight()

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

(state.list4 = data2 %>% 
        group_by(Country.Region) %>% 
        arrange(desc(date)) %>% 
        slice(1) %>% 
        filter((Count) < 700, (Count) > 400))

data2 %>% 
        filter(Country.Region %in% state.list4$Country.Region, Count > 50) %>% 
       # filter(Country.Region %in% c('Japan', 'Singapore', 'Sweden', 'Korea, South', 'Philippines', 'Malaysia', 'Qatar')) %>% 
       # filter(Country.Region %in% c('Japan', 'Singapore', 'Pakistan', 'Korea, South', 'Dominican Republic','Israel', 'Norway')) %>% 
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
