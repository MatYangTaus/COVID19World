pacman::p_load(tidyverse, skimr, lubridate)

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

test = df %>% 
     filter(state %in% state.list$state, positive > 500) %>% 
     group_by(state) %>% 
     mutate(day = c(1:n())) %>% 
     ungroup() %>% 
     ggplot(aes(x = day, y = log(positive), color = state)) +
          geom_point() +
          geom_line() +
          coord_trans(y="log2") +
          theme_minimal()