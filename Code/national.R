pacman::p_load(tidyverse, skimr, sf, ggthemes, patchwork, tidycensus)

#df = read.csv('us-states.csv', colClasses = c("fips" = "character", "date" = "Date")) 
df = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv', colClasses = c("fips" = "character", "date" = "Date"))

latest.df = df %>% 
       group_by(state) %>% 
       arrange(desc(date)) %>% 
       slice(1) %>% 
       ungroup() %>% 
       filter(cases > 30000) %>% 
       arrange(desc(cases))

slice(latest.df, 1:15)

df %>% 
       #  filter(fips == '06') %>% 
       filter(state %in% latest.df$state, date > as.Date("2020-02-25")) %>% 
       group_by(state) %>%  
       mutate(New.Case = cases - lag(cases), New.Death = deaths - lag(deaths)) %>%
       ungroup() %>% 
       filter(!is.na(New.Case)) %>% 
       ggplot(aes(x = date, y = New.Case)) +
       #  ggplot(aes(x = date, y = New.Death)) +
       geom_bar(stat = 'identity', fill = 'steelblue') +
       facet_wrap(~state, scales = 'free') +
       theme_minimal()

df %>%
       filter(state %in% latest.df$state, date > as.Date("2020-02-25")) %>% 
       #group_by(state) %>%
       #arrange(date) %>%
       #mutate(cum.cases = cumsum(cases)) %>%
       #ungroup() %>%
       #group_by(type, Province.State, date) %>%
       #summarize(cum.cases2 = sum(cum.cases)) %>%
       #ungroup() %>%
       ggplot(aes(x = date, y = (cases))) + #, color = type)) +
       geom_point() +
       geom_line() +
       facet_wrap(~state, scales = 'free') +
       theme_fivethirtyeight()

df.k = df %>% 
       #  filter(fips == '06') %>% 
#       filter(state %in% latest.df$state, date > as.Date("2020-02-25")) %>% 
       filter(date > as.Date("2020-02-25")) %>% 
       group_by(state) %>%  
       arrange(date) %>% 
       mutate(New.Case.1wk = cases - lag(cases,7), K = 1- New.Case.1wk/cases) %>% 
       ungroup()  %>% 
       {.} 

df.k %>% 
       arrange(state, date) %>% 
       group_by(state) %>% 
       slice(n()) %>% 
       arrange(K)

df %>% 
       group_by(state) %>%  
       arrange(date) %>% 
       mutate(New.Case.1wk = cases - lag(cases,7), K = 1- New.Case.1wk/cases) %>% 
       arrange(date) %>% 
       slice(n()) %>% 
       # print(n = 20) %>%
       ggplot(aes(x = log(cases), y = K, label = state)) +
       geom_point() +
       geom_text(hjust = 0, nudge_x = 0.05) +
       theme_bw()

Pop2010.state = get_decennial(geography = "state", variables = "P001001", year = 2010) %>% 
     #  filter(substr(GEOID, 1, 2) == '06') %>% 
       select(fips = GEOID, Population = value) %>% 
       arrange(fips)

df.state = df %>% 
       inner_join(Pop2010.state, by = 'fips') %>% 
       group_by(state) %>%  
       arrange(date) %>% 
       mutate(New.Case.1wk = cases - lag(cases,7), K = 1- New.Case.1wk/cases, 
              New.Death.1wk = deaths - lag(deaths, 7), K.death = 1 - New.Death.1wk/cases
       ) %>% 
       mutate(Prevalence.case = cases*10000/Population, 
              Prevalence.death = deaths*100000/Population) %>% 
       ungroup()  %>%
       mutate(K = ifelse(K>1, 1, K)) %>% 
       {.} 

df.state %>% 
       group_by(state) %>% 
       arrange(date) %>% 
       slice(n()) %>% 
       filter(cases>10) %>% 
       #       mutate(Prevalence = cases*10000/Population) %>% 
       ggplot(aes(x = Prevalence.case, y = K, label = state)) +
              geom_point() +
              geom_text(hjust = 0, nudge_x = 0.05) +
              ggtitle('COVID19 Case Prevalence (per 10,000) vs Index') +
              ylab('Index') +
              xlab('Prevalence') +
              theme_bw() +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5))

df.state %>% 
       group_by(state) %>% 
       arrange(date) %>% 
       slice(n()) %>% 
       filter(cases>10) %>% 
       #       mutate(Prevalence = cases*10000/Population) %>% 
       ggplot(aes(x = Prevalence.death, y = K.death, label = state)) +
              geom_point() +
              geom_text(hjust = 0, nudge_x = 0.05) +
              ggtitle('COVID19 Death Prevalence (per 100,000) vs Index') +
              ylab('Index') +
              xlab('Prevalence') +
              theme_bw() +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5))

df.state %>% 
       group_by(state) %>% 
       arrange(date) %>% 
       slice(n()) %>% 
       filter(cases>10) %>% 
       #       mutate(Prevalence = cases*10000/Population) %>% 
       ggplot(aes(x = Prevalence.case, y = Prevalence.death, label = state)) +
              geom_point() +
              geom_text(hjust = 0, nudge_x = 0.05) +
              ggtitle('COVID19 Case Prevalence (per 10,000) vs Death Prevalence (per 100,000) vs Index') +
              ylab('Prevalence Death') +
              xlab('Prevalence Case') +
              theme_bw() +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5))

# Case by County

#df2 = read.csv('us-counties.csv', colClasses = c("fips" = "character", "date" = "Date"))
df2 = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv', colClasses = c("fips" = "character", "date" = "Date"))

Bay.Area = c('041', '097', '055', '095', '013', '001', '083', '085', '081', '075', '037', '073')
#Bay.Area = c('013', '001',  '075')

df2 %>% 
       filter(state == 'California', date > as.Date("2020-02-25")) %>% 
       filter(substr(fips, 3, 5) %in% Bay.Area) %>% 
       #  filter(county == 'San Francisco') %>% 
       group_by(county) %>%  
       mutate(New.Case = cases - lag(cases)) %>% 
       filter(!is.na(New.Case)) %>% 
       ungroup() %>% 
       ggplot(aes(x = date, y = New.Case)) +
       #geom_line() +
       #geom_point() +
       geom_bar(stat = 'identity', fill = 'steelblue') +
       facet_wrap(~county, scales = 'free') +
       #theme_minimal() +
       geom_vline(xintercept = as.Date('2020-03-16')) + 
       geom_text(mapping = aes(label = 'Shelter in place declared', x = as.Date('2020-03-16'), y = -0.75), angle = 0, hjust = 0) +
       theme_fivethirtyeight()

df.k.state = df2 %>% 
       filter(substr(fips,1,2) == '06') %>% 
       filter(state %in% latest.df$state, date > as.Date("2020-02-25")) %>% 
       group_by(county) %>%  
       arrange(date) %>% 
       mutate(New.Case.1wk = cases - lag(cases,7), K = 1- New.Case.1wk/cases) %>% 
       ungroup()  %>% 
       {.} 

df.k.state %>% 
       group_by(county) %>% 
       arrange(date) %>% 
       slice(n()) %>% 
       filter(cases>10) %>% 
       ggplot(aes(x = log(cases), y = K, label = county)) +
       geom_point() +
       geom_text(hjust = 0, nudge_x = 0.05) +
       theme_bw()

df.k.state %>% 
       arrange(county, date) %>% 
       group_by(county) %>% 
       slice(n()) %>% 
       arrange(K)

Pop2010.county = get_decennial(geography = "county", variables = "P001001", year = 2010) %>% 
       #  filter(substr(GEOID, 1, 2) == '06') %>% 
       select(fips = GEOID, Population = value) %>% 
       arrange(fips)

df.county = df2 %>% 
       inner_join(Pop2010.county, by = 'fips') %>% 
       group_by(fips) %>%  
       arrange(date) %>% 
       mutate(New.Case.1wk = cases - lag(cases,7), K = 1- New.Case.1wk/cases, 
              New.Death.1wk = deaths - lag(deaths, 7), K.death = 1 - New.Death.1wk/cases
       ) %>% 
       mutate(Prevalence.case = cases*10000/Population, 
              Prevalence.death = deaths*100000/Population) %>% 
       ungroup()  %>%
       mutate(K = ifelse(K>1, 1, K)) %>% 
       {.} 
       
df.county %>% 
       group_by(county) %>% 
       arrange(date) %>% 
       slice(n()) %>% 
       mutate(Name = paste(county, state, sep = ',')) %>% 
       filter(K < 0.85, Population > 100000, cases>100, Prevalence.case>50) %>% 
       #       mutate(Prevalence = cases*10000/Population) %>% 
       ggplot(aes(x = Prevalence.case, y = K, label = Name)) +
              geom_point() +
              geom_text(hjust = 0, nudge_x = 0.05) +
              ggtitle('COVID19 Case Prevalence (per 10,000) vs Index') +
              ylab('Index') +
              xlab('Prevalence') +
              theme_bw() +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5))       

df.county %>% 
       group_by(county) %>% 
       arrange(date) %>% 
       slice(n()) %>% 
       filter(cases>10) %>%
       mutate(Name = paste(county, state, sep = ',')) %>% 
       filter(Population > 100000, deaths > 100, Prevalence.death > 20) %>% 
       ggplot(aes(x = Prevalence.death, y = K.death, label = Name)) +
              geom_point() +
              geom_text(hjust = 0, nudge_x = 0.05) +
              ggtitle('COVID19 Death Prevalence (per 100,000) vs Index') +
              ylab('Index') +
              xlab('Prevalence') +
              theme_bw() +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5))

#df2 %>% 
#  filter(state == 'California',date == as.Date("2020-03-30")) %>% 
#  filter(county == 'Santa Clara') %>% 
#  arrange(-cases)
