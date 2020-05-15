pacman::p_load(tidyverse, lubridate, here, ggrepel, rvest)


url_google <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
# Attribution - Google (no equivalent for Apple apparent): 
#  "If you publish results based on this data set, please cite as:
#  Google LLC "Google COVID-19 Community Mobility Reports."
#  https://www.google.com/covid19/mobility/ Accessed: <Date>."
# read data from google
#  set 'location' to country name is missing
df_google <- 
       read_csv(url_google) %>%
       select(-sub_region_2) %>% 
       rename_at(vars(ends_with("_percent_change_from_baseline")), 
                 funs(str_replace(., "_percent_change_from_baseline", ""))) %>%
       rename(location = sub_region_1, 
              country = country_region,
              Retail_Recreation = retail_and_recreation, 
              Grocery_Pharmacy = grocery_and_pharmacy,
              Parks = parks, 
              Transit_Stations = transit_stations, 
              Workplaces = workplaces,
              Residential = residential) %>%
       mutate(location = case_when(is.na(location) ~ country, 
                                   TRUE ~ location)) %>%
       mutate(location = str_replace_all(location, " ", "_"), 
              country = str_replace_all(country, " ", "_")) %>%
       gather(key = "category", value = "level", 
              -c("country_region_code", "country", "location", "date")) 

df.google.jp = filter(df_google, country == 'Japan')
df.google.us = filter(df_google, country == 'United_States')

big.ken = c('Tokyo', 'Osaka', 'Fukuoka', 'Aichi', 'Hokkaido')
df.google.jp %>% 
       filter(location %in% big.ken) %>% 
       ggplot(aes(x = date, y = level, color = location)) +
              #geom_line(lwd = 1.3) +
              #geom_point() + 
              geom_smooth(span = 0.15, se = FALSE, lwd = 1.23) +
              facet_wrap(~category, scale = 'free') +
              geom_hline(yintercept = 0, colour = "darkgrey") +
              theme_bw()

df.google.us %>% 
       filter(location == 'Florida') %>% 
       group_by(date, category) %>% 
       summarize(level = mean(level, na.rm = TRUE)) %>% 
       ggplot(aes(x = date, y = level)) +
              geom_line() +
              #geom_point() + geom_smooth() +
              facet_wrap(~category) +
              geom_hline(yintercept = 0, colour = "darkgrey") +
              theme_bw()

big.state = c('California', 'Texas', 'Florida', 'New_York', 'Illinois')

df.google.us %>% 
       filter(location %in% big.state) %>% 
       #filter(location == 'California') %>% 
       group_by(date, location, category) %>% 
       summarize(level = mean(level, na.rm = TRUE)) %>% 
       ggplot(aes(x = date, y = level, color = location)) +
       #       geom_line(lwd = 1.32) +
              geom_smooth(span = 0.15, se = FALSE, lwd = 1.23) +
              facet_wrap(~category, scales = "free") +
              geom_hline(yintercept = 0, colour = "darkgrey") +
              theme_bw()
