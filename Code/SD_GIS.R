pacman::p_load(tidyverse, pdftools, geojsonio, tidycensus, tmap, nord)
path = ("E:\\Research\\COVID19World\\Data\\SDData.pdf")
#res.info = pdf_info(pdf = path)

df = pdf_data(path)[[1]]

zip = filter(df, width %in% c(19), height == 10)
rate = filter(df, width %in% c(7, 17), height == 10)


PDF <- pdf_text(path) %>%
     readr::read_lines()
PDF2 <-PDF[-c(1:6,57:64)] # remove lines

PDF3 = PDF2 %>%
     str_squish() %>%
     strsplit(split = " ")# remove empty spaces
#PDF3[[48]] <- c("92065", "17", "47.7", "Unknown***", "66",  "**")
PDF3[[49]] <- c("92066", "2", "**",  "San Diego County",  "3,711", "111.2")
PDF3[[50]] <- c("92067", "11", "**" , "Unknown", ".", ".")

temp = plyr::ldply(PDF3) #create a data frame
temp.1 = select(temp, V1:V3) %>% 
     rename(FIPS = V1, Count = V2, Rate = V3)
temp.2 = select(temp, V4:V6) %>% 
     rename(FIPS = V4, Count = V5, Rate = V6)
df = rbind(temp.1, temp.2) %>% 
          filter(Rate != '.', FIPS!= 'San Diego County', FIPS!='Unknown***') %>% 
          mutate(Count = as.numeric(Count), Rate = as.numeric(Rate))
 
nem = get_acs(
     geography = "zip code tabulation area",
     variables = c("B01001B_001", "B01001I_001", "B01003_001", "B14005_001"),
     year = 2016, geometry = TRUE
)

df2 = inner_join(nem, df, by = c("GEOID" = "FIPS")) %>% 
     filter(!is.na(Rate))

tm_shape(df2) +
     tm_polygons('Rate' , n = 4)


tm_shape(df2) +
     tm_polygons('Count' , n = 3)

ggplot(data = df2) +
     geom_sf(aes(fill = Rate)) +
 #    scale_fill_nord('halifax_harbor') +
     theme_minimal()