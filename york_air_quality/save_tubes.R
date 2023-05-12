AQ_monitoring <- read.csv('AQ_Monitoring.csv')
tubes_Readings <- read.csv('york_tubesreadings.csv')
library(tidyverse)
locations <- AQ_monitoring %>% 
  select(X,Y,Site_ID) %>% 
  separate(Site_ID,into = c('t0','SiteId'),sep = '_') %>% 
  select(-t0)

# saves only the data points for 2020.
tubes <- tubes_Readings %>% 
  filter(CalendarYear == 2020) %>% 
  select(-BiasCorrectedAv.ppb.,-BiasCorrectedAv.ug.m3.) %>% 
  gather(-(1:3),key = 'month',value = 'value') %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value)) %>% 
  inner_join(locations) 
save(tubes,file = 'tubes.RData')