#MPA analysis for paper
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(padr)


### Get DATA

mpa <- fread("D:/Thesis/Data/mpa_cleaned.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))

### Congestion
congested <- mpa %>% select(settlementdate) %>% unique() %>% mutate(constrained = 1) %>% 
    pad(interval = "5 min", break_above = 2000000, start_val = ymd_hms("2009-07-01 00:05:00"), 
        end_val = "2019-07-01 00:00:00") %>% replace(is.na(.), 0)#add 0 if not congested

#by year table
congested %>% group_by(year = floor_date(settlementdate, "year")) %>% 
    summarise(perc = sum(constrained)/n()) %>% fwrite("Output/congestion by year.csv")

#graph by month, grouped by state
congested_state <- mpa %>% select(settlementdate, regionid) %>% unique() %>% mutate(constrained = 1) %>% 
    pad(interval = "5 min", start_val = ymd_hms("2009-07-01 00:05:00"), 
        end_val = ymd_hms("2019-07-01 00:00:00"), break_above = 10000000, group = "regionid") %>% 
    replace(is.na(.), 0)#add 0 if not congested

congested_state %>% group_by(year = floor_date(settlementdate, "year"), regionid) %>% 
    summarise(perc = sum(constrained)/n()) %>% 
    ggplot(aes(x = year, y = perc, colour = regionid, group = regionid))+
    geom_line(size = 2)+
    facet_wrap(~regionid) +
    labs(title = "Proportion of time state is congested", y = "Proportion", x = "Year")+
    ylim(0,0.5) +
    ggsave("Output/Congested_state.png")
    
    
