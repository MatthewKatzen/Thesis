library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
#Create MPA graphs, grouped by DUID, coloured by AGE


mpa <- fread("D:/Thesis/Data/mpa_cleaned.csv") %>% 
    mutate(settlementdate = ymd_hms(settlementdate))


#TOTAL LMP0 WIND DUID MONTH
mpa %>% filter(fuel_type == "Wind") %>% 
    group_by(month = floor_date(settlementdate, "month"), duid) %>% 
    summarise(dif_total_0 = sum(rev_dif_0), age = age[1]) %>% 
    ungroup() %>% 
    ggplot(aes(x = month, y = dif_total_0, group = duid, colour = age)) +
    geom_line(size = 1.5)+
    scale_color_gradient(low = "blue", high = "red") +
    ggtitle("WIND ONLY - Revenue Change in swith to LMP - Assume LMP cannot be Neg - Each individual Wind farm coloured by year of commission") +
    xlab("") +
    ylab("Rev Change")

#TOTAL LMP WIND DUID MONTH
mpa %>% filter(Fuel.Type == "Wind") %>% 
    group_by(YEAR = floor_date(SETTLEMENTDATE, "year"), DUID) %>% 
    summarise(Dif_Total = sum(Ave), Start = year(Start[1])) %>% 
    ungroup() %>% 
    ggplot(aes(x = YEAR, y = Dif_Total, group = DUID, colour = Start)) +
    geom_line(size = 1.5) +
    scale_color_gradient(low = "blue", high = "red")+
    ggtitle("WIND ONLY - Revenue Change in swith to LMP - Each individual Wind farm coloured by year of commission")+
    xlab("") +
    ylab("Rev Change")

### AVE REV, START + FUEL TYPE

#by YEAR, Grouped FUEL/START
mpa_year_fuel_start <- mpa %>% 
    group_by(YEAR = floor_date(SETTLEMENTDATE, "year"),  START = year(Start), Fuel.Type) %>% 
    summ_all()


mpa_year_fuel_start %>% filter(Fuel.Type =="Wind") %>% 
    filter(YEAR > ymd_hms("2013-01-01 00:00:00")) %>% 
    ggplot(aes(x = YEAR, y = Dif_Ave_0, group = START, colour = START)) + 
    geom_line(size = 2) +
    scale_color_gradient(low = "blue", high = "red") +
    facet_wrap(~ Fuel.Type)


temp <- mpa %>% filter(Fuel.Type == "Wind") %>% mutate(Year = year(Start)) %>% select(DUID, Year) 

