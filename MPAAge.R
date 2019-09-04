library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)

mpa <- fread("D:/Thesis/Data/COMPLETE/mpa_age.csv") %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))

mpa %>% filter(Fuel.Type == "Wind") %>% select(DUID, Start) %>% unique() %>% dim()

#Individual Wind Zero LMP
mpa %>% filter(Fuel.Type == "Wind") %>% 
    group_by(MONTH = floor_date(SETTLEMENTDATE, "month"), DUID) %>% 
    summarise(DIFFsum = sum(Rev_DIF_0), Start = year(Start[1])) %>% 
    ungroup() %>% 
    #pad(group = c("DUID")) %>% replace_na(list(DIFFsum = 0)) %>%  #add missing dates
    ggplot(aes(x = MONTH, y = DIFFsum, group = DUID, colour = Start)) +
    geom_line(size = 1.5)+
    scale_color_gradient(low = "blue", high = "red")+
    ggtitle("WIND ONLY - Revenue Change in swith to LMP - Assume LMP cannot be Neg - Each individual Wind farm coloured by year of commission")+
    xlab("") +
    ylab("Rev Change")

#Individual Wind True LMP
mpa %>% filter(Fuel.Type == "Wind") %>% 
    group_by(MONTH = floor_date(SETTLEMENTDATE, "year"), DUID) %>% 
    summarise(DIFFsum = sum(Rev_DIF), Start = year(Start[1])) %>% 
    ungroup() %>% 
    ggplot(aes(x = MONTH, y = DIFFsum, group = DUID, colour = Start)) +
    geom_line(size = 1.5) +
    scale_color_gradient(low = "blue", high = "red")+
    ggtitle("WIND ONLY - Revenue Change in swith to LMP - Each individual Wind farm coloured by year of commission")+
    xlab("") +
    ylab("Rev Change")

### Ave Rev at YEAR

mpa <- fread("D:/Thesis/Data/COMPLETE/mpa_age.csv") %>% mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))

#AVE REV of production (LMP0)
mpa_ave_fuel <- mpa %>% 
    group_by(YEAR = floor_date(SETTLEMENTDATE, "year"), Fuel.Type) %>% 
    summarise(Ave_Rev = ifelse(sum(TOTALCLEARED)>0,
                               sum(Rev_RRP_30)/sum(TOTALCLEARED),
                               NA),
              Ave_Rev_LMP = ifelse(sum(TOTALCLEARED)>0,
                                   sum(Rev_LMP)/sum(TOTALCLEARED),
                                   NA),
              Dif = Ave_Rev_LMP - Ave_Rev)

mpa_ave_fuel %>% 
    ggplot(aes(x = YEAR, y = Dif, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    #scale_color_gradient(low = "blue", high = "red")+
    ggtitle("Average Revenue change in swith to LMP")

#AVE Rev by year of production
temp2 <- mpa %>% 
    group_by(YEAR = floor_date(SETTLEMENTDATE, "year"),  START = year(Start), Fuel.Type) %>% 
    summarise(Ave_Rev = ifelse(sum(TOTALCLEARED)>0,
                               sum(Rev_RRP_30)/sum(TOTALCLEARED),
                               NA),
              Ave_Rev_LMP = ifelse(sum(TOTALCLEARED)>0,
                                   sum(Rev_LMP0)/sum(TOTALCLEARED),
                                   NA),
              Dif = Ave_Rev_LMP - Ave_Rev)

temp2 %>% 
    ggplot(aes(x = YEAR, y = Dif, group = START, colour = START)) + 
    geom_line(size = 2) +
    scale_color_gradient(low = "blue", high = "red") +
    facet_wrap(~ Fuel.Type) +
    ggtitle("Average Revenue change in swith to LMP - Assuming No Neg LMP")



temp <- mpa %>% filter(Fuel.Type == "Wind") %>% mutate(Year = year(Start)) %>% select(DUID, Year) 

