library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)

### GET DATA
mpa <- fread("D:/Thesis/Data/COMPLETE/mpacomplete.csv", stringsAsFactors = FALSE) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))


### REVENUE

mpa <- mpa %>% mutate(Rev_RRP_5 = RRP*TOTALCLEARED) %>% 
    mutate(Rev_RRP_30 = RRP30*TOTALCLEARED) %>% 
    mutate(Rev_LMP = LMP*TOTALCLEARED) %>% 
    mutate(Rev_DIF = Rev_LMP - Rev_RRP_30)  #how much you benefit from change to LMP system

head(mpa)


### How much would system save by changing?
sum(mpa$Rev_DIF) #60 billion over 10 years


mpa %>% group_by(YEAR = year(SETTLEMENTDATE)) %>% 
    summarise(RRP30sum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
              DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT) %>% 
    arrange(YEAR)#by year


### WHO WINS AND LOSES?
mpa %>% group_by(DUID) %>% summarise(RRP30sum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT, 
                                       REGIONID = REGIONID[1], Participant = Participant[1], 
                                       Fuel.Type = Fuel.Type[1]) %>% 
    arrange(-DIFFsum)#winners in change to LMP

mpa %>% group_by(DUID) %>% summarise(RRPsum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT, 
                                       REGIONID = REGIONID[1], Participant = Participant[1], 
                                       Fuel.Type = Fuel.Type[1]) %>% 
    arrange(DIFFsum)#losers in change to LMP

#losers make sense as they do not strategically bid (always bidding at floor and hence adding to congestion)

#loser w/o wind
mpa %>% filter(Fuel.Type != "Wind") %>% 
    group_by(DUID) %>% summarise(RRPsum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT, 
                                       REGIONID = REGIONID[1], Participant = Participant[1], 
                                       Fuel.Type = Fuel.Type[1]) %>% 
    arrange(DIFFsum)#losers 

### FUEL
mpa %>% group_by(Fuel.Type) %>% summarise(RRPsum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT) %>% 
    arrange(-DIFFsum)#winners in change to LMP

#fuel by year excl 09 and 19
mpa %>% group_by(Fuel.Type, YEAR = year(SETTLEMENTDATE)) %>% summarise(DIFFsum = sum(Rev_DIF)) %>% 
    filter(!(YEAR %in% c(2009, 2019))) %>% 
    ggplot(aes(x = YEAR, y = DIFFsum, colour = Fuel.Type))+
    geom_line(size = 2)

#fuel by month
mpa %>% group_by(Fuel.Type, MONTH = floor_date(SETTLEMENTDATE, "month")) %>% 
    summarise(DIFFsum = sum(Rev_DIF)) %>% 
    ggplot(aes(x = MONTH, y = DIFFsum, colour = Fuel.Type))+
    geom_line(size = 2)


###STATE
mpa %>% group_by(MONTH = floor_date(SETTLEMENTDATE, "month"), REGIONID) %>% 
    summarise(DIFFsum = sum(Rev_DIF)) %>% 
    ggplot(aes(x = MONTH, y = DIFFsum, colour = REGIONID))+
    geom_line(size = 2)

###FUEL AND STATE
mpa %>% group_by(MONTH = floor_date(SETTLEMENTDATE, "month"), REGIONID, Fuel.Type) %>% 
    summarise(DIFFsum = sum(Rev_DIF)) %>% 
    ggplot(aes(x = MONTH, y = DIFFsum, colour = Fuel.Type)) +
    geom_line(size = 2) +
    facet_wrap(~ REGIONID)

#how many pos and neg DIFFsums
mpa %>% group_by(Fuel.Type) %>% 
    summarise(SUM_POS = sum(Rev_DIF[Rev_DIF>0]), 
              SUM_NEG = sum(Rev_DIF[Rev_DIF<0]),
              COUNT_POS = sum(Rev_DIF>0),
              COUNT_NEG = sum(Rev_DIF<0))




