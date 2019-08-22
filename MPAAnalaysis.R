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
    mutate(Rev_DIF_30_LMP = Rev_LMP - Rev_RRP_30) %>% #how much you benefit from change to LMP system
    mutate(Rev_DIF_30_5 = Rev_RRP_5 - Rev_RRP_30) #how much you benefit from change to 5 min system

head(mpa)


### How much would system save by changing?
sum(mpa$Rev_DIF_30_LMP) #60 billion over 10 years

sum(mpa$Rev_DIF_30_5) #5 billion more over 10 years (only for congested gens) IGNORE



mpa %>% group_by(YEAR = year(SETTLEMENTDATE)) %>% 
    summarise(RRP30sum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
              DIFFsum = sum(Rev_DIF_30_LMP), COUNT = sum(Rev_DIF_30_LMP != 0), MEAN = DIFFsum/COUNT) %>% 
    arrange(YEAR)#by year


### WHO WINS AND LOSES?
mpa %>% group_by(DUID) %>% summarise(RRP30sum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF_30_LMP), COUNT = sum(Rev_DIF_30_LMP != 0), MEAN = DIFFsum/COUNT, 
                                       REGIONID = REGIONID[1], Participant = Participant[1], 
                                       Fuel.Type = Fuel.Type[1]) %>% 
    arrange(-DIFFsum)#winners in change to LMP

mpa %>% group_by(DUID) %>% summarise(RRPsum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF_30_LMP), COUNT = sum(Rev_DIF_30_LMP != 0), MEAN = DIFFsum/COUNT, 
                                       REGIONID = REGIONID[1], Participant = Participant[1], 
                                       Fuel.Type = Fuel.Type[1]) %>% 
    arrange(DIFFsum)#losers in change to LMP

#losers make sense as they do not strategically bid (always bidding at floor and hence adding to congestion)

#loser w/o wind
mpa %>% filter(Fuel.Type != "Wind") %>% 
    group_by(DUID) %>% summarise(RRPsum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF_30_LMP), COUNT = sum(Rev_DIF_30_LMP != 0), MEAN = DIFFsum/COUNT, 
                                       REGIONID = REGIONID[1], Participant = Participant[1], 
                                       Fuel.Type = Fuel.Type[1]) %>% 
    arrange(DIFFsum)#losers 

### FUEL
mpa %>% group_by(Fuel.Type) %>% summarise(RRPsum = sum(Rev_RRP_30), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF_30_LMP), COUNT = sum(Rev_DIF_30_LMP != 0), MEAN = DIFFsum/COUNT) %>% 
    arrange(-DIFFsum)#winners in change to LMP



#how many pos and neg DIFFsums
mpa %>% group_by(Fuel.Type) %>% 
    summarise(SUM_POS = sum(Rev_DIF_30_LMP[Rev_DIF_30_LMP>0]), 
              SUM_NEG = sum(Rev_DIF_30_LMP[Rev_DIF_30_LMP<0]),
              COUNT_POS = sum(Rev_DIF_30_LMP>0),
              COUNT_NEG = sum(Rev_DIF_30_LMP<0))




