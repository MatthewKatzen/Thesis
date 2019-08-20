library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)

### GET DATA
mpa <- fread("D:/Thesis/Data/COMPLETE/mpacomplete.csv", stringsAsFactors = FALSE) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))


### REVENUE

mpa <- mpa %>% mutate(Rev_RRP = RRP*TOTALCLEARED) %>% 
    mutate(Rev_LMP = LMP*TOTALCLEARED) %>% 
    mutate(Rev_DIF = Rev_LMP - Rev_RRP)#how much you benefit from change to LMP system

#how much would system save by changing?
mpa %>% group_by(YEAR = year(SETTLEMENTDATE)) %>% 
    summarise(SUM = sum(Rev_DIF))

sum(mpa$Rev_DIF) #65 billion

sum(mpa18$Rev_DIF)/sum(mpa18$Rev_DIF != 0) 

sum(mpa18$Rev_DIF <= 0)
sum(mpa18$Rev_DIF >= 0)



### WHO WINS AND LOSES?


mpa %>% group_by(DUID) %>% summarise(RRPsum = sum(Rev_RRP), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT, 
                                       REGIONID = REGIONID[1], Participant = Participant[1], 
                                       Fuel.Type = Fuel.Type[1]) %>% 
    arrange(-DIFFsum)#winners in change to LMP

mpa18 %>% group_by(DUID) %>% summarise(RRPsum = sum(Rev_RRP), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT, 
                                       REGIONID = REGIONID[1], Participant = Participant[1], 
                                       Fuel.Type = Fuel.Type[1]) %>% 
    arrange(DIFFsum)#losers in change to LMP

#losers make sense as they do not strategically bid (always bidding at floor and hence adding to congestion)

#loser w/o wind
mpa18 %>% filter(Fuel.Type != "Wind") %>% 
    group_by(DUID) %>% summarise(RRPsum = sum(Rev_RRP), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT, 
                                       REGIONID = REGIONID[1], Participant = Participant[1], 
                                       Fuel.Type = Fuel.Type[1]) %>% 
    arrange(DIFFsum)#losers 

### FUEL
mpa %>% group_by(Fuel.Type) %>% summarise(RRPsum = sum(Rev_RRP), LMPsum = sum(Rev_LMP),
                                       DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT) %>% 
    arrange(-DIFFsum)#winners in change to LMP



#how many pos and neg DIFFsums
mpa18 %>% group_by(Fuel.Type) %>% 
    summarise(SUM_POS = sum(Rev_DIF[Rev_DIF>0]), 
              SUM_NEG = sum(Rev_DIF[Rev_DIF<0]),
              COUNT_POS = sum(Rev_DIF>0),
              COUNT_NEG = sum(Rev_DIF<0))


#over time
mpa18 %>% group_by(MONTH = month(SETTLEMENTDATE)) %>% 
    summarise(RRPsum = sum(Rev_RRP), LMPsum = sum(Rev_LMP),
              DIFFsum = sum(Rev_DIF), COUNT = sum(Rev_DIF != 0), MEAN = DIFFsum/COUNT) %>% 
    arrange(MONTH)#winners in change to LMP

