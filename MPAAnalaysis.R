### GET DATA
mpa18 <- fread("D:/Thesis/Data/mpa18.csv", stringsAsFactors = FALSE) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))


### REVENUE

mpa18 <- mpa18 %>% mutate(Rev_RRP = RRP*TOTALCLEARED) %>% 
    mutate(Rev_LMP = LMP*TOTALCLEARED) %>% 
    mutate(Rev_DIF = Rev_RRP - Rev_LMP)#how much you benefit from current system


### OLD ANALYSIS


mpa %>% group_by(DUID) %>% summarise(SUM = sum(PRICE), COUNT = n(), MEAN = SUM/COUNT) %>% arrange(SUM)#winners 
mpa %>% group_by(DUID) %>% summarise(SUM = sum(PRICE), COUNT = n(), MEAN = SUM/COUNT) %>% arrange(-SUM)#losers


#by fuel
fuel_year <- mpa_comb %>% group_by(Fuel.Type, YEAR = year(SETTLEMENTDATE))  %>% 
    summarise(SUM = sum(PRICE), 
              COUNT = n(), MEAN = SUM/COUNT) %>% arrange(Fuel.Type, YEAR)

mpa_comb %>% group_by(Fuel.Type) %>% 
    summarise(SUM_POS = sum(PRICE[PRICE>0]), 
              SUM_NEG = sum(PRICE[PRICE<0]),
              COUNT_POS = sum(PRICE>0),
              COUNT_NEG = sum(PRICE<0))


mpa %>% group_by(DUID) %>% 
    summarise(SUM_POS = sum(PRICE[PRICE>0]), 
              SUM_NEG = sum(PRICE[PRICE<0]),
              COUNT_POS = sum(PRICE>0),
              COUNT_NEG = sum(PRICE<0)) %>% 
    merge(fuel, by = "DUID") %>% arrange(-SUM_POS) %>% as_tibble()#losers in current system

mpa %>% group_by(DUID) %>% 
    summarise(SUM_POS = sum(PRICE[PRICE>0]), 
              SUM_NEG = sum(PRICE[PRICE<0]),
              COUNT_POS = sum(PRICE>0),
              COUNT_NEG = sum(PRICE<0)) %>% 
    merge(fuel, by = "DUID") %>% arrange(SUM_NEG) %>% as_tibble()#winners 


