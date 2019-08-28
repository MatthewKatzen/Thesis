library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
#DIFFSUM v MONTH, grouped by Fuel, facet by State
mpa %>% group_by(MONTH = floor_date(SETTLEMENTDATE, "month"), REGIONID, Fuel.Type) %>% 
    summarise(DIFFsum = sum(Rev_DIF)) %>% 
    ungroup() %>% 
    pad(group = c("REGIONID", "Fuel.Type")) %>% replace_na(list(DIFFsum = 0)) %>%  #add missing dates
    ggplot(aes(x = MONTH, y = DIFFsum, colour = Fuel.Type)) +
    geom_line(size = 2) +
    facet_wrap(~ REGIONID) 

#no wind
mpa %>% group_by(MONTH = floor_date(SETTLEMENTDATE, "month"), REGIONID, Fuel.Type) %>% 
    summarise(DIFFsum = sum(Rev_DIF)) %>% 
    filter(Fuel.Type != "Wind", year(MONTH) > 2012) %>% #no wind, after 2012
    ungroup() %>% 
    pad(group = c("REGIONID", "Fuel.Type")) %>% replace_na(list(DIFFsum = 0)) %>%  #add missing dates
    ggplot(aes(x = MONTH, y = DIFFsum, colour = Fuel.Type)) +
    geom_line(size = 2) +
    facet_wrap(~ REGIONID)


#Clearly something going on in SA wind
#which makes sense, lots of wind bidding at floor price and congesting network

mpa %>% filter(REGIONID == "SA1", Fuel.Type == "Wind", year(SETTLEMENTDATE) == "2018") %>% 
    group_by(DUID, MONTH = floor_date(SETTLEMENTDATE, "day")) %>% 
    summarise(SUMdiff = sum(Rev_DIF)) %>% 
    arrange(SUMdiff)

#Non wind (only strategic bidders), wind isn't necessarily a problem as their MC = 0
mpa %>% filter(Fuel.Type != "Wind", year(SETTLEMENTDATE) > 2012) %>%
    group_by(DUID, MONTH = floor_date(SETTLEMENTDATE, "day")) %>% 
    summarise(SUMdiff = sum(Rev_DIF), REGIONID = REGIONID[1], Fuel.Type = Fuel.Type[1]) %>% 
    arrange(SUMdiff) %>% .[1:20,]

#Black coal QLD pushing out more efficient generators
mpa %>% filter(Fuel.Type == "Black Coal", year(SETTLEMENTDATE) > 2017, REGIONID == "QLD1") %>%
    group_by(DUID, DAY = floor_date(SETTLEMENTDATE, "day")) %>% 
    summarise(SUMdiff = sum(Rev_DIF), REGIONID = REGIONID[1], Fuel.Type = Fuel.Type[1]) %>% 
    arrange(SUMdiff) 

#when is CPP_4 LMP v low on 2019/6/4?
mpa %>% filter(DUID == "CPP_4",
               floor_date(SETTLEMENTDATE, "day") == ymd_hms("2019-06-04 00:00:00"))

#Low from 9:15 onwards
mpa_temp <- mpa %>% filter(SETTLEMENTDATE == ymd_hms("2019-06-04 9:15:00"))


rhs_temp <- rhs.fun("201906")#Q>>WOPW_WOSP_WOGP_2
rhs_temp %>% filter(SETTLEMENTDATE == ymd_hms("2019-06-04 9:15:00"))

eqs_temp <- eqs.fun("Q>>WOPW_WOSP_WOGP_2", "201905")
eqs_temp

#note: number of all mpa firms are in eqs :)

mpa_temp #heaps of firms bidding at or close to floor price, BERTRAND? WHY ARE SOMe NOT -1000?
mpa_temp %>% filter(LMP < -800, !(Fuel.Type %in% c("Wind", "Solar")))

#LMP = RRP - MPA = RRP - MV*K 

#GET OFFER DATA FOR ALL GENS
bids_temp <- bids_d.fun("201906", mpa_temp$DUID) %>% 
    filter(ymd_hms(SETTLEMENTDATE) == ymd_hms("2019-06-04 00:00:00"))

bids_temp %>% filter(DUID == "BARRON-1") %>% head()

bids_temp %>% count(DUID) %>% as.data.frame()

summary(bids_temp)

#CALL_B_1 2019/6/4 DB!!!!!
