library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
###DISSORDERLY BIDDING

#Black coal QLD pushing out more efficient generators
mpa %>% filter(Fuel.Type == "Black Coal", year(SETTLEMENTDATE) > 2017, REGIONID == "QLD1") %>%
    group_by(DUID, DAY = floor_date(SETTLEMENTDATE, "day")) %>% 
    summarise(SUMdiff = sum(Rev_DIF), REGIONID = REGIONID[1], Fuel.Type = Fuel.Type[1]) %>% 
    arrange(SUMdiff) 

#when is CPP_4 LMP v low on 2019/6/4?
mpa %>% filter(DUID == "CPP_4",
               floor_date(SETTLEMENTDATE, "day") == ymd_hms("2019-06-04 00:00:00"))


#Low from 9:15 onwards. Look at all LMP for 9:15am
mpa_temp <- mpa %>% filter(SETTLEMENTDATE == ymd_hms("2019-06-04 9:15:00"))


rhs_temp <- rhs.fun("201906")#Q>>WOPW_WOSP_WOGP_2
rhs_temp %>% filter(SETTLEMENTDATE == ymd_hms("2019-06-04 9:15:00"))

eqs_temp <- eqs.fun("Q>>WOPW_WOSP_WOGP_2", "201905")
eqs_temp

#note: number of all mpa firms are in eqs :)
which(!(eqs_temp$DUID[-(1:2)] %in% mpa_temp$DUID))
which(!(mpa_temp$DUID %in% eqs_temp$DUID[-(1:2)]))
eqs_temp$DUID[16] #Clermont Solar Farm not-operational
mpa_temp$DUID[41] #NSW constraint, not of interest


mpa_temp #heaps of firms bidding at or close to floor price, BERTRAND? WHY ARE SOMe NOT -1000?
mpa_temp %>% filter(LMP < -800, !(Fuel.Type %in% c("Wind", "Solar")))

#LMP = RRP - MPA = RRP - MV*K 

#CHECK REBIDS
bids_temp_2 <- bids.fun("201906", mpa_temp$DUID) %>% 
    filter(ymd_hms(SETTLEMENTDATE) == ymd_hms("2019-06-04 00:00:00"))

bids_temp_2 %>% filter(ymd_hms(OFFERDATE) > ymd_hms("2019-06-04 00:00:00")) %>%
    select(DUID) %>% unique() %>% as.list()

mpa %>% filter(Fuel.Type == "Wind") %>% head()