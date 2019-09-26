library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
###DISSORDERLY BIDDING

#Black coal QLD pushing out more efficient generators
mpa %>% filter(fuel_type == "Black Coal", year(settlementdate) > 2017, state == "QLD") %>%
    group_by(duid, day = floor_date(settlementdate, "day")) %>% 
    summarise(SUMdiff = sum(rev_dif), state = state[1], fuel_type = fuel_type[1]) %>% 
    arrange(SUMdiff) 

#when is CPP_4 LMP v low on 2019/6/4?
mpa %>% filter(duid == "CPP_4",
               floor_date(settlementdate, "day") == ymd_hms("2019-06-04 00:00:00"))


#Low from 9:15 onwards. Look at all LMP for 9:15am
mpa_temp <- mpa %>% filter(settlementdate == ymd_hms("2019-06-04 9:15:00"))


rhs_temp <- rhs.fun("201906") %>% clean_names()#Q>>WOPW_WOSP_WOGP_2
rhs_temp %>% filter(settlementdate == ymd_hms("2019-06-04 9:15:00"))
rhs_temp %>% filter(constraintid == "Q>>WOPW_WOSP_WOGP_2", as.Date(settlementdate) == as.Date("2019-06-04 9:15:00")) %>% 
    select(settlementdate)

eqs_temp <- eqs.fun("Q>>WOPW_WOSP_WOGP_2", "201905") %>% clean_names()
eqs_temp

#note: number of all mpa firms are in eqs :)
which(!(eqs_temp$duid[-(1:2)] %in% mpa_temp$duid))
which(!(mpa_temp$duid %in% eqs_temp$duid[-(1:2)]))
eqs_temp$duid[16] #Clermont Solar Farm not-operational
mpa_temp$duid[41] #NSW constraint, not of interest


mpa_temp #heaps of firms bidding at or close to floor price, BERTRAND
mpa_temp %>% filter(lmp < -800, !(fuel_type %in% c("Wind", "Solar"))) -> temp
#check these firms in nemsight for rebids
#GSTONE1

#LMP = RRP - MPA = RRP - MV*K 

#CHECK REBIDS
bids_temp_2 <- bids.fun("201906", mpa_temp$duid) %>% 
    filter(ymd_hms(settlementdate) == ymd_hms("2019-06-04 00:00:00"))

bids_temp_2 %>% filter(ymd_hms(OFFERDATE) > ymd_hms("2019-06-04 00:00:00")) %>%
    select(duid) %>% unique() %>% as.list()
