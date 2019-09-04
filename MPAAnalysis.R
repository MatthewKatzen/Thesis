library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(padr)
#DIFFSUM v MONTH, grouped by Fuel, facet by State
mpa %>% group_by(MONTH = floor_date(SETTLEMENTDATE, "month"), REGIONID, Fuel.Type) %>% 
    summarise(DIFFsum = sum(Rev_DIF)) %>% 
    ungroup() %>% 
    pad(group = c("REGIONID", "Fuel.Type")) %>% replace_na(list(DIFFsum = 0)) %>%  #add missing dates
    ggplot(aes(x = MONTH, y = DIFFsum, colour = Fuel.Type)) +
    geom_line(size = 2) +
    facet_wrap(~ REGIONID) +
    ggtitle("Revenue Change in swith to LMP - Grouped by Fuel Type and State")+
    xlab("")+
    ylab("Rev Change")
    

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
mpa %>% filter(Fuel.Type == "Wind", year(SETTLEMENTDATE) == "2018") %>% 
    group_by(DUID, MONTH = floor_date(SETTLEMENTDATE, "day")) %>% 
    summarise(SUMdiff = sum(Rev_DIF_0)) %>% 
    arrange(SUMdiff)

#Non wind/solar (only strategic bidders), wind isn't necessarily a problem as their MC = 0
mpa %>% filter(!(Fuel.Type %in% c("Wind", "Solar")), year(SETTLEMENTDATE) > 2012) %>%
    group_by(DUID, MONTH = floor_date(SETTLEMENTDATE, "day")) %>% 
    summarise(SUMdiff = sum(Rev_DIF), REGIONID = REGIONID[1], Fuel.Type = Fuel.Type[1]) %>% 
    arrange(SUMdiff) 



