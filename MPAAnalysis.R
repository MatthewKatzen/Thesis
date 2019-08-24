library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)

mpa %>% group_by(MONTH = floor_date(SETTLEMENTDATE, "month"), REGIONID, Fuel.Type) %>% 
    summarise(DIFFsum = sum(Rev_DIF)) %>% 
    filter(Fuel.Type != "Wind", year(MONTH) > 2012) %>% 
    ggplot(aes(x = MONTH, y = DIFFsum, colour = Fuel.Type)) +
    geom_line(size = 2) +
    facet_wrap(~ REGIONID)

#Clearly something going on in SA wind
#which makes sense, lots of wind bidding at floor price and congesting network

mpa %>% filter(REGIONID == "SA1", Fuel.Type == "Wind", year(SETTLEMENTDATE) == "2018") %>% 
    group_by(DUID, MONTH = floor_date(SETTLEMENTDATE, "day")) %>% 
    summarise(SUMdiff = sum(Rev_DIF)) %>% 
    arrange(SUMdiff)

