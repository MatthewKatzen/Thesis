library(tidyverse)
library(tidyr)
library(lubridate)
mpa.sample <-  read.csv("D:/Thesis/Data/MPA/AEMO/2009_sample.csv", stringsAsFactors = FALSE) %>% 
    drop_na()#remove empty rows
    setNames(c("SETTLEMENTDATE", "DUID", "PRICE", "CONSTRAINED")) %>% 
    mutate(SETTLEMENTDATE = dmy_hm(SETTLEMENTDATE)) %>% 
    arrange(SETTLEMENTDATE)


#PRICE = RRP - MPA


summary(mpa.sample)#have data for april 2009

qplot(mpa$MPA)

#check first obs
mpa.sample %>% head()

#MPA = a * MV = 0.23* 0.9 = 

mpa.sample %>% group_by(DUID) %>% summarise(SUM = sum(PRICE), COUNT = n(), MEAN = SUM/COUNT) %>% arrange(SUM)#winners 
mpa.sample %>% group_by(DUID) %>% summarise(SUM = sum(PRICE), COUNT = n(), MEAN = SUM/COUNT) %>% arrange(-SUM)#losers

#GLADSTONE 3/4

mpa.sample %>% filter(DUID == "GSTONE3")

#get gen fuel types
fuel <- read.csv("data/dontupload/GenFuelTypes.csv", stringsAsFactors = FALSE) %>% 
    select(DUID, Region, Classification, 7:9) #remove extra detailed cols
colnames(fuel) <- str_remove_all(colnames(fuel), c("[.]"))

#combined
mpa.sample_comb <- mpa.sample %>% merge(fuel, by = "DUID")

#missing rows
(!(mpa.sample$DUID %in% fuel$DUID)) %>% which() %>% 
    mpa.sample$DUID[.] %>% table()
fuel %>% filter(DUID == "APS")
mpa.sample %>% count(DUID)

#by fuel
mpa.sample_comb %>% group_by(FuelSourcePrimary) %>% 
    summarise(SUM = sum(PRICE), 
              COUNT = n(), MEAN = SUM/COUNT) %>% arrange(SUM)

mpa.sample_comb %>% group_by(FuelSourcePrimary) %>% 
    summarise(SUM_POS = sum(PRICE[PRICE>0]), 
              SUM_NEG = sum(PRICE[PRICE<0]),
              COUNT_POS = sum(PRICE>0),
              COUNT_NEG = sum(PRICE<0))



mpa.sample %>% group_by(DUID) %>% 
    summarise(SUM_POS = sum(PRICE[PRICE>0]), 
              SUM_NEG = sum(PRICE[PRICE<0]),
              COUNT_POS = sum(PRICE>0),
              COUNT_NEG = sum(PRICE<0)) %>% 
    merge(fuel, by = "DUID") %>% arrange(-SUM_POS) %>% as_tibble()#winners 

mpa.sample %>% group_by(DUID) %>% 
    summarise(SUM_POS = sum(PRICE[PRICE>0]), 
              SUM_NEG = sum(PRICE[PRICE<0]),
              COUNT_POS = sum(PRICE>0),
              COUNT_NEG = sum(PRICE<0)) %>% 
    merge(fuel, by = "DUID") %>% arrange(SUM_NEG) %>% as_tibble()#losers 

