library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)

data_location <- "D:/Thesis/Data/MPA2"
files <- paste0(data_location, "/", list.files(data_location))

mpa <-  files %>% map(~ read.csv(.x, stringsAsFactors = FALSE)) %>% 
    bind_rows() %>% 
    select(-X) %>% 
    setNames(c("SETTLEMENTDATE", "DUID", "LMP", "CONSTRAINED")) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    arrange(SETTLEMENTDATE)

fwrite(mpa, "D:/Thesis/Data/MPA2/mpa_cleaned.csv")

###FUEL
fuel <- read.csv("data/dontupload/GenFuelTypeNemSight.csv", stringsAsFactors = FALSE) %>% 
    select(Region, DUID, Participant, Fuel.Type, Emission.Factor) %>%  #keep cols of interest
    rename(REGIONID = Region) %>% 
    mutate(REGIONID = case_when(REGIONID == "Queensland" ~ "QLD1",
                                REGIONID == "New South Wales" ~ "NSW1",
                                REGIONID == "Victoria" ~ "VIC1",
                                REGIONID == "South Australia" ~ "SA1",
                                REGIONID == "Tasmania" ~ "TAS1"))

fwrite(fuel, "D:/Thesis/Data/Fuel_cleaned.csv")




### RRP DATA 2018 
yearmonths <- c("201801", "201802", "201803", "201804", "201805", "201806", "201807", "201808", "201809", "201810", "201811", "201812")

rrp18 <- yearmonths %>% map(~ rrp.fun(.x)) %>% 
    bind_rows()

fwrite(rrp18, "D:/Thesis/Data/RRP/rrp18.csv")

### GENERATION

dispatch18 <- yearmonths %>% map(~ dispatch.fun(.x)) %>% 
    bind_rows()

fwrite(dispatch18, "D:/Thesis/Data/DISPATCH/dispatch18.csv")


### MERGE 

mpa18 <- fread("D:/Thesis/Data/MPA2/mpa_cleaned.csv", stringsAsFactors = FALSE, drop = 1) %>% #mpa
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    filter(year(SETTLEMENTDATE) == "2018", month(SETTLEMENTDATE) == "12") %>% #2018
    inner_join(fread("D:/Thesis/Data/Fuel_cleaned.csv", stringsAsFactors = FALSE, drop = 1),
               by = "DUID") %>%  #fuel
    inner_join(fread("D:/Thesis/Data/RRP/rrp18.csv", stringsAsFactors = FALSE, drop = 1) %>% 
                   mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)), 
               by = c("SETTLEMENTDATE", "REGIONID")) %>% #rrp
    inner_join(fread("D:/Thesis/Data/DISPATCH/dispatch18.csv", stringsAsFactors = FALSE) %>% 
                   mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)),
               by = c("DUID", "SETTLEMENTDATE")) #dispatch

fwrite(mpa18, "D:/Thesis/Data/mpa18.csv")






###MISSING GENS
(!(mpa$DUID %in% fuel$DUID)) %>% which() %>% 
    mpa$DUID[.] %>% table() #only 3 missing which aren't in NEM registration and exemption list either


###check whether MPA or LMP
mpa18 %>% filter(year(SETTLEMENTDATE) == "2018") %>% 
    arrange(DUID, desc(SETTLEMENTDATE)) %>% 
    head()

#AGLHAL 2018-12-21 20:30:00
#PRICE = 81.75

rhs <- rhs.fun("201812")
rhs %>% filter(ymd_hms(SETTLEMENTDATE) == ymd_hms("2018-12-21 20:30:00"))#MV = -5.01108

eqs <- eqs.fun("S>>PARB_RBTU_WEWT", "201809")
eqs %>% filter(str_detect(SPD_ID, "AGLHAL"))#k = 0.4447

rrp <- rrp.fun("201812")
rrp %>% filter(SETTLEMENTDATE == ymd_hms("2018-12-21 20:30:00"))#SA RRP = 83.97958

#MV*K = -2.22
#MPA = 2.22
#LP = RRP - MPA = 83.979 - 2.22 = 81.75958
#WHICH EQUALS PRICE IN MPA_COMB!
#t/f MPA PRICE = LMP