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
    select(Region, DUID, Participant, Station, Fuel.Type, Emission.Factor) %>%  #keep cols of interest
    rename(REGIONID = Region) %>% 
    mutate(REGIONID = case_when(REGIONID == "Queensland" ~ "QLD1",
                                REGIONID == "New South Wales" ~ "NSW1",
                                REGIONID == "Victoria" ~ "VIC1",
                                REGIONID == "South Australia" ~ "SA1",
                                REGIONID == "Tasmania" ~ "TAS1"))

fwrite(fuel, "D:/Thesis/Data/Fuel_cleaned.csv")

#get all yearmonths
year <- as.character(c(2009:2019))
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
yearmonths <- NULL
for (i in year){
    if (i == "2009"){
        temp <- paste0(i, months[7:12])
    } else if (i == "2019"){
        temp <- paste0(i, months[1:6])
    } else{
        temp <- paste0(i, months)
    }
    yearmonths <- c(yearmonths,temp)
}

### RRP 
rrpfull <- yearmonths %>% map(~ rrp.fun(.x)) %>% 
    group_by(INTERVAL = cut(SETTLEMENTDATE, breaks = "30 min"), REGIONID) %>% #add RRP30
    mutate(RRP30 = mean(RRP)) %>% 
    ungroup() %>% 
    select(-INTERVAL) %>% 
    rbindlist()

fwrite(rrpfull, "D:/Thesis/Data/RRP/rrpfull.csv")


### GENERATION
for (i in year){
    if (i == "2009"){
        temp <- paste0(i, months[7:12])
    } else if (i == "2019"){
        temp <- paste0(i, months[1:6])
    } else{
        temp <- paste0(i, months)
    }

    file_location <- paste0("D:/Thesis/Data/DISPATCH/dispatch", i, ".csv")
    
    dispatchtemp <- temp %>% map(~ dispatch.fun(.x)) %>% 
        rbindlist()  
    
    fwrite(dispatchtemp, file_location)
    
}


dispatchfull <- yearmonths %>% map(~ dispatch.fun(.x)) %>% 
    bind_rows()

fwrite(dispatchfull, "D:/Thesis/Data/DISPATCH/dispatchfull.csv")


### MERGE 

#merge mpa, fueltype, rrp data
mpa_nodisp <- fread("D:/Thesis/Data/MPA2/mpa_cleaned.csv", stringsAsFactors = FALSE, drop = 1) %>% #mpa
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    inner_join(fread("D:/Thesis/Data/Fuel_cleaned.csv", stringsAsFactors = FALSE, drop = 1),
               by = "DUID") %>%  #fuel
    inner_join(fread("D:/Thesis/Data/RRP/rrpfull.csv", stringsAsFactors = FALSE) %>% 
                   mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)), 
               by = c("SETTLEMENTDATE", "REGIONID"))#rrp

fwrite(mpa_nodisp, "D:/Thesis/Data/mpa_nodisp.csv")

#loop through years to add dispatch
for (i in year){
    temp <- mpa_nodisp %>% 
        inner_join(fread(paste0("D:/Thesis/Data/DISPATCH/dispatch", i, ".csv"),
                         stringsAsFactors = FALSE) %>%
                       mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)),
                   by = c("DUID", "SETTLEMENTDATE")) #dispatch
    
    fwrite(temp, paste0("D:/Thesis/Data/COMPLETE/mpa", i, ".csv"))
}


data_location <- "D:/Thesis/Data/COMPLETE"
files <- paste0(data_location, "/", list.files(data_location))

mpa_complete <- files %>% map(~ fread(.x, stringsAsFactors = FALSE)) %>% 
    rbindlist() %>% mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))

fwrite(mpa_complete, "D:/Thesis/Data/COMPLETE/mpacomplete.csv")


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

#ADD REVENUE
mpa <- mpa %>% mutate(Rev_0 = 0) %>% 
    mutate(Rev_RRP_30 = RRP30*TOTALCLEARED) %>% 
    mutate(Rev_LMP = LMP*TOTALCLEARED) %>% 
    mutate(Rev_LMP0 = pmax(LMP, 0)*TOTALCLEARED) %>% 
    mutate(Rev_DIF = Rev_LMP - Rev_RRP_30) %>%   #how much you benefit from change to LMP system
    mutate(Rev_DIF_0 = pmax(Rev_LMP, Rev_LMP0) - Rev_RRP_30) #assume no neg LMP (no neg bids)

fwrite(mpa, "D:/Thesis/Data/COMPLETE/mpacomplete.csv")

#ADD AGE AND GEN/LOAD
weekly <- fread("D:/Thesis/Data/weekly_data_construct_2.csv", stringsAsFactors = FALSE)
colnames(disp_weekly)
weekly_cleaned <- weekly %>% mutate(Station = station_name) %>% 
    select(Year, Week, Station, output_week) %>% 
    mutate(DATE = ymd(str_c(Year, "-01-01")) + weeks(Week - 1)) %>% 
    filter(output_week != 0) %>% 
    group_by(Station) %>% 
    summarise(Start = sort(DATE)[1])

fuel <- read.csv("data/dontupload/GenFuelTypeNemSight.csv", stringsAsFactors = FALSE) %>% 
    select(DUID, Station, Type)

merged <- fuel %>% inner_join(weekly_cleaned, by = 'Station')#merge fuel and station name

missing <- fuel[which(!(fuel$Station %in% weekly_cleaned$Station)) %>% as.numeric(),] #gens in fuel but not weekly

mpa_missing <- mpa %>% select(DUID) %>% unique() %>% filter(DUID %in% missing$DUID) %>% as.list() %>% .$DUID#gens in mpa and fuel(missing)


missing_df <- read.csv("data/dontupload/missing.csv") %>% #data for missing in mpa
    mutate(Start = ymd(str_c(Year, "-01-01"))) %>% #create year
    mutate(Type = "Gen")#add Type


merged2 <- rbind(missing_df %>% select(-Station, -Year), merged %>% select(-Station))#merged and manual year find

mpa_age <- mpa %>% inner_join(merged2, by = "DUID")

fwrite(mpa_age, "D:/Thesis/Data/COMPLETE/mpa_age.csv")
