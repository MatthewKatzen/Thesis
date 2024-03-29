#Clean LMP files given to us by AEMO
#Final file used is mpa_final.csv

library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)

# YEARS
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

# MPA
data_location <- "D:/Thesis/Data/MPA"
files <- paste0(data_location, "/", list.files(data_location))

mpa_cleaned <-  files %>% map(~ read.csv(.x, stringsAsFactors = FALSE)) %>% 
    bind_rows() %>% 
    select(-X) %>% 
    setNames(c("SETTLEMENTDATE", "DUID", "LMP", "CONSTRAINED")) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    arrange(SETTLEMENTDATE)

fwrite(mpa, "D:/Thesis/Data/MPA/mpa_cleaned.csv")

# RRP
rrpfull <- yearmonths %>% map(~ rrp_fun(.x)) %>% 
    group_by(INTERVAL = cut(SETTLEMENTDATE, breaks = "30 min"), REGIONID) %>% #add RRP30
    mutate(RRP30 = mean(RRP)) %>% 
    ungroup() %>% 
    select(-INTERVAL) %>% 
    rbindlist() %>% 
    unique() #some are repeated for some reason

fwrite(rrpfull, "D:/Thesis/Data/RRP/rrpfull.csv")

# DISPATCH (TOO LARGE TO MERGE WITH IN ONE GO)

for (i in year){
    if (i == "2009"){
        temp <- paste0(i, months[7:12])
    } else if (i == "2019"){
        temp <- paste0(i, months[1:6])
    } else{
        temp <- paste0(i, months)
    }
    file_location <- paste0("D:/Thesis/Data/DISPATCH/yearly/dispatch_initial_", i, ".csv")
    dispatchtemp <- temp %>% map(~ dispatch_fun(.x)) %>% 
        rbindlist()  
    fwrite(dispatchtemp, file_location)
}

# DUID
duid_details <- fread("D:/Thesis/Data/NEMSIGHT/duid_details.csv") %>% filter(Type == "Gen") %>% 
    select(-c("ConnectionPtId", "Thermal Efficiency", "Auxiliaries", "Emission Intensity Sent-Out", 
              "Capacity")) %>% 
    rename(REGIONID = Region) %>% 
    mutate(REGIONID = case_when(REGIONID == "Queensland" ~ "QLD1",
                                REGIONID == "New South Wales" ~ "NSW1",
                                REGIONID == "Victoria" ~ "VIC1",
                                REGIONID == "South Australia" ~ "SA1",
                                REGIONID == "Tasmania" ~ "TAS1"))

fwrite(duid_details,"D:/Thesis/Data/duid_details_clean.csv")

# AGE
yearly_gen <- fread("D:/Thesis/Data/NEMSIGHT/yearly_gen.csv") 
gens <- colnames(yearly_gen)[-1]


yearvals_gens <- yearly_gen$Yearly[ifelse(rowSums(t(yearly_gen>0))==0, #if no generation b/w 00-19
                         NA, 
                         max.col(t(yearly_gen>0), "first"))][-1]  #find index of first TRUE

age_gen <- data_frame(Station = gens, AGE = yearvals_gens)
fwrite(age_gen, "D:/Thesis/Data/age_gen.csv")

### MERGE

mpa_nodisp <- fread("D:/Thesis/Data/MPA/mpa_cleaned.csv", stringsAsFactors = FALSE, drop = 1) %>% #mpa
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    filter(SETTLEMENTDATE %within% interval(ymd_hms("2009-07-01 00:05:00"), 
                                            ymd_hms("2019-07-01 00:00:00"))) %>% 
    inner_join(fread("D:/Thesis/Data/duid_details_clean.csv"),#duid details
               by = "DUID") %>%  
    inner_join(fread("D:/Thesis/Data/RRP/rrpfull_unique.csv", stringsAsFactors = FALSE) %>% #rrp
                   mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)), 
               by = c("SETTLEMENTDATE", "REGIONID")) %>%
    inner_join(fread("D:/Thesis/Data/age_gen.csv"), #age
          by = "Station")

fwrite(mpa_nodisp, "D:/Thesis/Data/mpa_nodisp.csv")

#loop through years to add dispatch
for (i in year){
    temp <- fread("D:/Thesis/Data/mpa_nodisp.csv") %>% 
        inner_join(fread(paste0("D:/Thesis/Data/DISPATCH/yearly/dispatch_initial_", i, ".csv"),
                         stringsAsFactors = FALSE),
                   by = c("DUID", "SETTLEMENTDATE")) #dispatch
    
    fwrite(temp, paste0("D:/Thesis/Data/COMPLETE/initial/mpa_initial", i, ".csv"))
}


data_location <- "D:/Thesis/Data/COMPLETE/initial"
files <- paste0(data_location, "/", list.files(data_location))

mpa_complete <- files %>% map(~ fread(.x, stringsAsFactors = FALSE)) %>% 
    rbindlist() %>% mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) 

fwrite(mpa_complete, "D:/Thesis/Data/COMPLETE/initial/mpamerged.csv")

#remove repeat mpa
mpa <- fread("D:/Thesis/Data/COMPLETE/initial/mpamerged.csv") 
mpa_dup <- mpa %>% select(SETTLEMENTDATE, DUID) %>% duplicated()
mpa_unique <- mpa[!mpa_dup,]

fwrite(mpa_unique, "D:/Thesis/Data/COMPLETE/initial/mpa_unique.csv")

#add revenue
mpa_rev <- fread("D:/Thesis/Data/COMPLETE/initial/mpa_unique.csv") %>% 
    mutate(DISPATCHMWH = INITIALMW/12) %>% 
    mutate(Rev_RRP_30 = RRP30*DISPATCHMWH) %>% 
    mutate(Rev_LMP = LMP*DISPATCHMWH) %>% 
    mutate(Rev_LMP0 = pmax(LMP, 0)*DISPATCHMWH) %>% 
    mutate(Rev_DIF = Rev_LMP - Rev_RRP_30) %>%   #how much you benefit from change to LMP system
    mutate(Rev_DIF_0 = pmax(Rev_LMP, Rev_LMP0) - Rev_RRP_30) %>%  #assume no neg LMP (no neg bids)
    mutate(STATE = case_when(REGIONID == "QLD1" ~ "QLD",
                             REGIONID == "NSW1" ~ "NSW",
                             REGIONID == "VIC1" ~ "VIC",
                             REGIONID == "SA1" ~ "SA",
                             REGIONID == "TAS1" ~ "TAS")) %>% 
    select(-REGIONID) %>% 
    clean_names()#clean up colnames



fwrite(mpa_rev, "D:/Thesis/Data/mpa_final.csv")





#intervention pricing
rrp_int <- yearmonths %>% map(~ rrp_fun(.x, int = 1)) %>% 
    rbindlist() %>% 
    clean_names

fwrite(rrpfull_int, "D:/Thesis/Data/RRP/rrpfull_int.csv")

rrpfull <- fread("D:/Thesis/Data/RRP/rrpfull.csv") %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    clean_names() %>% 
    mutate(intervention = 0) %>% 
    select(-rrp30)

rrpfull_int <- rbind(head(rrp_int),head(rrpfull))