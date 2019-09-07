library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)

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
data_location <- "D:/Thesis/Data/MPA2"
files <- paste0(data_location, "/", list.files(data_location))

mpa <-  files %>% map(~ read.csv(.x, stringsAsFactors = FALSE)) %>% 
    bind_rows() %>% 
    select(-X) %>% 
    setNames(c("SETTLEMENTDATE", "DUID", "LMP", "CONSTRAINED")) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    arrange(SETTLEMENTDATE)

fwrite(mpa, "D:/Thesis/Data/MPA2/mpa_cleaned.csv")

# RRP
rrpfull <- yearmonths %>% map(~ rrp.fun(.x)) %>% 
    group_by(INTERVAL = cut(SETTLEMENTDATE, breaks = "30 min"), REGIONID) %>% #add RRP30
    mutate(RRP30 = mean(RRP)) %>% 
    ungroup() %>% 
    select(-INTERVAL) %>% 
    rbindlist()

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
    file_location <- paste0("D:/Thesis/Data/DISPATCH/yearly/dispatch", i, ".csv")
    dispatchtemp <- temp %>% map(~ dispatch.fun(.x)) %>% 
        rbindlist()  
    fwrite(dispatchtemp, file_location)
}

# DUID
duid_details <- fread("D:/Thesis/Data/duid_details.csv") %>% filter(Type == "Gen") %>% 
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

yearly_gen <- fread("D:/Thesis/Data/yearly_gen.csv") 
gens <- colnames(yearly_gen)[-1]


yearvals_gens <- yearly_gen$Yearly[ifelse(rowSums(t(yearly_gen>0))==0, #if no generation b/w 00-19
                         NA, 
                         max.col(t(yearly_gen>0), "first"))][-1]  #find index of first TRUE

age_gen <- data_frame(Station = gens, AGE = yearvals_gens)
fwrite(age_gen, "D:/Thesis/Data/age_gen.csv")

### MERGE
mpa <- fread("D:/Thesis/Data/MPA2/mpa_cleaned.csv", stringsAsFactors = FALSE, drop = 1) %>% 
    filter(ymd_hms(SETTLEMENTDATE)>=ymd_hms("2009-07-01 00:00:00")) %>% 
    filter(ymd_hms(SETTLEMENTDATE)<=ymd_hms("2019-07-01 00:00:00")) 
    

mpa_nodisp <- fread("D:/Thesis/Data/MPA2/mpa_cleaned.csv", stringsAsFactors = FALSE) %>% #mpa
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    inner_join(fread("D:/Thesis/Data/duid_details_clean.csv"),#duid details
               by = "DUID") %>%  
    inner_join(fread("D:/Thesis/Data/RRP/rrpfull.csv", stringsAsFactors = FALSE) %>% #rrp
                   mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)), 
               by = c("SETTLEMENTDATE", "REGIONID")) %>%
    inner_join(fread("D:/Thesis/Data/age_gen.csv"), #age
          by = "Station")

fwrite(mpa_nodisp, "D:/Thesis/Data/mpa_nodisp.csv")