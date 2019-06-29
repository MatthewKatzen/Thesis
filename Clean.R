setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/Dissorderly Bidding")
#install.packages('tidyverse')
#install.packages('openxlsx')
#install.packages('chron')
library(tidyverse)
library(openxlsx)

### Download RHS and MV
yearmonth <- "201905" #change this to the date the constraint became effective
year <- substr(yearmonth, 1, 4)
month <- substr(yearmonth, 5, 6)
url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", year, "_", month, "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHCONSTRAINT_", yearmonth, "010000.zip")
location <- paste0(getwd(),"/data")
temp <- tempfile()
download.file(url, temp, mode="wb")
unzip(temp, paste0("PUBLIC_DVD_DISPATCHCONSTRAINT_", yearmonth, "010000.CSV"), exdir = location)

#Clean RHS and MV
rhs <- read.csv(paste0("data/PUBLIC_DVD_DISPATCHCONSTRAINT_", yearmonth, "010000.CSV"), sep=",",skip=1)
rhs <- rhs %>% filter(MARGINALVALUE != 0) %>% #remove unconstrained
    filter(substr(CONSTRAINTID,1,1) %in% c('Q','N','V','S','T','I')) #no fcas and other weird types
write.csv(rhs, "data/rhs.csv")
unlink(paste0("data\\PUBLIC_DVD_DISPATCHCONSTRAINT_", yearmonth, "010000.CSV"))#delete big csv


### Download Constraint Equations (RHS) e.g. S>V_NIL_SETX_SETX1
#for now it is a function which takes the constraint name and yearmonth it was made as inputs and ouputs
#the lhs form (Scale, and T)
#In the future should probably create entire database

EQS.fun <- function(constraint, effective.ym) {
    e.year <- substr(effective.ym, 1, 4)
    e.month <- substr(effective.ym, 5, 6)
    e.url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", e.year,"/MMSDM_", e.year, "_", e.month, "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, "010000.zip")
    location <- paste0(getwd(),"/data")
    temp <- tempfile()
    download.file(e.url, temp, mode="wb")
    unzip(temp, paste0("PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, "010000.CSV"), exdir = location)
    
    #Clean EQS
    eqs <- read.csv(paste0("data/PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, "010000.CSV"), sep=",",skip=1) %>% #load csv
    select(GENCONID, EFFECTIVEDATE, SCOPE, SPD_ID, SPD_TYPE, FACTOR) %>% #keep cols we are interested in
        filter(SCOPE == "DS") %>% #only care about dipatch constraints 
        filter(GENCONID == constraint) %>% #get constraint we care about
        distinct() %>% #remove duplicate rows
        filter(SPD_TYPE %in% c("T","I") | SPD_ID == "Scale") %>%  #only get scale value, interconnector, and generator/load data
        mutate(EFFECTIVEDATE = as.character(EFFECTIVEDATE))
    file.remove(paste0("data/PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, "010000.CSV"))#delete big csv
    return(eqs)
}

### Download Bids

