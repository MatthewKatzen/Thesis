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
rhs <- read.csv("data/PUBLIC_DVD_DISPATCHCONSTRAINT_201904010000.CSV", sep=",",skip=1)
rhs <- rhs %>% filter(MARGINALVALUE != 0) %>% #remove unconstrained
    filter(substr(CONSTRAINTID,1,1) %in% c('Q','N','V','S','T','I')) #no fcas and other weird types
write.csv(rhs, "data/rhs.csv")
unlink("data/PUBLIC_DVD_DISPATCHCONSTRAINT_201904010000.CSV")#delete big csv


### Download Constraint Equations (RHS) e.g. S>V_NIL_SETX_SETX1
effective.ym <- "201905" #change this to the date the constraint became effective
e.year <- substr(effective.ym, 1, 4)
e.month <- substr(effective.ym, 5, 6)
e.url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", e.year,"/MMSDM_", e.year, "_", e.month, "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, "010000.zip")
location <- paste0(getwd(),"/data")
temp <- tempfile()
download.file(e.url, temp, mode="wb")
unzip(temp, paste0("PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, "010000.CSV"), exdir = location)

#clean
eqs <- read.csv(paste0("data/PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective, "010000.CSV"), sep=",",skip=1)
