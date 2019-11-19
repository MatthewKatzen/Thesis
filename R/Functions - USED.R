#Functions for retrieving data from NEMWEB

#install.packages('tidyverse')
#install.packages('openxlsx')
#install.packages('sqldf')
library(tidyverse)
library(openxlsx)
library(sqldf)
library(data.table)
setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/Mispricing")
external_data_location <- "D:/Thesis/Data" #for big data

###RRP
rrp_fun <- function(yearmonth){
    external_data_location <- "D:/Thesis/Data/RRP" #for big data
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0
    csv_name <- paste0(external_data_location, "/PUBLIC_DVD_DISPATCHPRICE_", yearmonth, "010000.CSV")
    if(!file.exists(csv_name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                      year, "_", month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHPRICE_",yearmonth,
                      "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_DISPATCHPRICE_", yearmonth, "010000.CSV"),
              exdir = external_data_location)
    }
    rrp <- fread(csv_name, sep=",", stringsAsFactors = FALSE) %>% 
        filter(INTERVENTION == 0) %>% 
        select(SETTLEMENTDATE, REGIONID, RRP) %>% 
        mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))
    
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(rrp)
}

###DISPACTCH
dispatch_fun <- function(yearmonth){
    external_data_location <- "D:/Thesis/Data/DISPATCH" #for big data
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0
    csv_name <- paste0(external_data_location, "/PUBLIC_DVD_DISPATCHLOAD_", yearmonth, "010000.CSV")
    if(!file.exists(csv_name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                      year, "_", month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHLOAD_",yearmonth,
                      "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_DISPATCHLOAD_", yearmonth, "010000.CSV"),
              exdir = external_data_location)
    }
    dispatch <- fread(csv_name, sep=",", skip=1, stringsAsFactors = FALSE)
    dispatch <- dispatch %>%
        filter(INTERVENTION == 0) %>%
        select(DUID, SETTLEMENTDATE, INITIALMW) %>%
        mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(dispatch)
}