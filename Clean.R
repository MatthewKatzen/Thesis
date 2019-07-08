#install.packages('tidyverse')
#install.packages('openxlsx')
#install.packages('sqldf')
library(tidyverse)
library(openxlsx)
library(sqldf)
setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/Dissordely Bidding")
external.data.location <- "D:/Thesis/Data" #for big data

### RHS and MV
#gets rhs and values for one month period
rhs.fun <- function(yearmonth){
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0 #initialise
    #check if already downloaded
    csv.name <- paste0(external.data.location,"/PUBLIC_DVD_DISPATCHCONSTRAINT_", yearmonth, 
                       "010000.CSV")
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_",
                      year, "_",month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHCONSTRAINT_",
                      yearmonth, "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb") #download zip
        unzip(temp, paste0("PUBLIC_DVD_DISPATCHCONSTRAINT_", yearmonth, "010000.CSV"), 
              exdir = external.data.location) #unzip[ zipped file and save csv to external storage
    }
    #Clean RHS and MV
    rhs <- read.csv(csv.name, sep=",",skip=1)
    rhs <- rhs %>% filter(MARGINALVALUE != 0) %>% #remove unconstrained
        filter(substr(CONSTRAINTID,1,1) %in% c('Q','N','V','S','T','I')) #no fcas and other weird types
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(rhs)#save locally
}

### EQS
#for now it is a function which takes the constraint name and yearmonth it was made as inputs and outputs
#the lhs form (Scale, and T)
#In the future should probably create entire database
eqs.fun <- function(constraint, effective.ym) {
    e.year <- substr(effective.ym, 1, 4)
    e.month <- substr(effective.ym, 5, 6)
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, 
                       "010000.CSV")
    url <- 0 #initialise
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", e.year,"/MMSDM_",
                      e.year, "_", e.month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_GENERICCONSTRAINTRHS_", 
                      effective.ym, "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb")
        unzip(temp, paste0("PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, "010000.CSV"), 
              exdir = external.data.location)
    }
    #Clean EQS
    eqs <- read.csv(csv.name, sep=",",skip=1) %>% #load csv
        select(GENCONID, EFFECTIVEDATE, SCOPE, SPD_ID, SPD_TYPE, FACTOR) %>% #keep cols we are interested in
        filter(SCOPE == "DS") %>% #only care about dispatch constraints 
        filter(GENCONID == constraint) %>% #get constraint we care about
        distinct() %>% #remove duplicate rows
        filter(SPD_TYPE %in% c("T","I") | SPD_ID == "Scale") %>%  #only get scale value, interconnector, and generator/load data
        mutate(EFFECTIVEDATE = as.character(EFFECTIVEDATE))
    if(url != 0){ #checks if previous if statement was run
        unlink(temp) #delete zip
    }
    return(eqs)
}

###BANDS
#only uses last band before settlement date
#i.e. check example at end
bands.fun <- function(yearmonth){
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_BIDDAYOFFER_", yearmonth, "010000.CSV")
    url <- 0
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                      year,"_", month, "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_BIDDAYOFFER_",
                      yearmonth,"010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb")
        unzip(temp, paste0("PUBLIC_DVD_BIDDAYOFFER_", yearmonth, "010000.CSV"), 
              exdir = external.data.location)
    }
    bands <- read.csv(csv.name, sep=",",skip=1)
    bands <- bands %>% filter(BIDTYPE== "ENERGY") %>% 
        group_by(DUID, SETTLEMENTDATE) %>% 
        filter(VERSIONNO == max(VERSIONNO)) %>% 
        select(DUID, SETTLEMENTDATE, OFFERDATE, VERSIONNO, PRICEBAND1, PRICEBAND2, PRICEBAND3, PRICEBAND4,
               PRICEBAND5, PRICEBAND6, PRICEBAND7, PRICEBAND8, PRICEBAND9, PRICEBAND10, LASTCHANGED) %>% 
        as.data.frame() 
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(bands)
}

###BIDS
#for now it just takes th elast bid file for each day. 
#Note that the file can be altered throughout the day.
bids.fun <- function(yearmonth = "201905",generator){
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_BIDPEROFFER_", yearmonth, "010000.CSV")
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_",
                      year,"_", month, "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_BIDPEROFFER_",
                      yearmonth,
                      "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb")
        unzip(temp, paste0("PUBLIC_DVD_BIDPEROFFER_", yearmonth, "010000.CSV"), 
              exdir = external.data.location)
    }
    bids <- read.csv(csv.name, sep=",",skip=1)
    bids <- bids %>% filter(DUID == generator, BIDTYPE== "ENERGY") %>%
        filter(VERSIONNO == max((VERSIONNO))) %>% #get last version of bid file used
        select(DUID, SETTLEMENTDATE, OFFERDATE, VERSIONNO, PERIODID, BANDAVAIL1, BANDAVAIL2, BANDAVAIL3,
               BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8, BANDAVAIL9, BANDAVAIL10)
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(bids)
}
    
###MATCH ACTUAL BIDS USED 
#This is used for matchign what BID files are used throughout the day
#Not used yet

# yearmonth <- "201905" #change this to the date the constraint became effective
# year <- substr(yearmonth, 1, 4)
# month <- substr(yearmonth, 5, 6)
# url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", year, "_", month, "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHOFFERTRK_", yearmonth, "010000.zip")
# temp <- tempfile()
# download.file(url, temp, mode="wb")
# unzip(temp, paste0("PUBLIC_DVD_DISPATCHOFFERTRK_", yearmonth, "010000.CSV"), exdir = location)
# 
# match <- read.csv(paste0("data/PUBLIC_DVD_DISPATCHOFFERTRK_", yearmonth, "010000.CSV"), sep=",",skip=1)
# match %>% filter(DUID == "GORDON", BIDTYPE== "ENERGY") 


###DISPACTCH
#gets one day of dispatch for one generator
dispatch.fun <- function(yearmonth, date = "2019-05-10", generator){
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_DISPATCH_UNIT_SCADA_", yearmonth, "010000.CSV")
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                      year, "_", month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCH_UNIT_SCADA_",yearmonth,
                      "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb")
        unzip(temp, paste0("PUBLIC_DVD_DISPATCH_UNIT_SCADA_", yearmonth, "010000.CSV"),
              exdir = external.data.location)
    }
    dispatch <- read.csv(paste0(external.data.location, "/PUBLIC_DVD_DISPATCH_UNIT_SCADA_", yearmonth,
                                "010000.CSV"), 
                         sep=",",skip=1)
    dispatch <- dispatch %>% filter(DUID == generator) %>% 
        filter(as.Date(SETTLEMENTDATE) == date)
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(dispatch)
}
