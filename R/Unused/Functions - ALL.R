#Functions for retrieving data from NEMWEB
#RHS, MV, Constraint Equations

#Functions for cleaning


#install.packages('tidyverse')
#install.packages('openxlsx')
#install.packages('sqldf')
library(tidyverse)
library(openxlsx)
library(sqldf)
library(data.table)
setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/Mispricing")
external.data.location <- "D:/Thesis/Data" #for big data
#"C:/Users/Matthew/Downloads/Temp"
#"D:/Thesis/Data"

### RHS and MV
#gets rhs and values for one month period
rhs.fun <- function(yearmonth){
    external.data.location <- "D:/Thesis/Data/RHS" 
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
        download.file(url, temp, mode="wb", method = "curl") #download zip
        unzip(temp, paste0("PUBLIC_DVD_DISPATCHCONSTRAINT_", yearmonth, "010000.CSV"), 
              exdir = external.data.location) #unzip[ zipped file and save csv to external storage
    }
    #Clean RHS and MV
    rhs <- read.csv(csv.name, sep=",", skip=1, stringsAsFactors = FALSE)
    rhs <- rhs %>% filter(MARGINALVALUE != 0) %>% #remove unconstrained
        filter(substr(CONSTRAINTID,1,1) %in% c('Q','N','V','S','T','I')) %>%  #no fcas and other weird types
        mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(rhs)#save locally
}

#find interval for which rhs is constrained for atleast `len_con` periods
#input: datetimes = list of all times when rhs is constrained, len_con: minimum length of constraint we want to consider
constrained.fun <- function(datetimes, len_con){
    temp <- as.POSIXct(datetimes)
    temp2 <- seq.POSIXt(temp[1], temp[length(temp)], by = "5 min")
    temp3 <- rle(temp2 %in% (temp))
    temp4 <- which(temp3$values==TRUE & temp3$lengths > len_con)
    int <- list()
    for (i in 1:length(temp4)){
        start <- sum(temp3$lengths[1:(temp4[i]-1)][temp3$values[1:(temp4[i]-1)]==T])+1
        end <- start + temp3$lengths[temp4[i]] - 1
        int[[i]] <- interval(temp[start], temp[end])
    }
    return(do.call("c", int))#return int in better form
}

### EQS
#for now it is a function which takes the constraint name and yearmonth it was made as inputs and outputs
#the lhs form (Scale, and T)
#In the future should probably create entire database
eqs.fun <- function(constraint, effective.ym) {
    external.data.location <- "D:/Thesis/Data/EQS" #for big data
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
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_GENERICCONSTRAINTRHS_", effective.ym, "010000.CSV"), 
              exdir = external.data.location)
    }
    #Clean EQS
    eqs <- read.csv(csv.name, sep=",", skip=1, stringsAsFactors = FALSE) %>% #load csv
        select(GENCONID, EFFECTIVEDATE, SCOPE, SPD_ID, SPD_TYPE, FACTOR) %>% #keep cols we are interested in
        filter(SCOPE == "DS") %>% #only care about dispatch constraints 
        filter(GENCONID == constraint) %>% #get constraint we care about
        #distinct() %>% #remove duplicate rows
        filter(SPD_TYPE %in% c("T","I") | SPD_ID == "Scale") %>%  #only get scale value, interconnector, and generator/load data
        mutate(EFFECTIVEDATE = ymd_hms(EFFECTIVEDATE)) %>% 
        mutate(DUID = str_replace(SPD_ID, "\\..*","")) %>% #remove SPD_ID left of `.`
        select(-SPD_ID)
    if(url != 0){ #checks if previous if statement was run
        unlink(temp) #delete zip
    }
    return(eqs)
}

###BANDS
#only uses last band before settlement date
#i.e. check example at end
bands.fun <- function(yearmonth){
    external.data.location <- "D:/Thesis/Data/BANDS" #for big data
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_BIDDAYOFFER_", yearmonth, "010000.CSV")
    url <- 0
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                      year,"_", month, "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_BIDDAYOFFER_D_",
                      yearmonth,"010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_BIDDAYOFFER_", yearmonth, "010000.CSV"), 
              exdir = external.data.location)
    }
    bands <- read.csv(csv.name, sep=",", skip=1, stringsAsFactors = FALSE)
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
bids.fun <- function(yearmonth, generators){
    external.data.location <- "D:/Thesis/Data/BIDS" #for big data
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
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_BIDPEROFFER_", yearmonth, "010000.CSV"), 
              exdir = external.data.location)
    }
    bids <- read.csv(csv.name, sep=",", skip=1, stringsAsFactors = FALSE)
    bids <- bids %>% filter(DUID %in% generators, BIDTYPE== "ENERGY") %>% 
        select(DUID, SETTLEMENTDATE, OFFERDATE, PERIODID, BANDAVAIL1, BANDAVAIL2, BANDAVAIL3,
               BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8, BANDAVAIL9, BANDAVAIL10, MAXAVAIL) %>% 
        mutate(INTERVAL_DATETIME = as.POSIXct(SETTLEMENTDATE) + minutes(PERIODID*30 + 210))
    if(url != 0){
        unlink(temp) #delete zip
    } 
    return(as.data.frame(bids))
}

bids_d.fun <- function(yearmonth, generators){
    external.data.location <- "D:/Thesis/Data/BIDS_D" #for big data
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_BIDPEROFFER_D_", yearmonth, "010000.CSV")
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_",
                      year,"_", month, "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_BIDPEROFFER_D_",
                      yearmonth,
                      "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_BIDPEROFFER_D_", yearmonth, "010000.CSV"), 
              exdir = external.data.location)
    }
    bids <- read.csv(csv.name, sep=",", skip=1, stringsAsFactors = FALSE)
    bids <- bids %>% filter(DUID %in% generators, BIDTYPE== "ENERGY") %>%
    select(DUID, SETTLEMENTDATE, INTERVAL_DATETIME, OFFERDATE, VERSIONNO, PERIODID, BANDAVAIL1, BANDAVAIL2, BANDAVAIL3,
           BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8, BANDAVAIL9, BANDAVAIL10, MAXAVAIL)
    if(url != 0){
        unlink(temp) #delete zip
    } 
    return(as.data.frame(bids))
}

#for some reason bids can exceed the maxavail, therefore need to cut bids off at that point
fixbid.fun <- function(data){
    temp <- data %>% select(MAXAVAIL, BANDAVAIL1, 
                            BANDAVAIL2, BANDAVAIL3, BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8,
                            BANDAVAIL9, BANDAVAIL10)
    for (i in 1:nrow(temp)){
        j <- 2
        while ((sum(temp[i, 2:j]) < temp[i,1]) & (j<=10)) {#find col where colsum > maxavail
            j <- j + 1
        }
        temp[i,j] <- min(abs(temp[i,1] - sum(temp[i, 2:(j-1)])), temp[i,j], temp[i,1]) #fix last col to not exceed maxail
        
        if (j <=10){
            temp[i,c((j+1):ncol(temp))] <- 0 #make leftover cols zero
        }
    }
    data[,colnames(temp)] <- temp #add back into original data
    return(data)
}

#DISPATCHOFFERTRK
#get offer description
dispatch.tracker.fun <- function(yearmonth, generator){
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_DISPATCHOFFERTRK_", yearmonth, "010000.CSV")
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                      year, "_", month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHOFFERTRK_",yearmonth,
                      "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_DISPATCHOFFERTRK_", yearmonth, "010000.CSV"),
              exdir = external.data.location)
    }
    dispatch <- read.csv(csv.name, sep=",", skip=1, stringsAsFactors = FALSE)
    dispatch <- dispatch %>% filter(DUID == generator) #%>% 
        #select(DUID, SETTLEMENTDATE, TOTALCLEARED)
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(dispatch)
}

###DISPACTCH
#gets one day of dispatch for one generator
dispatch.fun <- function(yearmonth){
    external.data.location <- "D:/Thesis/Data/DISPATCH" #for big data
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_DISPATCHLOAD_", yearmonth, "010000.CSV")
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                      year, "_", month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHLOAD_",yearmonth,
                      "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_DISPATCHLOAD_", yearmonth, "010000.CSV"),
              exdir = external.data.location)
    }
    dispatch <- fread(csv.name, sep=",", skip=1, stringsAsFactors = FALSE)
    dispatch <- dispatch %>%
        filter(INTERVENTION == 0) %>%
        select(DUID, SETTLEMENTDATE, INITIALMW) %>%
        mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(dispatch)
}

### MAX BAND
#get the max band used in dispatch. This is essentially the gens bid
#Note: Since some files start at 4am and others at 12am, the file only goes from 4am-12am
maxband.fun <- function(bids, dispatch, bands){
    bids <- bids %>% select(MAXAVAIL, BANDAVAIL1, 
                            BANDAVAIL2, BANDAVAIL3, BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8,
                            BANDAVAIL9, BANDAVAIL10, INTERVAL_DATETIME) 
    
    dispatch <- dispatch %>% select(INTERVAL_DATETIME = SETTLEMENTDATE, TOTALCLEARED)
    bands <- bands %>% select(PRICEBAND1, PRICEBAND2, PRICEBAND3, PRICEBAND4, PRICEBAND5, PRICEBAND6, 
                              PRICEBAND7, PRICEBAND8, PRICEBAND9, PRICEBAND10)
    
    temp4 <- merge(dispatch, bids, by = 'INTERVAL_DATETIME')
    
    for (i in 1:nrow(temp4)){
        j <- 4
        while ((sum(temp4[i, 4:j]) < temp4[i,2]) & (j<=12)) {#find col where colsum > maxavail
            j <- j + 1
        }
        temp4[i, 'MAXBAND']<-bands[1,j-3]
    }
    return(temp4)
}

###RRP
rrp.fun <- function(yearmonth){
    external.data.location <- "D:/Thesis/Data/RRP" #for big data
    year <- substr(yearmonth, 1, 4)
    month <- substr(yearmonth, 5, 6)
    url <- 0
    csv.name <- paste0(external.data.location, "/PUBLIC_DVD_DISPATCHPRICE_", yearmonth, "010000.CSV")
    if(!file.exists(csv.name)){
        url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                      year, "_", month, 
                      "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHPRICE_",yearmonth,
                      "010000.zip")
        temp <- tempfile()
        download.file(url, temp, mode="wb", method = "curl")
        unzip(temp, paste0("PUBLIC_DVD_DISPATCHPRICE_", yearmonth, "010000.CSV"),
              exdir = external.data.location)
    }
    rrp <- fread(csv.name, sep=",", stringsAsFactors = FALSE) %>% 
        filter(INTERVENTION == 0) %>% 
        select(SETTLEMENTDATE, REGIONID, RRP) %>% 
        mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))
    
    if(url != 0){
        unlink(temp) #delete zip
    }    
    return(rrp)
}

loop.fun <- function(fun,...){
    vars <- as.character(as.list(match.call()))[-1]
    
    temp2 <- paste0(vars[-1], sep = ",", collapse = "")
    temp2 <- substr(temp2, 1, (nchar(temp2)-1))
    fun <- paste0(vars[1],"(",temp2,")")
    eval(parse(text = fun))
}

#Input: dates = list of rebid datetimes, ints = list of intervals
#output: vector of logical which says whether the rebid lies within any of the intervals
within_ints <- function(dates, ints){
    out <- c()
    for (i in 1:length(dates)){
        out[i] <- any(dates[i] %within% ints)
    }
    return(dates[out])
}

#Removes all rebids that occur before the date we are looking at except the initial one
#date is chosen from data, checks if all data has same settlement date
#input: bid_data = data from various generators within the same settlement date
remove.old.rebids.fun <- function(data){
    #check if all the same settlementdate
    if(all((data %>% select(SETTLEMENTDATE) %>% 
            unique() %>% dim()) != c(1,1))) {#check if only one date in dataset
        stop()
    }
    
    date <- data %>% select(SETTLEMENTDATE) %>% unique() %>% 
        as.character() %>% as.Date()
    
    datetime <- paste0(date, "04:05:00 AEST")#convert date to NEM start time
    dates <- data %>% transmute(OFFERDATE = as.POSIXct(OFFERDATE)) %>% 
        unique() %>% as.data.frame() %>% .[,1] #get list of dates rebid
    initial <- dates[which(dates < date) %>% max()] #get last rebid before day starts
    out <- data %>% filter(OFFERDATE >= initial)#only keep rebids that occur at or before initial
    return(as.data.frame(out))
}

#remove old and duplicate bids for various generators
#input: bid_data = data from various generators within the same settlement date
clean.bids <- function(bid_data){
    temp <- bid_data %>% group_split(DUID) %>% #split by generator
        map(~ remove.old.rebids.fun(.x)) %>%  #remove old rebids
        map(~ group_split(.x, OFFERDATE)) #split by offerdate
    
    temp2<- modify_depth(temp, 2,  ~ select(.x, -OFFERDATE)) %>% #remove offerdate
        map( ~ !duplicated(.x))#get list of list of duplicate dfs
    
    temp3 <- map2(temp, (1:length(temp)), 
                  function(x,y) x[temp2[[y]]]) %>% #remove duplicate dfs
        map(~ bind_rows(.x)) %>% #bind all bids within generator
        bind_rows() %>% #bind all generators
        as.data.frame
    return(temp3)
}

#read and clean mpa files
#input: csv file name
#output: only MPA data
mpa.clean.fun <- function(file){
    out <- read.csv(file, sep=",", header = F, stringsAsFactors = FALSE) 
    if(nrow(out)==0){#delete empty dfs with no Data
        out <- NULL
    } 
    return(out)
}

#merge all mpa files within one day
#input: date
#output: df of entire day
mpa.fun <- function(DATE){
    temp <- tempfile()#zip location
    temp2 <- tempfile()#unzipped location
    external.data.location <- "D:/Thesis/Data/MPA" #for big data
    url <- paste0("http://nemweb.com.au/Reports/Archive/DispatchIS_Reports/PUBLIC_DISPATCHIS_", DATE, ".zip")
    download.file(url, temp, mode="wb", method = "curl")
    unzip(temp, exdir = temp2)#unzip outter file
    temp3 <- paste0(temp2,"\\",list.files(temp2))#all subzips
    temp3 %>% map(~ unzip(.x, exdir = external.data.location)) %>% invisible()#unzip sub files
    temp4 <- paste0(external.data.location,"/",list.files(external.data.location))#all csvs
    temp5 <- map(temp4, mpa.clean.fun) %>% 
        bind_rows()#read, clean and bind all csvs
    return(temp5)
    unlink(temp)
    unlink(temp2)
}   

###RESTART
#restart session and clear ram
restart<- function(){
    gc()
    rm(list = ls())
    .rs.restartR()    
}