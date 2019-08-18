library(tidyverse)
library(lubridate)
Sys.setenv(TZ="Australia/Melbourne")


#unzip mpa
from <- "20180710"
to <- "20190808"
dates <- seq(ymd(from), ymd(to), by = "day") %>% #list of dates
    str_replace_all("-", "")#remove all `-`

dates %>% map(~ mpa.unzip(.x)) 

mpa.unzip <- function(DATE){
    temp <- tempfile()#zip location
    temp2 <- tempfile()#unzipped location
    external.data.location <- "D:/Thesis/Data/MPA" #for big data
    url <- paste0("http://nemweb.com.au/Reports/Archive/DispatchIS_Reports/PUBLIC_DISPATCHIS_", DATE, ".zip")
    download.file(url, temp, mode="wb")
    unzip(temp, exdir = temp2)#unzip outter file
    temp3 <- paste0(temp2,"\\",list.files(temp2))#all subzips
    temp3 %>% map(~ unzip(.x, exdir = external.data.location)) %>% invisible()#unzip sub files
    unlink(temp)
    unlink(temp2)
}   

#check missing files
from <- "20180710 00:10"
to <- "20190808 00:10"
dates <- seq(ymd_hm(from), ymd_hm(to), by = "5 min") %>% #list of dates
    str_remove_all("-") %>% #remove all `-`
    str_remove_all(":") %>% 
    str_remove_all(" ") %>% 
    str_remove_all("UTC") %>% 
    substr(1, 12)

files <- paste0(external.data.location, "/", list.files(external.data.location))
files.check <- substr(files, 38,49)

missing <- which(!(dates %in% files.check)) %>% dates[.]#missing datetimes

missing.dates <- substr(missing, 1, 8) %>% unique() #missing dates

missing.dates %>% map(~ mpa.unzip(.x)) #download missing dates

#manual check
#all missing data indeed is missing from zip files

#remove duplicate files
files %>% duplicated() %>% which() #no duplicates

#merge mpa files into chucks of 1000
external.data.location <- "D:/Thesis/Data/MPA" #for big data
files <- paste0(external.data.location, "/", list.files(external.data.location, pattern = "*.CSV"))

splits <- c(seq(1, length(files), 1000), length(files)+1)

for (i in 1:(length(splits)-1)){
    file_name <- paste0("D:/Thesis/Data/MPA/Merged/mpa", i, ".csv")
    mpa.current <- files[(splits[i]): (splits[i+1]-1)] %>% 
        map(~ read.csv(.x, sep=",", header = F, stringsAsFactors = FALSE)) %>% 
        bind_rows()
    write.csv(mpa.current, file_name)
}


#clean to only get mpa

for (i in 1:(length(splits)-1)){
    mpa <- read.csv(paste0("D:/Thesis/Data/MPA/Merged/mpa",i,".csv"), stringsAsFactors = FALSE)
    mpa <- mpa %>% 
        filter(V3 == "LOCAL_PRICE") %>% 
        filter(V5 != "SETTLEMENTDATE") %>% 
        select(c(6:8))  %>% 
        setNames(c("SETTLEMENTDATE", "DUID", "MPA")) %>% 
        mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE), DUID = as.character(DUID), MPA = as.numeric(MPA))
    file_name <- paste0("D:/Thesis/Data/MPA/Merged/Cleaned/mpa_cleaned", i, ".csv")
    write.csv(mpa, file_name)    
}

#Bind all cleaned files
files <- paste0("D:/Thesis/Data/MPA/Merged/Cleaned/", list.files("D:/Thesis/Data/MPA/Merged/Cleaned/"))
mpa.complete <- files %>% map(~ read.csv(.x, stringsAsFactors = FALSE)) %>% 
    bind_rows() %>% select(-1)

write.csv(mpa.complete, "D:/Thesis/Data/MPA/Merged/Cleaned/mpa_complete.csv")



#get MPA
mpa <- read.csv("D:/Thesis/Data/MPA/Merged/Cleaned/mpa_complete.csv", stringsAsFactors = FALSE) %>% select(-1) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    distinct()#heaps of duplicates for some reason


#get gen fuel types
fuel <- read.csv("data/dontupload/GenFuelTypes.csv", stringsAsFactors = FALSE) %>% 
    select(DUID, Region, Classification, 7:9) #remove extra detailed cols
colnames(fuel) <- str_remove_all(colnames(fuel), c("[.]"))

#get rrp
external.data.location <- "D:/Thesis/Data" #for big data
yearmonth <- c("201807","201808","201809","201810","201811","201812","201901","201902","201903","201904","201905","201906","201907")
rrp <- yearmonth %>% map(~ rrp.fun(.x)) %>% bind_rows() %>% 
    mutate(Region = REGIONID) %>% select(-REGIONID) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))



#merge datasets
mpa_comb <- mpa %>% merge(fuel, by = "DUID") %>% 
    inner_join(rrp, by = c("SETTLEMENTDATE", "Region"))

write.csv(mpa_comb, "D:/Thesis/Data/MPA/Merged/Cleaned/mpa_combined.csv")


#repeated rows?

temp <- mpa %>% select(SETTLEMENTDATE, DUID) %>% duplicated() %>% which() %>% mpa[.,]
temp %>% head()

mpa %>% filter(SETTLEMENTDATE == ymd_hms("2018-07-10 00:10:00"))

#mpa_comb fucking up
mpa_comb <- mpa %>% merge(fuel, by = "DUID")

mpa %>% filter(SETTLEMENTDATE == ymd_hms("2018-12-19 02:55:00"), DUID == "AGLHAL")

mpa_comb %>% filter(SETTLEMENTDATE == ymd_hms("2018-12-19 02:55:00"), DUID == "AGLHAL")
head(fuel)

mpa %>% head()
fuel %>% head()
