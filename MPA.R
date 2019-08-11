library(tidyverse)
#unzip
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

#get gen fuel types
fuel <- read.csv("data/dontupload/GenFuelTypes.csv")
fuel <- fuel %>% select(DUID, Region, Classification, 7:10)
head(fuel)

#get rrp
rrp <- rrp.fun("201807")
head(rrp)

#load as mpa
external.data.location <- "D:/Thesis/Data/MPA" #for big data
files <- paste0(external.data.location, "/", list.files(external.data.location))

splits <- c(seq(1, length(files), 1000), length(files)+1)

for (i in 26:(length(splits)-1)){
    file_name <- paste0("D:/Thesis/Data/MPA/Merged/mpa", i, ".csv")
    mpa.current <- files[(splits[i]): (splits[i+1]-1)] %>% 
        map(~ read.csv(.x, sep=",", header = F, stringsAsFactors = FALSE)) %>% 
        bind_rows()
    write.csv(mpa.current, file_name)
}


head(mpa.current)

write.csv(mpa.current, "data/mpa.current.csv")
mpa.current <- read.csv("data/mpa.current.csv")