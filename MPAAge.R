library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)

weekly <- fread("D:/Thesis/Data/weekly_data_construct_2.csv", stringsAsFactors = FALSE)
colnames(disp_weekly)
weekly_cleaned <- weekly %>% select(Year, Week, station_name, output_week) %>% 
    mutate(DATE = ymd(str_c(Year, "-01-01")) + weeks(Week - 1)) %>% 
    filter(output_week != 0) %>% 
    group_by(station_name) %>% 
    summarise(start = sort(DATE)[1])


        