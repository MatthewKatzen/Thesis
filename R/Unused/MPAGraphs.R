library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(padr)
#Create time series of Total Revenue, Average Revenue
#Grouped by Fuel Type, and Fuel Type + State


### CREATE DF

#add all sumarised cols of interest
#input: grouped df
#output: agregate summares
summ_all <- function(df){
    df %>% 
        summarise(quantity = sum(totalcleared),
                  ave_rev = ifelse(sum(totalcleared)>0,
                                   sum(rev_rrp_30)/sum(totalcleared),
                                   NA),
                  ave_rev_lmp = ifelse(sum(totalcleared)>0,
                                       sum(rev_lmp)/sum(totalcleared),
                                       NA),
                  ave_rev_lmp0 = ifelse(sum(totalcleared)>0,
                                        sum(rev_lmp0)/sum(totalcleared),
                                        NA),
                  total_rev_rrp = sum(rev_rrp_30),
                  total_rev_lmp = sum(rev_lmp),
                  total_rev_lmp0 = sum(rev_lmp0),
                  dif_ave = ave_rev_lmp - ave_rev,
                  dif_ave_0 = ave_rev_lmp0 - ave_rev,
                  dif_total = total_rev_lmp - total_rev_rrp,
                  dif_total_0 = total_rev_lmp0 - total_rev_rrp)
}

### Get DATA

mpa <- fread("D:/Thesis/Data/mpa_cleaned.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))

#month/Fuel Type
mpa_month_fuel <- mpa %>% 
    group_by(month = floor_date(settlementdate, "month"), fuel_type) %>% 
    summ_all() %>% 
    pad(group = c("fuel_type")) %>% replace(is.na(.), 0)#add missing rows of 0

#month/Fuel Type + STATE
mpa_month_fuel_region <- mpa %>%
    group_by(month = floor_date(settlementdate, "month"), fuel_type, state) %>% 
    summ_all() %>% 
    pad(group = c("state", "fuel_type")) %>% replace(is.na(.), 0)#add missing rows of 0

#year/Fuel Type
mpa_year_fuel <- mpa %>% group_by(year = floor_date(settlementdate, "year"), fuel_type) %>% 
    summ_all() %>% 
    pad(group = c("fuel_type")) %>% replace(is.na(.), 0)#add missing rows of 0


#year/Fuel Type + STATE
mpa_year_fuel_state <- mpa %>% group_by(year = floor_date(settlementdate, "year"), fuel_type, state) %>% 
    summ_all() %>% 
    pad(group = c("state", "fuel_type")) %>% replace(is.na(.), 0)#add missing rows of 0


### GRAPHS


#dif_total month, FUEL/STATE
mpa_month_fuel_region %>% 
    ggplot(aes(x = month, y = dif_total, colour = fuel_type)) +
    geom_line(size = 2) +
    facet_wrap(~ state) +
    ggtitle("Total Revenue Change in swith to LMP - Grouped by Fuel Type and State")+
    xlab("") +
    ylab("Rev Change")

#dif_total_0 month FUEL/STATE
mpa_month_fuel_region %>% 
    ggplot(aes(x = month, y = dif_total_0, colour = fuel_type)) +
    geom_line(size = 2) +
    facet_wrap(~ state) +
    ggtitle("Total Revenue Change in swith to LMP - Assume LMP cannot be Neg")+
    xlab("") +
    ylab("Rev Change")

#dif_total year FUEL
mpa_year_fuel %>% 
    ggplot(aes(x = year, y = dif_total, colour = fuel_type)) +
    geom_line(size = 2) +
    ggtitle("Total Revenue Change in swith to LMP - Assume LMP cannot be Neg")+
    xlab("") +
    ylab("Rev Change")

#dif_total_0 year FUEL
mpa_year_fuel %>% 
    ggplot(aes(x = year, y = dif_total_0, colour = fuel_type)) +
    geom_line(size = 2) +
    ggtitle("Revenue Change in swith to LMP - Assume LMP cannot be Neg")+
    xlab("") +
    ylab("Rev Change")





#Dif_Ave year FUEL
mpa_year_fuel %>% 
    ggplot(aes(x = year, y = dif_ave, group = fuel_type, colour = fuel_type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP") 

#without liquid fuel (outlier)
mpa_year_fuel %>% filter(fuel_type != "Liquid Fuel") %>% 
    ggplot(aes(x = year, y = dif_ave, group = fuel_type, colour = fuel_type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP - No Liquid Fuel")

#dif_ave_0
mpa_year_fuel %>% 
    ggplot(aes(x = year, y = dif_ave_0, group = fuel_type, colour = fuel_type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in swicth to LMP0 (No neg LMP)")

#without liquid fuel (outlier)
mpa_year_fuel %>% filter(fuel_type != "Liquid Fuel") %>% 
    ggplot(aes(x = year, y = dif_ave_0, group = fuel_type, colour = fuel_type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP0 (No neg LMP) - No Liquid Fuel")

