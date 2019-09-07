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
#output: agregate summires
summ_all <- function(df){
    df %>% 
        summarise(Quanity = sum(TOTALCLEARED),
                  Ave_Rev = ifelse(sum(TOTALCLEARED)>0,
                                   sum(Rev_RRP_30)/sum(TOTALCLEARED),
                                   NA),
                  Ave_Rev_LMP = ifelse(sum(TOTALCLEARED)>0,
                                       sum(Rev_LMP)/sum(TOTALCLEARED),
                                       NA),
                  Ave_Rev_LMP0 = ifelse(sum(TOTALCLEARED)>0,
                                        sum(Rev_LMP0)/sum(TOTALCLEARED),
                                        NA),
                  Total_Rev_RRP = sum(Rev_RRP_30),
                  Total_Rev_LMP = sum(Rev_LMP),
                  Total_Rev_LMP0 = sum(Rev_LMP0),
                  Dif_Ave = Ave_Rev_LMP - Ave_Rev,
                  Dif_Ave_0 = Ave_Rev_LMP0 - Ave_Rev,
                  Dif_Total = Total_Rev_LMP - Total_Rev_RRP,
                  Dif_Total_0 = Total_Rev_LMP0 - Total_Rev_RRP)
}

### Get DATA

mpa <- fread("D:/Thesis/Data/COMPLETE/mpa_age.csv") %>% mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))

#MONTH/Fuel Type
mpa_month_fuel <- mpa %>% 
    group_by(MONTH = floor_date(SETTLEMENTDATE, "month"), Fuel.Type) %>% 
    summ_all() %>% 
    pad(group = c("Fuel.Type")) %>% replace(is.na(.), 0)#add missing rows of 0

#MONTH/Fuel Type + STATE
mpa_month_fuel_region <- mpa %>% 
    group_by(MONTH = floor_date(SETTLEMENTDATE, "month"), Fuel.Type, REGIONID) %>% 
    summ_all() %>% 
    pad(group = c("REGIONID", "Fuel.Type")) %>% replace(is.na(.), 0)#add missing rows of 0

#YEAR/Fuel Type
mpa_year_fuel <- mpa %>% group_by(YEAR = floor_date(SETTLEMENTDATE, "year"), Fuel.Type) %>% 
    summ_all() %>% 
    pad(group = c("Fuel.Type")) %>% replace(is.na(.), 0)#add missing rows of 0


#YEAR/Fuel Type + STATE
mpa_year_fuel_state <- mpa %>% group_by(YEAR = floor_date(SETTLEMENTDATE, "year"), Fuel.Type, REGIONID) %>% 
    summ_all() %>% 
    pad(group = c("REGIONID", "Fuel.Type")) %>% replace(is.na(.), 0)#add missing rows of 0


### GRAPHS


#Dif_Total MONTH, FUEL/STATE
mpa_month_fuel_region %>% 
    ggplot(aes(x = MONTH, y = Dif_Total, colour = Fuel.Type)) +
    geom_line(size = 2) +
    facet_wrap(~ REGIONID) +
    ggtitle("Revenue Change in swith to LMP - Grouped by Fuel Type and State")+
    xlab("") +
    ylab("Rev Change")

#Dif_Total_0 MONTH FUEL/STATE
mpa_month_fuel_region %>% 
    ggplot(aes(x = MONTH, y = Dif_Total_0, colour = Fuel.Type)) +
    geom_line(size = 2) +
    facet_wrap(~ REGIONID) +
    ggtitle("Revenue Change in swith to LMP - Assume LMP cannot be Neg")+
    xlab("") +
    ylab("Rev Change")

#Dif_Total MONTH, FUEL/STATE
mpa_month_fuel_region %>% 
    ggplot(aes(x = MONTH, y = Dif_Total, colour = Fuel.Type)) +
    geom_line(size = 2) +
    facet_wrap(~ REGIONID) +
    ggtitle("Revenue Change in swith to LMP - Grouped by Fuel Type and State")+
    xlab("") +
    ylab("Rev Change")

#Dif_Total YEAR FUEL
mpa_year_fuel %>% 
    ggplot(aes(x = YEAR, y = Dif_Total, colour = Fuel.Type)) +
    geom_line(size = 2) +
    ggtitle("Revenue Change in swith to LMP - Assume LMP cannot be Neg")+
    xlab("") +
    ylab("Rev Change")

#Dif_Total_0 YEAR FUEL
mpa_year_fuel %>% 
    ggplot(aes(x = YEAR, y = Dif_Total_0, colour = Fuel.Type)) +
    geom_line(size = 2) +
    ggtitle("Revenue Change in swith to LMP - Assume LMP cannot be Neg")+
    xlab("") +
    ylab("Rev Change")


#Dif_Ave YEAR FUEL
mpa_year_fuel %>% 
    ggplot(aes(x = YEAR, y = Dif_Ave, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP") 

#without liquid fuel (outlier)
mpa_year_fuel %>% filter(Fuel.Type != "Liquid Fuel") %>% 
    ggplot(aes(x = YEAR, y = Dif_Ave_0, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP - No Liquid Fuel")

#Dif_Ave_0
mpa_year_fuel %>% 
    ggplot(aes(x = YEAR, y = Dif_Ave_0, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in swicth to LMP0 (No neg LMP)")

#without liquid fuel (outlier)
mpa_year_fuel %>% filter(Fuel.Type != "Liquid Fuel") %>% 
    ggplot(aes(x = YEAR, y = Dif_Ave_0, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP0 (No neg LMP) - No Liquid Fuel")

