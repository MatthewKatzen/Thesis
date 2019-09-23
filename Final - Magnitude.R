# Magnitude
# Is congestion an issue?
# How often is the network congested?
# What states?
# What gens were most congested?

library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(padr)

### Get DATA

mpa <- fread("D:/Thesis/Data/mpa_cleaned.csv") %>% mutate(settlementdate = ymd_hms(settlementdate))

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


### MAGNITUDE

# Congestion
congested <- mpa %>% select(settlementdate) %>% unique() %>% mutate(constrained = 1) %>% 
    pad(interval = "5 min", break_above = 2000000, start_val = ymd_hms("2009-07-01 00:05:00"), 
        end_val = ymd_hms("2019-07-01 00:00:00")) %>% replace(is.na(.), 0)#add 0 if not congested

#what proportion of the time is the network congested (by year)
congested %>% group_by(year = floor_date(settlementdate, "year")) %>% 
    summarise(perc = sum(constrained)/n()) %>% fwrite("Output/congestion by year.csv")

#graph by month, grouped by state
congested_state <- mpa %>% select(settlementdate, state) %>% unique() %>% mutate(constrained = 1) %>% 
    pad(interval = "5 min", start_val = ymd_hms("2009-07-01 00:05:00"), 
        end_val = ymd_hms("2019-07-01 00:00:00"), break_above = 10000000, group = "state") %>% 
    replace(is.na(.), 0)#add 0 if not congested

congested_state %>% group_by(year = floor_date(settlementdate, "year"), state) %>% 
    summarise(perc = sum(constrained)/n()) %>% 
    ggplot(aes(x = year, y = perc, colour = state, group = state))+
    geom_line(size = 2)+
    facet_wrap(~state) +
    labs(title = "Proportion of time state is congested", y = "Proportion", x = "Year")+
    ylim(0,0.5) + 
    theme(legend.position = "none") +
    ggsave("Output/Congested_state.png", width = 10)

#most congested generators
congested_duid <-  mpa %>% filter(year(settlementdate) == 2018, (lmp < rrp30)) %>% 
    select(settlementdate, duid, totalcleared, station, fuel_type) %>% 
    group_by(duid) %>% summarise(count = n(), station = station[1], fuel_type = fuel_type[1]) %>% 
    arrange(-count) %>% mutate(perc = count/(12*24*365))

#most congested generators removing intervals where didn't produce
congested_duid_2 <-  mpa %>% filter(year(settlementdate) == 2018, (lmp < rrp30), totalcleared > 0) %>% 
    select(settlementdate, duid, totalcleared, station, fuel_type) %>% 
    group_by(duid) %>% summarise(count = n(), station = station[1], fuel_type = fuel_type[1]) %>% 
    arrange(-count) %>% mutate(perc = count/(12*24*365)) 

congested_duid_2 %>% .[1:10,] %>% fwrite("Output/congestion by duid top 10.csv")


#congestion by generator type
#at any point in time, what is the chance that at least 1 generator of that fuel type is congested
congested_fuel <- mpa %>%  
    filter(lmp < rrp30, totalcleared > 0) %>% #constrained off and producing
    select(settlementdate, fuel_type) %>% 
    unique() %>% mutate(constrained = 1) %>% 
    pad(interval = "5 min", start_val = ymd_hms("2009-07-01 00:05:00"), 
        end_val = ymd_hms("2019-07-01 00:00:00"), break_above = 10000000, group = "fuel_type") %>% 
    replace(is.na(.), 0) %>% #add 0 if not congested
    group_by(year = floor_date(settlementdate, "year"), fuel_type) %>% 
    summarise(perc = sum(constrained)/n()) 
    

congested_fuel %>% 
    ggplot(aes(x = year, y = perc, colour = fuel_type, group = fuel_type))+
    geom_line(size = 1.5)+
    facet_wrap(~fuel_type) +
    labs(title = "Proportion of time at least one generator is constrained off", y = "Proportion", x = "Year")+
    ylim(0,0.5) + 
    theme(legend.position = "none") +
    ggsave("Output/Charts/Congested_fuel.png", width = 10)
