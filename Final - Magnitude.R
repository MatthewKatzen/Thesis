# Magnitude
# Is congestion an issue?
# How often is the network congested?
# What states?
# Wat gens were most congested?

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
        end_val = "2019-07-01 00:00:00") %>% replace(is.na(.), 0)#add 0 if not congested

#by year table
congested %>% group_by(year = floor_date(settlementdate, "year")) %>% 
    summarise(perc = sum(constrained)/n()) %>% fwrite("Output/congestion by year.csv")

#graph by month, grouped by state
congested_state <- mpa %>% select(settlementdate, regionid) %>% unique() %>% mutate(constrained = 1) %>% 
    pad(interval = "5 min", start_val = ymd_hms("2009-07-01 00:05:00"), 
        end_val = ymd_hms("2019-07-01 00:00:00"), break_above = 10000000, group = "regionid") %>% 
    replace(is.na(.), 0)#add 0 if not congested

congested_state %>% group_by(year = floor_date(settlementdate, "year"), regionid) %>% 
    summarise(perc = sum(constrained)/n()) %>% 
    ggplot(aes(x = year, y = perc, colour = regionid, group = regionid))+
    geom_line(size = 2)+
    facet_wrap(~regionid) +
    labs(title = "Proportion of time state is congested", y = "Proportion", x = "Year")+
    ylim(0,0.5) +
    ggsave("Output/Congested_state.png")

    