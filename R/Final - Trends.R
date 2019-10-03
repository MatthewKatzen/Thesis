# Trends

### TOTAL REVENUE CHANGES 

mpa_year_fuel_region <- mpa %>%  filter(settlementdate < ymd_hms("2019-01-01 00:05:00 UTC"), 
                                         settlementdate > ymd_hms("2010-01-01 00:00:00 UTC")) %>% 
    group_by(year = floor_date(settlementdate, "year"), fuel_type, state) %>% 
    summ_all() %>% 
    pad(group = c("state", "fuel_type")) %>% replace(is.na(.), 0)#add missing rows of 0


#dif_total year, FUEL/STATE
mpa_year_fuel_region %>% 
    ggplot(aes(x = year, y = dif_total/1000000, colour = fuel_type)) +
    geom_line(size = 1.5) +
    facet_wrap(~ state) +
    ggtitle("Total Overcompensation by Year")+
    xlab("Year") +
    ylab("Total Overcompensation ($m)") +
    labs(colour = "Fuel Type") +
    ggsave("Output/Charts/Total Rev LMP.png", width = 10, height = 5)

#only SA
mpa_year_fuel_region %>% filter(state == "SA") %>% 
    ggplot(aes(x = year, y = dif_total/1000000000, colour = fuel_type)) +
    geom_line(size = 3) +
    facet_wrap(~ state) +
    ggtitle("Total Overcompensation by Year")+
    xlab("Year") +
    ylab("Total Overcompensation ($m)") +
    labs(colour = "Fuel Type")+
    ggsave("Output/Charts/Total Rev LMP SA.png", width = 10, height = 5)

#dif_total_0 year FUEL/STATE
mpa_year_fuel_region %>% 
    ggplot(aes(x = year, y = dif_total_0/1000000000, colour = fuel_type)) +
    geom_line(size = 1.5) +
    facet_wrap(~ state) +
    ggtitle("Total Revenue Change in switch to LMP")+
    xlab("Year") +
    ylab("Revenue Change ($m)") +
    labs(colour = "Fuel Type") +
           ggsave("Output/Charts/Total Rev LMP0.png", width = 10, height = 5)


### AVE REVENUE

mpa_year_fuel <- mpa %>% group_by(year = floor_date(settlementdate, "year"), fuel_type) %>% 
    summ_all() %>% 
    pad(group = c("fuel_type")) %>% replace(is.na(.), 0)#add missing rows of 0

#Dif_Ave year FUEL
mpa_year_fuel %>% 
    ggplot(aes(x = year, y = dif_ave, group = fuel_type, colour = fuel_type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP") 

#without liquid fuel (outlier)
mpa_year_fuel %>% filter(fuel_type != "Liquid Fuel") %>% 
    ggplot(aes(x = year, y = dif_ave, group = fuel_type, colour = fuel_type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP - No Liquid Fuel Outlier")

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

### WIND AGE

mpa_year_fuel_age <- mpa %>% 
    group_by(year = floor_date(settlementdate, "year"), fuel_type, age, state) %>% 
    summ_all()

#SA wind becoming less valuable to market over time (only when congested)
mpa_year_fuel_age %>% filter(fuel_type == "Wind", state == "SA") %>% 
    filter(year > ymd_hms("2013-01-01 00:00:00")) %>% 
    filter(year < ymd_hms("2019-01-01 00:00:00")) %>% 
    ggplot(aes(x = year, y = dif_ave, group = age, colour = age)) + 
    geom_line(size = 2) + 
    facet_wrap(~ state) +
    xlab("Year") +
    ylab("Average Overcompensation ($/MWh)") +
    labs(colour = "Commission Year") +
    scale_color_gradient(low = "blue", high = "red", breaks = seq(2007,2018,3)) +
    ggtitle("Average Overcompensation ($/MWh) - South Australian Wind")+
    ggsave("Output/Charts/Ave Rev SA Wind.png", width = 10)

mpa_year_fuel_age %>% filter(fuel_type == "Wind", state == "SA") %>% 
    filter(year > ymd_hms("2013-01-01 00:00:00")) %>% 
    filter(year < ymd_hms("2019-01-01 00:00:00")) %>% 
    ggplot(aes(x = year, y = dif_ave_0, group = age, colour = age)) + 
    geom_line(size = 2) +
    facet_wrap(~ state)


mpa %>% filter(state == "SA", fuel_type == "Wind") %>% filter(year(settlementdate) == 2018) %>% 
    group_by(duid) %>% summarise(sum = sum(totalcleared), age = age[1]) %>% arrange(age)

#wind production in SA increasing
mpa %>% filter(state == "SA", fuel_type == "Wind") %>% 
    group_by(year = floor_date(settlementdate, "year")) %>% 
    summarise(Output = sum(dispatchmwh)/1000, fuel_type = "Wind") %>% 
    filter(year < ymd_hms("2019-01-01 00:00:00")) %>% 
    filter(year > ymd_hms("2009-01-01 00:00:00")) %>% 
    ggplot(aes(x = year, y = Output))+
    geom_line(size = 2, colour = "blue") +
    xlab("Year")+
    ylab("GWh")+
    facet_wrap(~fuel_type) +
    ggtitle("Quantity Produced by South Australian Wind Farms in MWh whilst Congested")+
    ggsave("Output/Charts/SA Wind Output.png", width = 10)

#new generators being built
SA_age <- mpa %>% filter(state == "SA", fuel_type == "Wind") %>% 
    select(station, age) %>% unique() %>% arrange(age) 
fwrite(SA_age, "Output/Wind SA Age.csv")

#get total wind gen
library(reshape2)
gen <- fread("D:/Thesis/Data/DISPATCH/yearly/dispatch_by_year.csv") %>% melt(id.vars = 'Year')
SA_wind_total <- gen %>% filter(variable %in% SA_age$station) %>% 
    group_by(Year) %>% summarise(Total = sum(value)) 

SA_wind_con <- mpa %>% filter(state == "SA", fuel_type == "Wind", lmp<rrp30, dispatchmwh > 0) %>% 
    group_by(Year = year(floor_date(settlementdate, "year"))) %>% summarise(Con = sum(dispatchmwh)/1000) %>% 
    filter(Year < 2019) %>% 
    filter(Year > 2009) 

merge(SA_wind_total, SA_wind_con, by = "Year") %>% mutate(Uncon = Total - Con) %>% melt(id.vars = "Year") %>% 
    ggplot(aes(x = Year, y = value, group = variable, colour = variable))+
    geom_line()
head(mpa)    


