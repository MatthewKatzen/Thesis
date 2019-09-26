# Trends

### TOTAL REVENUE CHANGES 

mpa_month_fuel_region <- mpa %>%
    group_by(month = floor_date(settlementdate, "month"), fuel_type, state) %>% 
    summ_all() %>% 
    pad(group = c("state", "fuel_type")) %>% replace(is.na(.), 0)#add missing rows of 0


#dif_total month, FUEL/STATE
mpa_month_fuel_region %>% 
    ggplot(aes(x = month, y = dif_total, colour = fuel_type)) +
    geom_line(size = 1.5) +
    facet_wrap(~ state) +
    ggtitle("Total Revenue Change in swith to LMP - Grouped by Fuel Type and State")+
    xlab("") +
    ylab("Rev Change") +
    ggsave("Output/Charts/Total Rev LMP.png", width = 10, height = 5)

#dif_total_0 month FUEL/STATE
mpa_month_fuel_region %>% 
    ggplot(aes(x = month, y = dif_total_0, colour = fuel_type)) +
    geom_line(size = 1.5) +
    facet_wrap(~ state) +
    ggtitle("Total Revenue Change in swith to LMP - Assume LMP cannot be Neg")+
    xlab("") +
    ylab("Rev Change") +
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
    scale_color_gradient(low = "blue", high = "red") + 
    facet_wrap(~ state) +
    ggtitle("Average Revenue Increase in switch to LMP - South Australian Wind")+
    ggsave("Output/Charts/Ave Rev SA Wind.png", width = 10)

mpa_year_fuel_age %>% filter(fuel_type == "Wind", state == "SA") %>% 
    filter(year > ymd_hms("2013-01-01 00:00:00")) %>% 
    filter(year < ymd_hms("2019-01-01 00:00:00")) %>% 
    ggplot(aes(x = year, y = dif_ave_0, group = age, colour = age)) + 
    geom_line(size = 2) +
    scale_color_gradient(low = "blue", high = "red") +
    facet_wrap(~ state)


mpa %>% filter(state == "SA", fuel_type == "Wind") %>% filter(year(settlementdate) == 2018) %>% 
    group_by(duid) %>% summarise(sum = sum(totalcleared), age = age[1]) %>% arrange(age)

#wind production in SA increasing
mpa %>% filter(state == "SA", fuel_type == "Wind") %>% 
    group_by(year = floor_date(settlementdate, "year")) %>% summarise(Output = sum(totalcleared), fuel_type = "Wind") %>% 
    filter(year < ymd_hms("2019-01-01 00:00:00")) %>% 
    ggplot(aes(x = year, y = Output))+
    geom_line(size = 2, colour = "blue") +
    facet_wrap(~fuel_type) +
    ggtitle("Quanity Produced by Wind Farms in South Australia in MWh whilst Congested")+
    ggsave("Output/Charts/SA Wind Output.png", width = 10)

#new generators being built
mpa %>% filter(state == "SA", fuel_type == "Wind") %>% 
    select(station, age) %>% unique() %>% arrange(age) %>% fwrite("Output/Wind SA Age.csv")
