# Trends

### TOTAL REVENUE CHANGES 

mpa_year_fuel_region <- mpa %>%  filter(settlementdate < ymd_hms("2019-01-01 00:00:00 UTC"), 
                                         settlementdate > ymd_hms("2010-01-01 00:00:00 UTC")) %>% 
    group_by(year = floor_date(settlementdate, "year"), fuel_type, state) %>% 
    summ_all() %>% 
    pad(group = c("state", "fuel_type")) %>% replace(is.na(.), 0)#add missing rows of 0


#dif_total year, FUEL/STATE
mpa_year_fuel_region %>% 
    ggplot(aes(x = year, y = dif_total/1000000000, colour = fuel_type)) +
    geom_line(size = 1.5) +
    facet_wrap(~ state) +
    ggtitle("Total Overcompensation ($b) by Year")+
    xlab("Year") +
    ylab("Total Overcompensation ($b)") +
    labs(colour = "Fuel Type") + 
    theme(text = element_text(size = 12))+
    theme(axis.text.x = element_text(angle = -45, hjust = 0))+
    ggsave("Output/Charts/Total Rev LMP.png", width = 10, height = 5)

#only SA
mpa_year_fuel_region %>% filter(state == "SA") %>% 
    ggplot(aes(x = year, y = dif_total/1000000000, colour = fuel_type)) +
    geom_line(size = 3) +
    facet_wrap(~ state) +
    ggtitle("Total Overcompensation ($b) by Year")+
    xlab("Year") +
    ylab("Total Overcompensation ($b)") +
    labs(colour = "Fuel Type")+
    theme(text = element_text(size = 16))+
    theme(axis.text.x = element_text(angle = -45, hjust = 0))+
    ggsave("Output/Charts/Total Rev LMP SA.png", width = 10, height = 5)

#dif_total_0 year FUEL/STATE
mpa_year_fuel_region %>% 
    ggplot(aes(x = year, y = dif_total_0/1000000, colour = fuel_type)) +
    geom_line(size = 1.5) +
    facet_wrap(~ state) +
    ggtitle("Adjusted Total Overcompensation ($m) by Year")+
    xlab("Year") +
    ylab("Adjusted Total Overcompensation ($m)") +
    theme(text = element_text(size = 12))+
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

mpa_year_fuel_age <- mpa %>% filter(fuel_type == "Wind", state == "SA") %>% 
    group_by(year = floor_date(settlementdate, "year"), duid, state, age, station) %>% 
    summ_all()

#SA wind becoming less valuable to market over time (only when congested)
mpa_year_fuel_age %>% 
    filter(year > ymd_hms("2013-01-01 00:00:00")) %>% 
    filter(year < ymd_hms("2019-01-01 00:00:00")) %>% 
    ggplot(aes(x = year, y = dif_ave, group = duid, colour = age)) + 
    geom_line(size = 2) + 
    facet_wrap(~ state) +
    xlab("Year") +
    ylab("Average Overcompensation ($/MWh)") +
    labs(colour = "Commission Year") +
    scale_color_gradient(low = "blue", high = "red", breaks = seq(2007,2018,3)) +
    ggtitle("Average Overcompensation ($/MWh) - South Australian Wind Farm")+
    theme(text = element_text(size = 12))+
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
    theme(text = element_text(size = 12))+
    ggsave("Output/Charts/SA Wind Output.png", width = 10)


#NEW generators gens take 2
age <- fread("D:/Thesis/Data/age_gen.csv") %>% clean_names()
duid_details <- fread("D:/Thesis/Data/duid_details_clean.csv") %>% 
    mutate(state = case_when(REGIONID == "QLD1" ~ "QLD",
                             REGIONID == "NSW1" ~ "NSW",
                             REGIONID == "VIC1" ~ "VIC",
                             REGIONID == "SA1" ~ "SA",
                             REGIONID == "TAS1" ~ "TAS")) %>% 
    clean_names() %>% 
    select(station, state, fuel_type) %>% 
    unique()
SA_age <- merge(duid_details, age, by = "station") %>% filter(state == "SA", fuel_type == "Wind") %>% 
    select(station, age) %>% arrange(age)
fwrite(SA_age, "Output/Wind SA age.csv")

#SA wind generation
library(reshape2)
dispatch <- fread("D:/Thesis/Data/DISPATCH/yearly/dispatch_by_year.csv") %>% melt(id.vars = 'Year')
SA_wind_dispatch <- dispatch %>% filter(variable %in% SA_age$station) %>% 
    group_by(Year) %>% summarise(Total = sum(value)) 

SA_wind_constrained <- mpa %>% filter(state == "SA", fuel_type == "Wind", lmp<rrp30, dispatchmwh > 0) %>% 
    group_by(Year = year(floor_date(settlementdate, "year"))) %>% summarise(Con = sum(dispatchmwh)/1000) %>% 
    filter(Year < 2019) %>% 
    filter(Year > 2009) 

merge(SA_wind_dispatch, SA_wind_constrained, by = "Year") %>% mutate(Uncon = Total - Con) %>% melt(id.vars = "Year") %>% 
    ggplot(aes(x = Year, y = value, colour = variable))+
    geom_line(size = 2)+
    scale_colour_discrete(name = "", limits=c("Total", "Uncon", "Con"),
                          labels = c("Total", "Unconstrained", "Constrained"))+
    ylab("MWh")+
    scale_y_continuous(breaks = seq(0,6000,1000)) +
    ggtitle("Wind Generation in South Australia") +
    theme(text = element_text(size = 12))+
    ggsave('Output/Charts/SA Wind Production.png', width = 7)
    
#SA wind farm capacity
capacity_factor <- fread("Output/Capacity Factor.csv") %>% melt(id.vars = "Year")
capacity_total <- merge(capacity_factor, dispatch, by = c("Year","variable")) %>% 
    filter(variable %in% SA_age$station) %>% 
    mutate(capacity = (value.y/8760)*(100/value.x)*1000) %>% 
    mutate(capacity = ifelse(is.na(capacity),
                             0,
                             capacity))

capacity_total %>% group_by(Year) %>% summarise(sum = sum(capacity)) %>% 
    ggplot(aes(x = Year, y = sum)) + 
    geom_line(size=2) +
    xlab("Year") +
    ylab("MW")+
    ggtitle("Wind Farm Capacity in South Australian")+
    theme(text = element_text(size = 12))+
    ggsave('Output/Charts/SA Wind Capacity.png', width = 6)

capacity_total %>% select(-c(value.x, value.y)) %>% arrange(variable)
