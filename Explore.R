#First exploration of DB 
#install.packages("lubridate")
library(lubridate) # for manipulating time. as.POSXct doesn't like midnight for some reason
library(tidyverse)

###DATA
#Get 2017 data
rhs <- rhs.fun("201701") %>% rbind(rhs.fun("201702")) %>% 
    rbind(rhs.fun("201703")) %>% 
    rbind(rhs.fun("201704")) %>% 
    rbind(rhs.fun("201705")) %>% 
    rbind(rhs.fun("201706")) %>% 
    rbind(rhs.fun("201707")) %>% 
    rbind(rhs.fun("201708")) %>% 
    rbind(rhs.fun("201709")) %>% 
    rbind(rhs.fun("201710")) %>% 
    rbind(rhs.fun("201711")) %>% 
    rbind(rhs.fun("201712"))  
write.csv(rhs, "data/2017rhs.csv") 

#get 2014 to 2018 data
month <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2014, 2015, 2016, 2017, 2018)
rhs <- NULL
for (i in year){
    for (j in month){
        yearmonth <- paste0(i,j)
        rhs <- rbind(rhs, rhs.fun(yearmonth))
    }
}
write.csv(rhs, "data/14-18rhs.csv")


rhs <- read.csv("data/14-18rhs.csv", stringsAsFactors = FALSE) %>% 
    filter(grepl(">", CONSTRAINTID)) %>% #only thermal constraints
    mutate(CONSTRAINTID = as.character(CONSTRAINTID)) %>% #convert to character to get rid of unused levels
    mutate(STATE = substr(CONSTRAINTID, 1, 1)) %>% #create state variable
    mutate(STATE = plyr::mapvalues(STATE, 
                                   c("Q","N","V","S","T"),
                                   c("QLD", "NSW", "VIC", "SA", "TAS"))) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% #use this as.POSIXct removes time for everything as one constains midnight. Note its also in UTC 
    mutate(DATE = as.Date(SETTLEMENTDATE)) %>% 
    mutate(MONTH = month(SETTLEMENTDATE)) %>% 
    mutate(WEEK = week(SETTLEMENTDATE)) %>% 
    mutate(DAY = day(SETTLEMENTDATE)) %>% 
    mutate(HOUR = hour(SETTLEMENTDATE)*60*60)#given in seconds
    
    

#2018
rhs18 <- rhs %>% filter(year(DATE) == "2018")

table(rhs$DATE %>% year()) #6024 events in may
rhs$CONSTRAINTID %>% table() %>% sort(decreasing = TRUE) %>% .[c(1:10)]


### GRAPHS 2018
#number of events per week 
p3 <- ggplot(rhs18, aes(x = WEEK, fill = STATE)) + 
    geom_bar()
p3
#time of day of events split by state
p4 <- ggplot(rhs18, aes(x = HOUR,
                      fill = STATE)) + 
    geom_bar() + 
    facet_wrap(. ~ STATE) +
    scale_x_time()
p4 

#time of day split by state and month
p4.2 <- ggplot(rhs18, aes(x = HOUR, fill = STATE)) + 
    geom_bar() + 
    facet_grid(MONTH ~ STATE) +
    scale_x_time()
p4.2

#HOW HAS THIS CHANGED OVER TIME

p5.1 <- ggplot(rhs %>% filter(STATE == "NSW"),
             aes(x= HOUR))+
    geom_bar()+
    facet_grid(MONTH ~ year(DATE)) +
    scale_x_time()
p5.1

p5.2 <- ggplot(rhs %>% filter(STATE == "QLD"),
             aes(x= HOUR))+
    geom_bar()+
    facet_grid(MONTH ~ year(DATE)) +
    scale_x_time()
p5.2

p5.3 <- ggplot(rhs %>% filter(STATE == "VIC"),
             aes(x= HOUR))+
    geom_bar()+
    facet_grid(MONTH ~ year(DATE)) +
    scale_x_time()
p5.3

p5.4 <- ggplot(rhs %>% filter(STATE == "SA"),
             aes(x= HOUR))+
    geom_bar()+
    facet_grid(MONTH ~ year(DATE)) +
    scale_x_time()
p5.4

p5.5 <- ggplot(rhs %>% filter(STATE == "TAS"),
             aes(x= HOUR))+
    geom_bar()+
    facet_grid(MONTH ~ year(DATE)) +
    scale_x_time()
p5.5



### MARGINAL VALUE
#by week
p5 <- ggplot(rhs %>% 
                 group_by(WEEK, STATE) %>% 
                 summarise(SUM = sum(MARGINALVALUE)),
             aes(x = WEEK, y = SUM, fill = STATE)) +
    geom_bar(stat = "identity")+
    facet_grid( ~ STATE)
p5 

#by time of day split by state and month
p6 <- ggplot(rhs %>% 
                 group_by(HOUR, STATE, MONTH) %>% 
                 summarise(SUM = sum(MARGINALVALUE)),
             aes(x = HOUR, y = SUM, fill = STATE)) +
    geom_bar(stat = "identity") +
    scale_x_time() + 
    theme(axis.text.x = element_text(angle = 270)) +
    facet_grid(MONTH ~ STATE)
p6

#Longest binding constraints are looked at in `Finding Constraints.R`
#Below we will look at worst constraints in terms of = Sum(MV)
temp <- rhs %>% group_by(CONSTRAINTID) %>% 
    summarise(SUM = sum(MARGINALVALUE)) %>% 
    arrange(SUM) %>% 
    select(CONSTRAINTID) %>% 
    as.data.frame() %>% 
    .[(1:50),]


temp2 <- map(.x = temp, 
             .f =  ~(rhs %>% filter(CONSTRAINTID == .x) %>% 
                         select(GENCONID_EFFECTIVEDATE) %>% 
                         table() %>% 
                         as.numeric() %>% 
                         length()) == 1) %>% 
    unlist #get logical of equations with multiple eqs

temp3 <- temp[temp2] #list of constraints with one's that have multiple equations removed 

temp4 <- rhs %>% filter(CONSTRAINTID %in% temp3) %>% select(CONSTRAINTID, GENCONID_EFFECTIVEDATE) %>% unique() %>% 
    mutate(YEARDATE = paste0(substr(GENCONID_EFFECTIVEDATE, 1, 4),
                             substr(GENCONID_EFFECTIVEDATE, 6, 7)))#get constraints and YEARDATES of interest


temp5 <- map2(.x = temp4$CONSTRAINTID, .y = temp4$YEARDATE, ~ eqs.fun(.x, .y))  #get all eqs

#"T>T_TUNN3_110_1" first line of interest (non 1 coefs, )
