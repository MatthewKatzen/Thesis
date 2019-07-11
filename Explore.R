#First exploration of DB 
#install.packages("lubridate")
library(lubridate) # for manipulating time. as.POSXct doesn't like midnight for some reason
library(tidyverse)

###DATA
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

rhs <- read.csv("data/2017rhs.csv") %>% 
    filter(grepl(">", CONSTRAINTID)) %>% #only thermal constraints
    mutate(CONSTRAINTID = as.character(CONSTRAINTID)) %>% #convert to character to get rid of unused levels
    mutate(STATE = substr(CONSTRAINTID, 1, 1)) %>% #create state variable
    mutate(SETTLEMENTDATE = as.POSIXct(SETTLEMENTDATE))

#total constraints
rhs1 <- read.csv("data/201701rhs.csv") %>% 
    filter(grepl(">", CONSTRAINTID)) %>% #only thermal constraints
    mutate(CONSTRAINTID = as.character(CONSTRAINTID)) %>% #convert to character to get rid of unused levels
    mutate(STATE = substr(CONSTRAINTID, 1, 1)) %>% #create state variable
    mutate(SETTLEMENTDATE = as_datetime(SETTLEMENTDATE))

nrow(rhs1) #6024 events in may
rhs1$CONSTRAINTID %>% table() %>% sort(decreasing = TRUE) #table of each constraint binding


### GRAPHS JAN
#number of events per day in january
p1 <- ggplot(rhs1 %>% mutate(SETTLEMENTDATE = as.Date(SETTLEMENTDATE)), 
             aes(x = SETTLEMENTDATE, fill = STATE)) + 
    geom_bar()
p1
#time of day of events
p2 <- ggplot(rhs1 %>% mutate(SETTLEMENTDATE = hour(SETTLEMENTDATE)*60*60),
             aes(x = SETTLEMENTDATE, fill = STATE)) + 
    geom_bar()
p2 + scale_x_time()


### GRAPHS YEAR
#number of events per week 
p3 <- ggplot(rhs %>% mutate(SETTLEMENTDATE = week(SETTLEMENTDATE)), 
             aes(x = SETTLEMENTDATE, fill = STATE)) + 
    geom_bar()
p3
#time of day of events
p4 <- ggplot(rhs %>% mutate(SETTLEMENTDATE = hour(SETTLEMENTDATE)*60*60),
             aes(x = SETTLEMENTDATE, fill = STATE)) + 
    geom_bar()
p4 + scale_x_time()

### MARGINAL VALUE
#by week
p5 <- ggplot(rhs %>% mutate(WEEK = week(SETTLEMENTDATE)) %>% 
                 group_by(WEEK, STATE) %>% 
                 summarise(SUM = sum(MARGINALVALUE)),
             aes(x = WEEK, y = SUM, fill = STATE)) +
    geom_bar(stat = "identity")
p5

    

