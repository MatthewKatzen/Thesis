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
    mutate(YEAR = year(SETTLEMENTDATE)) %>% 
    mutate(MONTH = month(SETTLEMENTDATE)) %>% 
    mutate(WEEK = week(SETTLEMENTDATE)) %>% 
    mutate(DAY = day(SETTLEMENTDATE)) %>% #not very insightful
    mutate(DOW = factor(weekdays(SETTLEMENTDATE), levels = 
                               c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% #better
    mutate(HOUR = hour(SETTLEMENTDATE)*60*60)#given in seconds
    
    

#2018
rhs18 <- rhs %>% filter(YEAR == "2018")

#summary stats
rhs %>% count(YEAR) #huge drop-off after 2016
rhs %>% count(CONSTRAINTID) %>% arrange(-n) %>% head()#most common constraints
rhs %>% group_by(YEAR) %>% count(CONSTRAINTID) %>% group_by(YEAR) %>% 
    top_n(2, wt = n)#most common constraints each year

rhs %>% count(CONSTRAINTID) %>% ggplot(aes(x = (n))) + geom_histogram()

### GRAPHS 2018
#number of events per week 
p3 <- ggplot(rhs18, aes(x = MONTH, fill = STATE)) + 
    geom_bar()+
    facet_grid(~ STATE)
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

#time of day split by state and DOW
ggplot(rhs18, aes(x = HOUR, fill = STATE, group = DOW)) +
    geom_bar() + 
    facet_grid(DOW ~ STATE)+
    scale_x_time()


#HOW HAS THIS CHANGED OVER TIME


#Distribution
rhs %>% count(YEAR) #huge drop-off after 2016
table(rhs$STATE, rhs$YEAR) #huge drop in 2016 by VIC, why? Also QLD fluctuating alot

#GRAPHS
p5 <- ggplot(rhs, aes(x = DATE)) +#date, hard to read
    geom_bar()
p5

ggplot(rhs, aes(x = (DATE - DAY + 1))) + #month, much better
    geom_bar()

p5.1 <- ggplot(rhs %>% filter(STATE == "NSW"), #Time of day data grouped by month and year
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

#DISTRIBUTION

length(rhs$X)#276,124 events
rhs$CONSTRAINTID %>% unique() %>% length()#573 unique constraints (note may be similar one's due to gens going offline)
sum(rhs$MARGINALVALUE>-10)#118,760 > $-10
min(rhs$MARGINALVALUE)#most expensive = $-5,112,000
rhs[which(rhs$MARGINALVALUE == min(rhs$MARGINALVALUE)),]

rhs %>% group_by(STATE, YEAR) %>% #sum of all MV by STATE x YEAR
    summarise(SUM = sum(MARGINALVALUE)) %>% 
    reshape2::dcast(STATE ~ YEAR, value.var = "SUM")

ggplot(rhs, aes(x = (-MARGINALVALUE)))+ #histogram of MVs
    geom_histogram(bins = 50)  #largely clustered at 0
    
ggplot(rhs, aes(x = log(-MARGINALVALUE)))+ #log histogram of MVs
    geom_histogram(bins = 50)+  #largely clustered at 0
    scale_x_continuous(breaks = seq(-12,16,4), labels = floor(exp(seq(-12,16,4))))#weird peak at ~1000


#GROUPED BY STATE AND YEAR
ggplot(rhs, 
       aes(x = log(-MARGINALVALUE)))+
    geom_histogram(bins = 50)+  
    scale_x_continuous(breaks = seq(-12,16,4), labels = floor(exp(seq(-12,16,4)))) +
    facet_grid(STATE ~ YEAR)#TAS and SA are culprit for weird second peak




#by week
p6 <- ggplot(rhs18 %>% 
                 group_by(WEEK, STATE) %>% 
                 summarise(SUM = (sum(-MARGINALVALUE))),
             aes(x = WEEK, y = SUM, fill = STATE)) +
    geom_bar(stat = "identity") +
    facet_grid( ~ STATE)
p6

#by time of day split by state and month
p6.2 <- ggplot(rhs %>% 
                 group_by(HOUR, STATE, MONTH) %>% 
                 summarise(SUM = sum(MARGINALVALUE)),
             aes(x = HOUR, y = SUM, fill = STATE)) +
    geom_bar(stat = "identity") +
    scale_x_time() + 
    theme(axis.text.x = element_text(angle = 270)) +
    facet_grid(MONTH ~ STATE)
p6.2


#VIC DROP OFF

#LOOK INTO WEIRD BLIP

-rhs$MARGINALVALUE %>% cut(breaks = exp(c(-Inf, seq(-11, 15, 0.25), Inf))) %>% table()

rhs %>% filter((-854 > MARGINALVALUE) & (MARGINALVALUE >= -1.1e+03)) %>% summary()

rhs %>% filter((-854 > MARGINALVALUE) & (MARGINALVALUE >= -1.1e+03)) %>% ggplot(aes(x = MARGINALVALUE))+
    geom_histogram()

rhs %>% filter((-854 > MARGINALVALUE) & (MARGINALVALUE >= -1.1e+03)) %>% count(STATE)
rhs %>% filter((-854 > MARGINALVALUE) & (MARGINALVALUE >= -1.1e+03)) %>% count(STATE, YEAR) %>% as.data.frame()




# temp <- rhs %>% group_by(CONSTRAINTID) %>% 
#     summarise(SUM = sum(MARGINALVALUE)) %>% 
#     arrange(SUM) %>% 
#     select(CONSTRAINTID) %>% 
#     as.data.frame() %>% 
#     .[(1:50),]
# 
# 
# temp2 <- map(.x = temp, 
#              .f =  ~(rhs %>% filter(CONSTRAINTID == .x) %>% 
#                          select(GENCONID_EFFECTIVEDATE) %>% 
#                          table() %>% 
#                          as.numeric() %>% 
#                          length()) == 1) %>% 
#     unlist #get logical of equations with multiple eqs
# 
# temp3 <- temp[temp2] #list of constraints with one's that have multiple equations removed 
# 
# temp4 <- rhs %>% filter(CONSTRAINTID %in% temp3) %>% select(CONSTRAINTID, GENCONID_EFFECTIVEDATE) %>% unique() %>% 
#     mutate(YEARDATE = paste0(substr(GENCONID_EFFECTIVEDATE, 1, 4),
#                              substr(GENCONID_EFFECTIVEDATE, 6, 7)))#get constraints and YEARDATES of interest
# 
# 
# temp5 <- map2(.x = temp4$CONSTRAINTID, .y = temp4$YEARDATE, ~ eqs.fun(.x, .y))  #get all eqs
# 
# #"T>T_TUNN3_110_1" first line of interest (non 1 coefs, )