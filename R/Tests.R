###t test constrained price at gen v unconstrained
#problem: only looking at one constraint, may be included in multiple constraints
install.packages("stringr")
install.packages("tidyverse")
install.packages("dplyr")

library(lubridate) # for manipulating time. as.POSXct doesn't like midnight for some reason
library(tidyverse)
library(stringr)
library(dplyr)#need new verison of dplyr not in tidyverse yet for group_split()
#find interesting event 
#few lhs generators
#mix of types 
#not all owned by same operator
#no wind
#binds consistently over a period

rhs <- read.csv("data/2017rhs.csv")

#constraint we are interested in
constraint <-  con[20]

#get binding events
rhs.temp <- rhs %>% filter(CONSTRAINTID == constraint)

#find long stretch of trues
int <- constrained.fun(rhs.temp$SETTLEMENTDATE, 24)

#get gens
gens <- temp6 %>% filter(GENCONID == constraint) %>% 
    filter(FACTOR > 0) %>% #remove negetive factored generators (don't add to constraint)
    filter(SPD_ID != "Scale") %>% #remove scale factor
    select(SPD_ID) %>% .[,] %>% word(1,sep = "\\.") #clean up names of generators

#get bids
bids <- bids.fun("201711", gens)
bids.temp <- bids %>% filter(as.Date(SETTLEMENTDATE) == "2017-11-24") 
bids.temp$OFFERDATE %>% unique()#lots of old rebids to remove   

#remove old and duplicate bids
bids.temp.2 <- clean.bids(bids.temp)


#check if any offers occured whilst binding
bid.times <- bids.temp.2$OFFERDATE %>% unique()  %>% as.POSIXct()#rebid times
c.datetime <- within_ints(bid.times,int[1]) # rebids occuring whilst constrained

#which gens bid whilst constrained
temp8 <- bids.temp.2 %>% group_by(DUID) %>% filter(OFFERDATE == c.datetime) %>% as.data.frame() %>% 
    select(DUID) %>% unique %>% .[,]


#when are they rebidding
temp9 <-  bids.temp.2 %>% filter(DUID == temp8[2]) %>% select(OFFERDATE) %>% unique()
int



# GRAPHS
# 
# 
# c.gen <- bids.temp %>% filter(as.POSIXct(OFFERDATE) %in% c.datetime) %>% 
#     select(DUID) %>% unique() %>% .[,]#get gens that rebid when constrained
# rebid <- bids.temp %>% filter(DUID == c.gen[1]) %>% select(OFFERDATE) %>% 
#     unique() %>% .[,] %>% as.POSIXct() #get rebids of gens that rebid when constrained
# 
# 
# rebid <- rebid[initial:length(rebid)]
# 
# 
# #Graphs
# 
# c.rects <- data.frame(start = int_start(int)[1:2], end = int_end(int)[1:2], group=seq_along(start))
# day.rects <- data.frame(start = c.rects[1,1] - 60*60*(hour(c.rects[1,1]) - 4) - 60*(minute(c.rects[1,1]) - 5),
#                         end = c.rects[1,1] - 60*60*(hour(c.rects[1,1]) - 28) - 60*(minute(c.rects[1,1]) - 5))
# 
# p6 <- ggplot() +
#     geom_vline(xintercept = rebid) +
#     geom_rect(c.rects, mapping = aes(xmin = start, xmax = end, ymin = 0, ymax = 1, alpha = "Binding", 
#                                      fill = "Binding")) + #constraint boxes
#     geom_rect(day.rects, mapping = aes(xmin = start, xmax = end, ymin = 0, ymax = 1, alpha = "Day", 
#                                        fill = "Day")) + #day box
#     scale_x_datetime(limits = (range(rebid, c.rects$start, c.rects$end, day.rects$start, 
#                                      day.rects$end)))+ #make sure verything fits
#     scale_fill_manual(values = c("Binding" = "red", "Day" = "blue"))+
#     scale_alpha_manual(values = c("Binding" = 0.5, "Day" = 0.1))
#     
# p6

#compare rebid to previous one
# t1 <- bids.temp %>% filter(DUID == c.gen)#, OFFERDATE == c.datetime[2]) 
# t1 %>% tail
# t2 <- bids.temp %>% filter(DUID == c.gen, OFFERDATE == rebid[3]) 
# 
# 
# (data.frame(t1 == t2) %>% select(-c(OFFERDATE,VERSIONNO)))
# ###all are equal therefore no db
# 
# #for some reason bids don't change within files, need to remove irrelevant ones
# track <- dispatch.tracker.fun("201704", "MEADOWBK")
# track <- track %>% filter(BIDTYPE == "ENERGY", as.Date(BIDSETTLEMENTDATE) == "2017/04/28")
# 
# track %>% select(BIDOFFERDATE) %>% table()






#get dispatch amount
dispatch <- dispatch.fun("201701", "YWPS1")
fromdate <- as.POSIXct("2017/01/02 04:05:00")
todate <- as.POSIXct("2017/01/03 04:00:00")
int <- interval(fromdate, todate)
dispatch.temp <- dispatch %>% filter(as.POSIXct(SETTLEMENTDATE) %within% int)

#get bands
bands <- bands.fun("201701")
bands.temp <- bands %>% filter(DUID == "YWPS1", SETTLEMENTDATE == "2017/01/02 00:00:00")

#get max band used
maxband <- maxband.fun(bids.temp, dispatch.temp, bands.temp)

#get RRP
rrp <- rrp.fun("201701") 
rrp.temp<- rrp %>% filter(REGIONID == "VIC1") %>% 
    filter(as.Date(SETTLEMENTDATE) == "2017/01/02") %>% 
    .[50:288,]


### TEST 1 bid v rrp 
t.test(maxband$MAXBAND, rrp.temp$RRP, paired = TRUE)

qplot(maxband$MAXBAND)#not normally distibuted
qplot(rrp.temp$RRP)#somewhat normally distributed

### TEST 2



