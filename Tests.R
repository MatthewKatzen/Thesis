###t test constrained price at gen v unconstrained
#problem: only looking at one constraint, may be included in multiple constraints
install.packages("stringr")
library(lubridate) # for manipulating time. as.POSXct doesn't like midnight for some reason
library(tidyverse)
library(stringr)
#find interesting event 
#few lhs generators
#mix of types 
#not all owned by same operator
#no wind
#binds consistently over a period

rhs %>% 
    select(CONSTRAINTID) %>% 
    mutate(CONSTRAINTID = as.character(CONSTRAINTID)) %>% 
    table() %>% sort(decreasing = TRUE)



#T>T_TUNN3_110_1 




#check if always the same equation
rhs %>% filter(CONSTRAINTID == "T>T_TUNN3_110_1") %>% 
    transmute(GENCONID_EFFECTIVEDATE = as.character(GENCONID_EFFECTIVEDATE)) %>% 
    table() #yes :)

eqs <- eqs.fun("T>T_TUNN3_110_1", "201606") 
gens <- eqs %>% filter(FACTOR > 0) %>% #remove negetive factored generators (don't add to constraint)
    select(SPD_ID) %>% .[-1,] %>% 
    word(1,sep = "\\.")


#get binding events
rhs.temp <- rhs %>% filter(CONSTRAINTID == "T>T_TUNN3_110_1")

#find long stretch of trues
int <- constrained.fun(rhs.temp$SETTLEMENTDATE, 25)


qplot(temp[start:end])
temp[87:88]#6:35 till nect bind
temp[57:58]#10 minutes since previous bind
qplot(temp[51:58])
temp[51:58]

#can see that there are a few events before the main chunk. Ignore them for now



#get bids
bids <- bids.fun("201704", gens)

bids.temp <- bids %>% filter(SETTLEMENTDATE == "2017/04/28 00:00:00") %>% 
    filter(as.Date(OFFERDATE) >= "2017/04/27")

bids_d <- bids_d.fun("201704", "TUNGATIN")

bids_d.temp <- bids_d %>% filter(SETTLEMENTDATE == "2017/04/28 00:00:00", BIDTYPE == "ENERGY") %>% 
    select(DUID, SETTLEMENTDATE, INTERVAL_DATETIME, OFFERDATE, VERSIONNO, PERIODID, MAXAVAIL, BANDAVAIL1, 
           BANDAVAIL2, BANDAVAIL3, BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8, BANDAVAIL9, 
           BANDAVAIL10)

#check if any offers occured whilst binding
temp6 <- bids.temp$OFFERDATE %>% unique() %>% as.POSIXct()#rebid times
c.datetime <- temp6[temp6 %within% int]#rebids occuring whilst constrained
map(temp6, .%within% int)
any(temp6 %within% int)
temp6 %within% int


c.gen <- bids.temp %>% filter(as.POSIXct(OFFERDATE) %in% c.datetime) %>% select(DUID) %>% unique() %>% 
    as.character#get gens that rebid when constrained
rebid <- bids.temp %>% filter(DUID == c.gen) %>% select(OFFERDATE) %>% 
    unique() %>% .[,'OFFERDATE'] %>% as.POSIXct() #get rebids of gens that rebid when constrained

c.rects <- data.frame(start = int_start(int)[1:2], end = int_end(int)[1:2], group=seq_along(start))
day.rects <- data.frame(start = c.rects[1,1] - 60*60*(hour(c.rects[1,1]) - 4) - 60*(minute(c.rects[1,1]) - 5),
                        end = c.rects[1,1] - 60*60*(hour(c.rects[1,1]) - 28) - 60*(minute(c.rects[1,1]) - 5))

p6 <- ggplot() +
    geom_vline(xintercept = rebid) +
    geom_rect(c.rects, mapping = aes(xmin = start, xmax = end, ymin = 0, ymax = 1, alpha = "Binding", fill = "Binding")) +
    geom_rect(day.rects, mapping = aes(xmin = start, xmax = end, ymin = 0, ymax = 1, alpha = "Day", fill = "Day"))+
    scale_x_datetime(limits = (range(rebid, c.rects$start, c.rects$end, day.rects$start, day.rects$end)))+
    scale_fill_manual(values = c("Binding" = "red", "Day" = "blue"))+
    scale_alpha_manual(values = c("Binding" = 0.5, "Day" = 0.1))
    
p6

#compare rebid to previous one
bids.temp %>% filter(DUID == c.gen) %>% select(OFFERDATE) %>% unique()
bids.temp %>% filter(DUID == c.gen, OFFERDATE == c.datetime) %>% maxband.fun()
bids.temp %>% filter(DUID == c.gen, OFFERDATE == c.datetime) %>% maxband.fun()









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



