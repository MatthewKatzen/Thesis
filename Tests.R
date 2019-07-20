###t test constrained price at gen v unconstrained
#problem: only looking at one constraint, may be included in multiple constraints

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
rhs1 %>% filter(CONSTRAINTID == "T>T_LIPM_110_2A") %>% 
    transmute(GENCONID_EFFECTIVEDATE = as.character(GENCONID_EFFECTIVEDATE)) %>% 
    table() #yes :)

eqs <- eqs.fun("T>T_NIL_110_1", "201403") 
    
#get bidnding events
rhs.temp <- rhs %>% filter(CONSTRAINTID == "T>T_TUNN3_110_1")


#get bids
bids <- bids.fun("201701", "YWPS1")

bids.temp <- bids %>% filter(SETTLEMENTDATE == "2017/01/02 00:00:00") %>% 
    select(DUID, SETTLEMENTDATE, INTERVAL_DATETIME, OFFERDATE, VERSIONNO, PERIODID, MAXAVAIL, BANDAVAIL1, 
           BANDAVAIL2, BANDAVAIL3, BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8, BANDAVAIL9, 
           BANDAVAIL10) %>% fixbid.fun()

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



