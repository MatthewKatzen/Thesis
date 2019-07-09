#total constraints in may
rhs <- read.csv("data/201701rhs.csv")

nrow(rhs) #16852 events in may

#number of events per day
p1 <- ggplot(rhs %>% mutate(SETTLEMENTDATE = as.Date(SETTLEMENTDATE)), 
             aes(x = SETTLEMENTDATE)) + 
    geom_bar()

#time of day of events
#ignore the date in the graph
p2 <- ggplot(rhs %>% mutate(SETTLEMENTDATE = as.POSIXct(strftime(as.POSIXct(SETTLEMENTDATE), format = "%H"), 
                                                        format = "%H")), 
             aes(x = SETTLEMENTDATE)) + 
    geom_bar()


###t test constrained price at gen v unconstrained
#problem: only looking at one constraint, may be included in multiple constraints
#V>>V_NIL_2A_R
eqs <- eqs.fun("V>>V_NIL_2A_R", "201606")
eqs[2,"SPD_ID"]

#get bids
bids <- bids.fun("201701", "YWPS1")

bids.temp <- bids %>% filter(SETTLEMENTDATE == "2017/01/02 00:00:00") %>% 
    select(DUID, SETTLEMENTDATE, INTERVAL_DATETIME, OFFERDATE, VERSIONNO, PERIODID, MAXAVAIL, BANDAVAIL1, 
           BANDAVAIL2, BANDAVAIL3, BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8, BANDAVAIL9, 
           BANDAVAIL10) %>% fixbid.fun()

#get dispatch amount
dispatch <- dispatch.fun("201701", "YWPS1")
dispatch.temp <- dispatch %>% filter(as.Date(SETTLEMENTDATE) == "2017/01/02")

#get bands
bands <- bands.fun("201701")
bands.temp <- bands %>% filter(DUID == "YWPS1", SETTLEMENTDATE == "2017/01/02 00:00:00")

#get max band used
maxband.fun <- function(bids, dispatch, bands){
    bids <- bids %>% select(MAXAVAIL, BANDAVAIL1, 
                            BANDAVAIL2, BANDAVAIL3, BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8,
                            BANDAVAIL9, BANDAVAIL10, INTERVAL_DATETIME) 
    
    dispatch <- dispatch %>% select(INTERVAL_DATETIME = SETTLEMENTDATE, TOTALCLEARED)
    bands <- bands %>% select(PRICEBAND1, PRICEBAND2, PRICEBAND3, PRICEBAND4, PRICEBAND5, PRICEBAND6, 
                              PRICEBAND7, PRICEBAND8, PRICEBAND9, PRICEBAND10)
    
    temp4 <- merge(dispatch, bids, by = 'INTERVAL_DATETIME')
    
    for (i in 1:nrow(temp4)){
        j <- 4
        while ((sum(temp4[i, 4:j]) < temp4[i,2]) & (j<=12)) {#find col where colsum > maxavail
            j <- j + 1
        }
        temp4[i, 'MAXBAND']<-bands[1,j-3]
    }
    return(temp4)
}

temp4 %>% head()

maxband.fun(bids.temp, dispatch.temp, bands.temp) %>% tail()
