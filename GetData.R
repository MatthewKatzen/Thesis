###Run Clean.R to get dfs
setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/Dissordely Bidding")
#install.packages('tidyverse')
library(tidyverse)

constraint <- "T>T_LIPM_110_2A"
external.data.location <- "D:/Thesis/Data" #for big data

### GET DATA
#get all rhs values
rhs <- rhs.fun("201701")
write.csv(rhs, "data/201701rhs.csv")

# get lhs equation
# eqs <- eqs.fun("T>T_LIPM_110_2A", "201904")
# write.csv(eqs, "data/eqs.csv")

#bands
bands <- bands.fun("201701")
write.csv(bands, "data/201701bands.csv")

#bids
bids <- bids.fun("201701", "HWPS1")
write.csv(bids, "data/201701bids.csv")

#bids at tim eof dispatch
bids_d <- bids_d.fun("201701", gens)
write.csv(bids_d, "data/201701bids_d.csv")


#dispatch
dispatch <- dispatch.fun("201701", "2017/01/10", "HWPS1")
write.csv(dispatch, "data/201701dispatch.csv")


### EXPLORE
rhs %>% filter(CONSTRAINTID == constraint) %>% head()
eqs
bands %>% filter(DUID == "LK_ECHO") %>% head()
bids %>% head()
dispatch %>% head()
select(DUID, SETTLEMENTDATE, INTERVAL_DATETIME, OFFERDATE, VERSIONNO, PERIODID, MAXAVAIL, BANDAVAIL1, BANDAVAIL2,
       BANDAVAIL3, BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8, BANDAVAIL9, BANDAVAIL10)