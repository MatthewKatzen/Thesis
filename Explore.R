###Run Clean.R to get dfs
setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/Dissordely Bidding")
#install.packages('tidyverse')
library(tidyverse)

constraint <- "T>T_LIPM_110_2A"
external.data.location <- "D:/Thesis/Data" #for big data

#get all rhs values
rhs <- rhs.fun("201905")
write.csv(rhs, "data/rhs.csv")

# get lhs equation
eqs <- eqs.fun("T>T_LIPM_110_2A", "201904")
write.csv(eqs, "data/eqs.csv")

#bands
bands <- bands.fun("201905")
write.csv(bands, "data/bands.csv")

#bids
bids <- bids.fun("201905", "2019/05/10 00:00:00", "LK_ECHO")
write.csv(bids, "data/bids.csv")

#dispatch
dispatch <- dispatch.fun("201905", "2019/05/10", "LK_ECHO")
write.csv(dispatch, "data/dispatch.csv")


rhs %>% filter(CONSTRAINTID == constraint) %>% head()
eqs
bands %>% filter(DUID == "LK_ECHO")
bids
dispatch %>% head()
