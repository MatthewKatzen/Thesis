###Run Clean.R to get dfs
setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/Dissordely Bidding")
#install.packages('tidyverse')
library(tidyverse)

constraint <- "T>T_LIPM_110_2A"
rhs <- read.csv("data/rhs.csv") 
temp <- rhs %>% filter(CONSTRAINTID == constraint)
head(temp)
tail(temp)

#get dates 
#need to convert for multiple dates
dates <- temp$GENCONID_EFFECTIVEDATE %>% unique()#get constraint dates
yearmonth <- paste0(substr(dates, 1, 4), substr(dates, 6, 7))

lhs <- EQS.fun("T>T_LIPM_110_2A", yearmonth) 
    
lhs %>% filter(EFFECTIVEDATE == dates[1])

