setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/Dissorderly Bidding")
#install.packages('tidyverse')
#install.packages('openxlsx')
#install.packages('chron')
library(tidyverse)
library(openxlsx)

#Constraint RHS and MV

address <- "http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/2019/MMSDM_2019_04/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHCONSTRAINT_201904010000.zip"
temp <- tempfile()
download.file(address, temp, mode="wb")
unzip(temp, "PUBLIC_DVD_DISPATCHCONSTRAINT_201904010000.CSV")
rhs <- read.csv("PUBLIC_DVD_DISPATCHCONSTRAINT_201904010000.CSV", sep=",",skip=1)

rhs <- rhs %>% filter(MARGINALVALUE != 0) %>% #remove unconstrained
    select(SETTLEMENTDATE, CONSTRAINTID, RHS, MARGINALVALUE) 
temp <- rhs %>% filter(CONSTRAINTID == "N^^V_NIL_1")


#rhs.2 <- read.csv("data/rhs201904.csv", skip = 1) #old version which took spreadhseet but only the first 1048574 lines


#Constraint Equations (LHS) (make sure in long format)
lhs <- read.csv("data/lhs201904.csv", skip = 1)

lhs <- lhs %>% select(GENCONID, EFFECTIVEDATE, VERSIONNO, SCOPE, TERMID, SPD_ID, SPD_TYPE, FACTOR, LASTCHANGED)

temp1 <- lhs %>% filter(GENCONID == "S>SETXH1_SETXL2") %>% filter(VERSIONNO==1)
temp2 <- lhs %>% filter(GENCONID == "S>SETXH1_SETXL2") %>% filter(VERSIONNO==2)
temp1==temp2
temp1[2,]
temp2[2,]

#Generator Bids