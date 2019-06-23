setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/dissorderly")
#install.packages('tidyverse')
#install.packages('openxlsx')
#install.packages('chron')
library(tidyverse)
library(openxlsx)

#Constraint RHS and MV
rhs <- read.csv("data/rhs201904.csv", skip = 1)

rhs <- rhs %>% filter(MARGINALVALUE != 0) %>% #remove unconstrained
    select(SETTLEMENTDATE, CONSTRAINTID, RHS, MARGINALVALUE) 
    
temp <- data %>% filter(CONSTRAINTID == "N^^V_NIL_1")

#Constraint Equations (LHS) (make sure in long format)
lhs <- read.csv("data/lhs201904.csv", skip = 1)

lhs <- lhs %>% select(GENCONID, EFFECTIVEDATE, VERSIONNO, SCOPE, TERMID, SPD_ID, SPD_TYPE, FACTOR, LASTCHANGED)

temp1 <- lhs %>% filter(GENCONID == "S>SETXH1_SETXL2") %>% filter(VERSIONNO==1)
temp2 <- lhs %>% filter(GENCONID == "S>SETXH1_SETXL2") %>% filter(VERSIONNO==2)
temp1==temp2
temp1[2,]
temp2[2,]

#Generator Bids