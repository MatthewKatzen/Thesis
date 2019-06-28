###Run Clean.R to get dfs
setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis/Dissordely Bidding")
#install.packages('tidyverse')
library(tidyverse)

constraint <- "T>T_LIPM_110_2A"
rhs <- read.csv("data/rhs.csv") 
temp <- rhs %>% filter(CONSTRAINTID == "T>T_LIPM_110_2A")
head(temp)
lhs <- EQS.fun("T>T_LIPM_110_2A", 201904)

