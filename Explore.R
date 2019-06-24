###Run Clean.R to get df
setwd("C:/Users/Matthew/Google Drive/Uni/19/Thesis/Analysis")
#install.packages('tidyverse')
library(tidyverse)
rhs <- read.csv("data/rhs.csv") %>% 
    select(SETTLEMENTDATE, CONSTRAINTID, RHS, MARGINALVALUE) 