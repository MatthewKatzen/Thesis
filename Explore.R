#First exploration of DB as well as looking at a particular case
install.packages("lubridate")
library(lubridate)
library(tidyverse)

#total constraints in may
rhs <- read.csv("data/201701rhs.csv")

nrow(rhs) #16852 events in may
rhs$CONSTRAINTID %>% table() %>% sort(decreasing = TRUE) #table of each constraint binding

#number of events per day in january
p1 <- ggplot(rhs %>% mutate(SETTLEMENTDATE = as.Date(SETTLEMENTDATE)), 
             aes(x = SETTLEMENTDATE)) + 
    geom_bar()

#time of day of events
#ignore the date in the graph
p2 <- ggplot(rhs %>% mutate(SETTLEMENTDATE = as.POSIXct(strftime(as.POSIXct(SETTLEMENTDATE), format = "%H"), 
                                                        format = "%H")), 
             aes(x = SETTLEMENTDATE)) + 
    geom_bar()





