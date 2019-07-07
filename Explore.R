#total constraints in may
rhs <- read.csv("data/rhs.csv")

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

