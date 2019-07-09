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


#t test constrained price at gen v unconstrained
#problem: only looking at one constraint, may be included in multiple constraints
#V>>V_NIL_2A_R
eqs <- eqs.fun("V>>V_NIL_2A_R", "201606")
eqs[2,"SPD_ID"]

bids <- bids.fun("201701", "HWPS1")

temp <- bids %>% filter(SETTLEMENTDATE == "2017/01/02 00:00:00") %>% 
    select(DUID, SETTLEMENTDATE, INTERVAL_DATETIME, OFFERDATE, VERSIONNO, PERIODID, MAXAVAIL, BANDAVAIL1, 
           BANDAVAIL2, BANDAVAIL3, BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8, BANDAVAIL9, 
           BANDAVAIL10)

#fix where bids > max avail (cap at max avail)
temp2 <- data.frame(matrix(c(sample(90:110, 10),sample(5:15, 100, replace = TRUE)), nrow = 10, ncol = 11))
temp2

fixbid.fun <- function(data){
    temp <- data %>% select(MAXAVAIL, BANDAVAIL1, 
                             BANDAVAIL2, BANDAVAIL3, BANDAVAIL4, BANDAVAIL5, BANDAVAIL6, BANDAVAIL7, BANDAVAIL8,
                             BANDAVAIL9, BANDAVAIL10) %>% head()
    for (i in 1:nrow(temp)){
        j <- 2
        while ((sum(temp[i, 2:j]) < temp[i,1]) & (j<=10)) {
            j <- j + 1
        }
        temp[i,j] <- min(temp[i,1] - sum(temp[i, 2:(j-1)]), temp[i,j])
        
        if (j <=10){
            temp[i,c((j+1):ncol(temp))] <- 0
        }
    }
    return(temp)
}
fixbid.fun(temp2)

