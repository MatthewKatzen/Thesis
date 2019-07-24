#find constraint that has:
#1. not simply interconnector like most common ones are (eqs is empty)
#2. has a few positive eqs coefficients
#3. rebids during a constrained period


rhs <- read.csv("data/2017rhs.csv") %>% mutate(GENCONID_EFFECTIVEDATE = as.character(GENCONID_EFFECTIVEDATE))

#look at most common constraints
constraints <- rhs %>% 
    group_by(CONSTRAINTID) %>% 
    tally() %>% 
    arrange(-n)

constraints$CONSTRAINTID[1:10]


#check if always the same equation
temp <- constraints$CONSTRAINTID[1:10] #get top 10 constraints
temp2 <- map(.x = temp, 
    .f =  ~(rhs %>% filter(CONSTRAINTID == .x) %>% 
                            select(GENCONID_EFFECTIVEDATE) %>% 
                            table() %>% 
                            as.numeric() %>% 
                            length()) == 1) %>% 
    unlist #get logical of equations with multiple eqs

temp3 <- temp[temp2] #list of constraints with one's that have multiple equations removed 

temp4 <- rhs %>% filter(CONSTRAINTID %in% temp3) %>% select(CONSTRAINTID, GENCONID_EFFECTIVEDATE) %>% unique() %>% 
    mutate(YEARDATE = paste0(substr(GENCONID_EFFECTIVEDATE, 1, 4),substr(GENCONID_EFFECTIVEDATE, 6, 7)))#get constraints and YEARDATES of interest


###PMAP not woring!!
temp5 <- pmap(temp4, ~ eqs.fun(..1,..3)) 

eqs.fun(temp4[1,1], temp4[1,3])
    
eqs <- eqs.fun("N_X_MBTE2_B", "201311") 
gens <- eqs %>% filter(FACTOR > 0) %>% #remove negetive factored generators (don't add to constraint)
    select(SPD_ID) %>% .[-1,] %>% 
    word(1,sep = "\\.")