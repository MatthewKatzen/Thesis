#find constraint that has:
#1. not simply interconnector like most common ones are (eqs is empty)
#2. has a few positive eqs coefficients
#3. rebids during a constrained period


rhs <- read.csv("data/2017rhs.csv", stringsAsFactors = F) 

#look at most common constraints
constraints <- rhs %>% 
    group_by(CONSTRAINTID) %>% 
    tally() %>% 
    arrange(-n)

constraints$CONSTRAINTID[1:20]

temp <- constraints$CONSTRAINTID[1:100] #get top 20 constraints


#check if always the same equation
temp2 <- map(.x = temp, 
    .f =  ~(rhs %>% filter(CONSTRAINTID == .x) %>% 
                            select(GENCONID_EFFECTIVEDATE) %>% 
                            table() %>% 
                            as.numeric() %>% 
                            length()) == 1) %>% 
    unlist #get logical of equations with multiple eqs

temp3 <- temp[temp2] #list of constraints with one's that have multiple equations removed 

temp4 <- rhs %>% filter(CONSTRAINTID %in% temp3) %>% select(CONSTRAINTID, GENCONID_EFFECTIVEDATE) %>% unique() %>% 
    mutate(YEARDATE = paste0(substr(GENCONID_EFFECTIVEDATE, 1, 4),
                             substr(GENCONID_EFFECTIVEDATE, 6, 7)))#get constraints and YEARDATES of interest


temp5 <- map2(.x = temp4$CONSTRAINTID, .y = temp4$YEARDATE, ~ eqs.fun(.x, .y))  #get all eqs


temp6 <- temp5 %>% bind_rows() %>% group_by(GENCONID) %>% 
    filter(any(SPD_TYPE=="T")) %>% #removes df if has no type T (generators)
    filter(substr(GENCONID, 2, 3)!=">") %>% #only look at thermal overload
    as.data.frame()

### top constraints
con <- temp6 %>% select(GENCONID) %>% unique() %>% .[,] 


