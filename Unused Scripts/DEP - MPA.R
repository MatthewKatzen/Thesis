mpa <-  read.csv("D:/Thesis/Data/MPA/Merged/Cleaned/mpa_combined.csv")
mpa <- mpa %>% arrange(SETTLEMENTDATE) %>% select(-X)

mpa %>% head()

#DUID with max MPAs
mpa %>% group_by(DUID) %>% summarise(SUM = sum(MPA), COUNT = n(), MEAN = SUM/COUNT) %>% arrange(SUM)

#Fuel Source
mpa %>% group_by(FuelSourcePrimary) %>% summarise(SUM = sum(MPA), COUNT = n(), MEAN = SUM/COUNT) %>% arrange(SUM)

#HIghest MPAs
mpa %>% arrange(MPA) %>% .[1:20,]

#ladbrok1
mpa %>% filter(DUID == "LADBROK1", as.Date(SETTLEMENTDATE) == "2018-07-10")


