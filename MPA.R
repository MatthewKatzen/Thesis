from <- "20180710"
to <- "20190808"
dates <- seq(ymd(from), ymd(to), by = "day") %>% #list of dates
    str_replace_all("-", "")#remove all `-`

mpa.current <- dates %>% map(~ mpa.fun(.x)) %>% 
    bind_rows()

#stopped
external.data.location <- "D:/Thesis/Data/MPA" #for big data
files <- paste0(external.data.location, "/", list.files(external.data.location))
mpa.current <- files %>% map(~ mpa.clean.fun(.x)) %>% 
    bind_rows()

write.csv(mpa.current, "data/mpa.current.csv")
mpa.current <- read.csv("data/mpa.current.csv")

#get gen fuel types
fuel <- read.csv("data/dontupload/GenFuelTypes.csv")
fuel <- fuel %>% select(DUID, Region, Classification, 7:10)
head(fuel)

#get rrp
rrp <- rrp.fun("201807")
head(rrp)
