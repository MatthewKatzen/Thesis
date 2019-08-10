from <- "20180710"
to <- "20190808"
dates <- seq(ymd(from), ymd(to), by = "day") %>% #list of dates
    str_replace_all("-", "")#remove all `-`

mpa.current <- dates %>% map(~ mpa.fun(.x)) %>% 
    bind_rows()
