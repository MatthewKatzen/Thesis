url <- "http://nemweb.com.au/Reports/Archive/DispatchIS_Reports/PUBLIC_DISPATCHIS_20180710.zip"
external.data.location <- "D:/Thesis/Data/MPA" #for big data

#unzip files within zip within zip
temp <- tempfile()
download.file(url, temp, mode="wb")
temp2 <- tempfile()
unzip(temp, exdir = temp2)
temp3 <- paste0(temp2,"\\",list.files(temp2))
temp3 %>% map(~ unzip(.x, exdir = external.data.location)) %>% invisible()
unlink(temp)




