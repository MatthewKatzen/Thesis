library(ggplot2)


### Ave Rev at YEAR
mpa <- fread("D:/Thesis/Data/COMPLETE/mpa_age.csv") %>% mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE))

#AVE REV of production (LMP0)
mpa_year <- mpa %>% 
    group_by(YEAR = floor_date(SETTLEMENTDATE, "year"), Fuel.Type) %>% 
    summarise(Quanity = sum(TOTALCLEARED),
              Ave_Rev = ifelse(sum(TOTALCLEARED)>0,
                               sum(Rev_RRP_30)/sum(TOTALCLEARED),
                               NA),
              Ave_Rev_LMP = ifelse(sum(TOTALCLEARED)>0,
                                   sum(Rev_LMP)/sum(TOTALCLEARED),
                                   NA),
              Ave_Rev_LMP0 = ifelse(sum(TOTALCLEARED)>0,
                                   sum(Rev_LMP0)/sum(TOTALCLEARED),
                                   NA),
              Total_Rev_RRP = sum(Rev_RRP_30),
              Total_Rev_LMP = sum(Rev_LMP),
              Total_Rev_LMP0 = sum(Rev_LMP0),
              Dif_Ave = Ave_Rev_LMP - Ave_Rev,
              Dif_Ave_0 = Ave_Rev_LMP0 - Ave_Rev,
              Dif_Total = Total_Rev_LMP - Total_Rev_RRP,
              Dif_Total_0 = Total_Rev_LMP0 - Total_Rev_RRP)

mpa_year %>% 
    ggplot(aes(x = YEAR, y = Dif_Ave, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP") 

    ggsave(file = "Ave_change.png",  width = 7)

mpa_year %>% filter(Fuel.Type != "Liquid Fuel") %>% 
    ggplot(aes(x = YEAR, y = Dif_Ave, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP - No Liquid Fuel")

    ggsave(file = "Ave_change_noliquidfuel.png",  width = 7)


mpa_year %>% 
    ggplot(aes(x = YEAR, y = Dif_Ave_0, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in swicth to LMP0 (No neg LMP)")
    
    ggsave(file = "Ave_change_LMP0.png",  width = 7)


mpa_year %>% filter(Fuel.Type != "Liquid Fuel") %>% 
    ggplot(aes(x = YEAR, y = Dif_Ave_0, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Average Revenue Increase in switch to LMP0 (No neg LMP) - No Liquid Fuel")

    ggsave(file = "Ave_change_LMP0_noliquidfuel.png",  width = 7)

mpa_year %>% 
    ggplot(aes(x = YEAR, y = Dif_Total, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Total Revenue Increase in swith to LMP")

    ggsave(file = "Total_Revenue.png",  width = 7)


mpa_year %>% 
    ggplot(aes(x = YEAR, y = Dif_Total_0, group = Fuel.Type, colour = Fuel.Type)) + 
    geom_line(size = 2)+
    ggtitle("Total Revenue Increase in swith to LMP0 (No neg LMP)")

    ggsave(file = "Total_Revenue_LMP0.png",  width = 7)

fwrite(mpa_year %>% ungroup() %>% mutate(YEAR = as.character(YEAR)), "GORDON/mpa_year.csv")

#convert mpa_year figures to readable format
round_large <- function(tx) { 
    div <- findInterval(abs(as.numeric(gsub("\\,", "", tx))), 
                        c(0, 1e3, 1e6, 1e9, 1e12) )
    paste(round(as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 1), 
          c("","K","M","B","T")[div] )
}

round_mill <- function(tx) { 
    paste(round(as.numeric(gsub("\\,","",tx))/10^6, 2), 
          "M") 
}

round_2 <- function(tx){
    paste0(round(as.numeric(tx), 1))
}

mpa_year_read <- mutate_at(mpa_year, 
                           c("Quanity", "Total_Rev_RRP", "Total_Rev_LMP", "Total_Rev_LMP0", "Dif_Total", "Dif_Total_0"),
                           round_mill) %>% 
    mutate_at(c("Ave_Rev", "Ave_Rev_LMP", "Ave_Rev_LMP0", "Dif_Ave", "Dif_Ave_0"),
              round_2)

fwrite(mpa_year_read %>% ungroup() %>% mutate(YEAR = as.character(YEAR)), "GORDON/mpa_year_readable.csv")
