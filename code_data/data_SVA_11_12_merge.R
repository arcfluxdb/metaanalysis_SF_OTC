temperature <- read_excel("D:/flux_db/Working_folder/3_third round_2023/flux_Adventdalen_summer07&08_mats.xlsx", 
           skip = 1)

temperature$Date <- as.Date(temperature$Date)
temperature$`OBS! all data before 5.9. is modelled based on soil temperatures from CALM site (see Elkes htesis method part)` <- NULL
temperature$...6 <- NULL

colnames(temperature) <- c("date", "heath_ctl","meadow_ctl","heath_sf","heath_ctl")

temp <- temperature %>%
  pivot_longer(
    cols = -date,
    names_to = c("vegetation", "treatment"),
    names_sep = "_",
    values_to = "measurement"
  )


ggplot(temp)+
  geom_line(aes(x=date, y=measurement, color=treatment))+
  facet_wrap(~vegetation)


flux <- read_excel("D:/flux_db/Working_folder/3_third round_2023/flux_Adventdalen_summer07&08_mats.xlsx", 
                   sheet = "allt-i-ett_originaldata", col_types = c("text", 
                                                                    "text", "text", "date", "skip", "skip", 
                                                                    "numeric", "skip", "skip", "skip", 
                                                                    "skip", "skip", "skip", "skip", "skip", 
                                                                    "skip", "skip", "skip", "skip", "numeric", 
                                                                    "numeric", "numeric"))
colnames(flux) <- c("site","fc","collar","date", "co2", "Msoil", "veg", "treat")
flux$month <- month(flux$date)
flux$year <- year(flux$date)


ggplot(flux)+
  geom_boxplot(aes(x=as.factor(month), y=co2,fill=as.factor(treat)))+
  facet_wrap(year~veg)


colnames(flux)
colnames(temp)

heath_2007 <- flux[flux$veg == 1 &flux$year == 2007,]
heath_2008 <- flux[flux$veg == 1 &flux$year == 2008,]
meadow_2007 <- flux[flux$veg == 2 &flux$year == 2007,]
meadow_2008 <- flux[flux$veg == 2 &flux$year == 2008,]

X07JUL <- read_excel("D:/flux_db/Working_folder/3_third round_2023/Adventdalen 1993-2009/2007/Juli/07JUL.xls", 
                     skip = 3,
                     col_types = c("date",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "numeric",
                                   "skip"
                                   ))
jul07 <- X07JUL %>% 
  group_by(Date) %>% 
  dplyr::summarise(Tair = mean(Deg.C,na.rm=T))

jul07$day <- day(jul07$Date)
jul07$month <- 7
jul07$year <- 2007
jul07$Date <- as.Date(with(jul07, paste(jul07$year, jul07$month, jul07$day, sep = "-")), "%Y-%m-%d")
jul07$day <- NULL
jul07$month <- NULL
jul07$year <- NULL


X07AUG <- read_excel("D:/flux_db/Working_folder/3_third round_2023/Adventdalen 1993-2009/2007/August/07AUG.xls", 
                     skip = 3,
                     col_types = c("date",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "numeric",
                                   "skip"
                     ))
aug07 <-aug07 %>% 
  group_by(Date) %>% 
  dplyr::summarise(Tair = mean(Deg.C,na.rm=T))


aug07 <- X07AUG
aug07$day <- day(aug07$Date)
aug07$month <- 8
aug07$year <- 2007
aug07$Date <- as.Date(with(aug07, paste(aug07$year, aug07$month, aug07$day, sep = "-")), "%Y-%m-%d")
aug07$day <- NULL
aug07$month <- NULL
aug07$year <- NULL

aug07 <-aug07 %>% 
  group_by(Date) %>% 
  dplyr::summarise(Tair = mean(Deg.C,na.rm=T))


X07SEP <- read_excel("D:/flux_db/Working_folder/3_third round_2023/Adventdalen 1993-2009/2007/September/07SEP.xls", 
                     skip = 3,
                     col_types = c("date",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "numeric",
                                   "skip"
                     ))



sep07 <- X07SEP
sep07$day <- day(sep07$Date)
sep07$month <- 9
sep07$year <- 2007
sep07$Date <- as.Date(with(sep07, paste(sep07$year, sep07$month, sep07$day, sep = "-")), "%Y-%m-%d")
sep07$day <- NULL
sep07$month <- NULL
sep07$year <- NULL

sep07 <-sep07 %>% 
  group_by(Date) %>% 
  dplyr::summarise(Tair = mean(Deg.C,na.rm=T))

air2007 <- rbind(jul07,aug07,sep07)
heath_2007$date <- as.Date(heath_2007$date)

heathair_2007 <- merge(heath_2007,
                    air2007,
                    by.x= "date",
                    by.y = "Date",
                    all.x = T)

heathair_2007$veg <- "heath"
heathair_2007$treat <- ifelse(heathair_2007$treat == 1, "sf", "ctl")

meadow_2007$date <- as.Date(meadow_2007$date)

meadowair_2007 <- merge(meadow_2007,
                       air2007,
                       by.x= "date",
                       by.y = "Date",
                       all.x = T)

meadowair_2007$veg <- "meadow"
meadowair_2007$treat <- ifelse(meadowair_2007$treat == 1, "sf", "ctl")

colnames(temp) <- c("date", "veg", "treat", "Tsoil")


heathairsoil_2007 <- merge(heathair_2007,
                           temp,
                           by = c("date", "veg", "treat"),
                           all.x = T)
meadowairsoil_2007 <- merge(meadowair_2007,
                           temp,
                           by = c("date", "veg", "treat"),
                           all.x = T)








X08JUN <- read_excel("D:/flux_db/Working_folder/3_third round_2023/Adventdalen 1993-2009/2008/Juni/08JUNI.xls", 
                     skip = 3,
                     col_types = c("date",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "numeric",
                                   "skip"
                     ))
jun08 <- X08JUN

jun08$day <- day(jun08$Date)
jun08$month <- 6
jun08$year <- 2008
jun08$Date <- as.Date(with(jun08, paste(jun08$year, jun08$month, jun08$day, sep = "-")), "%Y-%m-%d")
jun08$day <- NULL
jun08$month <- NULL
jun08$year <- NULL

jun08 <- jun08 %>% 
  group_by(Date) %>% 
  dplyr::summarise(Tair = mean(Deg.C,na.rm=T))

jul08 <- read_excel("D:/flux_db/Working_folder/3_third round_2023/Adventdalen 1993-2009/2008/Juli/08JULI.xls", 
                     skip = 3,
                     col_types = c("date",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip",
                                   "skip", 
                                   "numeric",
                                   "skip"
                     ))

jul08$day <- day(jul08$Date)
jul08$month <- 7
jul08$year <- 2008
jul08$Date <- as.Date(with(jul08, paste(jul08$year, jul08$month, jul08$day, sep = "-")), "%Y-%m-%d")
jul08$day <- NULL
jul08$month <- NULL
jul08$year <- NULL

jul08 <-jul08 %>% 
  group_by(Date) %>% 
  dplyr::summarise(Tair = mean(Deg.C,na.rm=T))

air2008 <- rbind(jun08,jul08)


heath_2008$date <- as.Date(heath_2008$date)

heathair_2008 <- merge(heath_2008,
                       air2008,
                       by.x= "date",
                       by.y = "Date",
                       all.x = T)

heathair_2008$veg <- "heath"
heathair_2008$treat <- ifelse(heathair_2008$treat == 1, "sf", "ctl")

meadow_2008$date <- as.Date(meadow_2008$date)

meadowair_2008 <- merge(meadow_2008,
                        air2008,
                        by.x= "date",
                        by.y = "Date",
                        all.x = T)

meadowair_2008$veg <- "meadow"
meadowair_2008$treat <- ifelse(meadowair_2008$treat == 1, "sf", "ctl")

colnames(temp) <- c("date", "veg", "treat", "Tsoil")


heathairsoil_2008 <- merge(heathair_2008,
                           temp,
                           by = c("date", "veg", "treat"),
                           all.x = T)
meadowairsoil_2008 <- merge(meadowair_2008,
                            temp,
                            by = c("date", "veg", "treat"),
                            all.x = T)

writexl::write_xlsx(heathairsoil_2008, "D:/flux_db/Working_folder/3_third round_2023/SVA_11_2008.xlsx")
writexl::write_xlsx(heathairsoil_2007, "D:/flux_db/Working_folder/3_third round_2023/SVA_11_2007.xlsx")
writexl::write_xlsx(meadowairsoil_2008, "D:/flux_db/Working_folder/3_third round_2023/SVA_12_2008.xlsx")
writexl::write_xlsx(meadowairsoil_2007, "D:/flux_db/Working_folder/3_third round_2023/SVA_12_2007.xlsx")














