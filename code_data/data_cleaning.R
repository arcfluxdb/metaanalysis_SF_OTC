# 2024.01.23
# Jan, Sarah
rm(list = ls())
library(readr)
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(effsize)
library(esc)
library(metafor)
library(lme4)


#full_data_wrong_units <- read_csv("full_data_wrong_units.csv", 
#                                  col_types = cols(...1 = col_skip(), flux_date = col_date(format = "%d/%m/%Y")))

##flux data
flux_data <- readRDS("database/database.rds")$fluxdata


flux_data <- flux_data %>% 
  mutate(treatment = ifelse(treatment == "sf", "SNOWFENCE",
                            ifelse(treatment == "ctl", "CTL",treatment)))

colnames(flux_data) <- gsub("_automatic","", colnames(flux_data))
flux_data <- flux_data %>%
  mutate(flux_date = parse_date_time(flux_date, orders = c("ymd", "mdy", "dmy")))

##site data
site_data <- readRDS("database/database.rds")$sitedata
site_data$year <- as.numeric(site_data$year)
site_data <- site_data[!duplicated(site_data[,c(1,2)]),]
## method data
method_data <- readRDS("database/database.rds")$methoddata
method_data$year<-as.numeric(method_data$year)

## merge flux and site data
working_data <- flux_data %>%
  dplyr::left_join(site_data, by = c("site_id" = "site_id", "flux_year" = "year"))

#merge method data
working_data_methods <- working_data %>%
  dplyr::left_join(method_data, by = c("site_id" = "site_id", "flux_year" = "year"))

working_data <- working_data %>% 
  filter(!is.na(reco)) %>% 
  filter(treatment %in% c("CTL", "OTC","SNOWFENCE","OTCxSNOWFENCE")) %>% 
  filter(site_id!= "AUS_1") 


outlierremoval <- working_data %>% 
  group_by(site_id,flux_year) %>% 
  dplyr::summarise(recosd = sd(reco, na.rm=T),
                   recomean = mean(reco, na.rm=T))
outlierremoval$upper <- outlierremoval$recomean + 3 * (outlierremoval$recosd)  
outlierremoval$lower <- outlierremoval$recomean - 3 * (outlierremoval$recosd)  

working_data <- working_data %>% 
  dplyr::left_join(outlierremoval[,c(1,2,5,6)])

working_data$outlier <- with(working_data, reco < lower | reco > upper)

working_data <- working_data[!working_data$outlier,]

working_data <- working_data %>% 
  mutate(air_temp = ifelse(is.na(air_temp),NA, as.numeric(air_temp)))

as.numeric(working_data$air_temp)

working_data$air_temp[grepl("NA", working_data$air_temp)] <- NA

working_data$air_temp[!is.na(working_data$air_temp)] <- as.numeric(working_data$air_temp[!is.na(working_data$air_temp)])


# # change date column
# 
# all_dates <- all_data_230823 %>% 
#   group_by(site_id, year) %>% 
#   dplyr::summarise()
# 
# 
# all_dates <- all_data_230823 %>%
#   mutate(flux_date = parse_date_time(flux_date, orders = c("ymd", "mdy", "dmy")))
# 
# 
# View(all_data_230823[all_data_230823$site_id=="ALA_1",])
# 
# full_data$site_id <- recode(full_data$site_id, "Toolik MAT SF" = "ALA_13")
# 
# full_data$site_id <- recode(full_data$site_id, "Toolik MAT SF" = "ALA_13")