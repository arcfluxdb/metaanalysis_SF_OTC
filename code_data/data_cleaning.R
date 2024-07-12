# 2024.01.23
# Jan, Sarah

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


all_data <- readRDS("D:/flux_db/metaanalysis_SF_OTC/database/database.rds")$fluxdata


unique(all_data$site_id_automatic)

working_data <- all_data %>% 
  filter(!is.na(reco)) %>% 
  filter(treatment %in% c("CTL", "OTC","SNOWFENCE","OTCxSNOWFENCE")) %>% 
  filter(site_id_automatic != "AUS_1")  



working_data <- working_data %>%
    mutate(flux_date = parse_date_time(flux_date, orders = c("ymd", "mdy", "dmy")))

colnames(working_data) <- gsub("_automatic","", colnames(working_data))

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