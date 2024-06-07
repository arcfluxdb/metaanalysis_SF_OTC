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

all_data_230823 <- readRDS(file = "flux_db_FULL_230823.rds")

working_data <- all_data_230823 %>% 
  filter(!is.na(co2)) %>% 
  filter(treatment %in% c("CTL", "OTC","SNOWFENCE","OTCxSNOWFENCE")) %>% 
  filter(site_id != "AUS_1")  
  
working_data$site_id <- recode(working_data$site_id, "Toolik MAT SF" = "ALA_13")

working_data <- working_data %>%
    mutate(flux_date = parse_date_time(flux_date, orders = c("ymd", "mdy", "dmy")))


rm(all_data_230823)

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