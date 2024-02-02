# 2024.01.23
# Jan, Sarah

library(readr)
library(tidyverse)


library(dplyr)
library(lubridate)
library(ggplot2)
library(effsize)
library(esc)
library(metafor)
library(lme4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(effsize)
library(esc)
library(metafor)
library(lme4)

full_data_wrong_units <- read_csv("full_data_wrong_units.csv", 
                                  col_types = cols(...1 = col_skip(), flux_date = col_date(format = "%d/%m/%Y")))

all_data_230823 <- read_csv("flux_db_FULL_230823.csv", 
                                col_types = cols(...1 = col_skip()))



# change date column

all_dates <- all_data_230823 %>% 
  group_by(site_id, year) %>% 
  dplyr::summarise()


all_dates <- all_data_230823 %>%
  mutate(flux_date = parse_date_time(flux_date, orders = c("ymd", "mdy", "dmy")))


View(all_data_230823[all_data_230823$site_id=="ALA_1",])

full_data$site_id <- recode(full_data$site_id, "Toolik MAT SF" = "ALA_13")

full_data$site_id <- recode(full_data$site_id, "Toolik MAT SF" = "ALA_13")