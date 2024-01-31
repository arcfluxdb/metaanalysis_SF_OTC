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

full_data <- read_csv("full_data_wrong_units.csv", 
                                  col_types = cols(...1 = col_skip(), flux_date = col_date(format = "%d/%m/%Y")))


full_data$site_id <- recode(full_data$site_id, "Toolik MAT SF" = "ALA_13")