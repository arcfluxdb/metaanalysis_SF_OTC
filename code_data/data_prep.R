# 29.01.2024

# Jan Sarah


source("data_cleaning.R")





working_data <- full_data %>% 
  filter(treatment %in% c("CTL","OTC","SNOWFENCE","OTCxSNOWFENCE"))