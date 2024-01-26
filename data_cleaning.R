# 2024.01.23
# Jan, Sarah

library(readr)

full_data <- read_csv("full_data_wrong_units.csv", 
                                  col_types = cols(...1 = col_skip(), flux_date = col_date(format = "%d/%m/%Y")))

#Test test
