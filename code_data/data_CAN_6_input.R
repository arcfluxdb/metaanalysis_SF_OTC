### new data imputs

rm(list = ls())


##### libraries ####
library(readxl)
library(tidyverse)
# library(stringr)
# library(writexl)
library(plyr)
library(dplyr)
library(janitor)
# library(tibble)
# library(lubridate)
# library(readr)
# library(effsize)
library(svMisc)
library(testit)
###### get data ####
#source(paste(getwd(),"/R_scripts/functions.R", sep=""))

#####
sites <- data.frame()
methods <- data.frame()
datasets <- list()
#### Korea

CAN6_v2 <- read_excel("D:/flux_db/Working_folder/3_third round_2023/Korea/NEE_data_import_external_CAN6_v3.xlsx", 
                                               sheet = "5_FLUX_DATA", skip = 1)

flux_data <- CAN6_v2 %>%  
  clean_names()

colnames(flux_data)[3] <- "flux_date"

# delete empty columns
drop_columns <- str_extract(colnames(flux_data),"x..$")[!is.na(str_extract(colnames(flux_data),"x..$"))]
if (length(drop_columns) > 1){
  flux_data <- flux_data[ , -which(names(flux_data) %in% drop_columns)]
}

colnames(flux_data) <- gsub("_ecosystem_respiration","_raw",colnames(flux_data))
colnames(flux_data) <- gsub("r2_14","reco_raw_r2",colnames(flux_data))

colnames(flux_data) <- gsub("_net_ecosystem_exchange","_raw",colnames(flux_data))
colnames(flux_data) <- gsub("r2_16","nee_raw_r2",colnames(flux_data))

colnames(flux_data) <- gsub("_gross_primary_production","_raw",colnames(flux_data))
colnames(flux_data) <- gsub("r2_18","gpp_raw_r2",colnames(flux_data))

colnames(flux_data) <- gsub("r2_20","ch4_raw_r2",colnames(flux_data))
colnames(flux_data) <- gsub("ch4","ch4_raw",colnames(flux_data))
# add new columns for reco units and extra info ----
flux_data <- add_column(flux_data,
                        reco_raw_unit = rep(ifelse(flux_data$reco_raw[3] == "other", flux_data$reco_raw[4], flux_data$reco_raw[3]),
                                       length(flux_data$reco_raw)),
                        .after = "reco_raw")

flux_data <- add_column(flux_data,
                        reco_raw_freq = rep(flux_data$reco_raw[1],
                                       length(flux_data$reco_raw)),
                        .after = "reco_raw_unit")
flux_data <- add_column(flux_data,
                        reco_raw_slope = rep(flux_data$reco_raw[2],
                                        length(flux_data$reco_raw)),
                        .after = "reco_raw_freq")

# add new columns for nee units and extra info ----
flux_data <- add_column(flux_data,
                        nee_raw_unit = rep(ifelse(flux_data$nee_raw[3] == "other", flux_data$nee_raw[4], flux_data$nee_raw[3]),
                                        length(flux_data$nee_raw)),
                        .after = "nee_raw")

flux_data <- add_column(flux_data,
                        nee_raw_dir = rep(flux_data$nee_raw[4],
                                        length(flux_data$nee_raw)),
                        .after = "nee_raw_unit")

flux_data <- add_column(flux_data,
                        nee_raw_freq = rep(flux_data$nee_raw[1],
                                       length(flux_data$nee_raw)),
                        .after = "nee_raw_dir")
flux_data <- add_column(flux_data,
                        nee_raw_slope = rep(flux_data$nee_raw[2],
                                         length(flux_data$nee_raw)),
                        .after = "nee_raw_freq")

# add new columns for gpp units and extra info ----
flux_data <- add_column(flux_data,
                        gpp_raw_unit = rep(ifelse(flux_data$gpp_raw[3] == "other", flux_data$gpp_raw[4], flux_data$gpp_raw[3]),
                                       length(flux_data$gpp_raw)),
                        .after = "gpp_raw")

flux_data <- add_column(flux_data,
                        gpp_raw_freq = rep(flux_data$gpp_raw[1],
                                       length(flux_data$gpp_raw)),
                        .after = "gpp_raw_unit")
flux_data <- add_column(flux_data,
                        gpp_raw_slope = rep(flux_data$gpp_raw[2],
                                        length(flux_data$gpp_raw)),
                        .after = "gpp_raw_freq")


# add new columns for ch4 units and extra info ----
flux_data <- add_column(flux_data,
                        ch4_raw_unit = rep(ifelse(flux_data$ch4_raw[3] == "other", flux_data$ch4_raw[4], flux_data$ch4_raw[3]),
                                       length(flux_data$ch4_raw)),
                        .after = "ch4_raw")
flux_data <- add_column(flux_data,
                        ch4_raw_freq = rep(flux_data$ch4_raw[1],
                                       length(flux_data$ch4_raw)),
                        .after = "ch4_raw_unit")
flux_data <- add_column(flux_data,
                        ch4_raw_slope = rep(flux_data$ch4_raw[2],
                                        length(flux_data$ch4_raw)),
                        .after = "ch4_raw_freq")
flux_data <- add_column(flux_data,
                        ch4_raw_dir = rep(flux_data$ch4_raw[5],
                                      length(flux_data$ch4_raw)),
                        .after = "ch4_raw_slope")

#change columns for soiltemp and soilmosi ----


if ("soil_temp_19" %in% colnames(flux_data)){
  colnames(flux_data) <- gsub("_19","",colnames(flux_data))
  flux_data$soil_temp_20[3] <- "°C"
  colnames(flux_data) <- gsub("soil_temp_20", "soil_temp_1",colnames(flux_data))
  flux_data <- add_column(flux_data,
                          soil_temp_1_unit = rep(flux_data$soil_temp_1[3],
                                                 length(flux_data$site_id_automatic)),
                          .after = "soil_temp_1")
  
  flux_data <- add_column(flux_data,
                          soil_temp_1_depth = rep(flux_data$soil_temp_1[5],
                                                  length(flux_data$site_id_automatic)),
                          .after = "soil_temp_1_unit")
}

flux_data <- add_column(flux_data,
                        par_unit = rep(flux_data$par[3],
                                       length(flux_data$site_id_automatic)),
                        .after = "par")

flux_data <- add_column(flux_data,
                        air_temp_unit = rep(flux_data$air_temp[3],
                                            length(flux_data$site_id_automatic)),
                        .after = "air_temp")

flux_data <- add_column(flux_data,
                        soil_temp_unit = rep(flux_data$soil_temp[3],
                                             length(flux_data$site_id_automatic)),
                        .after = "soil_temp")

flux_data <- add_column(flux_data,
                        soil_temp_depth = rep(flux_data$soil_temp[5],
                                              length(flux_data$site_id_automatic)),
                        .after = "soil_temp_unit")


flux_data <- add_column(flux_data,
                        soil_moist_unit = rep(flux_data$soil_moist[3],
                                              length(flux_data$site_id_automatic)),
                        .after = "soil_moist")


flux_data <- add_column(flux_data,
                        soil_moist_depth = rep(flux_data$soil_moist[5],
                                               length(flux_data$site_id_automatic)),
                        .after = "soil_moist_unit")

flux_data <- add_column(flux_data,
                        water_table_depth_unit = rep(flux_data$water_table_depth[3],
                                                     length(flux_data$site_id_automatic)),
                        .after = "water_table_depth")

flux_data <- add_column(flux_data,
                        thaw_depth_unit = rep(flux_data$thaw_depth[3],
                                              length(flux_data$site_id_automatic)),
                        .after = "thaw_depth")

if(sum(str_detect(colnames(flux_data),"add")) > 0){
  stop()
}
if (sum(!is.na(str_extract(colnames(flux_data), "add"))) !=0){
  
  
  add_columns <- colnames(flux_data)[!is.na(str_extract(colnames(flux_data), "add"))]
  
  
  for (i in 1:length(add_columns)){
    
    column_nr <- grep(paste("^",colnames(flux_data)[colnames(flux_data) == add_columns[i]],"$",sep=""), colnames(flux_data))
    
    colnames(flux_data)[column_nr] <- word(unname(c(flux_data[3,column_nr], recursive=TRUE)),2,-1)
    
    flux_data <- add_column(flux_data,
                            new_unit = rep(word(unname(c(flux_data[3,column_nr], recursive=TRUE))),
                                           length(flux_data$site_id_automatic)),
                            .after = column_nr)
    
    colnames(flux_data)[column_nr + 1 ] <- paste(word(unname(c(flux_data[3,column_nr], recursive=TRUE)),2,-1),"_unit",sep="")
    
    
    flux_data <- add_column(flux_data,
                            new_depth = rep(word(unname(c(flux_data[6,column_nr], recursive=TRUE))),
                                            length(flux_data$site_id_automatic)),
                            .after = column_nr+1)
    
    colnames(flux_data)[column_nr + 2 ] <- paste(word(unname(c(flux_data[3,column_nr], recursive=TRUE)),2,-1),"_depth",sep="")
    
  }
  
}



flux_data <- clean_names(flux_data[!is.na(flux_data$site_id_automatic),])

flux_data$var <- NULL


flux_data$flux_year_automatic <- as.numeric(flux_data$flux_year_automatic)


flux_data <- flux_data[!is.na(flux_data$flux_date),]
flux_data$flux_date <- as.Date(flux_data$flux_date)


colnames(flux_data) <- gsub("water_table_depth","water_table",colnames(flux_data))
colnames(flux_data) <- gsub("thaw_depth","thaw",colnames(flux_data))




## leap year
year <- unique(flux_data$flux_year_automatic)
leap <- ""
if((year %% 4) == 0){
  if((year %% 100) == 0){
    if((year %% 400) == 0){
      leap <- "Leap"
    } else {
      leap <- "NonLeap"
    }
  } else {
    leap <- "Leap"
  }
} else {
  leap <- "NonLeap"
}

flux_data$leap_year <- leap

## season_automatic



if(grepl("aus", unique(flux_data$site_id_automatic))){
  flux_data <- flux_data %>% 
    mutate(season_automatic = ifelse(leap_year == "Leap",
                                     ifelse(as.numeric(flux_julian_day_automatic) < 61 &
                                              as.numeric(flux_julian_day_automatic) >= 275,
                                            "GS",
                                            "NGS"),
                                     ifelse(as.numeric(flux_julian_day_automatic) >= 274 &
                                              as.numeric(flux_julian_day_automatic) < 60,
                                            "GS",
                                            "NGS")))
} else {
  flux_data <- flux_data %>% 
    mutate(season_automatic = ifelse(leap_year == "Leap",
                                     ifelse(as.numeric(flux_julian_day_automatic) <= 244 &
                                              as.numeric(flux_julian_day_automatic) >=153,
                                            "GS",
                                            "NGS"),
                                     ifelse(as.numeric(flux_julian_day_automatic) >= 152 &
                                              as.numeric(flux_julian_day_automatic) <= 243,
                                            "GS",
                                            "NGS")))
}


### FLUX_ID_AUTOMATIC
#flux_data$c_loss
flux_data <- flux_data %>% 
  mutate(flux_id_automatic = paste(site_id_automatic, "_", c_loss, season_automatic, "_",flux_year_automatic,sep = ""))

flux_data$reco <- ((as.numeric(flux_data$reco_raw)* 1e-3) / 44) * 12
flux_data$reco_unit <- "g C m-2 d-1"

flux_data$nee <- ((as.numeric(flux_data$nee_raw)* 1e-3) / 44) * 12
flux_data$nee_unit <- "g C m-2 d-1"

flux_data$gpp <- ((as.numeric(flux_data$gpp_raw)* 1e-3) / 44) * 12
flux_data$gpp_unit <- "g C m-2 d-1"

flux_data$ch4 <- ((as.numeric(flux_data$ch4_raw)) / 18) * 12
flux_data$raw_unit <- "mg C m-2 d-1"


colnames(flux_data) <- gsub("water_table_depth","water_table",colnames(flux_data))
colnames(flux_data) <- gsub("thaw_depth","thaw",colnames(flux_data))


#### Plot_data 
plot_data <- suppressMessages(read_excel("D:/flux_db/Working_folder/3_third round_2023/Korea/NEE_data_import_external_CAN6_v3.xlsx",
                                         sheet = "6_PLOT_DATA", skip = 2))


plot_data <- plot_data %>%
  clean_names()

  if (sum(!is.na(plot_data$plot_id)) != 0){
    
    
    
    mutates <- names(plot_data)[c(-1:-4)]
    
    # mutates <- mutates[!is.na(mutates)]
    
    
    plot_data[paste0(mutates,"_info")] <- NA
    plot_data[paste0(mutates,"_year")] <- NA
    plot_data[paste0(mutates,"_unit")] <- NA
    
    infos <- names(plot_data)[str_detect(names(plot_data), "_info")]
    
    for (i in 1:length(mutates)){
      
      plot_data[,c(infos[grep(mutates[i], infos)])] <- plot_data[1,c(mutates[i])]
    }
    
    years <- names(plot_data)[str_detect(names(plot_data), "_year")]
    
    for (i in 1:length(mutates)){
      
      plot_data[,c(years[grep(mutates[i], years)])] <- plot_data[2,c(mutates[i])]
    }
    
    units <- names(plot_data)[str_detect(names(plot_data), "_unit")]
    
    for (i in 1:length(mutates)){
      
      plot_data[,c(units[grep(mutates[i], units)])] <- ifelse(plot_data[3,
                                                                        c(mutates[i])] == "other",
                                                              plot_data[4,
                                                                        c(mutates[i])],
                                                              plot_data[3,
                                                                        c(mutates[i])])
    }
  }
  plot_data <- plot_data[,which(unlist(lapply(plot_data, function(x)!all(is.na(x))))),with=F]
  
  
  
  plot_data[plot_data == "NA"] <- NA
  
  
  plot_data <- plot_data %>%
    drop_na(`treatment`) %>%
    clean_names() %>%
    remove_empty("cols")
  
  colnames(plot_data) <-  gsub("n_coord","n_coord_plot", colnames(plot_data))
  colnames(plot_data) <-  gsub("e_coord","e_coord_plot", colnames(plot_data))
  
  
  plot_data <- data.frame(plot_data[,c(1:7)],plot_data[,sort(names(plot_data[-1:-7]))])
  
  # clean treatments
  plot_data$treatment <- gsub("CTR", "CTL",plot_data$treatment)
  
  
  # #### GRE_6 vegetation ----
  # if (str_detect(file, "gre_6")){
  #
  #   plot_data <- plot_data %>%
  #     mutate(plot_id = ifelse(treatment == "CTL",
  #                             paste(plot_id,"C",sep=""),
  #                             ifelse(treatment == "OTC",
  #                                    paste(plot_id,"T",sep=""),
  #                                    ""
  #                             )))
  #
  #
  #   plot_data <- merge(plot_data,
  #                      gre_6_veg_cover,
  #                      by = "plot_id",
  #                      all = T)
  #
  #   plot_data <- plot_data %>%
  #     mutate(site_id_automatic = "GRE_6",
  #            flux_year_automatic = unique(flux_year_automatic[!is.na(flux_year_automatic)]),
  #            treatment = ifelse(substr(plot_data$plot_id,2,6)== "LG","LongGrowing",
  #                               ifelse(substr(plot_data$plot_id,2,6)== "S","Shading",
  #                                      ifelse(substr(plot_data$plot_id,2,6)== "SG","ShortGrowing",treatment))))
  #
  #
  #
  #   fillers <- colnames(plot_data)[str_detect(colnames(plot_data), "_unit$")]
  #   for(column in fillers){
  #     plot_data[,column] <- unique(plot_data[!is.na(plot_data[,column]),column])
  #   }
  #
  # }
  colnames(plot_data) <- gsub("soil_moist", "soil_moist_plot",colnames(plot_data))
  
  
  for (doubles in intersect(colnames(plot_data),colnames(flux_data))[-c(1:4)]){
    colnames(plot_data)[grep(doubles, colnames(plot_data))] <- paste(colnames(plot_data)[grep(doubles, colnames(plot_data))],"plot",sep="_")
  }
  plot_data<- plot_data[plot_data$plot_id != 2,]
  
  assert(length(setdiff(as.factor(plot_data$plot_id),as.factor(flux_data$plot_id)))==0)
  
  flux_data <- merge(flux_data,
                     plot_data,
                     by = c("treatment","plot_id"),
                     all.x=TRUE)

  
  
  
  site_data <- suppressMessages(read_excel("D:/flux_db/Working_folder/3_third round_2023/Korea/NEE_data_import_external_CAN6_v3.xlsx",
                                           sheet = "4_SITE_DATA", skip = 2))
  site_data <- site_data[, c(1, 2)]
  
  
  
  
  
  #names(site_data) <- NULL
  site_data <- as.data.frame(t(site_data))
  site_data <- row_to_names(site_data, 1, remove_row = T, remove_rows_above = T)
  site_data <- clean_names(site_data)
  
  
  site_data$n_coord_dd <-  as.numeric(site_data$n_coord_dd)
  site_data$e_coord_dd <-  as.numeric(site_data$e_coord_dd)
  site_data$mean_annual_temperature_c <-  as.numeric(site_data$mean_annual_temperature_c)
  site_data$mean_july_temperature_c <-  as.numeric(site_data$mean_july_temperature_c)
  site_data$mean_february_temperature_c <-  as.numeric(site_data$mean_february_temperature_c)
  
  if ("annual_precipitation_mm_yr_1" %in% colnames(site_data)){
    
    site_data$mean_annual_precipitation_mm_yr_1 <-  as.numeric(site_data$annual_precipitation_mm_yr_1)
    site_data$annual_precipitation_mm_yr_1 <- NULL
    
  } else if ("mean_annual_precipitation_mm_yr_1" %in% colnames(site_data)){
    
    site_data$mean_annual_precipitation_mm_yr_1 <-  as.numeric(site_data$mean_annual_precipitation_mm_yr_1)
    
  }
  #assign(name, climate_data)
  
  site_data$site_id <- "CAN_6"
  site_data$year <- 2022
  colnames(site_data)
  
  flux_data <- merge(flux_data,
                     site_data[,c(1:6,21:23)],
                     by.x = c("site_id_automatic","flux_year_automatic"),
                     by.y = c("site_id","year"),
                     all.x = TRUE
  )
  sites <- rbind(sites,
                 site_data)
  print("   site_data")
  
  ## add climate data from sheets
  method_data <- suppressMessages(read_excel("D:/flux_db/Working_folder/3_third round_2023/Korea/NEE_data_import_external_CAN6_v3.xlsx",
                                             sheet = "3_METHOD"))
  
  method_data <- method_data[, c(1, 2)]
  #names(site_data) <- NULL
  method_data <- as.data.frame(t(method_data))
  method_data <- row_to_names(method_data, 1, remove_row = T, remove_rows_above = T)
  colnames(method_data) <- gsub("OTCx","otc_x_", colnames(method_data))
  method_data <- clean_names(method_data)
  method_data$experiment_design <- NULL
  method_data$co2 <- NULL
  method_data$ch4 <- NULL
  #colnames(method_data) <- gsub("_2", "_METH",colnames(method_data))
  co2_method <- method_data[,-c(9:14)]
  co2_method$Site_Flux_ID <- "CAN_6_2022_ER"
  ch4_method <- method_data[,-c(3:8)]
  colnames(ch4_method) <- gsub("_2","",colnames(ch4_method))
  ch4_method$Site_Flux_ID <- "CAN_6_2022_METH"
  
  method_data <- rbind.fill(co2_method,
                       ch4_method)
  
  CAN_6_v3 <- list()
  CAN_6_v3[["data"]] <- flux_data
  CAN_6_v3[["methods"]] <- method_data
  CAN_6_v3[["site"]] <- sites

  saveRDS(CAN_6_v3,"database/CAN_6_v3.rds")
  
  
  







