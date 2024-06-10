# 2024.01.23
# Jan, Sarah

# Arctic_flux_database Flux Data

# 08/06/2024

# Jan Dietrich

### Import of site specific data in r dataframe

# this script pulls out all SITE_DATA sheets from all excel sheets
# and merges them into on big table with SITE_ID_YEAR as a unique key

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



##### ALA_1 ----
ala_1_data <- clean_names(read_csv("D:/flux_db/Working_folder/itex_flux_winter_redo_fix_td.csv"))
ala_1_data$flux_date <- NULL
colnames(ala_1_data) <- c("flux_date",
                          "flux_year_automatic",
                          "flux_julian_day_automatic",
                          "treatment",
                          "treatment_if_other",
                          "plot_id",
                          "c_loss",
                          "season_automatic",
                          "flux_id_automatic",
                          "co2",
                          "co2_r2",
                          "ch4",
                          "ch4_r2",
                          "par",
                          "air_temp",
                          "soil_temp",
                          "soil_moist",
                          "water_table_depth",
                          "thaw_depth")
ala_1_data$site_id_automatic <- "ALA_1"
ala_1_data$flux_year_automatic <- year(ala_1_data$flux_date)
ala_1_data$flux_julian_day_automatic <- yday(ala_1_data$flux_date)
unique(ala_1_data$flux_year_automatic)

#### GRE_6 #####

##### add vegetation cover for GRE_6 ----
gre_6_veg_cover <- read_excel("D:/flux_db/Working_folder/GRE_6_veg_cover.xlsx")
colnames(gre_6_veg_cover) <- c("plot_id",
                         "evergreen_shrubs",
                         "deciduous_shrubs",
                         "forbs",
                         "graminoids",
                         "lichens",
                         "mosses",
                         "other",
                         "sum")
gre_6_veg_cover <- gre_6_veg_cover[,c(1,2,3,4,5,6,7)]
gre_6_veg_cover <- gre_6_veg_cover %>% 
  mutate(total_vascular_gram_forb_shrub = evergreen_shrubs + 
           deciduous_shrubs +
           forbs + 
           graminoids)

#####
 #getwd()
# enter your directory here where the files are stored
folder <- "D:/flux_db/Working_folder/1_first round_2020/"

files <- dir(folder,
             pattern = ".xlsx")

#file <- files[55]
file <- files[str_detect(files, "ala_1_2011")]

datasets <- list()
for (file in files){
  
  print(file)
  # create path to files
  path <- suppressMessages(paste(folder,
                                 file,
                                 sep=""))
  
  ###  import flux data FLUX DATA
  flux_data <- suppressMessages(read_excel(path, 
                                           sheet = "3_FLUX_DATA",
                                           skip = 1))
 
  flux_data <- flux_data %>% 
    clean_names()
  colnames(flux_data) <- tolower(colnames(flux_data))
    
    
  # delete empty columns
  drop_columns <- str_extract(colnames(flux_data),"x..$")[!is.na(str_extract(colnames(flux_data),"x..$"))]
  if (length(drop_columns) > 1){
    flux_data <- flux_data[ , -which(names(flux_data) %in% drop_columns)]
  }
    
  if (!str_detect(file, "ala_1")){
  # add new columns for co2 units and extra info ----
  flux_data <- add_column(flux_data,
                          co2_unit = rep(ifelse(flux_data$co2[3] == "other", flux_data$co2[4], flux_data$co2[3]),
                                         length(flux_data$co2)),
                          .after = "co2")
    
  flux_data <- add_column(flux_data,
                            co2_freq = rep(flux_data$co2[1],
                                           length(flux_data$co2)),
                            .after = "co2_unit")
  flux_data <- add_column(flux_data,
                            co2_slope = rep(flux_data$co2[2],
                                            length(flux_data$co2)),
                            .after = "co2_freq")
    
    
  # add new columns for ch4 units and extra info ----
  flux_data <- add_column(flux_data,
                          ch4_unit = rep(ifelse(flux_data$ch4[3] == "other", flux_data$ch4[4], flux_data$ch4[3]),
                                           length(flux_data$ch4)),
                            .after = "ch4")
    flux_data <- add_column(flux_data,
                            ch4_freq = rep(flux_data$ch4[1],
                                           length(flux_data$ch4)),
                            .after = "ch4_unit")
    flux_data <- add_column(flux_data,
                            ch4_slope = rep(flux_data$ch4[2],
                                            length(flux_data$ch4)),
                            .after = "ch4_freq")
    flux_data <- add_column(flux_data,
                            ch4_dir = rep(flux_data$ch4[5],
                                          length(flux_data$ch4)),
                            .after = "ch4_slope")
    
    
    # change columns for soiltemp and soilmosi ----
    
    
    if (file == "ala_1_2013.xlsx"){
      flux_data$soil_temp[6] <- 10
      flux_data$soil_moist[6] <- 10
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
                            soil_temp_depth = rep(flux_data$soil_temp[6],
                                                  length(flux_data$site_id_automatic)),
                            .after = "soil_temp_unit")
    
    
    flux_data <- add_column(flux_data,
                            soil_moist_unit = rep(flux_data$soil_moist[3],
                                                  length(flux_data$site_id_automatic)),
                            .after = "soil_moist")
    
    
    flux_data <- add_column(flux_data,
                            soil_moist_depth = rep(flux_data$soil_moist[6],
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
  }
  if (str_detect(file,  "ala_1")){
    ### ALA_1_redo
      ala_1_data_redo <- ala_1_data[ala_1_data$flux_year_automatic == unique(flux_data$flux_year_automatic)[2],]
      
      # add new columns for co2 units and extra info ----
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              co2_unit = rep(ifelse(flux_data$co2[3] == "other", flux_data$co2[4], flux_data$co2[3]),
                                             length(ala_1_data_redo$co2)),
                              .after = "co2")
      
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              co2_freq = rep(flux_data$co2[1],
                                             length(ala_1_data_redo$co2)),
                              .after = "co2_unit")
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              co2_slope = rep(flux_data$co2[2],
                                              length(ala_1_data_redo$co2)),
                              .after = "co2_freq")
      
      
      # add new columns for ch4 units and extra info ----
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              ch4_unit = rep(ifelse(flux_data$ch4[3] == "other", flux_data$ch4[4], flux_data$ch4[3]),
                                             length(ala_1_data_redo$ch4)),
                              .after = "ch4")
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              ch4_freq = rep(flux_data$ch4[1],
                                             length(ala_1_data_redo$ch4)),
                              .after = "ch4_unit")
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              ch4_slope = rep(flux_data$ch4[2],
                                              length(ala_1_data_redo$ch4)),
                              .after = "ch4_freq")
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              ch4_dir = rep(flux_data$ch4[5],
                                            length(ala_1_data_redo$ch4)),
                              .after = "ch4_slope")
      
      
      # change columns for soiltemp and soilmosi ----
      
      
      if (file == "ala_1_2013.xlsx"){
        flux_data$soil_temp[6] <- 10
        flux_data$soil_moist[6] <- 10
      }
      
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              par_unit = rep(flux_data$par[3],
                                             length(ala_1_data_redo$site_id_automatic)),
                              .after = "par")
      
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              air_temp_unit = rep(flux_data$air_temp[3],
                                                  length(ala_1_data_redo$site_id_automatic)),
                              .after = "air_temp")
      
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              soil_temp_unit = rep(flux_data$soil_temp[3],
                                                   length(ala_1_data_redo$site_id_automatic)),
                              .after = "soil_temp")
      
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              soil_temp_depth = rep(flux_data$soil_temp[6],
                                                    length(ala_1_data_redo$site_id_automatic)),
                              .after = "soil_temp_unit")
      
      
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              soil_moist_unit = rep(flux_data$soil_moist[3],
                                                    length(ala_1_data_redo$site_id_automatic)),
                              .after = "soil_moist")
      
      
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              soil_moist_depth = rep(flux_data$soil_moist[6],
                                                     length(ala_1_data_redo$site_id_automatic)),
                              .after = "soil_moist_unit")
      
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              water_table_depth_unit = rep(flux_data$water_table_depth[3],
                                                           length(ala_1_data_redo$site_id_automatic)),
                              .after = "water_table_depth")
      
      ala_1_data_redo <- add_column(ala_1_data_redo,
                              thaw_depth_unit = rep(flux_data$thaw_depth[3],
                                                    length(ala_1_data_redo$site_id_automatic)),
                              .after = "thaw_depth")
      
      flux_data <- ala_1_data_redo
      
  }
    
    if ((file == "swe_16_2003.xlsx"|file =="swe_16_2007.xlsx")|file =="swe_16_2011.xlsx"){
      flux_data$soil_temp_depth <- 10
      flux_data$soil_moist_depth <- "0-15"
    }
    if (file == "swe_16_2005.xlsx" | file == "swe_16_2006.xlsx" ){
      colnames(flux_data) <- gsub("additional_data_2", "soil_temp_3",colnames(flux_data))
      flux_data$soil_temp_3_depth <- 40
      colnames(flux_data) <- gsub("additional_data_1", "soil_temp_2",colnames(flux_data))
      flux_data$soil_temp_2_depth <- 30
      colnames(flux_data) <- gsub("additional_data", "soil_temp_1",colnames(flux_data))
      flux_data$soil_temp_1_depth <- 20
    }
    
    
    if (sum(!is.na(str_extract(colnames(flux_data), "add"))) !=0){
      
      
      add_columns <- colnames(flux_data)[!is.na(str_extract(colnames(flux_data), "add"))]
      
      
      for (i in 1:length(add_columns)){
        
        column_nr <- grep(paste("^",colnames(flux_data)[colnames(flux_data) == add_columns[i]],"$",sep=""), colnames(flux_data))
        
        colnames(flux_data)[column_nr] <- paste(word(unname(c(flux_data[3,column_nr], recursive=TRUE)),2,-1),i,sep="_")
        
        flux_data <- add_column(flux_data,
                                new_unit = rep(word(unname(c(flux_data[3,column_nr], recursive=TRUE))),
                                               length(flux_data$site_id_automatic)),
                                .after = column_nr)
        
        colnames(flux_data)[column_nr + 1 ] <- paste(word(unname(c(flux_data[3,column_nr], recursive=TRUE)),2,-1),i,"unit",sep="_")
        
        
        flux_data <- add_column(flux_data,
                                new_depth = rep(word(unname(c(flux_data[6,column_nr], recursive=TRUE))),
                                                length(flux_data$site_id_automatic)),
                                .after = column_nr+1)
        
        colnames(flux_data)[column_nr + 2 ] <- paste(word(unname(c(flux_data[3,column_nr], recursive=TRUE)),2,-1),i,"depth",sep="_")
        
      }
      
    }
    
    
    
    #   if (file == "swe_17_2015.xlsx" |file == "swe_18_2015.xlsx" ){
    #     colnames(flux_data)[!is.na(str_extract(colnames(flux_data), "add"))] <- word(unname(c(flux_data[3,!is.na(str_extract(colnames(flux_data), "add"))], recursive=TRUE)),2,-1)
    #   
    #     flux_data <- flux_data %>% 
    #       clean_names()
    #     
    #     
    #     word(unname(c(flux_data[3,column_nr], recursive=TRUE)))
    #     
    #     
    #     } else {
    #   
    #     colnames(flux_data)[!is.na(str_extract(colnames(flux_data), "add"))] <- paste0(word(unname(c(flux_data[3,!is.na(str_extract(colnames(flux_data), "add"))], recursive=TRUE)),2,-1),
    #                                                                                "_",
    #                                                                                as.vector(flux_data[6,!is.na(str_extract(colnames(flux_data), "add"))]) )
    #     }
    #   colnames(flux_data) <- gsub("_NA","", colnames(flux_data))
    # }
    
    flux_data <- clean_names(flux_data[!is.na(flux_data$site_id_automatic),])
    
    flux_data$var <- NULL
    
    if (sum(!is.na(str_extract(colnames(flux_data), "x23"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "x23"] <- "soil_temp_15"
      
    }
    
    
    if (sum(!is.na(str_extract(colnames(flux_data), "temp_inside_chamber_air_t_measured_inside_the_chmber_during_measurement"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "temp_inside_chamber_air_t_measured_inside_the_chmber_during_measurement"] <- "temp_inside_chamber"
      
    }
    
    if (sum(!is.na(str_extract(colnames(flux_data), "temp_inside_chamber_measured_inside_gas_chamber"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "temp_inside_chamber_measured_inside_gas_chamber"] <- "temp_inside_chamber"
      
    }
    
    if (sum(!is.na(str_extract(colnames(flux_data), "temp_inside_chamber_inside_gas_chamber_temp"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "temp_inside_chamber_inside_gas_chamber_temp"] <- "temp_inside_chamber"
      
    }
    
    if (sum(!is.na(str_extract(colnames(flux_data), "temp_inside_chamber_measured_inside_the_chamber"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "temp_inside_chamber_measured_inside_the_chamber"] <- "temp_inside_chamber"
      
    }
    
    if (sum(!is.na(str_extract(colnames(flux_data), "air_tremp"))) !=0){
      
      flux_data$air_temp <- flux_data$air_tremp
      
      flux_data$air_temp_unit <- flux_data$air_tremp_unit 
      
      flux_data$air_tremp_depth <- NULL
      
      flux_data$air_tremp <- NULL
      
      flux_data$air_tremp_unit <- NULL
    }
    
    
    
    
    
    if(file == "gre_6_2010.xlsx"){
      flux_data$co2_unit <- "mgCO2m-2h-1"
      flux_data$co2_freq <- "1 measurement per day"
      flux_data$co2_slope <- "Linear slope"
      flux_data$soil_moist_depth <- 6
    }
    
    flux_data$co2[flux_data$co2 == "NA"] <- NA
    if (sum(!is.na(flux_data$co2)) != 0 && !is.na(unique(flux_data$co2_unit))){
      
      unitco2 <- unique(flux_data$co2_unit)
      
      
      
      if (unitco2 == "µmol CO2 m-2 s-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-6 * 12 * (60*60*24)
      } 
      
      else if (unitco2 == "g CO2 m-2 h-1"){
        flux_data$co2_gCm2d1 <- (as.numeric(flux_data$co2) / 44) * 12 *24
      } 
      
      else if (unitco2 == "g co2 m-2 hr-1"){
        flux_data$co2_gCm2d1 <- (as.numeric(flux_data$co2) / 44) * 12 *24
      }
      
      else if (unitco2 == "mg CO2 m-2 hr-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 *24
      }
      
      else if (unitco2 == "mg CO2 m-2 day-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12
      }
      
      else if (unitco2 == "µmol CO2 m-2 h-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-6 * 12 * 24
      }
      
      else if (unitco2 == "mmol CO2 m-2 s-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-3 * 12 * (60*60*24)
      }
      
      else if (unitco2 == "g CO2 m-2 day-1"){
        flux_data$co2_gCm2d1 <- (as.numeric(flux_data$co2) / 44) * 12
      }
      
      else if (unitco2 == "mg CO2 m-2 h-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 *24
      }
      
      else if (unitco2 == "g CO2-C m-2 half-hour-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 24 * 2
      }
      
      else if (unitco2 == "mg CO2m-2h-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2) * 1e-3) / 44) * 12 * 24
      }
      
      else if (unitco2 == "mg CO2-C m-2 h-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-3 * 24
      } 
      
      else if (unitco2 == "gCO2 m-2 hr-1"){
        flux_data$co2_gCm2d1 <- (as.numeric(flux_data$co2) / 44) * 12 * 24
      }
      
      else if (unitco2 == "mg C-CO2 /m2 / day"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-3
      }
      
      else if (unitco2 == "mg C-CO2 / m2 day"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-3
      }
      
      else if (unitco2 == "mgCO2m-2h-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 * 24
      }
      
      else if (unitco2 == "mgC.m2.hr"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2)* 1e-3 * 24
      }
      else {
        
        print("what up?!")
        stop()
      }
      range(flux_data$co2_gCm2d1)
    }
    
    
    
    # harry ----
    
    
    
    # if (flux_data$site_id_automatic == "CAN_3"){
    #   
    #   flux_data$ch4[flux_data$ch4 == "NA"] <- NA
    # }
    if (sum(!is.na(flux_data$ch4)) != 0 && !is.na(unique(flux_data$ch4_unit))){
      
      if (unique(flux_data$site_id_automatic) == "CAN_3"|unique(flux_data$site_id_automatic) == "CAN_4"){
        flux_data$ch4_dir <- "Negative = Incoming to soil vs. Positive = Outgoing from soil"
      }
      
      
      if (unique(flux_data$site_id_automatic) %in%  c("NOR_13",
                                                      "NOR_14",
                                                      "NOR_15",
                                                      "NOR_16",
                                                      "NOR_2",
                                                      "NOR_3",
                                                      "NOR_4",
                                                      "NOR_5",
                                                      "SWE_12",
                                                      "SWE_13",
                                                      "SWE_14",
                                                      "SWE_15")){
        
        flux_data$ch4_dir <- "Negative = Incoming to soil vs. Positive = Outgoing from soil"
      }
      
      unitch4 <- unique(flux_data$ch4_unit)
      
      if (unique(flux_data$ch4_dir)!="Negative = Incoming to soil vs. Positive = Outgoing from soil"){
        flux_data$ch4 <- -as.numeric(flux_data$ch4)
      }
      
      if (unitch4 == "g CH4 m-2 h-1"){
        
        flux_data$ch4_mgCm2d1  <- ((as.numeric(flux_data$ch4) * 1e-6) / 18)* 1e3 * 12 * 24
        
      } else if (unitch4 == "µmol CH4 m-2 s-1"){
        flux_data$ch4_mgCm2d1 <- (as.numeric(flux_data$ch4) * 1e-6)* 1e3 * 12 * (60*60*24)
        
      } else if (unitch4 == "µmol CH4 m-2 h-1"){
        flux_data$ch4_mgCm2d1 <- (as.numeric(flux_data$ch4)* 1e-6) * 1e3  * 12 * 24
        
      } else if (unitch4 == "mg CH4 m-2 day-1"){
        flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4)) / 18) * 12
        
      } else if (unitch4 == "mg CH4 m-2 hr-1"){
        flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4)) / 18) * 12 * 24
        
      } else if (unitch4 == "mg CH4 m-2 hr1"){
        flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4)) / 18) * 12 * 24
        
      } else if (unitch4 == "mg C-CH4 /m2 / day"){
        flux_data$ch4_mgCm2d1 <- as.numeric(flux_data$ch4)
        
      } else if (unitch4 == "mg C-CH4 / m2 day"){
        flux_data$ch4_mgCm2d1 <- as.numeric(flux_data$ch4)
        
      } else if (unitch4 == "g CH4 m-2 day-1"){
        flux_data$ch4_mgCm2d1 <- (as.numeric(flux_data$ch4) *1e3 / 18) * 12
        
      } else if ( unitch4 == "mg C m-2 day-1"){
        flux_data$ch4_mgCm2d1 <- as.numeric(flux_data$ch4) 
      } 
      
    }
    
    # single case unit conversion
    if (file == "chi_4_2014.xlsx"){
      flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4) * 1e-6) / 18) * 12 * 24
    }
    
    
    if (file == "chi_4_2015.xlsx"){
      flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4) * 1e-6) / 18) * 12 * 24
    }
    
    
    
    # special transoformations ----  
    
    # missing data for ex, gathered from other datasdets
    if (file == "NOR_14_2000.xlsx"){
      flux_data$plot_id[flux_data$treatment == "OTC"& flux_data$plot_id == "C_3"] <- "O_3"
    }
    
    
    
    flux_data$flux_year_automatic <- as.numeric(flux_data$flux_year_automatic)
    
    
    flux_data <- flux_data[!is.na(flux_data$flux_date),]
    
    if (file == "fin_1_2013.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format= "%d/%m/%Y")  
      flux_data$flux_year_automatic <- year(flux_data$flux_date)
      flux_data$flux_julian_day_automatic <- yday(flux_data$flux_date)
      flux_data$leap_year <- "Non-Leap"
    }
    
    colnames(flux_data)[grep("^soiltemp$",colnames(flux_data))] <- "soil_temp_1"
    colnames(flux_data)[grep("^soiltemp_unit$",colnames(flux_data))] <- "soil_temp_unit_1"
    colnames(flux_data)[grep("^soiltemp_depth$",colnames(flux_data))] <- "soil_temp_depth_1"
    
    
    colnames(flux_data)[grep("soiltemp",colnames(flux_data))] <- gsub("soiltemp","soil_temp",colnames(flux_data)[grep("soiltemp",colnames(flux_data))])
    
    colnames(flux_data) <- gsub("co2", "co2_raw", colnames(flux_data))
    colnames(flux_data)[colnames(flux_data)=="r2_14"] <- "co2_raw_r2"
    
    colnames(flux_data) <- gsub("ch4", "ch4_raw", colnames(flux_data))
    colnames(flux_data)[colnames(flux_data)=="r2_16"] <- "ch4_raw_r2"
    
    
    
    if ("co2_raw_gCm2d1" %in% colnames(flux_data)){
      colnames(flux_data)[colnames(flux_data)=="co2_raw_gCm2d1"] <- "co2"
      flux_data <- add_column(flux_data,
                              co2_unit = rep("g C m-2 d-1",
                                             length(flux_data$site_id_automatic)),
                              .after = "co2")
    }
    
    
    if ("ch4_raw_mgCm2d1" %in% colnames(flux_data)){ 
      colnames(flux_data)[colnames(flux_data)=="ch4_raw_mgCm2d1"] <- "ch4"
      flux_data <- add_column(flux_data,
                              ch4_unit = rep("mg C m-2 d-1",
                                             length(flux_data$site_id_automatic)),
                              .after = "ch4")
      flux_data <- add_column(flux_data,
                              ch4_dir = ifelse(!is.na(flux_data$ch4_raw_dir),
                                               "Negative = Incoming to soil vs. Positive = Outgoing from soil",
                                               NA),
                              .after = "ch4_unit")
      
      
    }
    
    
    
    
    colnames(flux_data) <- gsub("water_table_depth","water_table",colnames(flux_data))
    colnames(flux_data) <- gsub("thaw_depth","thaw",colnames(flux_data))
    
    
    
    ## switch date formats
    if (file == "swe_16_2011.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_4_2019.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_4_2018.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "fin_1_2013.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "nor_11_2020.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "nor_12_2020.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_1_2016.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_1_2017.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_2_2016.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_2_2017.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_3_2016.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_3_2017.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "ala_2_2015.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else {
      flux_data$flux_date <- as.Date(flux_data$flux_date)
    }
    
    
    
    
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
    
    
    
    if(grepl("aus", file)){
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
    
    flux_data <- flux_data %>% 
      mutate(flux_id_automatic = paste(site_id_automatic, "_", c_loss, season_automatic, "_",flux_year_automatic,sep = ""))
    
   print("   fluxdata")
    
    
    #### PLOT DATA
    plot_data <- read_excel(path, 
                            sheet = "4_PLOT_METADATA",
                            skip = 2)
    
    
    
    plot_data <- plot_data %>% 
      clean_names()
    
     if (sum(!is.na(plot_data$plot_id)) != 0){
         
      
      
      mutates <- names(plot_data)[c(-1:-6)]
      
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
      
      plot_data <- plot_data[,which(unlist(lapply(plot_data, function(x)!all(is.na(x))))),with=F]
      
      
      plot_data <- add_column(plot_data,
                              site_id_automatic = rep(paste(toupper(unlist(strsplit(file,
                                                                                    "_"))[1]),
                                                            unlist(strsplit(file,
                                                                            "_"))[2],
                                                            sep = "_"),
                                                      length(plot_data$plot_id)),
                              .before = "treatment")
      
      
      plot_data <- add_column(plot_data,
                              flux_year_automatic = rep(as.numeric(gsub(".xlsx",
                                                                        "",
                                                                        unlist(strsplit(file,
                                                                                        "_"))[3])),
                                                        length(plot_data$plot_id)),
                              .before = "treatment")
      
      
      
      plot_data[plot_data == "NA"] <- NA
      
      
      plot_data <- plot_data %>%
        drop_na(`treatment`) %>% 
        clean_names() %>% 
        remove_empty("cols")
      
      
      plot_data <- data.frame(plot_data[,c(1:7)],plot_data[,sort(names(plot_data[-1:-7]))])
      
      
      if ( file == "sva_8_2018.xlsx"){
        plot_data$treatment[plot_data$treatment == "SVA_8"] <- plot_data$e_coord[plot_data$treatment == "SVA_8"] 
        plot_data$treatment <- gsub("otc", "OTC",plot_data$treatment)
        plot_data$treatment <- gsub("c", "CTL",plot_data$treatment)
      }
      
      # clean treatments 
      plot_data$treatment <- gsub("CTR", "CTL",plot_data$treatment)
      
      
      
      
      #### GRE_6 vegetation ----
      if (str_detect(file, "gre_6")){
        
        plot_data <- plot_data %>% 
          mutate(plot_id = ifelse(treatment == "CTL",
                                  paste(plot_id,"C",sep=""),
                                  ifelse(treatment == "OTC",
                                         paste(plot_id,"T",sep=""),
                                         ""
                                  )))
        
        
        plot_data <- merge(plot_data,
                           gre_6_veg_cover,
                           by = "plot_id",
                           all = T)
        
        plot_data <- plot_data %>% 
          mutate(site_id_automatic = "GRE_6",
                 flux_year_automatic = unique(flux_year_automatic[!is.na(flux_year_automatic)]),
                 treatment = ifelse(substr(plot_data$plot_id,2,6)== "LG","LongGrowing",
                                    ifelse(substr(plot_data$plot_id,2,6)== "S","Shading",
                                           ifelse(substr(plot_data$plot_id,2,6)== "SG","ShortGrowing",treatment))))
        
        
        
        fillers <- colnames(plot_data)[str_detect(colnames(plot_data), "_unit$")]
        for(column in fillers){
          plot_data[,column] <- unique(plot_data[!is.na(plot_data[,column]),column]) 
        } 
        
      }
      
      
      if (file == "SVA_2018"){
        plot_data$treatment[plot_data$treatment == "SVA_8"] <- plot_data$e_coord[plot_data$treatment == "SVA_8"]
        plot_data$treatment[plot_data$treatment == "c"] <- "CTL"
        plot_data$treatment[plot_data$treatment == "otc"] <- "OTC"
      }
      for (doubles in intersect(colnames(plot_data),colnames(flux_data))[-c(1:4)]){
        colnames(plot_data)[grep(doubles, colnames(plot_data))] <- paste(colnames(plot_data)[grep(doubles, colnames(plot_data))],"plot",sep="_")
      } 
      
      flux_data <- merge(flux_data,
                         plot_data,
                         by = c("site_id_automatic","flux_year_automatic","treatment","plot_id"),
                         all.x=TRUE)
      print("   plotdata")
     }
    
    
    
    
    ## add climate data from sheets
    site_data <- read_excel(path,
                               sheet = "2_SITE_DATA",
                               skip = 1
    )
    
    
    
    
    site_data <- site_data[, c(1, 2)]
    
    site_data <- rbind(
      row_to_names(data.frame(c("Site_ID", "Year"),
                              c(names(site_data)[2], str_sub(file, -9, -6)
                              )),
                   1,
                   remove_row = F),
      site_data
    )
    if (site_data[2,2] == "dart"){
      site_data[2,2] <- str_sub(gsub("_dart.xlsx","",file),-4,-1)
    }
    
    
    
    names(site_data) <- NULL
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
    
    flux_data <- merge(flux_data,
                          site_data,
                          by.x = c("site_id_automatic","flux_year_automatic"),
                          by.y = c("site_id","year"),
                          all.x = TRUE
    )
    print("   site_data")
    
    rm(site_data)
    
    
    
    datasets[[file]] <- flux_data
    
}

first_round <- list()

sites_first_round <- read_csv("D:/flux_db/Working_folder/1_first round_2020/method_site/site_data.csv", 
                      col_types = cols(...1 = col_skip()))

methods_first_round <- read_csv("D:/flux_db/Working_folder/1_first round_2020/method_site/method.csv", 
                   col_types = cols(...1 = col_skip()))

first_round[["datasets"]] <- datasets
first_round[["sites"]] <- sites_first_round
first_round[["methods"]] <- methods_first_round

saveRDS(first_round, file = "database/first_round.rds")

















