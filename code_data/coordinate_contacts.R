#
#get table
rm(list = ls())
database <- readRDS("database/database.rds")

fluxdata <- database$fluxdata
unique(fluxdata$site_id_automatic)


colnames(fluxdata)


overview <- fluxdata %>% 
  group_by(site_id_automatic) %>% 
  dplyr::summarise(year = paste(unique(flux_year_automatic),collapse = ","),
                   treatment = paste(unique(treatment), collapse = ","),
                   c_loss = paste(unique(c_loss), collapse = ","),
                   par = ifelse(sum(!is.na(par))>0 ,"X",""),
                   Tair = ifelse(sum(!is.na(air_temp))>0 ,"X",""),
                   Tsoil = ifelse(sum(!is.na(soil_temp))>0 ,"X",""),
                   Msoil = ifelse(sum(!is.na(soil_moist))>0 ,"X",""),
                   watertable = ifelse(sum(!is.na(water_table))>0 ,"X",""),
                   thaw  = ifelse(sum(!is.na(thaw))>0 ,"X",""),
                   biomass = ifelse(sum(!is.na(biomass))>0 ,"X",""),
                   bulk_dens_1 = ifelse(sum(!is.na(bulk_dens_1))>0 ,"X",""),
                   bulk_dens_2 = ifelse(sum(!is.na(bulk_dens_2))>0 ,"X",""),
                   c_n_1 = ifelse(sum(!is.na(c_n_1))>0 ,"X",""),
                   c_n_2 = ifelse(sum(!is.na(c_n_2))>0 ,"X",""),
                   deciduous_shrubs = ifelse(sum(!is.na(deciduous_shrubs))>0 ,"X",""),
                   evergreen_shrubs = ifelse(sum(!is.na(evergreen_shrubs))>0 ,"X",""),
                   forbs = ifelse(sum(!is.na(forbs))>0 ,"X",""),
                   graminoids = ifelse(sum(!is.na(graminoids))>0 ,"X",""),
                   lichens = ifelse(sum(!is.na(lichens))>0 ,"X",""),
                   mosses = ifelse(sum(!is.na(mosses))>0 ,"X",""),
                   org_layer_depth = ifelse(sum(!is.na(org_layer_depth))>0 ,"X",""),
                   p_hh2o_1 = ifelse(sum(!is.na(p_hh2o_1))>0 ,"X",""),
                   p_hh2o_2 = ifelse(sum(!is.na(p_hh2o_2))>0 ,"X",""),
                   snow_depth  = ifelse(sum(!is.na(snow_depth))>0 ,"X",""),
                   soc_1 = ifelse(sum(!is.na(soc_1))>0 ,"X",""),
                   soc_2 = ifelse(sum(!is.na(soc_2))>0 ,"X",""),
                   soil_moist_plot = ifelse(sum(!is.na(soil_moist_plot))>0 ,"X",""),
                   som_1 = ifelse(sum(!is.na(som_1))>0 ,"X",""),
                   som_2 = ifelse(sum(!is.na(som_2))>0 ,"X",""),
                   son_1 = ifelse(sum(!is.na(son_1))>0 ,"X",""),
                   son_2 = ifelse(sum(!is.na(son_2))>0 ,"X",""),
                   thaw_depth = ifelse(sum(!is.na(thaw_depth))>0 ,"X",""),
                   total_vascular_gram_forb_shrub = ifelse(sum(!is.na(total_vascular_gram_forb_shrub))>0 ,"X",""),
                   mean_annual_temperature_c = ifelse(sum(!is.na(mean_annual_temperature_c))>0 ,"X",""),
                   mean_july_temperature_c = ifelse(sum(!is.na(mean_july_temperature_c))>0 ,"X",""),
                   mean_february_temperature_c = ifelse(sum(!is.na(mean_february_temperature_c))>0 ,"X",""),
                   mean_annual_precipitation_mm_yr_1 = ifelse(sum(!is.na(mean_annual_precipitation_mm_yr_1))>0 ,"X",""),
                   community_mean_plant_height = ifelse(sum(!is.na(community_mean_plant_height))>0 ,"X",""),
                   temp_inside_chamber_1 = ifelse(sum(!is.na(temp_inside_chamber_1))>0 ,"X",""),
                   bacterial_bm_dna = ifelse(sum(!is.na(bacterial_bm_dna))>0 ,"X",""),
                   bacterial_bm_weight = ifelse(sum(!is.na(bacterial_bm_weight))>0 ,"X",""),
                   fungal_bm_dna = ifelse(sum(!is.na(fungal_bm_dna))>0 ,"X",""),
                   fungal_bm_weight = ifelse(sum(!is.na(fungal_bm_weight))>0 ,"X",""),
                   f_b_ratio = ifelse(sum(!is.na(f_b_ratio))>0 ,"X",""))


writexl::write_xlsx(overview,"data_gathering/overview_table.xlsx")


site_data <- distinct(database$sitedata[,c(1,4,5)])
contact_data <- distinct(database$methoddata[,c(1,3,4,6)])

contact_data <- distinct(merge(contact_data,
                      site_data,
                      by = "site_id",
                      all.y = F))

contacts <- merge(contact_data,
                  overview[,c(1,2)],
                  by.x = "site_id",
                  by.y = "site_id_automatic")

writexl::write_xlsx(contacts,"data_gathering/contact_data_table.xlsx")
