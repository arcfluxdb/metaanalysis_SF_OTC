## compile first and second round
rm(list = ls())

library(plyr)

first <- readRDS("database/first_round.rds")
second <- readRDS("database/second_round.rds")


### site data merge

site1 <- first[["sites"]]
site2 <- second[["sites"]]

colnames(site1) <- c("site_id",
                     "year",
                     "site_name",
                     "n_coord_dd",
                     "e_coord_dd",
                     "mean_annual_temperature_c",
                     "mean_july_temperature_c",
                     "mean_february_temperature_c",
                     "mean_annual_precipitation_mm_yr_1",
                     "zone",
                     "altitude_m",
                     "Veg_Type",
                     "soil_type",
                     "active_layer_depth",
                     "parent_material",
                     "parent_material_other",
                     "soil_moisture_category",
                     "organic_layer_depth",
                     "Ambient_Snow_D",
                     "permafrost_category_site",                       
                     "p_h_category",
                     "vegetation_type_walker_et_al_2005")
colnames(site2)

setdiff(colnames(site1),colnames(site2))
setdiff(colnames(site2),colnames(site1))

sites_combined <- rbind.fill(site1,
                             site2)
sites_combined$zone

###method
methods1 <- first[["methods"]]
methods2 <- second[["methods"]]

colnames(methods1) <- c("site_id",
                        "Site_Name.x",
                        "Contact.x",
                        "Additional_Contacts.x",
                        "year",
                        "c_loss",
                        "Site_Flux_ID",
                        "measurment_method",
                        "time_of_measurement",
                        "Analysis",
                        "machine",
                        "machine_type",
                        "experimental_warming",
                        "total_number_of_plots",
                        "number_of_otc_plots",
                        "number_of_snowfence_plots",
                        "number_of_ot_cx_snowfence_plots",
                        "number_of_control_plots",       
                        "number_of_snowfence",
                        "plot_size_m2",
                        "flux_plot_size_m2",
                        "otc_height_cm",
                        "otc_removal",
                        "flux_measurement_start_year",
                        "warming_start_year",
                        "Exp_End",
                        "comments_on_experiment_design",
                        "comments_on_methodology",
                        "Weath_Stat",
                        "Weath_Stat_Loc",
                        "Comm_Env"  )
colnames(methods2)

setdiff(colnames(methods1),colnames(methods2))
setdiff(colnames(methods2),colnames(methods1))

methods_combined <- rbind.fill(methods1,
                               methods2)



#### fluxes


flux1 <- first[["datasets"]]

first_round <- data.frame()

for(dataset in flux1){
  first_round <- rbind.fill(first_round,
                             dataset)
}




flux2 <- second[["datasets"]]

second_round <- data.frame()

for(dataset in flux2){
  second_round <- rbind.fill(second_round,
                            dataset)
}


colnames(first_round)
colnames(second_round)

setdiff(colnames(first_round),colnames(second_round))
setdiff(colnames(second_round),colnames(first_round))

colnames(second_round) <- gsub("_above","",colnames(second_round))
colnames(first_round) <- gsub("_coord","_coord_plot",colnames(first_round))

second_round$zone <- NULL
second_round$e_coord_dd <- NULL
second_round$n_coord_dd <- NULL
second_round$biomass_below_unit <- NULL
second_round$biomass_below_info <- NULL
second_round$e_coord_plot_unit <- NULL
second_round$site_name <- NULL

combined_data <- rbind.fill(first_round,
                            second_round)
 

database <- list()


database[["fluxdata"]] <- combined_data
database[["sitedata"]] <- sites_combined
database[["methoddata"]] <- methods_combined


saveRDS(database,"database/database.rds")


