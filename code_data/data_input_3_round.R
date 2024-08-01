
getwd()
database <- readRDS("database/database.rds")
gre1 <- readRDS("database/GRE_1.rds")
can6 <- readRDS("database/CAN_6_v3.rds")
ala14 <- readRDS("database/ALA_14.rds")

fluxes <- database$fluxdata
fluxesgre <- gre1$data
fluxescan <- can6$data
fluxesala <- ala14$data


setdiff(colnames(fluxescan),colnames(fluxes))

colnames(fluxes) <- gsub("co2","reco",colnames(fluxes))
fluxescan$n_coord_dd <- NULL
fluxescan$e_coord_dd <- NULL
fluxescan$site_name <- NULL
fluxescan$raw_unit <- NULL
fluxescan$e_coord_plot_unit <- NULL


fluxes <- rbind.fill(fluxes,
                fluxescan)


setdiff(colnames(fluxesala),colnames(fluxes))
fluxesala$n_coord_dd <- NULL
fluxesala$e_coord_dd <- NULL
fluxesala$site_name <- NULL
fluxesala$raw_unit <- NULL
fluxesala$e_coord_plot_unit <- NULL

fluxes <- rbind.fill(fluxes,
                     fluxesala)

database[["fluxdata"]] <- fluxes

sites <- database$sitedata
sitegre <- gre1$site
sitecan <- can6$site
siteala <- ala14$site

sites <- rbind.fill(sites,
                     sitegre)


sitecan <- sitecan %>% 
  dplyr::rename(Veg_Type = vegetation_type,
                vegetation_type_walker_et_al_2005 = vegetation_class,
                permafrost_category_site = permafrost_category,
                Ambient_Snow_D = ambient_snow_cover_cm)

setdiff(colnames(sitecan),colnames(sites))

sites <- rbind.fill(sites,
                    sitecan)

siteala <- siteala %>% 
  dplyr::rename(Veg_Type = vegetation_type,
                vegetation_type_walker_et_al_2005 = vegetation_class,
                permafrost_category_site = permafrost_category,
                Ambient_Snow_D = ambient_snow_cover_cm)

setdiff(colnames(siteala),colnames(sites))
sites <- rbind.fill(sites,
                    siteala)

database[["sitedata"]] <- sites

meths <- database$methoddata
methgre <- gre1$methods
methcan <- can6$methods
methala <- ala14$methods

setdiff(colnames(methgre),colnames(meths))

meths <- rbind.fill(meths,
                    methgre)

setdiff(colnames(methcan),colnames(meths))
setdiff(colnames(meths),colnames(methcan))
colnames(meths) <- gsub("Site_Name.x","Site_Name",colnames(meths))
colnames(meths) <- gsub("Contact.x","Contact",colnames(meths))
colnames(meths) <- gsub("Additional_Contacts.x","Additional_Contacts",colnames(meths))
colnames(meths) <- gsub("number_of_ot_cx_snowfence_plots","number_of_otc_x_snowfence_plots",colnames(meths))
methcan$Site_Name <- "Cambridge Bay"
methcan$Contact <- "Ji Young Jung"
methcan$Additional_Contacts <- ""
methcan$c_loss <- c("ER","METH")
colnames(methcan) <- gsub("analysis","Analysis", colnames(methcan))
colnames(methcan) <- gsub("experiment_start_year","warming_start_year", colnames(methcan))
colnames(methcan) <- gsub("experiment_end_year","Exp_End", colnames(methcan))


meths <- rbind.fill(meths,
                    methcan)

setdiff(colnames(methala),colnames(meths))
setdiff(colnames(meths),colnames(methala))
methala$Site_Name <- "Alaska Council"
methala$Contact <- "Ji Young Jung"
methala$Additional_Contacts <- ""
methala$c_loss <- c("ER","METH")
colnames(methala) <- gsub("analysis","Analysis", colnames(methala))
colnames(methala) <- gsub("experiment_start_year","warming_start_year", colnames(methala))
colnames(methala) <- gsub("experiment_end_year","Exp_End", colnames(methala))

meths <- rbind.fill(meths,
                    methala)

database[["methoddata"]] <- meths


saveRDS(database,"database/database.rds")


##########

s1107 <- read_excel("D:/flux_db/Working_folder/3_third round_2023/SVA_11_2007.xlsx", 
                          col_types = c("date", "text", "text", 
                                        "text", "text", "text", "numeric", 
                                        "numeric", "skip", "skip", "numeric", 
                                        "numeric"))
s1108 <- read_excel("D:/flux_db/Working_folder/3_third round_2023/SVA_11_2008.xlsx", 
                    col_types = c("date", "text", "text", 
                                  "text", "text", "text", "numeric", 
                                  "numeric", "skip", "skip", "numeric", 
                                  "numeric"))
s1207 <- read_excel("D:/flux_db/Working_folder/3_third round_2023/SVA_12_2007.xlsx", 
                    col_types = c("date", "text", "text", 
                                  "text", "text", "text", "numeric", 
                                  "numeric", "skip", "skip", "numeric", 
                                  "numeric"))
s1208 <- read_excel("D:/flux_db/Working_folder/3_third round_2023/SVA_12_2008.xlsx", 
                    col_types = c("date", "text", "text", 
                                  "text", "text", "text", "numeric", 
                                  "numeric", "skip", "skip", "numeric", 
                                  "numeric"))
sva1112 <- rbind(s1107,s1108,s1207,s1208)
sva1112$veg <- ifelse(sva1112$veg == "heath", "SVA_11", "SVA_12")
colnames(sva1112) <- c("flux_date",
                       "site_id_automatic",
                       "treatment",
                       "remove",
                       "remove1",
                       "plot_id",
                       "reco_raw",
                       "soil_moist",
                       "air_temp",
                       "soil_temp")
sva1112$flux_year_automatic <- year(sva1112$flux_date)
sva1112$flux_julian_day_automatic <- yday(sva1112$flux_date)
sva1112$c_loss <- "CO2"
sva1112$season_automatic <- ifelse(month(sva1112$flux_date) > 8,"NGS","GS")
sva1112$flux_id_automatic <- paste(sva1112$site_id_automatic,
                                   "_",
                                   sva1112$c_loss,
                                   sva1112$season_automatic,
                                   "_",
                                   sva1112$flux_year_automatic,
                                   sep="")
sva1112$reco_raw_unit <- "μmol m-2 s-1"
sva1112$soil_temp_depth <- "5"
sva1112$soil_moist_depth <- "5"
sva1112$reco <- as.numeric(sva1112$reco_raw) * 1e-6 * 12 * (60*60*24)
sva1112$reco_unit <- "g C m-2 d-1"
sva1112$remove1 <- NULL
sva1112$remove <- NULL




fluxes <- rbind.fill(fluxes,
                     sva1112)
database[["fluxdata"]] <- fluxes
saveRDS(database,"database/database.rds")
