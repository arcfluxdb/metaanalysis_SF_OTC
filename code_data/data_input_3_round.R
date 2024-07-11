
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

