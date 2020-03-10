#' Collect external data
#'
#' This function collects external data from NOAA and WoRMS.
#' @details
#' Bathymetry data is collected from the NOAA database using the \code{marmap} R-package.
#' You need internet connection to do this. The default resolution is 1 minute.
#' You can also choose to collect your own bathymetry with a different resolution from NOAA by using the
#' package function \code{collect_bathymetry}, or not to collect bathymetry externally at all
#' if you have your own dataset.
#' \cr
#' Taxonomic data is collected from the WoRMS database using the \code{worrms} R-package.
#' You need internet connection to do this.
#' The reported taxonomic names of the specimens in the initial database are matched against
#' the WoRMS database (also fuzzy matches, i.e. where typos and phonetic spelling is allowed).
#' @return This function does not return an object, but stores the information in the specified
#' \code{out_folder} under the names 'bathymetry.rda' and 'worms.rds'.
#' @param stations The initial database for stations with track midpoints.
#' @param species The initial database for species with reported specimen names.
#' @param lats (optional) You can specify latitudes you want to use to collect bathymetry.
#' If not specified, the track midpoints in the database are used.
#' @param lons (optional) You can specify longitudes you want to use to collect bathymetry.
#' If not specified, the track midpoints in the database are used.
#' @param data_folder If the stations and/or species database are not provided, the function will search
#' for it ('stations_initial.rda' and 'species_initial.rda') in this folder. Default is 'data'.
#' @param out_folder The external data is stored in this folder. Default is 'data'.
#' @export
collect_external_data <- function(stations = NULL, species = NULL, lats = NULL, lons = NULL,
                                  data_folder = "data", out_folder = "data", as_CSV = FALSE){
  if(is.null(stations)){
    message("Loading intitial database with stations...")
    load(paste0(data_folder,"/stations_initial.rda"))
  }
  if(is.null(species)){
    message("Loading intitial database with species")
    load(paste0(data_folder,"/species_initial.rda"))
  }
  # Collect bathymetry from NOAA
  message("Collecting bathymetry from NOAA. This can take a while...")
  bathymetry <- collect_bathymetry(stations, lats, lons)
  save(bathymetry, file = paste0(out_folder,"/bathymetry.rda"))
  message(paste0("Bathymetry stored as ",out_folder,"/bathymetry.rda."))
  # Collect taxonomy of species from WoRMs
  message("Collecting taxonomy from the WoRMS database. This can take a while...")
  worms <- get_worms_taxonomy(as.character(species$Species_reported))
  save(worms, file = paste0(out_folder,"/worms.rda"))
  message(paste0("WoRMS taxonomic information is stored as ",out_folder,"/worms.rda."))

  if(as_CSV){
    write.csv(bathymetry, file = "bathymetry.csv")
    write.csv(worms, file = "worms_taxonomy.csv")
  }
  }

#' Complete database with external data and calculations
#'
#' This function contains a workflow to add information to the original data
#' through external datasets and calculations. Please review this workflow as
#' explained in \code{Details}. You can also create your own workflow, for example by using
#' other functions contained in this package.
#' @details
#' The initial database created with \code{construct_database} only contains
#' raw data as imported from the CSV files. Sometimes data is missing (e.g.
#' water depth is optional and not always reported) or you wish to include
#' extra information (e.g. taxonomic information). This workflow is a chain
#' of functions to improve the database with external data and calculations.
#' The chain of functions is explained here, but you are of course free to
#' adjust this chain to your own wishes.
#' \cr
#' #' The latest accepted name for the specimen is added to the database including the taxonomic classification (phylum, class, order, family, genus).
#' @param data_folder This is the folder that contains the initial database.
#' Default is 'data', as created by the \code{construct_database} function.
#' @param out_folder This is the folder where you want your improved, but not yet finalized database,
#' to be stored. Default is 'data'.
#' @param bathymetry (optional) Water depth can be added to the database if you have a
#' bathymetry file. If no such file is given, bathymetry is collected from NOAA.
#' @return This function does not return an object, but stores 'stations_additions.rda'
#' and 'species_additions.rda' to the out_folder.
#' @seealso \code{add_track_midpoints}, \code{add_track_length_GPS},
#' \code{add_track_length_Odometer}, \code{add_water_depth}.
#' @export
complete_database <- function(data_folder = "data", out_folder = "data", input_folder = "inputfiles",
                              bathymetry = NULL, as_CSV = FALSE){
  # Load initial database
  message("Loading initial database...")
  load(paste0(data_folder,"/stations_initial.rda"))
  load(paste0(data_folder,"/species_initial.rda"))

  # Load external data
  if(is.null(bathymetry)){
    message("Loading bathymetric data...")
    load(paste0(data_folder,"/bathymetry.rda"))
  }else{
    # TODO: CHECK FORMAT BATHYMETRY WHEN GIVEN
  }
  message("Loading WoRMS taxonomic data...")
  load(paste0(data_folder,"/worms.rda"))
  message("Loading size to weight conversion data...")
  conversion_data <- read.csv(paste0(input_folder, "/bioconversion.csv"),stringsAsFactors = F)

  # TODO: Check format of units and values with the attributes table
  # TODO: size_unit must always be mm
  # TODO: The same WW_to_AFDW for a single species in multiple rows.
  message("Checking taxa in bioconversion.csv to the WoRMS database...")
  worms_conversion <- get_worms_taxonomy(conversion_data$Taxon)
  conversion_data <- left_join(conversion_data, select(worms_conversion, Query, valid_name),
                               by = c("Taxon" = "Query"))

  message("Adding additional data to stations...")
  stations_additions <- stations %>%
    add_track_midpoints() %>%
    add_track_length_GPS() %>%
    add_track_length_Odometer() %>%
    add_water_depth(bathymetry = bathymetry)

  message("Adding additional data to species...")
  species_additions <- species %>%
    # Add taxonomic data
    dplyr::left_join(., dplyr::select(worms,
                        Query, valid_name, rank, phylum, class, order,
                        family, genus, hasNoMatch, isFuzzy),
              by = c("Species_reported" = "Query")) %>%
    # Attach conversion factors (irrespective of Size_dimension)
    dplyr::left_join(dplyr::select(conversion_data, valid_name, WW_to_AFDW, Reference_conversion_WW_AFDW),
                     by = c("valid_name"),
                     suffix = c("_species", "_conversion")) %>%
    # Attach regression formula (taking into account Size_dimension)
    dplyr::left_join(dplyr::select(conversion_data, -Taxon, -Size_unit,
                                   -WW_to_AFDW, -Reference_conversion_WW_AFDW),
                     by = c("valid_name", "Size_dimension"),
                     suffix = c("_species", "_conversion")) %>%
    # Convert length to mm from other units
    # 1/2cm are classes, so 0x1/2cm  = 5 mm, and 1x1/2cm is 10 mm.
    # cm are simply multiplied x10.
    # TODO: how to deal with mm2 and cm2?
    dplyr::mutate(Size_mm =
           ifelse(Size_unit == "1/2cm", Size_value*5+5,
           ifelse(Size_unit == "cm", Size_value*10, Size_value))) %>%
    # Calculate WW from size (ww = A*size^B -> multiply by count)
    # If count is NA, WW_g_calc is NA.
    dplyr::mutate(WW_g_calc =
           ifelse(Output_unit == "WW_g", A_factor*(Size_mm^B_exponent)*Count, NA)) %>%
    # Calculate AFDW from size (AFDW = A*size^B*Count OR AFDW = WW*convesion_factor)
    dplyr::mutate(AFDW_g_calc =
           ifelse(Output_unit == "AFDW_g", A_factor*(Size_mm^B_exponent)*Count, WW_g_calc*WW_to_AFDW)) %>%
    # Calculate AFDW from reported WW: if reported WW is 0, use the scale threshold.
    dplyr::mutate(WetWeight_g_threshold = ifelse(WetWeight_g == 0, Threshold_Scale, WetWeight_g)) %>%
    dplyr::mutate(AFDW_g_from_reported_WW = WetWeight_g_threshold * WW_to_AFDW)

  # Info / warnings
  # Give list of taxa that do not match to worms at all.
  no_match_i <- which(species_additions$hasNoMatch == 1)
  if(length(no_match_i) > 0){
    message(paste0("These printed taxa names from the corresponding files cannot be matched to the WoRMS database:"))
    print(species_additions[no_match_i,] %>%
            dplyr::select(File, Species_reported) %>%
            dplyr::distinct())
  }
  # Give list of taxa in bioconversion with no match to worms at all.
  no_match_i <- which(is.na(worms_conversion$valid_name))
  if(length(no_match_i) > 0){
    message(paste0("These taxa names from the bioconversion.csv file cannot be matched to the WoRMS database:"))
    print(unique(worms_conversion$Query[no_match_i]))
  }
  # Give list of taxa that do not have conversion factors
  no_WW_to_AFDW <- which(is.na(species_additions$WW_to_AFDW))
  if(length(no_WW_to_AFDW) > 0){
    message("These taxa names have no conversion factor WW_to_AFDW in the bioconversion.csv file:")
    print(unique(species_additions$valid_name[no_WW_to_AFDW]))
  }
  # Give list of taxa that do not have conversion factors
  no_regression <- which(is.na(species_additions$A_factor))
  if(length(no_regression) > 0){
    message("These taxa names have no regression formula in the bioconversion.csv file:")
    print(unique(species_additions$valid_name[no_regression]))
  }
  # TODO: average difference between bathymetry and reported depth.

  message(paste0("Saving results to ",out_folder))
  save(stations_additions, file = paste0(out_folder,"/stations_additions.rda"))
  save(species_additions, file = paste0(out_folder,"/species_additions.rda"))
  if(as_CSV){
    write.csv(stations_additions, paste0(out_folder,"stations_additions.csv"))
    write.csv(species_additions, paste0(out_folder,"species_additions.csv"))
  }
}


