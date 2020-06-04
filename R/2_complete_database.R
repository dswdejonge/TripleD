

#' Collect species names in bioconversion file from WoRMS
#'
#' This function queries reported species names from the bioconversion input file against
#' WoRMS and calculates average conversion factors and regression formula's for
#' higher taxonomic levels.
#' @details
#' Taxonomic data is collected from the WoRMS database using the \code{worrms} R-package.
#' You need internet connection to do this.
#' The reported taxonomic names of the specimens in the bioconversion.csv file
#' are matched against the WoRMS database (also fuzzy matches, i.e. where typos and phonetic spelling is allowed).
#' @return This function does not return an object, but stores the information in the specified
#' \code{out_folder} under the names 'worms_conversion.rda' and 'conversion_data.rda'.
#' @param conversion_data Dataframe with bioconversion data matching the given requirements from the
#' attributes_bioconversion file. If NULL (default) the bioconversion.csv will be searched for and
#' loaded from the input_folder.
#' @param input_folder The folder where to find the bioconversion.csv file. Default is 'inputfiles'.
#' @param out_folder The external data is stored in this folder. Default is 'data'.
#' @param as_CSV If you also want to store the collected external data as CSV, set to TRUE. Default is FALSE.
#' @export
collect_bioconversion_WORMS <- function(conversion_data = NULL,
                                      input_folder = "inputfiles", out_folder = "data",
                                      as_CSV = FALSE){
  no_match_i <- which(is.na(worms_conversion$valid_name))
  if(length(no_match_i) > 0){
    message(paste0("These taxa names from the bioconversion.csv file cannot be matched to the WoRMS database:",
                   paste0(unique(worms_conversion$Query[no_match_i]), collapse = ", ")))
  }

  message("Adding WoRMS valid names to conversion data...")
  conversion_data <- dplyr::left_join(conversion_data, dplyr::select(
    worms_conversion, Query, valid_name, isFuzzy,
    phylum, class, order, family, genus),
    by = c("Taxon" = "Query")) %>%
    dplyr::distinct()

  message("Calculating conversion and regression means for all taxonomic levels...")
  conversion_data <- calculate_mean_conversion(conversion_data)
  message(paste0("WoRMS taxonomic information for the bioconversion.csv file is stored as ",out_folder,"/worms_conversion.rda."))
  save(conversion_data, file = paste0(out_folder,"/conversion_data.rda"))
  if(as_CSV){
    write.csv(conversion_data, file = paste0(out_folder,"/conversion_data.csv"))
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
                              bathymetry = NULL, as_CSV = TRUE){
  message("Loading initial database...")
  load(paste0(data_folder,"/stations_initial.rda"))
  load(paste0(data_folder,"/species_initial.rda"))

  if(is.null(bathymetry)){
    message("Loading bathymetric data...")
    load(paste0(data_folder,"/bathymetry.rda"))
  }

  message("Loading WoRMS taxonomic data...")
  load(paste0(data_folder,"/worms.rda"))
  message("Loading size to weight conversion data...")
  load(paste0(data_folder,"/conversion_data.rda"))
  conversion_data <- conversion_data %>%
    dplyr::mutate(
      is_Shell_removed = ifelse(is_Shell_removed == 1, TRUE, FALSE)
      )
  conversion_list <- check_bioconversion_input(conversion_data)

  message("Adding additional data to stations...")
  stations_additions <- stations %>%
    add_track_midpoints() %>%
    add_track_length_GPS() %>%
    add_track_length_Odometer() %>%
    add_water_depth(bathymetry = bathymetry, col_lon = "Lon_DD_midpt", col_lat = "Lat_DD_midpt", col_name = "Water_depth_m_Bathy") %>%
    add_water_depth(bathymetry = bathymetry, col_lon = "Lon_DD_calc", col_lat = "Lat_DD_calc", col_name = "Water_depth_m_Bathy2")

  message("Adding additional data to species...")
  species_additions <- species %>%
    # Add taxonomic data
    dplyr::left_join(., dplyr::select(worms,
                        Query, valid_name, rank, phylum, class, order,
                        family, genus, hasNoMatch, isFuzzy),
              by = c("Species_reported" = "Query")) %>%
    # Attach conversion factors
    dplyr::left_join(., conversion_list$conversion_factors,
                     by = c("valid_name", "is_Shell_removed"),
                     suffix = c("_species", "_conversion")) %>%
    # Attach regression formulas
    dplyr::left_join(., conversion_list$regressions,
                     by = c("valid_name", "Size_dimension","is_Shell_removed"),
                     suffix = c("_species", "_conversion")) %>%
    # Convert length to mm from other units
    #   - 1/2cm are classes, so 0x1/2cm  = 5 mm, and 1x1/2cm is 10 mm.
    #   - cm are simply multiplied x10.
    # TODO: how to deal with mm2 and cm2? mm3, cm3
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
    dplyr::mutate(WW_g_threshold = ifelse(WW_g == 0, Threshold_scale/2, WW_g)) %>%
    dplyr::mutate(AFDW_g_from_WW = WW_g_threshold * WW_to_AFDW)

  # Info / warnings
  # Check for duplicated samples
  if(nrow(species) != nrow(species_additions)){
    stop("Duplication has occurred.")
  }

  # Give list of taxa that do not match to worms at all.
  no_match_i <- which(species_additions$hasNoMatch == 1)
  if(length(no_match_i) > 0){
    message(paste0("These printed taxa names from the corresponding files cannot be matched to the WoRMS database:"))
    to_print <- species_additions[no_match_i,] %>%
      dplyr::select(File, Species_reported) %>%
      dplyr::distinct()
    print(to_print)
  }

  # Give list of taxa in species_additions that did not have conversion factors
  no_conversion_factors <- species_additions %>%
    dplyr::filter(WW_g > 0 | WW_g_calc > 0) %>%
    dplyr::select(valid_name, is_Shell_removed, WW_to_AFDW) %>%
    dplyr::filter(is.na(WW_to_AFDW)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(valid_name)
  if(nrow(no_conversion_factors) > 0){
    message("These taxa names have no conversion factor WW_to_AFDW in the bioconversion.csv file:")
    print(no_conversion_factors)
  }
  # Give list of taxa that do not have a regression formula
  no_regressions <- species_additions %>%
    dplyr::filter(Size_value > 0) %>%
    dplyr::select(valid_name, Size_dimension, is_Shell_removed, A_factor) %>%
    dplyr::filter(!is.na(Size_dimension), is.na(A_factor)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(valid_name)
  if(nrow(no_regressions) > 0){
    message("These taxa names have no regression formula in the bioconversion.csv file:")
    print(no_regressions)
  }
  # Average difference between bathymetry and reported depth.
  mean_diff_depth <- mean(stations_additions$Water_depth_m_cruise, na.rm = T) -
    mean(stations_additions$Water_depth_m_Bathy, na.rm = T)
  message(paste0("The average difference between reported water depth and bathymetry depth is: ",
                 mean_diff_depth," meters."))

  message(paste0("Saving results to folder: ",out_folder))
  save(stations_additions, file = paste0(out_folder,"/stations_additions.rda"))
  save(species_additions, file = paste0(out_folder,"/species_additions.rda"))
  if(as_CSV){
    message(paste0("CSV files written to: ",out_folder))
    write.csv(stations_additions, file = paste0(out_folder,"/stations_additions.csv"))
    write.csv(species_additions, file = paste0(out_folder,"/species_additions.csv"))
  }
}


