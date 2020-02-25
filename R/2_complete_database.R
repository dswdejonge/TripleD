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
                                  data_folder = "data", out_folder = "data"){
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
complete_database <- function(data_folder = "data", out_folder = "data", bathymetry = NULL){
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

  # Add additional data to stations
  stations_additions <- stations %>%
    add_track_midpoints() %>%
    add_track_length_GPS() %>%
    add_track_length_Odometer() %>%
    add_water_depth(bathymetry = bathymetry)
  save(stations_additions, file = paste0(out_folder,"/stations_additions.rda"))

  # Add additional data to species data
  species_additions <- species %>%
    # Add taxonomic data
    dplyr::left_join(., dplyr::select(worms,
                        Query, valid_name, rank, phylum, class, order,
                        family, genus, hasNoMatch, isFuzzy),
              by = c("Species_reported" = "Query"))
  save(species_additions, file = paste0(out_folder,"/species_additions.rda"))
}



# Open csv file that contains conversion data. Columns:
# Species: species name for which the conversion data is valid.
# WW_to_AFDW: fraction of wet weight (WW) that should be taken to obtain Ash Free Dry Weight (AFDW).
# Length_to_Width: fraction of length that should be taken to obtain width.
# A_factor: The factor A in the power function AFDW = A * Length^B as regression of AFDW vs. Length.
# B_exponent: The exponent in the power function AFDW = A * Length^B as regression of AFDW vs. Length.
#conversion_data <- read.csv(
#  system.file("extdata", "bioconversion_data.csv", package = "TripleD"),
#  stringsAsFactors = F)
# Check names in the bioconversion data file
#worms_conversion <- get_worms_taxonomy(conversion_data$Species)
#conversion_data <- left_join(conversion_data, select(worms_conversion, Query, valid_name),
#                             by = c("Species" = "Query"))

 #%>%
  # Attach conversion data
  #left_join(conversion_data, by = "valid_name") %>%
  # Convert length to mm from other units
  #mutate(Length_mm =
  #         ifelse(Unit_Length == "0.5cm", Length*5+5,
  #         ifelse(Unit_Length == "cm", Length*10+10, Length))) %>%
  # Convert width to mm from other units
  #mutate(Width_mm =
  #         ifelse(Unit_Width == "0.5cm", Width*5+5,
  #         ifelse(Unit_Width == "cm", Width*10+10, Width))) %>%
  # Convert width in mm to length in mm
  #mutate(Length_from_Width_mm = Width_mm/Length_to_Width) %>%
  # Calculate AFDW from length and from length derived from width
  #mutate(AFDW_g_calc_L = length_to_weight(Length_mm, A = A_factor, B = B_exponent)) %>%
  #mutate(AFDW_g_calc_W = length_to_weight(Length_from_Width_mm, A = A_factor, B = B_exponent)) %>%
  # If wet weight is reported as 0, use the scale threshold as wet weight.
  #mutate(WetWeight_g_threshold = ifelse(WetWeight_g == 0, Threshold_Scale, WetWeight_g)) %>%
  # Calculate AFDW from wet weight
  #mutate(AFDW_g_calc_WW = WetWeight_g_threshold * WW_to_AFDW)
