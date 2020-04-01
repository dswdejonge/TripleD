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
#' The reported taxonomic names of the specimens in the initial database and in the bioconversion.csv file
#' are matched against the WoRMS database (also fuzzy matches, i.e. where typos and phonetic spelling is allowed).
#' @return This function does not return an object, but stores the information in the specified
#' \code{out_folder} under the names 'bathymetry.rda' and 'worms.rda'.
#' @param stations The initial database for stations with track midpoints.
#' If NULL (default) the function will automatically search the data_folder for 'stations_initial.rda'.
#' @param species The initial database for species with reported specimen names.
#' If NULL (default) the function will automatically search the data_folder for 'species_initial.rda'.
#' @param conversion_data Dataframe with bioconversion data matching the given requirements from the
#' attributes_bioconversion file. If NULL (default) the bioconversion.csv will be searched for and
#' loaded from the input_folder.
#' @param lats (optional) You can specify latitudes you want to use to collect bathymetry.
#' If not specified, the track midpoints in the database are used.
#' @param lons (optional) You can specify longitudes you want to use to collect bathymetry.
#' If not specified, the track midpoints in the database are used.
#' @param input_folder The folder where to find the bioconversion.csv file. Default is 'inputfiles'.
#' @param data_folder If the stations and/or species database are not provided, the function will search
#' for it ('stations_initial.rda' and 'species_initial.rda') in this folder. Default is 'data'.
#' @param out_folder The external data is stored in this folder. Default is 'data'.
#' @param as_CSV If you also want to store the collected external data as CSV, set to TRUE. Default is FALSE.
#' @export
collect_external_data <- function(stations = NULL, species = NULL, conversion_data = NULL, lats = NULL, lons = NULL,
                                  input_folder = "inputfiles", data_folder = "data", out_folder = "data", as_CSV = FALSE){
  if(is.null(stations)){
    message("Loading intitial database with stations...")
    load(paste0(data_folder,"/stations_initial.rda"))
  }
  if(is.null(species)){
    message("Loading intitial database with species")
    load(paste0(data_folder,"/species_initial.rda"))
  }
  message("Loading size to weight conversion data...")
  if(is.null(conversion_data)){
    conversion_data <- read.csv(paste0(input_folder, "/bioconversion.csv"),stringsAsFactors = F)
  }
  conversion_taxa <- conversion_data$Taxon

  # Collect bathymetry from NOAA
  message("Collecting bathymetry from NOAA. This can take a while...")
  bathymetry <- collect_bathymetry(stations, lats, lons)
  save(bathymetry, file = paste0(out_folder,"/bathymetry.rda"))
  message(paste0("Bathymetry stored as ",out_folder,"/bathymetry.rda."))

  # Collect taxonomy of species from WoRMs
  message("Collecting taxonomy from the WoRMS database of taxa reported in TripleD data. This can take a while...")
  worms <- get_worms_taxonomy(as.character(species$Species_reported))
  save(worms, file = paste0(out_folder,"/worms.rda"))
  message(paste0("WoRMS taxonomic information is stored as ",out_folder,"/worms.rda."))

  # Collect taxonomy of taxa in the bioconversion file
  message("Collecting taxonomy from the WoRMS database of taxa reported in the bioconversion.csv file. This can take a while...")
  worms_conversion <- get_worms_taxonomy(conversion_taxa)
  no_match_i <- which(is.na(worms_conversion$valid_name))
  if(length(no_match_i) > 0){
    message(paste0("These taxa names from the bioconversion.csv file cannot be matched to the WoRMS database:",
                paste0(unique(worms_conversion$Query[no_match_i]), collapse = ", ")))
  }
  message("Adding WoRMS valid names to conversion data...")
  conversion_data <- dplyr::left_join(conversion_data, dplyr::select(
    worms_conversion, Query, valid_name, isFuzzy),
    by = c("Taxon" = "Query"))
  save(worms_conversion, file = paste0(out_folder,"/worms_conversion.rda"))
  message(paste0("WoRMS taxonomic information for the bioconversion.csv file is stored as ",out_folder,"/worms_conversion.rda."))
  save(conversion_data, file = paste0(out_folder,"/conversion_data.rda"))

  if(as_CSV){
    write.csv(bathymetry, file = "bathymetry.csv")
    write.csv(worms, file = "worms_taxonomy.csv")
    write.csv(worms_conversion, file = "worms_conversion.csv")
    write.csv(conversion_data, file = "conversion_data.csv")
  }
}

check_bioconversion_input <- function(conversion_data){
  # Read attributes
  message("Checking data format of bioconversion.csv...")
  my_attributes <- read.csv(system.file("extdata", "attributes_bioconversion.csv", package = "TripleD"))

  # Check presence required attributes
  required_attributes <- dplyr::filter(my_attributes, Required_or_Optional == "Required")
  att_is_missing <- !required_attributes$Attribute %in% colnames(conversion_data)
  if(TRUE %in% (att_is_missing)){
    stop(paste0("The required attribute(s) ", paste(required_attributes$Attribute[att_is_missing], collapse = ", "),
                " is/are missing in bioconversion.csv."))
  }

  # No NA values are allowed in the required columns.
  columns <- colnames(conversion_data) %in% required_attributes$Attribute
  subset <- conversion_data[,columns, drop = F]
  if(TRUE %in% is.na(subset)){
    rowi <- unique(which(is.na(subset), arr.ind = T)[,"row"]+1)
    stop(paste0("NA values are not allowed for the required attributes ",
                paste(required_attributes$Attribute, collapse = ", "),
                ". Please check row(s): ",paste(sort(unique(rowi)), collapse = ", ")))
  }

  # Are groups complete
  my_groups <- my_attributes %>%
    dplyr::filter(!is.na(Group)) %>%
    dplyr::group_by(Group) %>%
    dplyr::group_split()
  for(my_group in my_groups){
    is_group_included <- colnames(conversion_data) %in% my_group$Attribute
    if(TRUE %in% is_group_included &&
       length(which(is_group_included)) != length(my_group$Attribute)){
      stop(paste0("The attributes ",
                  paste(my_group$Attribute, collapse = ", "),
                  " are needed but only ",
                  paste(colnames(conversion_data)[is_group_included], collapse = ", ")," are given."))
    }
  }

  #TODO: no NA values in grouped vars if a value is given.

  # Check doubles
  doubles_attributes <- dplyr::filter(my_attributes, Datatype == "Double")
  columns <- which(colnames(conversion_data) %in% doubles_attributes$Attribute)
  if(length(columns) > 0){
    for(j in 1:length(columns)){
      if(!is.double(conversion_data[,columns[j]])){
        stop(paste0("The column ", colnames(conversion_data)[columns[j]],
                    " must contain only values of datatype ",doubles_attributes$Datatype[1]))
      }
    }
  }

  # Check integers
  integer_attributes <- dplyr::filter(my_attributes, Datatype == "Integer")
  columns <- which(colnames(conversion_data) %in% integer_attributes$Attribute)
  if(length(columns) > 0){
    for(j in 1:length(columns)){
      if(!is.double(conversion_data[,columns[j]])){
        stop(paste0("The column ", colnames(conversion_data)[columns[j]],
                    " must contain only values of datatype ",integer_attributes$Datatype[1]))
      }
    }
  }

  # Check fractions
  fraction_attributes <- dplyr::filter(my_attributes, Unit == "Fraction")
  if(length(fraction_attributes$Attribute) > 0){
    columns <- which(colnames(conversion_data) %in% fraction_attributes$Attribute)
    subset <- conversion_data[,columns, drop = F]
    is_not_fraction <- which(subset < 0 | subset > 1, arr.ind = T)[,"row"]+1
    if(length(is_not_fraction) > 0){
      stop(paste0("The attributes ", paste(colnames(conversion_data)[columns], collapse = ", "),
                  " should lie between 0 and 1. Please check row(s) ",
                  paste(sort(is_not_fraction), collapse = ", ")))
    }
  }

  # Check booleans
  boolean_attributes <- dplyr::filter(my_attributes, Datatype == "Boolean")
  if(length(boolean_attributes$Attribute) > 0){
    columns <- which(colnames(conversion_data) %in% boolean_attributes$Attribute)
    subset <- conversion_data[,columns, drop = F]
    if(dim(subset)[2] != 0){
      is_not_boolean <- which(!is.na(subset) & subset != 0 & subset != 1, arr.ind = T)[,"row"]+1
      if(length(is_not_boolean) > 0){
        stop(paste0("The boolean attributes ", paste(colnames(conversion_data)[columns], collapse = ", "),
                    " may only be 0, 1, or NA. Please check row(s) ",
                    paste(sort(is_not_boolean), collapse = ", ")))
      }
    }
  }

  # Check predefined units
  remove_str <- "Predefined: "
  split_at <- ", "
  units <- my_attributes$Unit %>%
    .[grep(remove_str, .)] %>%
    gsub(remove_str, "", .) %>%
    strsplit(., split_at)
  names(units) <- my_attributes$Attribute[grep(remove_str, my_attributes$Unit)]
  for(i in 1:length(units)){
    att <- names(units)[i]
    if(!att %in% colnames(conversion_data)){
      next
    }else{
      subset <- conversion_data[,att]
      is_not_predefined <- which(!subset %in% units[[i]] & !is.na(subset))+1
      if(length(is_not_predefined) > 0){
        stop(paste0("In column ",att,
          " values exist that are not the predefined values ",
          paste(units[[i]], collapse = ", "),
          " or NA in row(s): ",paste(sort(is_not_predefined), collapse = ", ")
        ))
      }
    }
  }

  # TODO: check if all are positive?

  # Split in conversion data and regression
  conversion_factors <- conversion_data %>%
    dplyr::filter(!is.na(valid_name)) %>%
    dplyr::select(valid_name, WW_to_AFDW, Reference_WW_to_AFDW,
                  isShellRemoved, Comment_WW_to_AFDW) %>%
    dplyr::filter(WW_to_AFDW > 0) %>%
    dplyr::distinct()

  regressions <- conversion_data %>%
    dplyr::filter(!is.na(valid_name)) %>%
    dplyr::select(valid_name, Size_dimension, A_factor, B_exponent, Output_unit,
                  Reference_regression, Comment_regression, isShellRemoved) %>%
    dplyr::filter(!is.na(Output_unit)) %>%
    dplyr::distinct()

  # Only one conversion factor is allowed for each combination of
  # valid name and isShellRemoved
  check_conv_f <- conversion_factors %>%
     dplyr::group_by(valid_name, isShellRemoved) %>%
     dplyr::summarise(Count = dplyr::n()) %>%
     dplyr::filter(Count > 1)
  are_double <- which(check_conv_f$Count > 1)
  if(length(are_double) > 0){
    print(check_conv_f)
    stop(paste0("Multiple conversion factors WW_to_AFDW are present for the above species.\nBeware that these valid names might differ from the taxon name reported in bioconversion.csv.\nUse worms_conversion.rda to check."))
  }

  # Only one combination for each taxa, size_dimension, output_unit, and isShellRemoved is allowed.
  check_regressions <- regressions %>%
    dplyr::group_by(valid_name, Size_dimension, isShellRemoved) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    dplyr::filter(Count > 1)
  are_double <- which(check_regressions$Count > 1)
  if(length(are_double) > 0){
    print(check_regressions)
    stop(paste0("Multiple regressions are present for the above species.\nBeware that these valid names might differ from the taxon name reported in bioconversion.csv.\nUse worms_conversion.rda to check."))
  }

  return(list(
    conversion_factors = conversion_factors,
    regressions = regressions))
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
  #load(paste0(data_folder, "/worms_conversion.rda"))
  message("Loading size to weight conversion data...")
  #conversion_data <- read.csv(paste0(input_folder, "/bioconversion.csv"),stringsAsFactors = F)
  load(paste0(data_folder,"conversion_data.rda"))
  #message("Adding WoRMS valid names to conversion data...")
  #conversion_data <- dplyr::left_join(conversion_data, dplyr::select(
  #  worms_conversion, Query, valid_name, isFuzzy),
  #  by = c("Taxon" = "Query"))
  conversion_list <- check_bioconversion_input(conversion_data)

  message("Adding additional data to stations...")
  stations_additions <- stations %>%
    add_track_midpoints() %>%
    add_track_length_GPS() %>%
    add_track_length_Odometer() %>%
    add_water_depth(bathymetry = bathymetry)
  #TODO: allow water depth estimation from calculated midpoints.

  message("Adding additional data to species...")
  species_additions <- species %>%
    # Add taxonomic data
    dplyr::left_join(., dplyr::select(worms,
                        Query, valid_name, rank, phylum, class, order,
                        family, genus, hasNoMatch, isFuzzy),
              by = c("Species_reported" = "Query")) %>%
    # Attach conversion factors
    dplyr::left_join(., conversion_list$conversion_factors,
                     by = c("valid_name", "isShellRemoved"),
                     suffix = c("_species", "_conversion")) %>%
    # Attach regression formulas
    dplyr::left_join(., conversion_list$regressions,
                     by = c("valid_name", "Size_dimension","isShellRemoved"),
                     suffix = c("_species", "_conversion")) %>%
    # TODO: with join rows from species may not be duplicated!! Does this work now?
    # TODO: Only ONE regression is allowed per taxon and size dimension shell removed. so not both WW and AFDW. Write test.
    # Convert length to mm from other units
    #   - 1/2cm are classes, so 0x1/2cm  = 5 mm, and 1x1/2cm is 10 mm.
    #   - cm are simply multiplied x10.
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

  # Give list of taxa in species_additions that do not have conversion factors
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
  # Average difference between bathymetry and reported depth.
  mean_diff_depth <- mean(stations_additions$Water_depth_m_Cruise) -
    mean(stations_additions$Water_depth_m_Bathy)
  message(paste0("The average difference between reported water depth and bathymetry depth is: ",
                 mean_diff_depth," meters."))

  message(paste0("Saving results to ",out_folder))
  save(stations_additions, file = paste0(out_folder,"/stations_additions.rda"))
  save(species_additions, file = paste0(out_folder,"/species_additions.rda"))
  if(as_CSV){
    write.csv(stations_additions, paste0(out_folder,"stations_additions.csv"))
    write.csv(species_additions, paste0(out_folder,"species_additions.csv"))
  }
}


