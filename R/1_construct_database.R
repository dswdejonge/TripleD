#' Construct initial database
#'
#' This function loads CSV files, checks the data format, and constructs an initial table
#' with only raw data.
#' @param in_folder This folder must contain two subfolders called 'Species' and 'Stations' that contain
#' properly formatted CSV files. These CSV files can be requested from the NIOZ Data Archiving System.
#' Default is "inputfiles".
#' @param out_folder The function will create a new directoy to store the newly created data.
#' Default is "data".
#' @details Make sure this function is executed in the same working directory that contains the in_folder directory.
#' This function checks the data format of the CSV files contained by the 'Species' and 'Stations' folder
#' contained by the 'in-folder'. If the functions finds any irregularities, an error will be thrown.
#' @return This function does not return any objects. It will throw errors if anything is wrong with
#' your initial data. If there are no errors, it will store Rdata in the newly created folder 'out_folder'.
#' @export
construct_database <- function(in_folder = "inputfiles", out_folder = "data"){
  # Load functions written to check data formats
  source("check_format.R")

  # List of tables to import and check
  tables <- list(
    Stations <- list(folder = "Stations", att = "attributes_stations.csv"),
    Species <- list(folder = "Species", att = "attributes_species.csv")
  )

  # Create folder to store output
  dir.create(out_folder)

  for(table in tables){
    print(paste0("Checking the files in folder ", in_folder, "/", table$folder, ". Importing data."))

    # Read in attribute requirements
    my_attributes <- read.csv(system.file("extdata", table$att, package = "TripleD"))

    # Read in data
    data <- import_data(paste0(in_folder,"/",table$folder))

    # Subset data requirements
    required_attributes <- get_required_att(my_attributes)
    alternative_attributes <- get_alternative_att(my_attributes)
    doubles_attributes <- get_doubles_att(my_attributes)
    integer_attributes <- get_int_att(my_attributes)
    boolean_attributes <- get_boolean_att(my_attributes)
    fraction_attributes <- get_fraction_att(my_attributes)
    predefined_attributes <- get_predefined_att(my_attributes)

    # Check format requirements for each file
    for(i in 1:length(data)){
      file <- data[[i]]
      file_name <- paste(table$folder, names(data)[i], sep = "/")
      print(paste0("Checking file ", file_name))
      # Attribute presence
      are_required_att_present(file, file_name, required_attributes)
      are_alternative_required_att_present(file, file_name, alternative_attributes)
      are_att_names_valid(file, file_name, my_attributes)

      # Correct values and units
      are_StationIDs_unique(file, file_name)
      are_required_att_complete(file, file_name, required_attributes)
      are_alternative_required_att_complete(file, file_name, alternative_attributes)
      if(table$folder == "Stations"){
        data[[i]]$Date <- as.Date(data[[i]]$Date, format = "%d/%m/%Y")
        are_dates_converted(file = data[[i]], file_name)
      }
      are_groups_complete(file, file_name, my_attributes)
      are_values_correct_type(file, file_name, doubles_attributes, func = is.double)
      are_values_correct_type(file, file_name, integer_attributes, func = is.integer)
      are_booleans_correct(file, file_name, boolean_attributes)
      are_fractions_correct(file, file_name, fraction_attributes)
      are_predefined_atts_correct(file, file_name, predefined_attributes)
      if(table$folder == "Species"){
        are_species_metadata_consistent(file, file_name)
      }
      do_measurements_have_units(file, file_name)
    }
    if(table$folder == "Stations"){
      stations <- dplyr::bind_rows(data, .id = "File")
      save(stations, file = paste0(out_folder,"/","stations_initial.rda"))
      #usethis::use_data(stations, overwrite = T)
    }
    if(table$folder == "Species"){
      species <- dplyr::bind_rows(data, .id = "File")
      save(species, file = paste0(out_folder,"/","species_initial.rda"))
      #usethis::use_data(species, overwrite = T)
    }
  }

  missing_stationIDs <- unique(species$StationID[which(!species$StationID %in% stations$StationID)])
  if(length(missing_stationIDs > 0)){
    warning(paste0("The StationID(s) ", paste(missing_stationIDs, collapse = ", "), " are reported in the species file,
                   but are missing in the stations file, i.e. metadata is missing for these biological data points."))
  }

  # Test if stationIDs are unique over different files.
}
