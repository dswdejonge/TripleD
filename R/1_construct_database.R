# -----------------------
# Import data from folder
# -----------------------
import_data <- function(folder) {
  files <- list.files(folder)
  data <- lapply(paste0(folder,"/",files), read.csv)
  names(data) <- files
  return(data)
}

# -----------------------
# Get attributes
# -----------------------

# Get required attributes
get_required_att <- function(my_attributes) {
  req_att <- my_attributes %>%
    filter(Required_or_Optional == "Required")
  return(req_att)
}

# Get alternative attributes
get_alternative_att <- function(my_attributes) {
  alt_att <- my_attributes %>%
    filter(Required_or_Optional == "Required_alternative") %>%
    group_by(Group) %>%
    group_split
  return(alt_att)
}

# Get attributes with datatype double
get_doubles_att <- function(my_attributes) {
  doubles_attributes <- my_attributes %>%
    filter(Datatype == "Double")
  return(doubles_attributes)
}

# Get attributes with datatype integer
get_int_att <- function(my_attributes){
  int_att <- my_attributes %>%
    filter(Datatype == "Integer")
  return(int_att)
}

get_fraction_att <- function(my_attributes){
  frac_att <- my_attributes %>%
    filter(Unit == "Fraction")
  return(frac_att)
}

get_boolean_att <- function(my_attributes){
  bool_att <- my_attributes %>%
    filter(Datatype == "Boolean")
  return(bool_att)
}

get_predefined_att <- function(my_attributes){
  remove_str <- "Predefined: "
  split_at <- ", "
  units <- my_attributes$Unit %>%
    .[grep(remove_str, .)] %>%
    gsub(remove_str, "", .) %>%
    strsplit(., split_at)
  names(units) <- my_attributes$Attribute[grep(remove_str, my_attributes$Unit)]
  return(units)
}

# --------------------------
# Check presence attributes
# --------------------------
are_required_att_present <- function(file, file_name, required_attributes){
  att_is_missing <- !required_attributes$Attribute %in% colnames(file)
  if(TRUE %in% (att_is_missing)){
    stop(paste0("The required attribute(s) ", paste(required_attributes$Attribute[att_is_missing], collapse = ", "),
                " is/are missing in the file ", file_name))
  }
}

are_alternative_required_att_present <- function(file, file_name, alternative_attributes){
  # Is at least one of the alternatives for required fields present?
  if(length(alternative_attributes) > 0){
    present <- FALSE
    for(j in 1:length(alternative_attributes)){
      att_is_missing <- !alternative_attributes[[j]]$Attribute %in% colnames(file)
      if(all(!att_is_missing)){
        present <- TRUE
      }
    }
    if(!present) {
      stop(paste0("In ", file_name," please provide ",paste(alternative_attributes[[1]]$Attribute, collapse = ", ")," or an alternative set of required variables."))
    }
  }
}

are_att_names_valid <- function(file, file_name, my_attributes){
  # Are all provided attributes names (incl. optional) valid?
  att_is_invalid <- !colnames(file) %in% my_attributes$Attribute
  if(TRUE %in% att_is_invalid){
    stop(paste0("The attribute name(s) ",
                paste(colnames(file)[att_is_invalid], collapse = ", ") ,
                " in file ",file_name, " is/are not valid."))
  }
}

# -----------------------
# Check values validity
# -----------------------

are_StationIDs_unique <- function(file, file_name){
  ID_is_duplicated <- duplicated(file$StationID)
  if(!"Species_reported" %in% colnames(file)){
    if(TRUE %in% ID_is_duplicated){
      stop(paste0("The StationID(s) ", paste(file$StationID[ID_is_duplicated], collapse = ", "),
                  " in file ",file_name," is/are duplicated. StationID must be unique."))
    }
  }
}

are_required_att_complete <- function(file, file_name, required_attributes){
  # No NA values are allowed in the required columns.
  columns <- colnames(file) %in% required_attributes$Attribute
  subset <- file[,columns, drop = F]
  if(TRUE %in% is.na(subset)){
    rowi <- which(is.na(subset), arr.ind = T)[,"row"]+1
    stop(paste0("NA values are not allowed for the required attributes ",
                paste(required_attributes$Attribute, collapse = ", "),
                ". Please check the file ", file_name,
                " row(s): ",paste(rowi, collapse = ", ")))
  }
}

are_alternative_required_att_complete <- function(file, file_name, alternative_attributes){
  if(length(alternative_attributes) > 0){
    # No NA values are allowed in at least one of the required alternatives
    NA_index <- list()
    for(j in 1:length(alternative_attributes)){
      columns <- colnames(file) %in% alternative_attributes[[j]]$Attribute
      NA_index[[j]] <- unique(which(is.na(file[,columns, drop = F]), arr.ind = TRUE)[,"row"])
    }
    if(TRUE %in% duplicated(unlist(NA_index))){
      stop(paste0("Every entry must have at least one complete set of alternative required fields. ",
                  "Duplicate NAs are found in row(s) ",
                  paste(unlist(NA_index)[duplicated(unlist(NA_index))]+1, collapse = ", "),
                  " in file: ", file_name))
    }
  }
}

are_groups_complete <- function(file, file_name, my_attributes){
  my_groups <- my_attributes %>%
    filter(!is.na(Group)) %>%
    group_by(Group) %>%
    group_split()

  for(my_group in my_groups){
    is_group_included <- colnames(file) %in% my_group$Attribute
    if(TRUE %in% is_group_included &&
       length(which(is_group_included)) != length(my_group$Attribute)){
      stop(paste0("In file ",file_name," the attributes ",
                  paste(my_group$Attribute, collapse = ", "),
                  " are needed but only ",
                  paste(colnames(file)[is_group_included], collapse = ", ")," are given."))
    }
  }
}

are_dates_converted <- function(file, file_name){
  # All dates should be convertible from a string dd/mm/yyyy to R data format.
  is_unconverted_date <- is.na(file$Date)
  if(TRUE %in% is_unconverted_date){
    stop(paste0("The date is row(s) ",paste(which(is_unconverted_date)+1, collapse = ", "),
                " in file ", file_name,
                " cannot be converted. Ensure all dates have the right format: dd/mm/yyyy."))
  }
}

are_values_correct_type <- function(file, file_name, type_attributes, func){
  columns <- which(colnames(file) %in% type_attributes$Attribute)
  if(length(columns) > 0){
    for(j in 1:length(columns)){
      if(!func(file[,columns[j]])){
        stop(paste0("The column ", colnames(file)[columns[j]], " in file ",file_name,
                    " must contain only values of datatype ",type_attributes$Datatype[1]))
      }
    }
  }
}

are_booleans_correct <- function(file, file_name, boolean_attributes){
  if(length(boolean_attributes$Attribute) > 0){
    columns <- which(colnames(file) %in% boolean_attributes$Attribute)
    subset <- file[,columns, drop = F]
    if(dim(subset)[2] != 0){
      is_not_boolean <- which(!is.na(subset) & subset != 0 & subset != 1, arr.ind = T)[,"row"]+1
      if(length(is_not_boolean) > 0){
        stop(paste0("In file ",file_name,
                    " the boolean attributes ", paste(colnames(file)[columns], collapse = ", "),
                    " may only be 0, 1, or NA. Please check row(s) ",
                    paste(is_not_boolean, collapse = ", ")))
      }
    }
  }
}

are_fractions_correct <- function(file, file_name, fraction_attributes){
  if(length(fraction_attributes$Attribute) > 0){
    columns <- which(colnames(file) %in% fraction_attributes$Attribute)
    subset <- file[,columns, drop = F]
    is_not_fraction <- which(subset < 0 | subset > 1, arr.ind = T)[,"row"]+1
    if(length(is_not_fraction) > 0){
      stop(paste0("In file ",file_name,
                  " the attributes ", paste(colnames(file)[columns], collapse = ", "),
                  " should lie between 0 and 1. Please check row(s) ",
                  paste(is_not_fraction, collapse = ", ")))
    }
  }
}

are_predefined_atts_correct <- function(file, file_name, predefined_attributes){
  for(i in 1:length(predefined_attributes)){
    att <- names(predefined_attributes)[i]
    if(!att %in% colnames(file)){
      next
    }else{
      subset <- file[,att]
      is_not_predefined <- which(!subset %in% predefined_attributes[[i]] & !is.na(subset))+1
      if(length(is_not_predefined) > 0){
        stop(paste0(
          "In file ",file_name," in column ",att,
          " values exist that are not the predefined values ",
          paste(predefined_attributes[[i]], collapse = ", "),
          " or NA in row(s): ",paste(is_not_predefined, collapse = ", ")
        ))
      }
    }
  }
}

are_species_metadata_consistent <- function(file, file_name){
  n_unique_Expect <- file %>%
    group_by(Species_reported, StationID) %>%
    select(Species_reported, StationID) %>%
    distinct()

  n_unique_Obs <- file %>%
    group_by(Species_reported, StationID) %>%
    select(Species_reported, StationID, Fraction, isFractionAssumed) %>%
    distinct() %>%
    mutate(mistake_count = n())

  if(dim(n_unique_Expect)[1] != dim(n_unique_Obs)[1]){
    print(filter(n_unique_Obs, mistake_count > 1))
    stop(paste0("In file ", file_name," the reported Fraction and isFractionAssumed must be equal for all species in a sample."))
  }
}

do_measurements_have_units <- function(file, file_name){
  unit_cols <- grep("Unit_", colnames(file))
  if(length(unit_cols) > 0){
    for(unit_col in unit_cols){
      unit <- colnames(file)[unit_col]
      measurement_col <- gsub("Unit_", "", unit)
      if(!measurement_col %in% colnames(file)){
        stop(paste0("In file ",file_name," no corresponding measurement column called ",
                    measurement_col," was found for the unit column ",unit))
      }
      measurements <- which(!is.na(file[,measurement_col]))
      no_units <- which(is.na(file[measurements,unit_col]))+1
      if(length(no_units) > 0){
        stop(paste0("In file ",file_name," units are missing in column ",unit,
                    " in row(s) ",paste(no_units, collapse = ", ")))
      }
    }
  }
}

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