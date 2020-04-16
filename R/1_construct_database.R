# -----------------------
# Import data from folder
# -----------------------
import_data <- function(folder) {
  files <- list.files(folder)
  data <- lapply(paste0(folder,"/",files), read.csv, strip.white = T)
  names(data) <- files
  return(data)
}

# -----------------------
# Get attributes
# -----------------------

# Get required attributes
get_required_att <- function(my_attributes) {
  req_att <- my_attributes %>%
    dplyr::filter(Required_or_Optional == "Required")
  return(req_att)
}

# Get alternative attributes
get_alternative_att <- function(my_attributes) {
  alt_att <- my_attributes %>%
    dplyr::filter(Required_or_Optional == "Required_alternative") %>%
    dplyr::group_by(Group) %>%
    dplyr::group_split()
  return(alt_att)
}

# Get attributes with datatype double
get_doubles_att <- function(my_attributes) {
  doubles_attributes <- my_attributes %>%
    dplyr::filter(Datatype == "Double")
  return(doubles_attributes)
}

# Get attributes with datatype integer
get_int_att <- function(my_attributes){
  int_att <- my_attributes %>%
    dplyr::filter(Datatype == "Integer")
  return(int_att)
}

get_fraction_att <- function(my_attributes){
  frac_att <- my_attributes %>%
    dplyr::filter(Unit == "Fraction")
  return(frac_att)
}

get_boolean_att <- function(my_attributes){
  bool_att <- my_attributes %>%
    dplyr::filter(Datatype == "Boolean")
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

are_IDs_unique <- function(file, file_name, ID_column = NULL){
  if(!(ID_column %in% colnames(file))){
    stop("Unknown ID_column given.")
  }
  ID_is_duplicated <- duplicated(file[,ID_column])
  if(TRUE %in% ID_is_duplicated){
    stop(paste0("The ID(s) ", paste(file[ID_is_duplicated, ID_column], collapse = ", "),
                 " in file ",file_name," is/are duplicated. IDs must be unique."))
  }
}

are_required_att_complete <- function(file, file_name, required_attributes){
  # No NA values are allowed in the required columns.
  columns <- colnames(file) %in% required_attributes$Attribute
  subset <- file[,columns, drop = F]
  if(TRUE %in% is.na(subset)){
    rowi <- unique(which(is.na(subset), arr.ind = T)[,"row"]+1)
    stop(paste0("NA values are not allowed for the required attributes ",
                paste(required_attributes$Attribute, collapse = ", "),
                ". Please check the file ", file_name,
                " row(s): ",paste(sort(unique(rowi)), collapse = ", ")))
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
                  paste(sort(unlist(NA_index)[duplicated(unlist(NA_index))]+1), collapse = ", "),
                  " in file: ", file_name))
    }
  }
}

are_groups_complete <- function(file, file_name, my_attributes){
  my_groups <- my_attributes %>%
    dplyr::filter(!is.na(Group)) %>%
    dplyr::group_by(Group) %>%
    dplyr::group_split()

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
    stop(paste0("The date in row(s) ",paste(sort(which(is_unconverted_date)+1), collapse = ", "),
                " in file ", file_name,
                " cannot be converted. Ensure all dates have the right format: dd/mm/yyyy."))
  }
}

is_time_format_correct <- function(file, file_name){
  is_wrong_time_start <- gsub("[0-2][0-9]:[0-5][0-9]:[0-5][0-9]","", file$Time_start) != ""
  is_wrong_time_stop <- gsub("[0-2][0-9]:[0-5][0-9]:[0-5][0-9]","", file$Time_stop) != ""
  if(TRUE %in% is_wrong_time_stop){
    is_wrong_time_stop <- !all(is.na(file$Time_stop))
  }
  if(TRUE %in% is_wrong_time_start | TRUE %in% is_wrong_time_stop){
    stop(paste0("The time in column 'Time_start' and/or 'Time_stop' and row(s) ",
                paste(sort(which(c(is_wrong_time_start, is_wrong_time_stop))+1), collapse = ", "),
                " in file ", file_name, " should be in the format 'HH:MM:SS'."))
  }
}

is_Station_objective_correct <- function(file, file_name){
  incomplete <- (file$Station_objective == "Incomplete")
  if(TRUE %in% incomplete){
    if(is.null(file$Excluded)){
      stop(paste0("In file ",file_name,", some entries in column 'Station_objective' are 'Incomplete', but the necessary column 'Excluded' does not exist."))
    }
    incomplete_and_NA <- which(incomplete & is.na(file$Excluded))
    if(length(incomplete_and_NA) > 0){
      stop(paste0("In file ",file_name,", the entries in row(s) ",
                  paste(sort(incomplete_and_NA+1), collapse = ", "),
                  " are defined 'Incomplete' but no excluded taxons are given in the column 'Excluded'."))
    }
  }

  focus <- (file$Station_objective == "Focus")
  if(TRUE %in% focus){
    if(is.null(file$Focus)){
      stop(paste0("In file ",file_name,", some entries in column 'Station_objective' are 'Focus', but the necessary extra column 'Focus' does not exist."))
    }
    focus_and_NA <- which(focus & is.na(file$Focus))
    if(length(focus_and_NA > 0)){
      stop(paste0("In file ",file_name,", the entries in row(s) ",
                  paste(sort(focus_and_NA+1), collapse = ", "),
                  " are defined 'Focus' in the column 'Station_objective', but no taxons that were focussed on are given in the extra column 'Focus'."
                  ))
    }
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
                    paste(sort(is_not_boolean), collapse = ", ")))
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
                  paste(sort(is_not_fraction), collapse = ", ")))
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
          " or NA in row(s): ",paste(sort(is_not_predefined), collapse = ", ")
        ))
      }
    }
  }
}

are_species_metadata_consistent <- function(file, file_name){
  n_unique_Expect <- file %>%
    dplyr::group_by(Species_reported, StationID) %>%
    dplyr::select(Species_reported, StationID) %>%
    dplyr::distinct()

  n_unique_Obs <- file %>%
    dplyr::group_by(Species_reported, StationID) %>%
    dplyr::select(Species_reported, StationID, Fraction, is_Fraction_assumed) %>%
    dplyr::distinct() %>%
    dplyr::mutate(mistake_count = n())

  if(dim(n_unique_Expect)[1] != dim(n_unique_Obs)[1]){
    print(dplyr::filter(n_unique_Obs, mistake_count > 1))
    stop(paste0("In file ", file_name," the reported Fraction and is_Fraction_assumed must be equal for all species in a sample."))
  }
}

is_biomass_complete <- function(file, file_name){
  # WW
  should_be_complete <- which(!is.na(file$WW_g))
  if(length(should_be_complete > 0)){
    isNA <- is.na(file[should_be_complete,
                       c("Weight_type", "Threshold_scale",
                         "is_Shell_removed", "is_Partial_WW")])
    if(TRUE %in% isNA){
      stop("Wet weight is reported, but the columns Weight_type, Threshold_scale, isWithShell and is_Partial_WW are not filled for all weights.")
    }
  }

  # AFDW
  should_be_complete <- which(!is.na(file$AFDW_g))
  if(length(should_be_complete > 0)){
    isNA <- is.na(file[should_be_complete,
                       c("Weight_type_AFDW", "Threshold_scale_AFDW",
                         "is_Partial_AFDW")])
    if(TRUE %in% isNA){
      stop("AFDW is reported, but the columns Weight_type_AFDW, Threshold_scale_AFDW, and is_Partial_AFDW are not filled for all weights.")
    }
  }
}

do_measurements_have_units <- function(file, file_name){
  measurements <- which(!is.na(file$Size_value))
  no_units <- which(is.na(file$Size_unit[measurements]))+1
  no_dimensions <- which(is.na(file$Size_dimension[measurements]))+1
  if(length(no_units) > 0){
    stop(paste0("In file ",file_name,
                " size units are missing in row(s) ",
                paste(sort(no_units), collapse = ", ")))
  }
  if(length(no_dimensions) > 0){
    stop(paste0("In file ",file_name,
                " size dimensions are missing in row(s) ",
                paste(sort(no_dimensions), collapse = ", ")))
  }
}

are_measurements_positive <- function(file, file_name){
  # Stations
  tl <- file$Track_length_m_preset
  bd <- file$Blade_depth_cm
  bw <- file$Blade_width_cm
  ts <- file$Tow_speed_knots
  wd <- file$Water_depth_m_cruise
  oc <- file$Odometer_count
  if(!is.null(tl)){
    tl <- tl  < 0
    if(TRUE %in% tl){
      stop(paste0("In file ",file_name," column Track_length_cruise_m the values in row(s) ",
                  paste(sort(which(tl)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(bd)){
    bd <- bd < 0
    if(TRUE %in% bd){
      stop(paste0("In file ",file_name," column Blade_depth_cm the values in row(s) ",
                  paste(sort(which(bd)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(bw)){
    bw <- bw < 0
    if(TRUE %in% bw){
      stop(paste0("In file ",file_name," column Blade_width_cm the values in row(s) ",
                  paste(sort(which(bw)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(ts)){
    ts <- ts < 0
    if(TRUE %in% ts){
      stop(paste0("In file ",file_name," column Tow_speed_knots the values in row(s) ",
                  paste(sort(which(ts)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(wd)){
    wd <- wd < 0
    if(TRUE %in% wd){
      stop(paste0("In file ",file_name," column Water_depth_m_cruise the values in row(s) ",
                  paste(sort(which(wd)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(oc)){
    oc <- oc < 0
    if(TRUE %in% oc){
      stop(paste0("In file ",file_name," column Odometer_count the values in row(s) ",
                  paste(sort(which(oc)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  # Species
  ct <- file$Count
  sv <- file$Size_value
  ww <- file$WW_g
  aw <- file$AFDW_g
  tw <- file$Threshold_scale
  ta <- file$Threshold_scale_AFDW

  if(!is.null(ct)){
    ct <- ct  < 0
    if(TRUE %in% ct){
      stop(paste0("In file ",file_name," column Count the values in row(s) ",
                  paste(sort(which(ct)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(sv)){
    sv <- sv  < 0
    if(TRUE %in% sv){
      stop(paste0("In file ",file_name," column Size_value the values in row(s) ",
                  paste(sort(which(sv)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(ww)){
    ww <- ww  < 0
    if(TRUE %in% ww){
      stop(paste0("In file ",file_name," column WW_g the values in row(s) ",
                  paste(sort(which(ww)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(aw)){
    aw <- aw  < 0
    if(TRUE %in% aw){
      stop(paste0("In file ",file_name," column AFDW_g the values in row(s) ",
                  paste(sort(which(aw)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(tw)){
    tw <- tw  <= 0
    if(TRUE %in% tw){
      stop(paste0("In file ",file_name," column Threshold_scale the values in row(s) ",
                  paste(sort(which(tw)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
  if(!is.null(ta)){
    ta <- ta  <= 0
    if(TRUE %in% ta){
      stop(paste0("In file ",file_name," column Threshold_scale_AFDW the values in row(s) ",
                  paste(sort(which(ta)+1), collapse = ", "), " are negative but should be positive."))
    }
  }
}

#############
# pos2coord #
#############
# source: https://www.r-bloggers.com/array-position-to-matrix-coordinates-conversion/
# Function to go from position in matrix to coordinates of matrix
# position is index from 1 to length(matrix)
# coord is [row, col]
# dim.mat is c(nrow, ncol)
pos2coord <- function(pos=NULL, coord=NULL, dim.mat=NULL) {
  if(is.null(pos) & is.null(coord) | is.null(dim.mat)){
    stop("must supply either 'pos' or 'coord', and 'dim.mat'")
  }
  if(is.null(pos) & !is.null(coord) & !is.null(dim.mat)){
    pos <- ((coord[,2]-1)*dim.mat[1])+coord[,1]
    return(pos)
  }
  if(!is.null(pos) & is.null(coord) & !is.null(dim.mat)){
    coord <- matrix(NA, nrow=length(pos), ncol=2)
    colnames(coord) <- c("row", "col")
    coord[,1] <- ((pos-1) %% dim.mat[1]) +1
    coord[,2] <- ((pos-1) %/% dim.mat[1]) +1
    return(coord)
  }
}

add_missing_columns <- function(df, attributes){
  missing_columns <- attributes[which(!attributes %in% colnames(df))]
  if(length(missing_columns) > 0){
    for(i in 1:length(missing_columns)){
      df$newcol <- NA
      colnames(df)[which(colnames(df) == "newcol")] <- missing_columns[i]
    }
  }
  return(df)
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
#' @importFrom magrittr "%>%"
#' @export
construct_database <- function(in_folder = "inputfiles", out_folder = "data", as_CSV = TRUE){
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
    my_attributes <- read.csv(system.file("extdata", table$att, package = "TripleD")) %>%
      dplyr::filter(Attribute != "IGNORE")


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

      # Check if all cells are filled.
      # Empty cells are read in as "" or NaN and are not allowed.
      NaNs <- is.nan(unlist(file))
      emptystr <- (file == "")
      if(TRUE %in% c(NaNs, emptystr)){
        rowi <- unique(c(
          pos2coord(pos = which(NaNs), dim.mat = dim(file))[,"row"]+1,
          which(file == "", arr.ind = T)[,"row"]+1
        ))
        stop("No empty cells are allowed in the CSV. Check rows ",
             paste(sort(rowi), collapse = ", "))
      }

      # Attribute presence
      are_required_att_present(file, file_name, required_attributes)
      are_alternative_required_att_present(file, file_name, alternative_attributes)
      are_att_names_valid(file, file_name, my_attributes)

      # Correct values and units
      are_required_att_complete(file, file_name, required_attributes)
      are_alternative_required_att_complete(file, file_name, alternative_attributes)
      are_groups_complete(file, file_name, my_attributes)
      are_values_correct_type(file, file_name, doubles_attributes, func = is.double)
      are_values_correct_type(file, file_name, integer_attributes, func = is.integer)
      are_booleans_correct(file, file_name, boolean_attributes)
      are_fractions_correct(file, file_name, fraction_attributes)
      are_predefined_atts_correct(file, file_name, predefined_attributes)
      if(table$folder == "Stations"){
        are_IDs_unique(file, file_name, ID_column = "StationID")
        data[[i]]$Date <- as.Date(data[[i]]$Date, format = "%d/%m/%Y")
        are_dates_converted(file = data[[i]], file_name)
        is_time_format_correct(file, file_name)
        is_Station_objective_correct(file, file_name)
      }
      if(table$folder == "Species"){
        are_IDs_unique(file, file_name, ID_column = "EntryID")
        are_species_metadata_consistent(file, file_name)
        is_biomass_complete(file, file_name)
        do_measurements_have_units(file, file_name)
        # TODO: check if there is only one sample weight for the species/station combi??
        # TODO: for sample weight all isWithShell should be the same??
      }
      are_measurements_positive(file, file_name)
    }
    if(table$folder == "Stations"){
      stations <- dplyr::bind_rows(data, .id = "File")
      # If certain columns do not exist, create them with values NA, to prevent errors down the line.
      stations <- add_missing_columns(stations, as.character(my_attributes$Attribute))
    }
    if(table$folder == "Species"){
      species <- dplyr::bind_rows(data, .id = "File")
      # If certain columns do not exist, create them with values NA, to prevent errors down the line.
      species <- add_missing_columns(species, as.character(my_attributes$Attribute))
    }
  }
  # Test of all stationIDs used in the species files are also mentioned in the station files.
  missing_stationIDs <- as.character(
    unique(species$StationID[which(!species$StationID %in% stations$StationID)]))
  if(length(missing_stationIDs) > 0){
    stop(paste0("The StationID(s) ", paste(missing_stationIDs, collapse = ", "), " are reported in the species file,
                   but are missing in the stations file, i.e. metadata is missing for these biological data points."))
  }
  # Test if stationIDs are unique over different files.
  ID_is_duplicated <- which(duplicated(stations$StationID))
  if(length(ID_is_duplicated) > 0){
    stop(paste0("The StationID(s)" ,paste(stations$StationID[ID_is_duplicated], collapse = ", "),
                " occur multiple times in differen files, but they must be unique. Please check."))
  }

  # Save
  save(stations, file = paste0(out_folder,"/","stations_initial.rda"))
  save(species, file = paste0(out_folder,"/","species_initial.rda"))
  if(as_CSV){
    write.csv(stations, file = paste0(out_folder, "/stations_initial.csv"))
    write.csv(species, file = paste0(out_folder, "/species_initial.csv"))
  }
}
