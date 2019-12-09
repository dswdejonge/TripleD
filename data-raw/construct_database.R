# -------------------------------------------------------
# Workflow to construct and update the Triple-D database
# -------------------------------------------------------

library(dplyr)
source("check_format.R")
tables <- list(
  Stations <- c("attributes_stations.csv"),
  Species <- c("attributes_species.csv")
)

for(table in tables){
  # Read in attribute requirements
  my_attributes <- read.csv(system.file("extdata", "attributes_stations.csv", package = "TripleD"))

  # Read in data
  data <- import_data("Stations")

  # Subset data requirements
  required_attributes <- get_required_att(my_attributes)
  alternative_attributes <- get_alternative_att(my_attributes)
  doubles_attributes <- get_doubles_att(my_attributes)
  integer_attributes <- get_int_att(my_attributes)

  # Check requirements for each file
  for(i in 1:length(data)){
    file <- data[[i]]
    file_name <- names(data)[i]

    # Are all required attributes present?
    att_is_missing <- !required_attributes$Attribute %in% colnames(file)
    if(TRUE %in% (att_is_missing)){
      stop(paste0("The required attribute(s) ", paste(required_attributes$Attribute[att_is_missing], collapse = ", "),
                  "is/are missing in the file ", file_name))
    }

    # Are the stationIDs unique?
    ID_is_duplicated <- duplicated(file$StationID)
    if(TRUE %in% ID_is_duplicated){
      stop(paste0("The StationID(s) ", paste(file$StationID[ID_is_duplicated], collapse = ", "),
                  " is/are duplicated; StationID must be unique."))
    }

    # Is at least one of the alternatives for required fields present?
    present <- FALSE
    for(j in 1:length(alternative_attributes)){
      att_is_missing <- !alternative_attributes[[j]]$Attribute %in% colnames(file)
      if(all(!att_is_missing)){
        present <- TRUE
      }
    }
    if(!present) {
      stop(paste0("Please provide at least one alternative set of required variables in ", file_name))
    }

    # Are all provided attributes names (incl. optional) valid?
    att_is_invalid <- !colnames(file) %in% my_attributes$Attribute
    if(TRUE %in% att_is_invalid){
      stop(paste0("The attribute name(s) ",
                  paste(colnames(file)[att_is_invalid], collapse = ", ") ,
                  " in file ",file_name, " is not valid."))
    }

    # No NA values are allowed in the required columns.
    columns <- colnames(file) %in% required_attributes$Attribute
    if(TRUE %in% is.na(file[,columns])){
      stop(paste0("NA values are not allowed for the required attributes ",
                  paste(required_attributes$Attribute, collapse = ", "),
                  ". Please check the file ", file_name))
    }

    # No NA values are allowed in at least one of the required alternatives
    NA_index <- list()
    for(j in 1:length(alternative_attributes)){
      columns <- colnames(file) %in% alternative_attributes[[j]]$Attribute
      NA_index[[j]] <- unique(which(is.na(file[,columns]), arr.ind = TRUE)[,"row"])
    }
    if(TRUE %in% duplicated(unlist(NA_index))){
      stop(paste0("Every entry must have at least one complete set of alternative required fields. ",
                  "Duplicate NAs are found in row(s) ",
                  paste(unlist(NA_index)[duplicated(unlist(NA_index))], collapse = ", "),
                  " in file: ", file_name))
    }

    # All dates should be convertible from a string dd/mm/yyyy to R data format.
    data[[i]]$Date <- as.Date(data[[i]]$Date, format = "%d/%m/%Y")
    is_unconverted_date <- is.na(data[[i]]$Date)
    if(TRUE %in% is_unconverted_date){
      stop(paste0("The date is row(s) ",paste(which(is_unconverted_date), collapse = ", "),
                  " in file ", file_name,
                  " cannot be converted. Ensure all dates have the right format: dd/mm/yyyy."))
    }

    # All attributes assigned 'Double' must be datatype double
    columns <- which(colnames(file) %in% doubles_attributes$Attribute)
    for(j in 1:length(columns)){
      if(!is.double(file[,columns[j]])){
        stop(paste0("The column ", colnames(file)[columns[j]], " must contain only values of datatype double i.e. no strings or integers."))
      }
    }

    # All attributes assigned 'Integer' must be datatype double
    columns <- which(colnames(file) %in% integer_attributes$Attribute)
    for(j in 1:length(columns)){
      if(!is.integer(file[,columns[j]])){
        stop(paste0("The column ", colnames(file)[columns[j]], " must contain only values of datatype integer i.e. no strings or doubles."))
      }
    }
  }
}



# - If there are any suspicious values (very deep water depths, very large organisms, strange locations).
# - Track distances equal?
# - are there any suspicious values (very small/large/heavy organisms)

# Add findings to construct database log file
# - what data is missing
# - weird values


# Fraction, isFractionAssumed must be same for all species entries at a certain stationID.
# If the species name is present in WoRMS: the newest accepted synonym is added in a column.
# If the species name is not present in WoRMS (for example higher taxon): what then?
# All size measurements have units.
