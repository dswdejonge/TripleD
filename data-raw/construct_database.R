# -------------------------------------------------------
# Workflow to construct and update the Triple-D database
# -------------------------------------------------------

library(dplyr)
source("check_format.R")
tables <- list(
  Stations <- list(folder = "Stations", att = "attributes_stations.csv"),
  Species <- list(folder = "Species", att = "attributes_species.csv")
)

for(table in tables){
  # Read in attribute requirements
  my_attributes <- read.csv(system.file("extdata", table$att, package = "TripleD"))

  # Read in data
  data <- import_data(table$folder)

  # Subset data requirements
  required_attributes <- get_required_att(my_attributes)
  alternative_attributes <- get_alternative_att(my_attributes)
  doubles_attributes <- get_doubles_att(my_attributes)
  integer_attributes <- get_int_att(my_attributes)
  boolean_attributes <- get_boolean_att(my_attributes)

  # Check format requirements for each file
  for(i in 1:length(data)){
    file <- data[[i]]
    file_name <- paste(table$folder, names(data)[i], sep = "/")

    are_required_att_present(file, file_name, required_attributes)
    are_StationIDs_unique(file, file_name)
    are_alternative_required_att_present(file, file_name, alternative_attributes)
    are_att_names_valid(file, file_name, my_attributes)
    are_req_att_complete(file, file_name, required_attributes)
    are_alternative_req_att_complete(file, file_name, alternative_attributes)
    if(table$folder == "Stations"){
      data[[i]]$Date <- as.Date(data[[i]]$Date, format = "%d/%m/%Y")
      are_dates_converted(file = data[[i]], file_name)
    }
    are_values_correct_type(file, file_name, doubles_attributes, func = is.double)
    are_values_correct_type(file, file_name, integer_attributes, func = is.integer)
    are_booleans_correct(file, file_name, boolean_attributes)
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
