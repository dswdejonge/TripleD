# -------------------------------------------------------
# Workflow to construct and update the Triple-D database
# -------------------------------------------------------
library(dplyr)

# Read in constant parameters
source("parameters.R")

# Read station data
source("Stations.R")
data <- import_stations_data()

# For each file
for(file in data){
  areRequiredStationAttributesPresent(
    input = colnames(file),
    req = required_station_fields,
    alt = alternative_gps_fields)

}
# - are the required columns given

# - are the column/variable names valid

# - are the data types in the columns valid (numerical/char/factor)

# - If there are any suspicious values (very deep water depths, very large organisms, strange locations).

# - Track distances equal?

# Check for duplicates

# Add findings to construct database log file
# - what data is missing
# - weird values

# Read species data

# For each file
# - are the required columns given

# - are the column/variable names valid

# - are the data types in the columns valid (numerical/char/factor)

# - are there any suspicious values (very small/large/heavy organisms)

# Fraction, isFractionAssumed must be same for all species entries at a certain stationID.
# If the species name is present in WoRMS: the newest accepted synonym is added in a column.
# If the species name is not present in WoRMS (for example higher taxon): what then?
# All size measurements have units.
