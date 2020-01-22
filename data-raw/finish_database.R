# --------------------------------------------------
# Workflow to create a final version of the database
# --------------------------------------------------
# This workflow describes which data from species_additions and
# stations_additions are selected to form a final, clean, database
# that can be used as community matrix and in ecological analyses.

# Clean species database
species_final <- species_additions %>%
  # Remove species with no match to WoRMS database
  .[-which(.$hasNoMatch == 1),] %>%
  # Only select revelant columns
  select(
    File, StationID,
    Fraction, isFractionAssumed, Count,
    valid_name, rank, phylum, class, order, family, genus, isFuzzy
  )
usethis::use_data(species_final, overwrite = T)

# Function that will create one new variable (column) in which data
# from multiple columns are combined in order of importance.
# In other words, all data from the preferred data is used,
# then all the missing data (NA) thatare left will be filled with
# data from the secondly preferred data source, etc.
# Order of preference is a vector with column names
combine_data_sources <- function(mytable, new_column_name = "new", order_of_preference){
  mytable$new <- mytable[,order_of_preference[1]]
  for(i in 2:length(order_of_preference)){
    NA_i <- which(is.na(mytable$new))
    if(length(NA_i) == 0){
      break
    }
    mytable$new[NA_i] <- mytable[NA_i, order_of_preference[i]]
  }
  colnames(mytable)[colnames(mytable) == "new"] <- new_column_name
  return(mytable)
}

# Clean station database
stations_final <- stations_additions %>%
  combine_data_sources(
    ., new_column_name = "Lat_DD", order_of_preference = c("Lat_DD_midpt", "Lat_DD_calc")
  ) %>%
  combine_data_sources(
    ., new_column_name = "Lon_DD", order_of_preference = c("Lon_DD_midpt", "Lon_DD_calc")
  ) %>%
  combine_data_sources(
    ., new_column_name = "Water_depth_m", order_of_preference = c("Water_depth_m_cruise", "Water_depth_m_Bathy")
  ) %>%
  combine_data_sources(
    ., new_column_name = "Track_dist_m", order_of_preference = c("Track_dist_m_Odo", "Track_dist_m_BB")
  ) %>%
  select(
    File, Vessel, CruiseID, StationID, Station_name,
    Date, Time_start, Time_stop,
    Blade_depth_cm, Blade_width_cm,
    Lat_DD, Lon_DD, Water_depth_m, Track_dist_m
  )
usethis::use_data(stations_final, overwrite = T)



# Create community matrix
community <- species_final %>%
  group_by(StationID, valid_name) %>%
  summarise(
    Count = sum(Count, na.rm = T),
    Size_sd = sd(Size, na.rm = T),
    Size = mean(Size, na.rm = T),
    WetWeight = sum(WetWeight, na.rm = T)
  )

