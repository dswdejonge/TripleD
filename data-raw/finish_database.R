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
    ., new_column_name = "Track_length_m", order_of_preference = c("Track_length_m_Odo", "Track_dist_m_BB")
  ) %>%
  mutate(Sample_area_m2 = Track_length_m * (Blade_width_cm/100)) %>%
  mutate(Sample_volume_m3 = Sample_area_m2 * (Blade_depth_cm/100)) %>%
  select(
    File, Vessel, CruiseID, StationID, Station_name,
    Date, Time_start, Time_stop,
    Blade_depth_cm, Blade_width_cm,
    Lat_DD, source_Lat_DD, Lon_DD, source_Lon_DD,
    Water_depth_m, source_Water_depth_m, Track_length_m, source_Track_length_m,
    Sample_area_m2, Sample_volume_m3
  )
usethis::use_data(stations_final, overwrite = T)

# Create one large table for the Shiny app
st <- select(stations_final, -File)
sp <- species_final %>%
  group_by(StationID, valid_name) %>%
  summarise(Count = sum(Count, na.rm = T))
database <- inner_join(sp, st, by = "StationID")
usethis::use_data(database, overwrite = T)

# Create community matrix
#community <- species_final %>%
#  group_by(StationID, valid_name) %>%
#  summarise(
#    Count = sum(Count, na.rm = T),
#    Size_sd = sd(Size, na.rm = T),
#    Size = mean(Size, na.rm = T),
#    WetWeight = sum(WetWeight, na.rm = T)
#  )

