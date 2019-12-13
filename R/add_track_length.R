# Calculate track length from gps points

add_track_length_GPS <- function(stations){
  stations <- stations %>%
    dplyr::mutate(Track_dist_m_GPS = geosphere::distGeo(
      as.matrix(.[c("Lon_start_DD", "Lat_start_DD")]),
      as.matrix(.[c("Lon_stop_DD", "Lat_stop_DD")])
    ))
  return(stations)
}

# BB is without pre-run, SB is including pre-run.
add_track_length_Odometer <- function(stations){
  stations <- stations %>%
    dplyr::mutate(Track_dist_m_BB = BB_count * 2) %>%
    dplyr::mutate(Track_dist_m_SB = SB_count * 2)
  return(stations)
}
