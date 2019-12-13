# Add roxygen documentation on function
# how to include dependencies?
# Sometimes only start and end gps points are given.
# Calculate midpoints for these tracks.
# Include the midpoints in Lat_DD and Lon_DD if there don't exist
# Compare to existing midpoints to check inconsistencies.

find_track_midpoints <- function(stations){
  midpoints <- geosphere::midPoint(
    stations[,c("Lon_start_DD", "Lat_start_DD")],
    stations[,c("Lon_stop_DD", "Lat_stop_DD")])
  colnames(midpoints) <- c("Lon_DD_calc", "Lat_DD_calc")
  return(dplyr::as_tibble(midpoints))
}

add_track_midpoints <- function(stations){
  midpoints <- find_track_midpoints(stations)
  stations <- dplyr::bind_cols(stations, midpoints)
  return(stations)
}
