#' Add track bearing
#'
#' Function to add the initial bearing based on the start and stop coordinates
#' of the track.
#' @references geosphere R-package
#' @param stations Dataframe (tibble) of the samples stations including the attributes
#' "Lon_start_DD", "Lat_start_DD", "Lon_stop_DD", "Lat_stop_DD".
#' @return Returns the input dataframe (tibble) with an added attributes called
#' "Bearing_calc" and "Bearing_calc". Stations without start and stop coordinates have
#' NA in these added columns.
#' @export
add_bearings <- function(stations){
  bearings <- geosphere::bearing(
    stations[,c("Lon_start_DD", "Lat_start_DD")],
    stations[,c("Lon_stop_DD", "Lat_stop_DD")])
  stations <- dplyr::bind_cols(stations, data.frame(Bearing_calc = bearings))
  return(stations)
}
