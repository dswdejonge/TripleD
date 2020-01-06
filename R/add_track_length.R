#' Add track length based on GPS
#'
#' The function adds a column with track length in meters based on track start and stop GPS coordinates.
#' @references
#' \itemize{
#' \item{geosphere R-package}
#' }
#' @param stations (required) Dataframe (tibble) with all stations and the attributes
#' (columns) "Lon_start_DD", "Lat_start_DD","Lon_stop_DD", and "Lat_stop_DD".
#' @return This function returns the input tibble with an added column called
#' "Track_dist_m_GPS".
#' @export
add_track_length_GPS <- function(stations){
  stations <- stations %>%
    dplyr::mutate(Track_dist_m_GPS = geosphere::distGeo(
      as.matrix(.[c("Lon_start_DD", "Lat_start_DD")]),
      as.matrix(.[c("Lon_stop_DD", "Lat_stop_DD")])
    ))
  return(stations)
}

#' Add track length based on the odometer
#'
#' The function adds a column with track length in meters based on odometer ticks (one tick is two meters).
#' @references
#' \itemize{
#' \item{geosphere R-package}
#' }
#' @param stations (required) Dataframe (tibble) with all stations and the attributes
#' (columns) "Lon_start_DD", "Lat_start_DD","Lon_stop_DD", and "Lat_stop_DD".
#' @return This function returns the input tibble with an added column called
#' "Track_dist_m_BB" and "Track_dist_m_SB".
#' @export
add_track_length_Odometer <- function(stations){
  stations <- stations %>%
    dplyr::mutate(Track_dist_m_BB = BB_count * 2) %>%
    dplyr::mutate(Track_dist_m_SB = SB_count * 2)
  return(stations)
}
