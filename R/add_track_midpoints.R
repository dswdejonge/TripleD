#' Track midpoints
#'
#' Functions to find the geographic midpoint of each sample track and
#' add it the the stations dataframe (tibble)
#' @references geosphere R-package
#' @param stations Dataframe (tibble) of the samples stations including the attributes
#' "Lon_start_DD", "Lat_start_DD", "Lon_stop_DD", "Lat_stop_DD".
#' @return Returns the input dataframe (tibble) with an added attributes called
#' "Lon_DD_calc" and "Lat_DD_calc". Stations without start and stop coordinates have
#' NA in these added columsn.
#' @export
find_track_midpoints <- function(stations){
  midpoints <- geosphere::midPoint(
    stations[,c("Lon_start_DD", "Lat_start_DD")],
    stations[,c("Lon_stop_DD", "Lat_stop_DD")])
  colnames(midpoints) <- c("Lon_DD_calc", "Lat_DD_calc")
  return(dplyr::as_tibble(midpoints))
}

#' @rdname find_track_midpoints
add_track_midpoints <- function(stations){
  midpoints <- find_track_midpoints(stations)
  stations <- dplyr::bind_cols(stations, midpoints)
  return(stations)
}
