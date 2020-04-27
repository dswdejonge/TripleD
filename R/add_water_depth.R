#' Collect NOAA bathymetry
#'
#' This function collects NOAA bathymetry with the marmap R-pacakge
#' based on the min and max coordinates in the database.
#' @references marmap R-package
#' @param stations Dataframe (tibble) with the attributes "Lat_DD_midpt" and "Lon_DD_midpt".
#' @param lats (optional) You can specify latitudes you want to use to collect bathymetry.
#' If not specified, the track midpoints in the database are used.
#' @param lons (optional) You can specify longitudes you want to use to collect bathymetry.
#' If not specified, the track midpoints in the database are used.
#' @param buffer The size of the buffer zone in decimal degrees around the
#' min and max coordinates in the dataframe. Default = 2.
#' @param resolution The resolution in minutes the bathymetry is collected in.
#' A lower resolution provides better depth estimates from bathymetry, but takes
#' a longer time to collect from the NOAA database. Default resolution is 1 minute.
#' @return Returns a xyz dataframe (x = lon, y = lat, z = altitude i.e. negative values are depth)
#' with bathymetry of a resolution of 1 minute.
#' @export
#collect_bathymetry <- function(stations = NULL, lats = NULL, lons = NULL, buffer = 2, resolution = 1){
collect_bathymetry <- function(lats = NULL, lons = NULL, buffer = 2, resolution = 1){
  #if(!is.null(lats) & !is.null(lons)){
  #  lats <- lats ; lons <- lons
  #}else if(!is.null(stations)){
    # Window of coordinates within which to collect bathymetry
  #  all_Lat <- unlist(stations[,"Lat_DD_midpt"])
  #  all_Lon <- unlist(stations[,"Lon_DD_midpt"])
  #  lats <- c(min(all_Lat, na.rm = T)-buffer, max(all_Lat, na.rm = T)+buffer)
  #  lons <- c(min(all_Lon, na.rm = T)-buffer, max(all_Lon, na.rm = T)+buffer)
  #}else{
  #  stop("To execute the function needs a dataframe with station latitudes and longitudes, or manually provided latitudes and longitudes.")
  #}

  lon1 <- min(lons, na.rm = T)-buffer
  lon2 <- max(lons, na.rm = T)+buffer
  lat1 <- min(lats, na.rm = T)-buffer
  lat2 <- max(lats, na.rm = T)+buffer

  # Download bathymetry data from NOAA
  bathy <- marmap::getNOAA.bathy(
    #lon1 = lons[1], lon2 = lons[2], lat1 = lats[1], lat2 = lats[2],
    lon1 = lon1, lon2 = lon2, lat1 = lat1, lat2 = lat2,
    resolution = resolution, keep = T)

  # Convert to dataframe
  bathy <- marmap::fortify.bathy(bathy)
  return(bathy)
}

#' Add water depth
#'
#' This function adds an attribute with water depth based on track midpoints and the
#' collected NOAA bathymetry with the marmap R-pacakge using the function \code{get.depth()}.
#' @references maramp R-package
#' @param stations Dataframe (tibble) with the attributes "Lat_DD_midpt" and "Lon_DD_midpt".
#' @param bathymetry An xyz dataframe (tibble) or object of class bathy created by
#' the marmap package. Default = NULL, uses \code{collect_bathymetry()} function to first download data.
#' @return Returns a xyz dataframe (x = lon, y = lat, z = altitude i.e. negative values are depth)
#' with bathymetry of a resolution of 1 minute.
#' @export
add_water_depth <- function(stations, bathymetry = NULL, col_lon, col_lat, col_name){
  coordinates <- as.matrix(stations[,c(col_lon, col_lat)])
  rows_i <- !is.na(coordinates[,1])
  coordinates_no_NA <- coordinates[rows_i,]
  depths <- -(marmap::get.depth(
    mat = marmap::as.bathy(bathymetry),
    x = coordinates_no_NA,
    locator = F)$depth)
  stations$new_col <- NA
  stations$new_col[rows_i] <- depths

  #stations$Water_depth_m_Bathy <- -(marmap::get.depth(
    #  mat = marmap::as.bathy(bathymetry),
      #x = as.matrix(stations[,c("Lon_DD_midpt", "Lat_DD_midpt")]),
      #locator = F)$depth)
  colnames(stations)[which(colnames(stations) == "new_col")] <- col_name
  return(stations)
}
