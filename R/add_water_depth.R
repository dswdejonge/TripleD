#' Collect NOAA bathymetry
#'
#' This function collects NOAA bathymetry with the marmap R-pacakge
#' based on the min and max coordinates in the database.
#' @references marmap R-package
#' @param stations Dataframe (tibble) with the attributes "Lat_DD_midpt" and "Lon_DD_midpt".
#' @param buffer The size of the buffer zone in decimal degrees around the
#' min and max coordinates in the dataframe. Default = 2.
#' @param resolution The resolution in minutes the bathymetry is collected in.
#' A lower resolution provides better depth estimates from bathymetry, but takes
#' a longer time to collect from the NOAA database. Default resolution is 1 minute.
#' @return Returns a xyz dataframe (x = lon, y = lat, z = altitude i.e. negative values are depth)
#' with bathymetry of a resolution of 1 minute.
#' @export
collect_bathymetry <- function(stations = NULL, lats = NULL, lons = NULL, buffer = 2, resolution = 1){
  if(!null(lats) & !null(lons)){
    lats <- lats ; lons <- lons
  }else if(!null(stations)){
    # Window of coordinates within which to collect bathymetry
    all_Lat <- unlist(stations[,"Lat_DD_midpt"])
    all_Lon <- unlist(stations[,"Lon_DD_midpt"])
    lats <- c(min(all_Lat, na.rm = T)-buffer, max(all_Lat, na.rm = T)+buffer)
    lons <- c(min(all_Lon, na.rm = T)-buffer, max(all_Lon, na.rm = T)+buffer)
  }else{
    stop("To execute the function needs a dataframe with station latitudes and longitudes, or manually provided latitudes and longitudes.")
  }

  # Download bathymetry data from NOAA
  bathy <- marmap::getNOAA.bathy(
    lon1 = lons[1], lon2 = lons[2], lat1 = lats[1], lat2 = lats[2],
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
add_water_depth <- function(stations, bathymetry = NULL){
  if(is.null(bathymetry)){
    bathymetry <- collect_bathymetry(stations)
  }
  stations$Water_depth_m_Bathy <- -(marmap::get.depth(
      mat = marmap::as.bathy(bathymetry),
      x = as.matrix(stations[,c("Lon_DD_midpt", "Lat_DD_midpt")]),
      locator = F)$depth)
  return(stations)
}
