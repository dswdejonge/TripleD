#' Collect bathymetry from NOAA
#'
#' This function collects NOAA bathymetry with the marmap R-pacakge
#' based on the min and max coordinates in the database.
#' @details
#' Bathymetry data is collected from the NOAA database using the \code{marmap} R-package.
#' You need internet connection to do this. The default resolution is 1 minute.
#' You can also choose to collect your own bathymetry with a different resolution from NOAA by using the
#' package function \code{collect_bathymetry}, or not to collect bathymetry externally at all
#' if you have your own dataset.
#' @references marmap R-package
#' @return This function does not return an object, but stores the information in the specified
#' \code{out_folder} under the names 'bathymetry.rda'.
#' @param stations The initial database for stations with track midpoints (attributes "Lat_DD_midpt" and "Lon_DD_midpt").
#' If NULL (default) the function will automatically search the data_folder for 'stations_initial.rda'.
#' @param buffer The size of the buffer zone in decimal degrees around the given
#' min and max coordinates. Default = 2.
#' @param resolution The resolution in minutes the bathymetry is collected in.
#' A lower resolution provides better depth estimates from bathymetry, but takes
#' a longer time to collect from the NOAA database. Default resolution is 1 minute.
#' @param lats (optional) You can specify latitudes you want to use to collect bathymetry:
#' a numeric vector with latitudes of which the min and max are found and used.
#' If not specified, the latitudes from the database are used.
#' @param lons (optional) You can specify longitudes you want to use to collect bathymetry:
#' a numeric vector with longitudes of which the min and max are found and used.
#' If not specified, the longitudes from the database are used.
#' @param data_folder If the stations database is not provided, the function will search
#' for it ('stations_initial.rda') in this folder. Default is 'data'.
#' @param out_folder The external data is stored in this folder. Default is 'data'.
#' @param as_CSV If you also want to store the collected external data as CSV, set to TRUE. Default is FALSE.
#' @export
collect_from_NOAA <- function(stations = NULL, buffer = 2, resolution = 1,
                              data_folder = "data", out_folder = "data",
                              lats = NULL, lons = NULL, as_CSV = FALSE){
  if(is.null(stations)){
    message("Loading intitial database with stations...")
    load(paste0(data_folder,"/stations_initial.rda"))
  }
  # Collect bathymetry from NOAA
  message("Collecting bathymetry from NOAA. This can take a while...")
  if(is.null(lats) & is.null(lons)){
    lats <- unlist(stations[,c("Lat_DD_midpt","Lat_start_DD", "Lat_stop_DD")])
    lons <- unlist(stations[,c("Lon_DD_midpt","Lon_start_DD", "Lon_stop_DD")])
  }

  lon1 <- min(lons, na.rm = T)-buffer
  lon2 <- max(lons, na.rm = T)+buffer
  lat1 <- min(lats, na.rm = T)-buffer
  lat2 <- max(lats, na.rm = T)+buffer

  # Contact API
  bathymetry <- marmap::getNOAA.bathy(
    lon1 = lon1, lon2 = lon2, lat1 = lat1, lat2 = lat2,
    resolution = resolution, keep = T)

  # Convert to dataframe
  bathymetry <- marmap::fortify.bathy(bathymetry)

  #bathymetry <- collect_bathymetry(lats, lons)
  save(bathymetry, file = paste0(out_folder,"/bathymetry.rda"))
  message(paste0("Bathymetry stored as ",out_folder,"/bathymetry.rda."))
  if(as_CSV){
    write.csv(bathymetry, file = paste0(out_folder,"/bathymetry.csv"))
  }
}

#' Add water depth
#'
#' This function adds an attribute with water depth to the stations dataframe
#' extracted for a certain position (lat, lon) based on batymetry.
#' @references marmap R-package
#' @param stations Dataframe (tibble) with the attributes "Lat_DD_midpt" and "Lon_DD_midpt".
#' @param bathymetry An xyz dataframe (tibble) or object of class bathy created by
#' the marmap package.
#' @param col_lon Column name with longitudes in stations dataframe
#' @param col_lat Column name with latitudes in stations dataframe
#' @param col_name New column name in which the extracted depth will be added.
#' @return Returns a dataframe with depth added in a new column based on bathymetry.
#' @export
add_water_depth <- function(stations, bathymetry, col_lon, col_lat, col_name){
  coordinates <- as.matrix(stations[,c(col_lon, col_lat)])
  rows_i <- !is.na(coordinates[,1])
  coordinates_no_NA <- coordinates[rows_i,]
  depths <- -(marmap::get.depth(
    mat = marmap::as.bathy(bathymetry),
    x = coordinates_no_NA,
    locator = F)$depth)
  stations$new_col <- NA
  stations$new_col[rows_i] <- depths

  colnames(stations)[which(colnames(stations) == "new_col")] <- col_name
  return(stations)
}
