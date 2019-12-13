# Add missing water depth using the Bathymetry from NOAA using the marmap function

collect_bathymetry <- function(stations, buffer = 2, resolution = 1){
  # Window of coordinates within which to collect bathymetry
  all_Lat <- unlist(stations[,c("Lat_DD", "Lat_start_DD", "Lat_stop_DD")])
  all_Lon <- unlist(stations[,c("Lon_DD", "Lon_start_DD", "Lon_stop_DD")])
  lats <- c(min(all_Lat, na.rm = T)-buffer, max(all_Lat, na.rm = T)+buffer)
  lons <- c(min(all_Lon, na.rm = T)-buffer, max(all_Lon, na.rm = T)+buffer)

  # Download bathymetry data from NOAA
  bathy <- marmap::getNOAA.bathy(
    lon1 = lons[1], lon2 = lons[2], lat1 = lats[1], lat2 = lats[2],
    resolution = resolution, keep = T)

  # Convert to dataframe
  bathy <- marmap::fortify.bathy(bathy)
  return(bathy)
}
