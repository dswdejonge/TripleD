#' Finalize the database
#'
#' This function selects only relevant data and builds one large final data frame.
#' @details
#' This workflow describes which data from species_additions and
#' stations_additions are selected to form a final, clean, database
#' that can be used in the Shiny app and in ecological analyses.
#' @section Specimens:
#' Specimen entries are removed if:
#' \itemize{
#' \item{Its reported name has no match to the WoRMS database}
#' \item{Its count is "-1", meaning it the entry concerns fragments or material like eggs.}
#' }
#' Specimen counts are scaled-up according to the reported fraction.
#' \cr
#' Only the original filename, the StationID, the scale count, valid name, taxonomy (phylum, class, order
#' family, and genus), and whether or not the name was matched fuzzy against WoRMS is stored.
#' The latter information (isFuzzy) may be important to track if there was perhaps a wrongly assigned new
#' valid name if there are suspicious results.
#' @section Stations:
#' A column with final latitude and longitude are created, by first taking all originally reported midpoints,
#' and filling any empty data points with the calculated midpoint based on track start and stop coordinates.
#' \cr
#' A column with final water depth is created, by first taking the water depths as measured during the cruise,
#' and filling any empty data points with the water depth from bathymetry.
#' \cr
#' A column with final track length is created, by first taking the reported track length from the cruise,
#' and filling any empty data points with track length calculated from the odometer.
#' \cr
#' Sampled track area in meter squared and sampled track volume in cubic meters is calculated with the finalized
#' track length and reported blade width and depth.
#' \cr
#' The final dataframe contains the original file names, vessel names, CruiseIDs, StationIDs, station names,
#' sampling dates, start and stop times, blade depth and width, midpoint coordinates and the source of these
#' coordinates (reported or measured), water depth and the source of the depth (reported or bathymetry),
#' track lengths and the source of the length (reported or calculated), and the track area and volume.
#' @return This function does not return an object, but stores a finalized stations data frame in the out_folder,
#' a finalized species data frame in the out_folder, and a final database with stations and species data
#' combined in the database_folder.
#' @param data_folder This is the folder that contains the additions database.
#' Default is 'data', as created by the \code{construct_database} function.
#' @param out_folder This is the folder where you want your finalized species and stations database,
#' to be stored. Default is 'data'.
#' @param database_folder This is the folder where you want your final database (combined stations and species
#' data) to be stores. Default is NULL, which will store the database in your working directory.
#' @export
finalize_database <- function(data_folder = "data", out_folder = "data",
                              database_folder = NULL, as_CSV = FALSE){
  load(paste0(data_folder,"/species_additions.rda"))
  load(paste0(data_folder,"/stations_additions.rda"))
  # Clean species database
  species_final <- species_additions %>%
    # Remove species with no match to WoRMS database
    .[-which(.$hasNoMatch == 1),] %>%
    # Remove species that are not counted as organism (Count = -1)
    dplyr::filter(Count != -1) %>%
    # Upscale count based on fraction
    dplyr::mutate(Count_scaled = Count / Fraction) %>%
    # Only select revelant columns
    ### !!!! Later add in columns for biomass !!!
    dplyr::select(
      File, StationID,
      Count_scaled,
      valid_name, rank, phylum, class, order, family, genus, isFuzzy
    )

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
      ., new_column_name = "Track_length_m", order_of_preference = c("Track_length_m_cruise", "Track_dist_m_Odometer")
    ) %>%
    dplyr::mutate(Sample_area_m2 = Track_length_m * (Blade_width_cm/100)) %>%
    dplyr::mutate(Sample_volume_m3 = Sample_area_m2 * (Blade_depth_cm/100)) %>%
    dplyr::select(
      File, Vessel, CruiseID, StationID, Station_name,
      Date, Time_start, Time_stop,
      Blade_depth_cm, Blade_width_cm,
      Lat_DD, source_Lat_DD, Lon_DD, source_Lon_DD,
      Water_depth_m, source_Water_depth_m, Track_length_m, source_Track_length_m,
      Sample_area_m2, Sample_volume_m3
    )

  # Create one large table for the Shiny app
  # Deselect File in stations because it's double.
  st <- dplyr::select(stations_final, -File)
  # Count all species per station
  sp <- species_final %>%
    dplyr::group_by(StationID, valid_name, rank, phylum, class, order, family, genus, isFuzzy) %>%
    dplyr::summarise(Count = sum(Count_scaled, na.rm = T))
  # Join data into big table
  database <- dplyr::inner_join(sp, st, by = "StationID") %>%
    # Calculate density per site
    dplyr::mutate(Density_nr_per_m2 = Count / Sample_area_m2) %>%
    dplyr::mutate(Density_nr_per_m3 = Count / Sample_volume_m3)

  save(species_final, file = paste0(out_folder, "/species_final.rda"))
  save(stations_final, file = paste0(out_folder, "/stations_final.rda"))
  if(as_CSV){
    write.csv(species_final, "species_final.csv")
    write.csv(stations_final, "stations_final.csv")
  }
  if(is.null(database_folder)){
    save(database, file = "database.rda")
    if(as_CSV){write.csv(database, file = "database.csv")}
  }else{
    save(database, file = paste0(database_folder,"/database.rda"))
    if(as_CSV){write.csv(database, file = paste0(database_folder,"/database.csv"))}
  }
}


