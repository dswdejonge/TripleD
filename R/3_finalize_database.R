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
                              database_folder = NULL, as_CSV = TRUE){
  load(paste0(data_folder,"/species_additions.rda"))
  load(paste0(data_folder,"/stations_additions.rda"))

  ## Clean species table ## -----------
  # Set sample weights from NA to 0
  if(TRUE){
    species_final <- species_additions
    species_final[
      which(species_final$Weight_type_AFDW == "Sample" &
              is.na(species_final$AFDW_g)), "AFDW_g"] <- 0
    species_final[
      which(species_final$Weight_type == "Sample" &
              is.na(species_final$AFDW_g_from_WW)), "AFDW_g_from_WW"] <- 0

    species_final <- species_final %>%
      # Remove species with no match to WoRMS database (avoid removing NA)
      dplyr::filter(hasNoMatch != 1 | is.na(hasNoMatch)) %>%
      # Remove species that are not counted as organism (Count = -1) (avoid removing NA)
      dplyr::filter(Count != -1 | is.na(Count)) %>%
      # Upscale all values with the Fraction
      dplyr::mutate_at(
        dplyr::vars(Count, AFDW_g, AFDW_g_from_WW, AFDW_g_calc),
        function(x){x/.$Fraction}) %>%
      dplyr::select(-Fraction) %>%
      # Round Count to integer (e.g. 3 * 0.333 = 0.999 = 1)
      dplyr::mutate(Count = round(Count)) %>%
      # Find conflicting weight type fields between
      # Weight_type (WW and AFDW_from_WW) and Weight_type_AFDW (AFDW_g)
      dplyr::mutate(is_conflict = ifelse(
        Weight_type == "Sample" & !is.na(Weight_type_AFDW), TRUE, FALSE
      ))
    # Set all conflicting fields to NA, so they are skipped in combine_sources
    tempdf <- species_final %>%
      dplyr::filter(Weight_type == "Sample", is_conflict == TRUE) %>%
      dplyr::select(valid_name, Weight_type, is_conflict) %>%
      dplyr::distinct() %>%
      dplyr::rename(skip_WW_sample = is_conflict)
    species_final <- species_final %>%
      dplyr::left_join(., tempdf, by = c("valid_name", "Weight_type"))
    species_final$AFDW_g_from_WW[species_final$skip_WW_sample] <- NA
    species_final$Weight_type[species_final$skip_WW_sample] <- NA
    # If AFDW or WW is_Partial, use AFDW_calc unless it does not exist.
    species_final <- species_final %>%
      dplyr::mutate(
        skip_partial_WW = ifelse(is_Partial_WW & !is.na(AFDW_g_calc), TRUE, FALSE),
        skip_partial_AFDW = ifelse(is_Partial_AFDW & !is.na(AFDW_g_calc), TRUE, FALSE)
      )
    species_final$AFDW_g_from_WW[species_final$skip_partial_WW] <- NA
    species_final$AFDW_g[species_final$skip_partial_AFDW] <- NA
    # Combine AFDW columns
    species_final <- combine_data_sources(species_final,
                                          new_column_name = "AFDW_g_combined",
                                          order_of_preference = c("AFDW_g", "AFDW_g_from_WW", "AFDW_g_calc")
    ) %>%
      # Identify rows with unknown Count or Biomass
      dplyr::mutate(
        incomplete_count = ifelse(is.na(Count), 1, 0),
        incomplete_biomass = ifelse(is.na(AFDW_g_combined), 1, 0)
      ) %>%
      # Set Count = NA to Count = 1 (there was see at least 1)
      dplyr::mutate(
        Count = ifelse(is.na(Count), 1, Count)
      )
  }

  sp <- species_final %>%
    # Collapse to one count and biomass per station/species combi
    # Keep column with info if the count/biomass was complete or not
    dplyr::group_by(StationID, valid_name,
                    File, rank, phylum, class, order, family, genus) %>%
    dplyr::summarize(
      Count_total = sum(Count, na.rm = T),
      Biomass_g = sum(AFDW_g_combined, na.rm = T),
      isFuzzy = ifelse(sum(isFuzzy) > 0, TRUE, FALSE),
      incomplete_count = ifelse(sum(incomplete_count) > 0, TRUE, FALSE),
      incomplete_biomass = ifelse(sum(incomplete_biomass) > 0, TRUE, FALSE)
    ) %>%
    dplyr::ungroup()

  sp_sizes <- species_final %>%

  # Clean station database
  stations_final <- stations_additions %>%
    combine_data_sources(
      ., new_column_name = "Lat_DD", order_of_preference = c("Lat_DD_midpt", "Lat_DD_calc")
    ) %>%
    combine_data_sources(
      ., new_column_name = "Lon_DD", order_of_preference = c("Lon_DD_midpt", "Lon_DD_calc")
    ) %>%
    combine_data_sources(
      ., new_column_name = "Water_depth_m", order_of_preference = c("Water_depth_m_cruise", "Water_depth_m_Bathy", "Water_depth_m_Bathy2")
    ) %>%
    combine_data_sources(
      ., new_column_name = "Track_length_m", order_of_preference = c("Track_length_m_preset", "Track_dist_m_Odometer", "Track_dist_m_GPS")
    ) %>%
    combine_data_sources(
      ., new_column_name = "Bearings", order_of_preference = c("Bearing", "Bearing_calc")
    ) %>%
    dplyr::mutate(
      Sample_area_m2 = Track_length_m * (Blade_width_cm/100),
      Sample_volume_m3 = Sample_area_m2 * (Blade_depth_cm/100)) %>%
    dplyr::select(
      -Lat_DD_midpt, -Lon_DD_midpt,
      -Lat_start_DD, -Lon_start_DD,
      -Lat_stop_DD,	-Lon_stop_DD,
      -Lon_DD_calc,	-Lat_DD_calc,
      -Track_length_m_preset, -Water_depth_m_cruise, -Odometer_count,
      -Track_dist_m_GPS, -Track_dist_m_Odometer,
      -Water_depth_m_Bathy, -Water_depth_m_Bathy2,
      -Bearing, -Bearing_calc)

  # Create one large table with densities and biomasses for the Shiny app
  # Deselect File in stations because it's double.
  st <- dplyr::select(stations_final, -File)
  # Join data into big table
  database <- dplyr::inner_join(sp, st, by = "StationID") %>%
  # Calculate density and biomass per station
    dplyr::mutate(
      Density_nr_per_m2 = Count_total / Sample_area_m2,
      Density_nr_per_m3 = Count_total / Sample_volume_m3,
      Biomass_g_per_m2 = Biomass_g / Sample_area_m2,
      Biomass_g_per_m3 = Biomass_g / Sample_volume_m3) %>%
    dplyr::select(-Count_total, -Biomass_g)

  save(species_final, file = paste0(out_folder, "/species_final.rda"))
  save(stations_final, file = paste0(out_folder, "/stations_final.rda"))
  if(as_CSV){
    write.csv(species_final, paste0(out_folder,"/species_final.csv"))
    write.csv(stations_final, paste0(out_folder,"/stations_final.csv"))
  }
  if(is.null(database_folder)){
    save(database, file = "database.rda")
    if(as_CSV){write.csv(database, file = "database.csv")}
  }else{
    save(database, file = paste0(database_folder,"/database.rda"))
    if(as_CSV){write.csv(database, file = paste0(database_folder,"/database.csv"))}
  }
}


