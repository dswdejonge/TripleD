
get_community_matrix <- function(
  database, taxonomic_level, abundance_measure, include_incomplete = TRUE,
  taxonomic_level_options = c("all","species","genus","family","order","class","phylum"),
  abundance_measure_options = c("presence_absence","Density_nr_per_m2","Biomass_g_per_m2","Density_nr_per_m3","Biomass_g_per_m3")
  ){
  # Check input
  if(!taxonomic_level %in% taxonomic_level_options){
    stop("Provide a valid taxonomic level.")
  }else if(!abundance_measure %in% abundance_measure_options){
    stop("Provide a valid abundance measure.")
  }

  ## taxon_colum ## ----------------
    # If all taxonomic levels are used, use the valid_name column
  if(taxonomic_level == "all"){
    taxon_column <- "valid_name"
    # If only species level, use valid_name column with entries with rank Species
  }else if(taxonomic_level == "species"){
    taxon_column <- "valid_name"
    database <- database %>%
      dplyr::filter(rank == "Species")
  }else{
    taxon_column <- taxonomic_level
  }

  ## value_column ##-------------
  # Add presence-absence
  if(abundance_measure == "presence_absence"){
    mysubset <- database %>%
      dplyr::mutate(presence_absence = 1)
  }

  ## mysubset ## ---------------
  if(!include_incomplete){
    if(abundance_measure == "Density_nr_per_m2" | abundance_measure == "Density_nr_per_m3"){
      mysubset <- database %>%
        dplyr::filter(incomplete_count == TRUE)
    }else if(abundance_measure == "Biomass_g_per_m2" | abundance_measure == "Biomass_g_per_m3"){
      mysubset <- database %>%
        dplyr::filter(incomplete_biomass == TRUE)
    }
  }else{
    mysubset <- database
  }

  mysubset <- mysubset %>%
    dplyr::select(StationID, !!dplyr::sym(taxon_column), !!dplyr::sym(abundance_measure)) %>%
    dplyr::group_by(StationID, !!dplyr::sym(taxon_column)) %>%
    dplyr::summarise(Value = sum(!!dplyr::sym(abundance_measure)))

  # Get community matrix
  community_matrix <- tidyr::pivot_wider(
    data = mysubset,
    id_cols = StationID,
    names_from = !!dplyr::sym(taxon_column),
    values_from = Value,
    values_fill = list(Value = 0)
  )
  #environmental_matrix
  return(community_matrix)
}
