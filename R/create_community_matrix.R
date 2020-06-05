#' Get community matrix from the TripleD database
#'
#' This functions lets you extract a community matrix
#' (i.e. stations in rows, taxa in columns) from the TripleD databas.
#' @param database A dataframe, database output from the TripleD package.
#' @param taxonomic_level The taxonomic level for which you want to create
#' a community matrix. Options: \itemize{
#' \item{all}
#' \item{species (default)}
#' \item{genus}
#' \item{family}
#' \item{order}
#' \item{class}
#' \item{phylum}
#' }
#' @param abundance_measure The value in the cells. Options: \describe{
#' \item{presence_absence}{only 1 (present) or 0 (absence)}
#' \item{Density_nr_per_m2}{individual count per square meter}
#' \item{Biomass_g_per_m2"}{ash-free dry weight in grams per square meter}
#' \item{Density_nr_per_m3}{individual count per cubic meter}
#' \item{Biomass_g_per_m3}{ash-free dry weight in grams per cubic meter}
#' }
#' @param include_incomplete Include values based on incomplete data
#' i.e. underestimations? (Default TRUE)
#' @return Returns a dataframe with StationID as first row.
#' @details Missing data filled with 0.
#' @examples
#' get_community_matrix(database, "species", "Biomass_g_per_m2")
#' get_community_matrix(database, "phylum", "Density_nr_per_m2", include_incomplete = FALSE)
#' @export
get_community_matrix <- function(database, taxonomic_level, abundance_measure,
                                 include_incomplete = TRUE){
  # Check input
  if(!taxonomic_level %in%
     c("all","species","genus","family","order","class","phylum")){
    stop("Provide a valid taxonomic level.")
  }else if(!abundance_measure %in%
           c("presence_absence","Density_nr_per_m2","Biomass_g_per_m2","Density_nr_per_m3","Biomass_g_per_m3")){
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
        dplyr::filter(incomplete_count == !include_incomplete)
    }else if(abundance_measure == "Biomass_g_per_m2" | abundance_measure == "Biomass_g_per_m3"){
      mysubset <- database %>%
        dplyr::filter(incomplete_biomass == !include_incomplete)
    }
  }else{
    mysubset <- database
  }

  mysubset <- mysubset %>%
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
  return(community_matrix)
}
