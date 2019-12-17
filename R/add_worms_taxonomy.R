#' Add WoRMs taxonomy
#'
#' These functions are used to check the reported species names against the WoRMs datbase.
#' @references worrms R-package
#' @param species Tibble with reported species names.
#' @param species_names Only the species names.
#' @param fuzzy Uses the TAXAMATCH algorithm to match species names.
#' @details Searches in batches of 50.
#' @export
collect_from_worms <- function(species_names, fuzzy = FALSE){
  worms <- list()
  if(fuzzy){
    func <- worrms::wm_records_taxamatch
  }else{
    func <- worrms::wm_records_names
  }
  i1 <- 1 ; i2 <- 50
  while(i1 < length(species_names)){
    if(i2 > length(species_names)){i2 <- length(species_names)}
    collected_data <- func(species_names[i1:i2])
    worms <- c(worms, collected_data)
    i1 <- i1 + 50
    i2 <- i2 + 50
  }
  names(worms) <- species_names
  return(worms)
}


#' @describeIn collect_from_worms
get_worms_taxonomy <- function(species){
  # Get unique species names
  reported_species <- unique(species$Species_reported)

  # Collect exact matches
  worms <- collect_from_worms(reported_species)

  # Find index of complete matches and empty lists
  complete_matches <- which(lapply(worms,length) > 0)
  fuzzy_matches <- which(lapply(worms,length) == 0)

  # Collect fuzzy matches
  new_records <- collect_from_worms(names(fuzzy_matches), fuzzy = T)

  # Add fuzzy matches to worms list
  worms[fuzzy_matches] <- new_records

  # Find index of reported species without matches or with mulitple matches
  still_no_matches <- which(lapply(worms,length) == 0)
  multiple_matches <- which(lapply(worms, nrow) > 1)

  # Create an empty record for species without a (good) match
  empty_record <- worms[[complete_matches[1]]]
  empty_record[1,] <- NA
  empty_record <- mutate(empty_record, hasNoMatch = 1)

  # If a reported species has multiple matches, only the accepted match is used
  # If there are multiple accepted names, it is assumed a no match and gets an empty record.
  for(i in 1:length(multiple_matches)){
    record <- worms[[multiple_matches[i]]] %>%
      filter(status == "accepted")
    if(nrow(record) > 1){
      record <- empty_record
    }
    worms[[multiple_matches[i]]] <- record
  }

  # Reported species that cannot be fuzzy matches gets an empty record.
  for(i in 1:length(still_no_matches)){
    worms[[still_no_matches[i]]] <- empty_record
  }

  # Merge into tibble and save
  worms_df <- bind_rows(worms, .id = "Query") %>%
    add_column(isFuzzy = NA)
  worms_df[fuzzy_matches,"isFuzzy"] <- 1

  return(worms_df)
}
