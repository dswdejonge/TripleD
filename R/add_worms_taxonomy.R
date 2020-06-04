#' Connect to WoRMS API
#'
#' This function collects WoRMS entries from their database using the reported taxa in batches of 50,
#' using either the regular function or fuzzy function (taxamatch).
#' @references worrms R-package
#' @param species_names Character vector with taxa names.
#' @param fuzzy Uses the TAXAMATCH algorithm to fuzzy match species names.
#' @details Searches in batches of 50.
#' @export
connect_to_worms <- function(species_names, fuzzy = FALSE){
  worms <- list()
  if(fuzzy){
    func <- worrms::wm_records_taxamatch
  }else{
    func <- worrms::wm_records_names
  }
  i1 <- 1 ; i2 <- 50
  while(i1 <= length(species_names)){
    if(i2 > length(species_names)){i2 <- length(species_names)}
    collected_data <- func(species_names[i1:i2])
    worms <- c(worms, collected_data)
    i1 <- i1 + 50
    i2 <- i2 + 50
  }
  names(worms) <- species_names
  return(worms)
}


#' Get dataframe with valid taxa names
#'
#' This function checks all reported taxa against the WoRMS databse, and
#' returns a dataframe with currently accepted valid species names.
#' @references worrms R-package
#' @param species_names Character vector with taxa names.
#' @details This function collects WoRMS entries based on the given species names, and reports the valid name
#' and rank that is found. It first finds all direct matches, and then does a fuzzy match for queries that do
#' not have a direct match against the database.
#' Only extant species are considered, and if multiple matches are returned, either
#' 1) use the only accepted name,
#' 2) if all are unaccepted, use the only valid name, or
#' 3) use an empty record if there are multiple valid names returned.
#' @return Returns a dataframe.
#' @export
get_valid_names <- function(species_names){
  # Get unique species names
  reported_species <- unique(species_names)

  # Collect exact matches
  worms <- connect_to_worms(reported_species)

  # Find index of complete matches and empty lists
  complete_matches <- which(lapply(worms,length) > 0)
  fuzzy_matches <- which(lapply(worms,length) == 0)

  if(length(fuzzy_matches) > 0){
    # Collect fuzzy matches
    new_records <- connect_to_worms(names(fuzzy_matches), fuzzy = T)

    # Add fuzzy matches to worms list
    worms[fuzzy_matches] <- new_records
  }

  # Find index of reported species without matches or with mulitple matches
  still_no_matches <- which(lapply(worms,length) == 0)
  multiple_matches <- which(lapply(worms, nrow) > 1)

  # Create an empty record for species without a (good) match
  empty_record <- worms[[complete_matches[1]]]
  empty_record[1,] <- NA
  empty_record <- dplyr::mutate(empty_record, hasNoMatch = 1)

  # If a reported species has multiple matches, only the accepted match is used
  # If there are multiple accepted names, it is assumed a no match and gets an empty record.
  if(length(multiple_matches) > 0){
    for(i in 1:length(multiple_matches)){
      # Extract record and discard extinct species
      record <- worms[[multiple_matches[i]]] %>%
        dplyr::filter(is.na(isExtinct) | isExtinct != 1)
      # Only use accepted name if there is only 1
      accepted <- dplyr::filter(record, status == "accepted")
      if(nrow(accepted) == 1){
        worms[[multiple_matches[i]]] <- accepted
      # otherwise, take valid name if they are all the same
      }else if(length(unique(record$valid_name)) == 1){
        worms[[multiple_matches[i]]] <- record[1,]
      # if there are no accepted names, and multiple valid names, use empty record
      }else{
        worms[[multiple_matches[i]]] <- empty_record
      }
    }
  }

  # Reported species that cannot be fuzzy matches gets an empty record.
  if(length(still_no_matches) > 0){
    for(i in 1:length(still_no_matches)){
      worms[[still_no_matches[i]]] <- empty_record
    }
  }

  # Merge into tibble and return
  worms_df <- dplyr::bind_rows(worms, .id = "Query") %>%
    dplyr::mutate(isFuzzy = 0)
  worms_df[fuzzy_matches,"isFuzzy"] <- 1
  if(is.null(worms_df$hasNoMatch)){
    worms_df$hasNoMatch <- 0
  }else{
    worms_df$hasNoMatch[which(worms_df$hasNoMatch != 1)] <- 0
  }

  return(worms_df)
}

#' Collect species names from WoRMS
#'
#' This wrapper function collects accepted species names from WoRMS for all reported species in the
#' data files and the bioconversion file.
#' @details
#' Taxonomic data is collected from the WoRMS database using the \code{worrms} R-package.
#' You need internet connection to do this.
#' All reported taxonomic names of the specimens in the initial database and in the bioconversion file
#' are matched against the WoRMS database (also fuzzy matches, i.e. where typos and phonetic spelling is allowed).
#' @return This function does not return an object, but stores the information in the specified
#' \code{out_folder} under the name 'worms.rda'.
#' @param species The initial database for species with reported specimen names.
#' If NULL (default) the function will automatically search the data_folder for 'species_initial.rda'.
#' @param conversion_data Dataframe with bioconversion data matching the given requirements from the
#' attributes_bioconversion file. If NULL (default) the bioconversion.csv will be searched for and
#' loaded from the input_folder.
#' @param data_folder If the species database is not provided, the function will search
#' for it (species_initial.rda') in this folder. Default is 'data'.
#' @param input_folder The folder where to find the bioconversion.csv file. Default is 'inputfiles'.
#' @param out_folder The external data is stored in this folder. Default is 'data'.
#' @param as_CSV If you also want to store the collected external data as CSV, set to TRUE. Default is FALSE.
#' @export
collect_species_WORMS <- function(species = NULL, conversion_data = NULL,
                                  input_folder = "inputfiles", data_folder = "data", out_folder = "data",
                                  as_CSV = FALSE){
  if(is.null(species)){
    message("Loading intitial database with species")
    load(paste0(data_folder,"/species_initial.rda"))
  }
  if(is.null(conversion_data)){
    conversion_data <- read.csv(paste0(input_folder, "/bioconversion.csv"),stringsAsFactors = F)
  }

  all_taxa_names <- unique(as.character(c(conversion_data$Taxon, species$Species_reported)))
  message("Collecting taxonomy from the WoRMS database. This can take a while...")
  worms <- get_valid_names(all_taxa_names)
  save(worms, file = paste0(out_folder,"/worms.rda"))
  message(paste0("WoRMS taxonomic information is stored as ",out_folder,"/worms.rda."))
  if(as_CSV){
    write.csv(worms, file = paste0(out_folder,"/worms_taxonomy.csv"))
  }
}
