# Split conversion data to conversion factors and regression formulas
split_into_factors_and_regression <- function(conversion_data, name_column = "valid_name"){
  conversion_factors <- conversion_data %>%
    dplyr::filter(!is.na(!!dplyr::sym(name_column))) %>%
    dplyr::select(!!dplyr::sym(name_column), WW_to_AFDW, Reference_WW_to_AFDW,
                  is_Shell_removed, Comment_WW_to_AFDW) %>%
    dplyr::filter(WW_to_AFDW > 0) %>%
    dplyr::distinct()

  regressions <- conversion_data %>%
    dplyr::filter(!is.na(!!dplyr::sym(name_column))) %>%
    dplyr::select(!!dplyr::sym(name_column), Size_dimension, A_factor, B_exponent, Output_unit,
                  Reference_regression, Comment_regression, is_Shell_removed) %>%
    dplyr::filter(!is.na(Output_unit)) %>%
    dplyr::distinct()

  return(list(
    conversion_factors = conversion_factors,
    regressions = regressions))
}

# Only one conversion factor is allowed for each combination of
# valid name and is_Shell_removed
get_double_factors <- function(conversion_factors, name_column = "valid_name"){
  df <- conversion_factors %>%
    dplyr::group_by(!!dplyr::sym(name_column), is_Shell_removed) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    dplyr::filter(Count > 1)
  return(df)
}

# Only one regression formula combination for each taxa,
# size_dimension, output_unit, and is_Shell_removed is allowed.
get_double_regressions <- function(regressions, name_column = "valid_name"){
  df <- regressions %>%
    dplyr::group_by(!!dplyr::sym(name_column), Size_dimension, is_Shell_removed) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    dplyr::filter(Count > 1)
  return(df)
}

#' Check bioconversion input
#'
#' This function checks the format of the bioconversion input file.
#' @return This function does not return an object.
#' @param conversion_data Dataframe with bioconversion data matching the given requirements from the
#' attributes_bioconversion file. If NULL (default) the bioconversion.csv will be searched for and
#' loaded from the input_folder.
#' @param input_folder The folder where to find the bioconversion.csv file. Default is 'inputfiles'.
#' @export
check_bioconversion_input <- function(conversion_data = NULL, input_folder = "inputfiles"){

  message("Loading and checking bioconversion data...")
  if(is.null(conversion_data)){
    conversion_data <- read.csv(paste0(input_folder, "/bioconversion.csv"),stringsAsFactors = F)
  }
  # Read attributes
  my_attributes <- read.csv(system.file("extdata", "attributes_bioconversion.csv", package = "TripleD"))

  # Check presence required attributes
  required_attributes <- dplyr::filter(my_attributes, Required_or_Optional == "Required")
  att_is_missing <- !required_attributes$Attribute %in% colnames(conversion_data)
  if(TRUE %in% (att_is_missing)){
    stop(paste0("The required attribute(s) ", paste(required_attributes$Attribute[att_is_missing], collapse = ", "),
                " is/are missing in bioconversion.csv."))
  }

  # No NA values are allowed in the required columns.
  columns <- colnames(conversion_data) %in% required_attributes$Attribute
  subset <- conversion_data[,columns, drop = F]
  if(TRUE %in% is.na(subset)){
    rowi <- unique(which(is.na(subset), arr.ind = T)[,"row"]+1)
    stop(paste0("NA values are not allowed for the required attributes ",
                paste(required_attributes$Attribute, collapse = ", "),
                ". Please check row(s): ",paste(sort(unique(rowi)), collapse = ", ")))
  }

  # Are groups complete
  my_groups <- my_attributes %>%
    dplyr::filter(!is.na(Group)) %>%
    dplyr::group_by(Group) %>%
    dplyr::group_split()
  for(my_group in my_groups){
    is_group_included <- colnames(conversion_data) %in% my_group$Attribute
    if(TRUE %in% is_group_included &&
       length(which(is_group_included)) != length(my_group$Attribute)){
      stop(paste0("The attributes ",
                  paste(my_group$Attribute, collapse = ", "),
                  " are needed but only ",
                  paste(colnames(conversion_data)[is_group_included], collapse = ", ")," are given."))
    }
  }

  #TODO: no NA values in grouped vars if a value is given.

  # Check doubles
  doubles_attributes <- dplyr::filter(my_attributes, Datatype == "Double")
  columns <- which(colnames(conversion_data) %in% doubles_attributes$Attribute)
  if(length(columns) > 0){
    for(j in 1:length(columns)){
      if(!is.double(conversion_data[,columns[j]])){
        stop(paste0("The column ", colnames(conversion_data)[columns[j]],
                    " must contain only values of datatype ",doubles_attributes$Datatype[1]))
      }
    }
  }

  # Check integers
  integer_attributes <- dplyr::filter(my_attributes, Datatype == "Integer")
  columns <- which(colnames(conversion_data) %in% integer_attributes$Attribute)
  if(length(columns) > 0){
    for(j in 1:length(columns)){
      if(!is.double(conversion_data[,columns[j]])){
        stop(paste0("The column ", colnames(conversion_data)[columns[j]],
                    " must contain only values of datatype ",integer_attributes$Datatype[1]))
      }
    }
  }

  # Check fractions
  fraction_attributes <- dplyr::filter(my_attributes, Unit == "Fraction")
  if(length(fraction_attributes$Attribute) > 0){
    columns <- which(colnames(conversion_data) %in% fraction_attributes$Attribute)
    subset <- conversion_data[,columns, drop = F]
    is_not_fraction <- which(subset < 0 | subset > 1, arr.ind = T)[,"row"]+1
    if(length(is_not_fraction) > 0){
      stop(paste0("The attributes ", paste(colnames(conversion_data)[columns], collapse = ", "),
                  " should lie between 0 and 1. Please check row(s) ",
                  paste(sort(is_not_fraction), collapse = ", ")))
    }
  }

  # Check booleans
  boolean_attributes <- dplyr::filter(my_attributes, Datatype == "Boolean")
  if(length(boolean_attributes$Attribute) > 0){
    columns <- which(colnames(conversion_data) %in% boolean_attributes$Attribute)
    subset <- conversion_data[,columns, drop = F]
    if(dim(subset)[2] != 0){
      #is_not_boolean <- which(!is.na(subset) & subset != 0 & subset != 1, arr.ind = T)[,"row"]+1
      is_not_boolean <- which(is.na(subset), arr.ind = T)[,"row"]+1
      if(length(is_not_boolean) > 0){
        stop(paste0("The boolean attributes in the bioconversion file", paste(colnames(conversion_data)[columns], collapse = ", "),
                    " may only be 0 or 1. Please check row(s) ",
                    paste(sort(is_not_boolean), collapse = ", ")))
      }
    }
  }

  # Check predefined units
  remove_str <- "Predefined: "
  split_at <- ", "
  units <- my_attributes$Unit %>%
    .[grep(remove_str, .)] %>%
    gsub(remove_str, "", .) %>%
    strsplit(., split_at)
  names(units) <- my_attributes$Attribute[grep(remove_str, my_attributes$Unit)]
  for(i in 1:length(units)){
    att <- names(units)[i]
    if(!att %in% colnames(conversion_data)){
      next
    }else{
      subset <- conversion_data[,att]
      is_not_predefined <- which(!subset %in% units[[i]] & !is.na(subset))+1
      if(length(is_not_predefined) > 0){
        stop(paste0("In column ",att,
                    " values exist that are not the predefined values ",
                    paste(units[[i]], collapse = ", "),
                    " or NA in row(s): ",paste(sort(is_not_predefined), collapse = ", ")
        ))
      }
    }
  }

  wa <- conversion_data$WW_to_AFDW
  if(!is.null(wa)){
    wa <- wa  < 0
    if(TRUE %in% wa){
      stop(paste0("In bioconversion file column WW_to_AFDW the values in row(s) ",
                  paste(sort(which(wa)+1), collapse = ", "),
                  " are negative and should be positive."))
    }
  }

  # Check for no double entries
  conversion_list <- split_into_factors_and_regression(conversion_data, name_column = "Taxon")
  check_factors <- get_double_factors(conversion_list$conversion_factors, name_column = "Taxon")
  are_double <- which(check_factors$Count > 1)
  if(length(are_double) > 0){
    print(check_factors, n=Inf)
    stop(paste0("Multiple conversion factors WW_to_AFDW are present for the above species."))
  }
  check_regressions <- get_double_regressions(conversion_list$regressions, name_column = "Taxon")
  are_double <- which(check_regressions$Count > 1)
  if(length(are_double) > 0){
    print(check_regressions, n=Inf)
    stop(paste0("Multiple regressions are present for the above species."))
  }
  message("All good!")
}

# Input list with conversion_factors df and regressions df
calculate_mean_conversion <- function(conversion_data){
  means_list <- list()
  tlevels <- c("phylum", "class", "order", "family", "genus")
  for(i in 1:length(tlevels)){
    # get taxonomic level
    tlevel <- tlevels[i]

    # get mean regressions
    subdf_r <- conversion_data %>%
      dplyr::group_by(!!dplyr::sym(tlevel), Size_dimension, Size_unit,
                      Output_unit) %>%
      dplyr::summarise(A_factor = mean(A_factor, na.rm = T),
                       B_exponent = mean(B_exponent, na.rm = T)) %>%
      dplyr::filter(!is.na(!!dplyr::sym(tlevel)),
                    !is.na(Size_dimension))
    isdouble <- subdf_r %>%
      dplyr::group_by(!!dplyr::sym(tlevel), Size_dimension) %>%
      dplyr::summarize(count = dplyr::n())
    subdf_r <- subdf_r %>%
      dplyr::left_join(., isdouble) %>%
      dplyr::filter(!(count > 1 & Output_unit == "WW_g")) %>%
      dplyr::select(-count)

    # get mean conversion factors
    subdf_c <- conversion_data %>%
      dplyr::group_by(!!dplyr::sym(tlevel), is_Shell_removed) %>%
      dplyr::summarise(WW_to_AFDW = mean(WW_to_AFDW, na.rm = T)) %>%
      dplyr::filter(!is.na(!!dplyr::sym(tlevel)),
                    !is.na(WW_to_AFDW))
    # Merge data
    subdf <- dplyr::full_join(subdf_c, subdf_r) %>%
      dplyr::rename(valid_name = !!dplyr::sym(tlevel)) %>%
      dplyr::mutate(Comment_WW_to_AFDW = "Automatic calculated mean.",
                    Comment_regression = "Automatic calculated mean.",
                    Taxon = "Ignore") %>%
      dplyr::filter(!is.na(is_Shell_removed))

    means_list[[1]] <- subdf
  }

  # Bind rows to original conversion_data
  means_df <- dplyr::bind_rows(means_list)
  result <- dplyr::bind_rows(conversion_data, means_df)
  return(result)
}

#' Prepare the bioconversion file
#'
#' This wrapper function adds valid taxon names to the bioconversion file and also calculates
#' mean conversion values for all higher taxonomic levels.
#' @return This function does not return an object, but stores a dataframe in the specified
#' \code{out_folder} under the name 'conversion_data.rda'.
#' @param conversion_data Dataframe with bioconversion data matching the given requirements from the
#' attributes_bioconversion file. If NULL (default) the bioconversion.csv will be searched for and
#' loaded from the input_folder.
#' @param worms Dataframe with query and valid names from worms. If NULL (default), the file
#' "worms.rda" is searched for in the data_folder.
#' @param input_folder The folder where to find the bioconversion.csv file. Default is 'inputfiles'.
#' @param out_folder The external data is stored in this folder. Default is 'data'.
#' @param as_CSV If you also want to store the collected external data as CSV, set to TRUE. Default is FALSE.
#' @export
prepare_bioconversion <- function(conversion_data = NULL, worms = NULL,
                                  input_folder = "inputfiles", data_folder = "data", out_folder = "data",
                                  as_CSV = FALSE){
  message("Loading bioconversion data...")
  if(is.null(conversion_data)){
    conversion_data <- read.csv(paste0(input_folder, "/bioconversion.csv"),stringsAsFactors = F)
  }
  message("Loading WoRMS taxonomic data...")
  if(is.null(worms)){
    load(paste0(data_folder,"/worms.rda"))
  }

  message("Adding WoRMS valid names to conversion data...")
  conversion_data <- dplyr::left_join(conversion_data, dplyr::select(
    worms, Query, valid_name, isFuzzy,
    phylum, class, order, family, genus),
    by = c("Taxon" = "Query")) %>%
    dplyr::distinct()
  no_match_i <- which(is.na(conversion_data$valid_name))
  if(length(no_match_i) > 0){
    message(paste0("These taxa names from the bioconversion.csv file cannot be matched to the WoRMS database:",
                   paste0(unique(conversion_data$Query[no_match_i]), collapse = ", ")))
  }

  message("Calculating conversion and regression means for all taxonomic levels...")
  conversion_data <- calculate_mean_conversion(conversion_data) %>%
    dplyr::mutate(
      is_Shell_removed = ifelse(is_Shell_removed == 1, TRUE, FALSE)
    )
  conversion_list <- split_into_factors_and_regression(conversion_data)
  conversion_factors <- conversion_list$conversion_factors
  regressions <- conversion_list$regressions

  # Remove automic calculated double factors
  check_factors <- get_double_factors(conversion_factors)
  conversion_factors <- dplyr::left_join(conversion_factors, check_factors) %>%
    dplyr::mutate(Count = ifelse(is.na(Count), 1, Count),
                  Comment_WW_to_AFDW = ifelse(is.na(Comment_WW_to_AFDW), "NA", Comment_WW_to_AFDW)) %>%
    dplyr::filter(!(Count == 2 & Comment_WW_to_AFDW == "Automatic calculated mean.")) %>%
    dplyr::select(-Count)
  check_factors <- get_double_factors(conversion_factors)
  are_double <- which(check_factors$Count > 1)
  if(length(are_double) > 0){
    print(check_conv_f, n=Inf)
    stop(paste0("Automatic calculated means introduced doubles."))
  }

  # Remove automic calculated double regressions
  check_regressions <- get_double_regressions(regressions)
  regressions <- dplyr::left_join(regressions, check_regressions) %>%
    dplyr::mutate(Count = ifelse(is.na(Count), 1, Count),
                  Comment_regression = ifelse(is.na(Comment_regression), "NA", Comment_regression)) %>%
    dplyr::filter(!(Count == 2 & Comment_regression == "Automatic calculated mean.")) %>%
    dplyr::select(-Count)
  check_regressions <- get_double_regressions(regressions)
  are_double <- which(check_regressions$Count > 1)
  if(length(are_double) > 0){
    print(check_regressions, n=Inf)
    stop(paste0("Automatic calculated means introduced doubles."))
  }

  save(conversion_factors, regressions, file = paste0(out_folder,"/conversion_data.rda"))
  message(paste0("Prepared bioconversion file saved as objects conversion_factors and regressions in the file "
                 ,out_folder,"/conversion_data.rda"))
  if(as_CSV){
    write.csv(conversion_factors, file = paste0(out_folder,"/conversion_factors.csv"))
    write.csv(regressions, file = paste0(out_folder,"/regressions.csv"))
  }
}





