# Construct and update table with station data
# --------------------------------------------
import_stations_data <- function() {
  folder_name <- "Stations"
  files <- list.files(folder_name)
  data <- lapply(paste0(folder_name,"/",files), read.csv)
  names(data) <- files
  return(data)
}

areRequiredStationAttributesPresent <- function(input, req, alt) {
  all(req %in% input)
}
#stations <- dplyr::bind_rows(data, .id = "File")
#usethis::use_data(sampling_stations, overwrite = T)
