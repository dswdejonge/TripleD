# Construct and update table with station data
# --------------------------------------------


areRequiredStationAttributesPresent <- function(input, req, alt) {
  all(req %in% input)
}
#stations <- dplyr::bind_rows(data, .id = "File")
#usethis::use_data(sampling_stations, overwrite = T)
