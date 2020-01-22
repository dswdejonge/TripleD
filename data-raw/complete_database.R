# ----------------------------------------
# Workflow to add missing data to database
# ----------------------------------------
# The functions for this workflow are documented in the package.
# The functions use external data sources and assumptions.
# If you would like to use other functions and assumptions to add missing data
# to the database, you should write and insert your own code.


# ----------#
# Libraries #
# ----------#
# These libraries should be installed to use the functions below.
# They should all be installed automatically when you installed the TripleD package.
#library(TripleD)
#library(dplyr)
#library(tibble)
#library(geosphere)
#library(marmap)


# ---------------#
# Secondary data #
# ---------------#
# Collect bathymetry and store in package
bathymetry <- collect_bathymetry(stations)
usethis::use_data(bathymetry, overwrite = T)

# Collect taxonomy of species from WoRMs
worms <- get_worms_taxonomy(species$Species_reported)
usethis::use_data(worms, overwrite = T)

# Open csv file that contains conversion data. Columns:
# Species: species name for which the conversion data is valid.
# WW_to_AFDW: fraction of wet weight (WW) that should be taken to obtain Ash Free Dry Weight (AFDW).
# Length_to_Width: fraction of length that should be taken to obtain width.
# A_factor: The factor A in the power function AFDW = A * Length^B as regression of AFDW vs. Length.
# B_exponent: The exponent in the power function AFDW = A * Length^B as regression of AFDW vs. Length.
conversion_data <- read.csv(
  system.file("extdata", "bioconversion_data.csv", package = "TripleD"),
  stringsAsFactors = F)
# Check names in the bioconversion data file
worms_conversion <- get_worms_taxonomy(conversion_data$Species)
conversion_data <- left_join(conversion_data, select(worms_conversion, Query, valid_name),
                             by = c("Species" = "Query"))

# --------------------------------#
# Stations and environmental data #
# --------------------------------#
# Add additional data to stations
stations_additions <- stations %>%
  add_track_midpoints() %>%
  add_track_length_GPS() %>%
  add_track_length_Odometer() %>%
  add_water_depth(bathymetry = bathymetry)
usethis::use_data(stations_additions, overwrite = T)


# ----------------------------#
# Species and biological data #
# ----------------------------#
# Add additional data to species data
species_additions <- species %>%
  # Add taxonomic data
  left_join(., select(worms,
                      Query, valid_name, rank, phylum, class, order,
                      family, genus, hasNoMatch, isFuzzy),
            by = c("Species_reported" = "Query")) #%>%
  # Attach conversion data
  #left_join(conversion_data, by = "valid_name") %>%
  # Convert length to mm from other units
  #mutate(Length_mm =
  #         ifelse(Unit_Length == "0.5cm", Length*5+5,
  #         ifelse(Unit_Length == "cm", Length*10+10, Length))) %>%
  # Convert width to mm from other units
  #mutate(Width_mm =
  #         ifelse(Unit_Width == "0.5cm", Width*5+5,
  #         ifelse(Unit_Width == "cm", Width*10+10, Width))) %>%
  # Convert width in mm to length in mm
  #mutate(Length_from_Width_mm = Width_mm/Length_to_Width) %>%
  # Calculate AFDW from length and from length derived from width
  #mutate(AFDW_g_calc_L = length_to_weight(Length_mm, A = A_factor, B = B_exponent)) %>%
  #mutate(AFDW_g_calc_W = length_to_weight(Length_from_Width_mm, A = A_factor, B = B_exponent)) %>%
  # If wet weight is reported as 0, use the scale threshold as wet weight.
  #mutate(WetWeight_g_threshold = ifelse(WetWeight_g == 0, Threshold_Scale, WetWeight_g)) %>%
  # Calculate AFDW from wet weight
  #mutate(AFDW_g_calc_WW = WetWeight_g_threshold * WW_to_AFDW)
usethis::use_data(species_additions, overwrite = T)
