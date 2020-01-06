# ----------------------------------------
# Workflow to add missing data to database
# ----------------------------------------
# The functions for this workflow are documented in the package.
# The functions use external data sources and assumptions.
# If you would like to use other functions and assumptions to add missing data
# to the database, you can write and insert your own code here.

library(dplyr)
library(TripleD)

# Collect bathymetry and store in package
bathymetry <- collect_bathymetry(stations)
usethis::use_data(bathymetry, overwrite = T)

# Collect taxonomy of species from WoRMs
worms <- get_worms_taxonomy(species$Species_reported)
usethis::use_data(worms, overwrite = T)

# Add additional data to stations
stations_additions <- stations %>%
  add_track_midpoints() %>%
  add_track_length_GPS() %>%
  add_track_length_Odometer() %>%
  add_water_depth(bathymetry = bathymetry)
usethis::use_data(stations_additions, overwrite = T)

# Add additional data to stations
species_additions <- species %>%
  left_join(., select(worms,
                      Query, valid_name, rank, phylum, class, order,
                      family, genus, hasNoMatch, isFuzzy),
            by = c("Species_reported" = "Query"))

# Species size to biomass
#test_input <- read.csv(system.file("extdata", "test_input.csv", package = "TripleD"))
conversion_data <- read.csv(system.file("extdata", "test_length_to_weight.csv", package = "TripleD"))

#result <- test_input %>%
#  left_join(conversion_data, by = "Species") %>%
#  mutate(Length_mm = ifelse(Unit_Length == "0.5cm", Length*5+5, Length)) %>%
#  mutate(AFDW_g_calc = size_to_weight(Length_mm, A = A_factor, B = B_exponent)) %>%
#  mutate(calc_diff = abs(AFDW_g - AFDW_g_calc)/AFDW_g*100)

result <- species_additions %>%
  left_join(conversion_data, by = "Species")

plot(result$AFDW_g ~ result$AFDW_g_calc)



