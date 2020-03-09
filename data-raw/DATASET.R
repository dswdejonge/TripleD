## code to prepare `DATASET` dataset goes here
stations_att <- read.csv(system.file("extdata", "attributes_stations.csv", package = "TripleD"))
species_att <- read.csv(system.file("extdata", "attributes_species.csv", package = "TripleD"))

usethis::use_data(stations_att)
usethis::use_data(species_att)