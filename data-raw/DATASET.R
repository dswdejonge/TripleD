## code to prepare `DATASET` dataset goes here
att_stations <- read.csv(system.file("extdata", "attributes_stations.csv", package = "TripleD"))
att_species <- read.csv(system.file("extdata", "attributes_species.csv", package = "TripleD"))
att_bioconversion <- read.csv(system.file("extdata", "attributes_bioconversion.csv", package = "TripleD"))
att_database <- read.csv(system.file("extdata", "attributes_database.csv", package = "TripleD"))
att_database_individuals <- read.csv(system.file("extdata", "attributes_database_individuals.csv", package = "TripleD"))

usethis::use_data(att_stations, overwrite = T)
usethis::use_data(att_species, overwrite = T)
usethis::use_data(att_bioconversion, overwrite = T)
usethis::use_data(att_database, overwrite = T)
usethis::use_data(att_database_individuals, overwrite = T)
