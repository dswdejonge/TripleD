context("Community Matrix func input and output")
library(TripleD)

#test_database <- dplyr::sample_n(database, 20)
load("database.rda")
load("expect_CM.rda")

test_that("errors are throwns", {
  expect_error(get_community_matrix(test_database, "species", "wrong"),
               "Provide a valid abundance measure.")
  expect_error(get_community_matrix(test_database, "wrong", "Biomass_g_per_m2"),
               "Provide a valid taxonomic level.")
})

test_that("the correct output is given", {
  expect_equal(get_community_matrix(test_database, "family", "Density_nr_per_m3"),
               expect1)
})
