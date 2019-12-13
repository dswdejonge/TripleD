# ----------------------------------------
# Workflow to add missing data to database
# ----------------------------------------
# The functions for this workflow are documented in the package.
# The functions use external data sources and assumptions.
# If you would like to use other functions and assumptions to add missing data
# to the database, you can write and insert your own code here.

library(dplyr)
library(TripleD)

stations_additions <- stations %>%
  add_track_midpoints(.) %>%



