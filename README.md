# TripleD
R-package to read and transform TripleD sample data to produce a database with presence-absence, density, and biomass data for benthic megafauna. The TripleD is a special quantitative sampling dredge produced and used by the NIOZ Royal Netherlands Institute for Sea Research.  

This package contains all the code necessary to set up the TripleD database with time-series data collected by NIOZ. This package does *not* contain data; data can be requested from the [NIOZ Data Archiving System (DAS)](https://www.nioz.nl/en/expertise/north-sea-research-centre/nwa-north-sea-in-transition/das).  

The database output of the package can be visually interacted with using a developed [Shiny app](https://www.github.com/dswdejonge/TripleD-app).

## Installation
You can install the `TripleD` R-package by running the following command (beware you need to have the R-package `devtools` installed):
```R
#install.packages("devtools")
devtools::install_github("dswdejonge/TripleD")
```

## Contructing the database
Set up your working directory as follows in order to start with the NIOZ TripleD database:  

1. Go to the NIOZ Data Archiving System (DAS) and request the formatted TripleD data CSV files.  
2. In your working directory create a folder called 'inputfiles'. Within this folder you have to create two other folders called 'Species' and 'Stations'.   
3. Put the CSVs with all species data in the folder 'inputfiles/Species' and the CSVs with all station data in the folder 'inputfiles/Stations' (do not put any files in these folders that are not csv data files formatted for the TripleD database).  
4. Add and run the following R-script to your working directory:

```R
# Load the library
library(TripleD)

# Loads all CSVs, checks format, and stores an R dataframe 
# in the newly created folder 'data'.
# Should not throw errors if the CSVs taken directly from DAS are used.
construct_database(in_folder = "inputfiles")

# Add extra data to the intial database (taxonomy, water depths, etc.)
collect_from_NOAA() # currently not working - cannot reach NOAA server
check_species_WORMS()
check_bioconversion_WORMS()

# Add taxonomy, add depth from bathymetry, calculate track lengths,
calculate AFDW.
complete_database()

# Finalize database, by aggregating data, selecting relevant columns, and
# calculating final densities and biomass per sampling station.
finalize_database()

# Load the final database and view within Rstudio
load("database.rda")
View(database)
```

## Cheat sheet
If you feel lost with the workflow and all the files, use this cheat sheet:
![cheat sheet](https://raw.githubusercontent.com/dswdejonge/TripleD/master/inst/extdata/cheatsheet.png)  


## Size dimension
A quick reference and reminder of which size dimensions are measured for different morphological groups. The names in the diagram correspond exactly with the names in the species CSVs and bioconversion CSV that can be requested from the NIOZ Data Archiving System.  
![size dimensions diagram](https://raw.githubusercontent.com/dswdejonge/TripleD/master/inst/extdata/_morphologies.png)

