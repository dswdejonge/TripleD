# TripleD
Scaffold script for the TripleD database of NIOZ.

NOTE: Code is not yet functional. In development stage.

This package contains all the code necessary to set up the TripleD database with time-series data collected by NIOZ. This package does NOT contain data; data can be requested from the [NIOZ Data Archiving System (DAS)](https://www.nioz.nl/en/expertise/north-sea-research-centre/nwa-north-sea-in-transition/das).

## Installation
```R
#install.packages("devtools")
devtools::install_github("dswdejonge/TripleD")
```

## Contructing the database
Set up your working directory as follows in order to start with the NIOZ TripleD database:  

1. Go to the NIOZ Data Archiving System (DAS) and request the formatted TripleD data files.  
2. In your working directory create a folder called 'inputfiles'. Within this folder you have to create two other folders called 'Species' and 'Stations'.   
3. Put the CSVs with all species data in the folder 'inputfiles/Species' and the CSVs with all station data in the foldr 'inputfiles/Stations' (do not put any files in these folders that are not csv data files formatted for the TripleD database).  
4. Add and run the following R-script to your working directory:

```R
# Load the library
library(TripleD)

# Loads all CSVs, checks format, and stores an R dataframe 
# in the newly created folder 'data'.
# Should not throw errors if the CSVs taken directly from DAS are used.
construct_database(in_folder = "inputfiles")

# Collects external data at NOAA and WoRMS.
collect_external_data()

# Add extra data to the intial database (taxonomy, water depths, etc.)
complete_database()

# Finalize database, by aggregating data, selecting relevant columns, and
# calculating final densities and biomass per sampling station.
finalize_database()

# Load the final database and view within Rstudio
load("database.rda")
View(database)
```

![size dimensions diagram](https://raw.githubusercontent.com/ddejonge/TripleD/master/inst/extdata/_morphologies.png)

