# TripleD
TripleD database NIOZ

## Installation
```R
require(devtools)
install_github("dswdejonge/TripleD")
```

## Contructing the database
In your working directory you will need two initial things to start with the NIOZ TripleD database:  
1. The formatted CSV files in a folder as outlined below.  
2. An R-script as outlined below.  

### CSV files
1. Go to the NIOZ Data Archiving System (DAS) and request the formatted TripleD data files.  
2. In your working directory create a folder which we will call the 'in_folder'. For example named 'inputfiles'. Within this 'in_folder' you have to create two other folders called 'Species' and 'Stations'.  
3. Put the CSVs with all species data in the folder 'inputfiles/Species' and the CSVs with all station data in the foldr 'inputfiles/Stations'.

## R-script
Put the following R-script in your working directory:
```R
# Load the library
library(TripleD)

# Loads all CSVs, checks format, and stores an R dataframe 
# in the newly created folder 'data'.
# Should not throw errors if the CSVs taken directly from DAS are used.
construct_database(in_file = <name of correct folder>)
```
