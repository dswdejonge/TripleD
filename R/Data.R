#' TripleD stations
#'
#' Data frame (tibble) with information on all stations where samples with the
#' TripleD were collected.
#'
#' @name stations
#' @format A dataframe with the following columns:
#' \describe{
#' \item{\code{File}}{File name of the csv that contained the data entry.}
#' \item{\code{Station}}{A unique sampling ID. Usually the cruiseID combined with a number. Multiple sampling stations can occur at the same geographic location (i.e. have the same station name).}
#' \item{\code{Station_name}}{A generic name of the station that may contain some information on the location.}
#' \item{\code{Year}}{The year when the specific station was sampled.}
#' \item{\code{Month}}{The month when the specific station was sampled (1 to 12, Jan to Dec).}
#' \item{\code{Day}}{The day of the month when the specific station was sampled (1 to 31).}
#' \item{\code{Time}}{The time when the specific station was sampled.}
#' \item{\code{Lat}}{The average latitude (GPS coordinates) of the sampling station (in between the start and end position of the ship during the dredge) expressed in decimal degrees. Reference coordinate system is WGS84.}
#' \item{\code{Lon}}{The average longitude (GPS coordinates) of the sampling station (in between the start and end position of the ship during the dredge) expressed in decimal degrees. Reference coordinate system is WGS84.}
#' \item{\code{Distance}}{Distance dredged at this sampling location expressed in meters.}
#' \item{\code{Direction}}{Average bearing of the ship during the dredge, expressed in ??.}
#' \item{\code{Blade_width}}{The width of the used blade expressed in cm.}
#' \item{\code{Blade_depth}}{The depth of the used blade expressed in cm.}
#' }
#' @references \itemize{
#' \item{Rob Witbaard (NIOZ), NICO 10, 64PE438}
#' \item{Marc Lavaleye (NIOZ), Access database}
#' }
#' @details Researchers collected raw data during TripleD field studies about the
#' sampling stations. They are encouraged to upload this raw data to the NIOZ
#' data archiving system (DAS) so that other database users can retrace the origin
#' of the data. CSV files in the correct database format were created by
#' researchers based on their original raw data.
#' A description of how the csv files were created based on this raw data
#' can be found in the vignette "cleaning_data.Rmd". The CSV files' format
#' was checked with the code in "construct_database.R" and merged into the
#' dataframe (tibble format from the dplyr package) called "stations".
#' Therefore, this dataframe should only include original data and no data from
#' other sources or calculations based on assumptions.
NULL

#' @rdname stations
"stations"

#' @rdname stations
"stations_additions"


#' TripleD species data
#'
#' Data frame (tibble) with count and sometimes measurements of all sampled individuals.
#' Raw biological data.
#' Preferably each data entry (row) concerns an individual. However, often a small group
#' of individuals from the same species was measured and the given metrics concern the
#' group as a whole.
#'
#' @name species
#' @format A dataframe with the following columns:
#' \describe{
#' \item{\code{File}}{File name of the csv that contained the data entry.}
#' \item{\code{Station}}{A unique sampling ID. Usually the cruiseID combined with a number. Multiple sampling stations can occur at the same geographic location (i.e. have the same station name).}
#' \item{\code{Unit}}{}
#' \item{\code{Factor}}{}
#' \item{\code{Species}}{Scientific species name of the sampled individual.}
#' \item{\code{Common_name}}{Common name of the sampled individual.}
#' \item{\code{Count}}{How many individuals are included in this data entry? Should be 1 if the reported information is per individual.}
#' \item{\code{Size}}{The size of the individual reported by the unit reported in the column "Unit".}
#' \item{\code{WetWeight}}{The wet weight of the (group of) individuals reported in grams. If sizes are reported for individuals, but wet weight is only known for the collection of individuals per species, then report a wet weight of zero, except for one individual. In the analysis the total wet weight per species is calculated.}
#' }
#' @references \itemize{
#' \item{Rob Witbaard (NIOZ), NICO 10, 64PE438}
#' \item{Marc Lavaleye (NIOZ), Access database}
#' }
#' @details Researchers collected species data during TripleD field studies.
#' They are encourage to store this raw data in the NIOZ Data Archiving System
#' (DAS) so that other users of the database can retrace the origin of the data.
#' The raw data was used to construct CSV files in the right database format.
#' A description of this process should be provided by the contributers to the
#' database in the vignette "cleaning_data.Rmd". The CSV files' format was
#' checked with the code in "construct_database.R" and merged into a datframe
#' (tibble from the dplyr package) called "species". This dataframe contains
#' only original data, so no data from other sources or calculations based on
#' assumptions.
NULL

#' @rdname species
"species"

# rdname species
#"species_additions"



#' TripleD NOAA bathymetry
#'
#' Data frame (tibble) xyz bathymetry data collected from NOAA with the marmap
#' R-package based on the min and max coordinates in the database.
#'
#' @name bathymetry
#' @format Bathymetry data collected from NOAA with the marmap package.
#' Format is xyz data in tibble, x is longitude, y is latitude, z is altitude
#' (so negative values are depth).
#' @references \itemize{
#' marmap R-package
#' }
NULL

#' @rdname bathymetry
"bathymetry"


#' TripleD WoRMS taxonomy
#'
#' Data frame (tibble) with taxonomy data of the reported species in the TripleD dataset
#' taken from the World Register of Marine Species database (package worrms). Added columns are
#' 'isFuzzy' and 'hasNoMatch', both boolean variables.
#'
#' @name worms
#' @format Tibble with queried name, its valid name according to WoRMS, its taxonomic rank,
#' taxonomic classification, the match type (exact, nr of differences, phonetic, etc.), if the match
#' was found with fuzzy searching (isFuzzy) or wether there was no match at all (hasNoMatch), and
#' other information automatically generated by WoRMS.
#' @references \itemize{
#' worrms R-package
#' }
NULL

#' @rdname worms
"worms"

