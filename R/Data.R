#' TripleD sampling stations
#'
#' Data frame (tibble) with information on all stations where samples with the
#' TripleD were collected.
#'
#' @name sampling_stations
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
#' \item{Rob Witbaard, NICO 10, 64PE438}
#' }
#' @details Details here.
NULL

#' @rdname sampling_stations
"sampling_stations"



#' TripleD species data
#'
#' Data frame (tibble) with size and wet weight of all sampled individuals.
#' Raw biological data.
#' Preferably each data entry (row) concerns an individual. However, sometimes a small group
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
#' \item{Rob Witbaard, NICO 10, 64PE438}
#' }
#' @details Details here.
NULL

#' @rdname species
"species"



#' TripleD community data
#'
#' Data frame (tibble) with the number of individuals and total wet weight per species
#' per sampling station. Also in matrix form to use in multivariate statistics.
#'
#' @name community
#' @format A dataframe with the following columns:
#' \describe{
#' \item{\code{File}}{File name of the csv that contained the data entry.}
#' \item{\code{Station}}{A unique sampling ID. Usually the cruiseID combined with a number. Multiple sampling stations can occur at the same geographic location (i.e. have the same station name).}
#' \item{\code{Unit}}{}
#' \item{\code{Factor}}{}
#' \item{\code{Species}}{Scientific species name of the sampled individual.}
#' \item{\code{Common_name}}{Common name of the sampled individual.}
#' \item{\code{Count}}{The total count of individuals}
#' \item{\code{Size}}{The mean size of the species reported in the unit as in column "Unit".}
#' \item{\code{Size_sd}}{The standard deviation in size}
#' \item{\code{WetWeight}}{The total wet weight of the species reported in grams.}
#' }
#' @references \itemize{
#' \item{Rob Witbaard, NICO 10, 64PE438}
#' }
#' @details The data frame 'community' is in tidy format (all data in columns).
#' The data frames 'community_matrix_counts' and 'community_matrix_biomass' are in the
#' form of community matrices (sample stations as rows, species as columns), and can be used
#' in multivariate statistics.
NULL

#' @rdname community
"community"

#' @rdname community
"community_matrix_counts"

#' @rdname community
"community_matrix_biomass"
