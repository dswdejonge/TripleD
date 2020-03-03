#' Attributes for CSV input files TripleD database
#'
#' Data frame with information on all attributes that should or could be
#' included in the CSVs that act as input for the formation of the database.
#'
#' @name attributes
#' @format A dataframe with the following columns:
#' \describe{
#' \item{\code{Attribute}}{The name of the attribute as it should be used as column name in the CSV.}
#' \item{\code{Unit}}{The unit the attribute should have.}
#' \item{\code{Required_or_Optional}}{Whether or not the attribute is required.
#' If this is filled with 'Required_alternative' it means the information is required, but can be given in different ways.}
#' \item{\code{Group}}{Sometimes attributes belong together, these are grouped.}
#' \item{\code{Datatype}}{The datatype of the values (integers, fractions, doubles, strings, etc.)}
#' \item{\code{Description}}{Perhaps the most important column. This contains an eleborate
#' description of what the information of this attribute includes; how data should be documented.}
#' }
#' @details TripleD data is summarized per cruise in a CSVs that can be read by
#' the package in order to create a database. These CSVs either contain station
#' or species data. The format of these CSVs, thus, the names of all variables
#' and the exact meaning of the values in those columns, is explained here.
#' \cr
#' A dataframe exists for the station CSVs called 'stations_att' and for the species
#' CSVs called 'species_att'.
#' @export
NULL

#' @rdname attributes
"stations_att"

#' @rdname attributes
"species_att"
