#' Calculate weight from length
#'
#' This function calculates Ash Free Dry Weight (g) from length (mm) using the power function
#' weight = A * length^B.
#' with the coefficients taken from regression lines.
#' @param length Length of organism in mm.
#' @param A The A factor in the power function A * length^B.
#' @param B The B exponent in the power function A * length^B.
#' @return Returns the weight as Ash Free Dry Weight (AFDW) in grams (g).
#' @export
length_to_weight <- function(length, A, B){
  weight = A * length^B
  return(weight)
}
