# entry is length in mm
# value is weight Ash Free Dry Weight in g
size_to_weight <- function(Length, A, B){
  W = A * Length^B
  return(W)
}
