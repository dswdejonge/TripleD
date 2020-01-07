# entry is length in mm
# value is weight Ash Free Dry Weight in g
length_to_weight <- function(length, A, B){
  weight = A * length^B
  return(weight)
}
