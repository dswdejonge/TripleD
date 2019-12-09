# Import data from folder
import_data <- function(folder) {
  files <- list.files(folder)
  data <- lapply(paste0(folder,"/",files), read.csv)
  names(data) <- files
  return(data)
}

# Get required attributes
get_required_att <- function(my_attributes) {
  req_att <- my_attributes %>%
    filter(Required_or_Optional == "Required")
  return(req_att)
}

# Get alternative attributes
get_alternative_att <- function(my_attributes) {
  alt_att <- my_attributes %>%
    filter(Required_or_Optional == "Required_alternative") %>%
    group_by(Group) %>%
    group_split
  return(alt_att)
}

# Get attributes with datatype double
get_doubles_att <- function(my_attributes) {
  doubles_attributes <- my_attributes %>%
    filter(Datatype == "Double")
  return(doubles_attributes)
}

# Get attributes with datatype integer
get_int_att <- function(my_attributes){
  int_att <- my_attributes %>%
    filter(Datatype == "Integer")
  return(int_att)
}

