# Code showing how the data frame with sampling stations is constructed
library(tidyr)
folder_name <- "files"
files <- list.files(folder_name)
data <- lapply(paste0(folder_name,"/",files), read.csv)
names(data) <- files
species <- dplyr::bind_rows(data, .id = "File")

write.csv(species, file = "species.csv")
usethis::use_data(species, overwrite = T)

