# Function: maxdepth
#
# This function takes a cryostratigraphic dataset and returns the maximum depth for each unique borehole
#
# data: Cryostratigraphic dataset with top and bottom depth values for each interval
#
# Output: This function outputs the input dataframe with a "maxdepth" column indicating the maximum depth of the borehole

maxdepth <- function(data) {
  data <- data[order(data$Bottom.depth),] #Make sure the depths are in ascending order
  nrow <- nrow(data)
  maxdepth <- data$Bottom.depth[nrow]
  data$maxdepth <- maxdepth
  return(data)
}