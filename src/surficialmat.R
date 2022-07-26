# Function: surficialmat
#
# This function takes a cryostratigraphic dataset and returns the surficial materials for each borehole
#
# data: A cryostratigraphic dataset with top and bottom values for each interval and associated material types
#
# Output: This function outputs a data frame with three columns: (1) Each unique material type in the cryostratigraphic dataset (2) The frequency
# of each material type as being at the surface (3) The percentage each material type represents as the surficial material in all boreholes

surficialmat <- function(data) {
  subset <- data[which(data$Top.depth == 0),]
  df <- as.data.frame(table(subset$Material))
  df$Perc <- df$Freq / sum(df$Freq) * 100
  return(df)
}