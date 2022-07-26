# Function: VIthickness
#
# This function uses a cryostratigraphic dataset and returns the total thickness of materials recorded for each visible ice class.
#
# data: A cryostratigraphic dataset with top and bottom values for intervals and associated visible ice classes
#
# Output: This function outputs a data frame with three columns: (1) Unique visible ice classes (2) The total thickness associated with each
# visible ice class (3) The percentage each visible ice class represents in all boreholes

VIthickness <- function(data) {
  df <- data.frame(Visible.ice = unique(data$Visible.ice), Sum.thickness = 0, Perc.thickness = 0) # Set up data frame
  for(i in 1:nrow(df)) {
    vi <- toString(df$Visible.ice[i[1]])
    subset <- data[which(data$Visible.ice == vi),]
    sum <- sum(subset$Thickness)
    df$Sum.thickness[i] <- sum
  }
  df$Perc.thickness <- df$Sum.thickness / sum(df$Sum.thickness) * 100
  return(df)
}