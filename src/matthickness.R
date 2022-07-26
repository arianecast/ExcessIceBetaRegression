# Function: matthickness
#
# This function take cryostratigraphic interval data and returns the sum of thickness of intervals for each material type
#
# data: Cryostratigraphic dataset of depth intervals with associated thickness and material type of the interval.
#
# Output: This function outputs a data frame with each unique material type, the sum of thickness of each material type,
# and the overall percentage each material type represents

matthickness <- function(data) {
  df <- data.frame(Material = unique(data$Material), Sum.thickness = 0, Perc.thickness = 0) # Set up data frame
  for(i in 1:nrow(df)) {
    mat <- toString(df$Material[i[1]])
    subset <- data[which(data$Material == mat),]
    sum <- sum(subset$Thickness)
    df$Sum.thickness[i] <- sum
  }
  df$Perc.thickness <- df$Sum.thickness / sum(df$Sum.thickness) * 100
  return(df)
}