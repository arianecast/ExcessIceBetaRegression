# Function: bhdepths (borehole depths)
#
# This function returns the frequency of borehole depths (in percentage) from a cryostratigraphic borehole dataset.
#
# data: Cryostratigraphic dataset, requires the column "Bottom.depth" to calculate the frequency of boreholes shallower than each depth.
#
# Output: Returns a dataframe of the frequency (in percentage) of boreholes within 1 to 10 meters

bhdepths <- function(data) {
  df <- data.frame(Depths = 1:10, Freq = 0, Perc = 0)
  for(i in 1:10) {
    subset <- data[which(data$Bottom.depth >= i),]
    bh <- unique(subset$Borehole.ID)
    df$Freq[i] <- length(bh)
  }
  df$Perc <- df$Freq / length(unique(data$Borehole.ID)) * 100
  return(df)
}