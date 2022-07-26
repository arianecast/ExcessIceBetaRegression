# Function: icysedsmeancumulative
#
# This function takes average interval excess ice data and converts it to cumulative data (icy sediments)
#
# data: Data frame with two columns: (1) Depth (2) Average massive ice content
# range: List of depths (must match depths in the input dataset)
#
# Output: This function outputs a data frame with two columns: (1) Depth (2) Average cumulative excess ice content

icysedsmeancumulative <- function(data, range) {
  df <- data.frame(Depth = range, estimate = 0, boot.upr = 0, boot.corr.lwr = 0) #create dataframe with specified intervals and add the average column
  for(i in 1:length(range)){
    if(i == 1) {
      df$estimate[i] <- data$estimate[i]
      df$boot.upr[i] <- data$boot.upr[i]
      df$boot.corr.lwr[i] <- data$boot.corr.lwr[i]
    } else {
      df$estimate[i] <- data$estimate[i] + df$estimate[i-1]
      df$boot.corr.lwr[i] <- df$boot.corr.lwr[i-1] - data$boot.corr.lwr[i]
      df$boot.upr[i] <- data$boot.upr[i] + df$boot.upr[i-1]
    }
    #Change negative upper CIs to zero
    df[df<0] <- 0
  }
  return(df)
}