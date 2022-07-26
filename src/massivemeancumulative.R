# Function: massivemeancumulative
#
# This function takes average interval excess ice data and converts it to cumulative data
#
# data: Data frame with two columns: (1) Depth (2) Average massive ice content
# range: List of depths (must match depths in the input dataset)
#
# Output: This function outputs a data frame with two columns: (1) Depth (2) Average cumulative massive ice content

massivemeancumulative <- function(data, range) {
  df <- data.frame(Depth = range, 
                   Massive.ice = 0, 
                   conf.upr.massive = 0, 
                   conf.lwr.massive = 0) #create dataframe with specified intervals and add the average column
  for(i in 1:length(range)){
    if(i == 1) {
      df$Massive.ice[i] <- data$Average.massive.ice[i]
      df$conf.lwr.massive[i] <- data$conf.upr.massive[i]
      df$conf.upr.massive[i] <- data$conf.lwr.massive[i]
    } else {
      df$Massive.ice[i] <- data$Average.massive.ice[i] + df$Massive.ice[i-1]
      df$conf.lwr.massive[i] <- df$conf.lwr.massive[i-1] - data$conf.upr.massive[i]
      df$conf.upr.massive[i] <- data$conf.lwr.massive[i] + df$conf.upr.massive[i-1]
    }
    #Change negative upper CIs to zero
    df[df<0] <- 0
  }
  return(df)
}