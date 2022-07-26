# Function: intdepthmeanboot
#
# Function to return the average predicted excess ice content and confidence intervals for a range of depths from cryostratigraphic data
#
# data: cryostratigraphic data frame with top and bottom depths for each interval and predicted excess ice content as well as upper and lower confidence intervals
# range: range of depths for which to derive average predicted values
#
# Output: The function outputs a data frame with depths (specified in "range"), the average value of predicted excess ice content, and upper and lower confidence intervals.

intdepthmeanboot <- function(data, range) {
  df <- data.frame(Intervals = range, Average.icy.seds = 0, CI.upr = 0, CI.lwr = 0) #create dataframe with specified intervals and add the average column
  for(i in 1:length(range)){
    subset1 <- data[which(data$Top.depth < range[i] & data$Bottom.depth > range[i]), ]
    subset2 <- data[which(data$Top.depth == range[i]), ]
    subset <- rbind(subset1, subset2)
    mean <- mean(subset$fittedvalues_beta)
    df$Average.icy.seds[i] <- mean
    df$CI.upr[i] <- mean(subset$fittedvalues_beta_uprCI_reg)
    df$CI.lwr[i] <- mean(subset$fittedvalues_beta_lwrCI_reg)
  }
  return(df)
}