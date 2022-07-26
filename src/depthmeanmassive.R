# Function: depthmeanmassive
#
# This function calculates the mean of massive ice content as well as the upper and lower confidence intervals in the mean for each specified
# depth interval.
#
# data: Cryostratigraphic dataset (including massive ground ice intervals). For each massive ice interval there must be an EI85 column which
# assumes that the excess ice content of massive ice is 85%. 
# range: List of depth intervals
#
# Output: Outputs a data frame with the columns Depth, average.massive.ice, conf.upr.massive, and conf.lwr.massive

depthmeanmassive <- function(data, range) {
  df <- data.frame(Depth = range, Average.massive.ice = 0, conf.upr.massive = 0, conf.lwr.massive = 0) #create dataframe with specified intervals and add the average column
  for(i in 1:length(range)){
    subset1 <- data[which(data$Top.depth < range[i] & data$Bottom.depth > range[i]), ]
    subset2 <- data[which(data$Top.depth == range[i]), ]
    subset <- rbind(subset1, subset2)
    mean85 <- mean(subset$EI85)
    ttest <- t.test(subset$EI85)
    df$conf.upr.massive[i] <- ttest$conf.int[1]
    df$conf.lwr.massive[i] <- ttest$conf.int[2]
    df$Average.massive.ice[i] <- ttest$estimate
  }
  return(df)
}