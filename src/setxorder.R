# Function: setxorder
#
# This function uses the output of the maxdepth function to set the order in which boreholes should be plotted with the rectplot function
# (from shallowest to deepest; see fig 4)
#
# data: Cryostratigraphic dataset with top and bottom values for intervals and a column for the maximum depth of each borehole
#
# Output: This function outputs the input data frame with an additional column for the order

setxorder <- function(data){
  data$xorder <- 0 #Create new order column
  nrow <- nrow(data)
  for(i in 1:nrow){
    if(i==1){
      data$xorder[i] <- 1
    } else {
      if(data$Borehole.ID[i] == data$Borehole.ID[i-1]){
        data$xorder[i] <- data$xorder[i-1]
      } else {
        data$xorder[i] <- data$xorder[i-1] + 1
      }
    }
  }
  return(data)
}