# Function: rectplot
#
# This function returns a plot visualizing the distribution of excess ground ice using the rect() function
#
# data: Cryostratigraphic dataset with top and bottom depth values for each interval and the predicted excess ice content
#
# Output: Drawn rectangle plot showing the distribution of excess ground ice

rectplot <- function(data) {
  nrow <- nrow(data)
  for(i in 1:nrow){
    if(data$fittedvalues_beta[i] == 0) {
      col <- "#765b41"
    } else if((data$fittedvalues_beta[i] > 0) && (data$fittedvalues_beta[i] <= 0.1)){
      col <- "#c8cbd3"
    } else if((data$fittedvalues_beta[i] > 0.1) && (data$fittedvalues_beta[i] <= 0.2)) {
      col <- "#b8c0c9"
    } else if((data$fittedvalues_beta[i] > 0.2) && (data$fittedvalues_beta[i] <= 0.3)) {
      col <- "#a1b2bc"
    } else if((data$fittedvalues_beta[i] > 0.3) && (data$fittedvalues_beta[i] <= 0.4)) {
      col <- "#86a1ab"
    } else if((data$fittedvalues_beta[i] > 0.4) && (data$fittedvalues_beta[i] <= 0.5)) {
      col <- "#688c98"
    } else if((data$fittedvalues_beta[i] > 0.5) && (data$fittedvalues_beta[i] <= 0.6)) {
      col <- "#4d7a87"
    } else if((data$fittedvalues_beta[i] > 0.6) && (data$fittedvalues_beta[i] <= 0.7)) {
      col <- "#326a76"
    } else if((data$fittedvalues_beta[i] > 0.7) && (data$fittedvalues_beta[i] <= 0.8)) {
      col <- "#1b5a69"
    } else if((data$fittedvalues_beta[i] > 0.8) && (data$fittedvalues_beta[i] <= 0.9)) {
      col <- "#0c515f"
    }
    rect((data$xorder[i]-1), -data$Top.depth[i], data$xorder[i], -data$Bottom.depth[i], angle=45, col=col, border=NA)
  }
}