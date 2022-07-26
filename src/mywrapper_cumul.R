# Function: mywrapper_cumul
#
# Wrapper for bootstrapping functions

mywrapper_cumul <- function(maxdepth) {
  library("boot")
  library("betareg")
  library("xts")
  cat("Max. depth:", maxdepth, "\n")
  boot::boot(statistic = estimate_depth_cumul, formula = fo, 
             group_by = "Site.ID",
             data = unique(data$Site.ID), 
             actual_data = data, 
             pred_data = pred_data, Depth = maxdepth, 
             from_Depth = 0.5, step_Depth = 0.1,
             R = Nrep, verbose = 0)
}