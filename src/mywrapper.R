# Function: mywrapper
#
# Wrapper for bootstrapping functions

mywrapper <- function(depth) {
  library("boot")
  library("betareg")
  library("xts")
  cat("Depth:", depth, "\n")
  boot::boot(statistic = estimate_depthmean2, formula = fo, 
             group_by = "Site.ID",
             data = unique(data$Site.ID), 
             actual_data = data, 
             pred_data = pred_data, Depth = depth,
             R = Nrep, verbose = 0)
}