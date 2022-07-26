# Function: boot_extract
#
# This function compiles the bootstrap results and model predictions into a list of data frames
#
# x: List of data frames (one data frame of bootstrapping results per depth; output from the parallelization of the bootstrapping in section 3 (all_pred))
#
# Output: Outputs a list of data frames (one data frame per depth) of extracted boot statistics (lower and upper CIs, mean boot and the fraction of NAs)

boot_extract <- function(x) {
  c(
    boot.lwr = boot.ci(boot.out = x, type = boot_method)[[boot_method]][4],
    boot.mean = mean(x$t, na.rm = TRUE),
    boot.upr = boot.ci(boot.out = x, type = boot_method)[[boot_method]][5],
    boot.NA.fraction = mean(is.na(x$t))
  )
}