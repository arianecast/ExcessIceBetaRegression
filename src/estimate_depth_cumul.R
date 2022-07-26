# Function: estimate_depth_cumul
#
# This function returns the estimated cumulative excess ice content for each specified depth.
#
# data: Cryostratigraphic dataset (needs the columns "From..m." and "To..m." to define the stratigraphic interval of each observation, as well as
# all the variables needed to apply the fitted model)
# x: Integer vector received from the boot function, or NULL if the entire data set is used (no bootstrap resampling).
# (a) Ordinary bootstrap: a row index for the data.frame 'data'. (b) Group-level resampling: an index vector identifying the groups (i.e. boreholes)
# Depth: List of depth values
# from_Depth: Specify the starting depth value
# step_Depth: Specify the interval between each depth value
# verbose: verbose: 0 (default) or 1. If set to 1, the function will print comments as it runs.
#
# Output: This function returns a list of estimated cumulative excess ice content for each specified depth.

estimate_depth_cumul <- function(data, x = NULL, Depth, from_Depth = 0.5, step_Depth = 0.1, 
                                 pred_data = data, verbose = 1, ...) {
  the.depthmean <- function(Depth, data, fit) {
    # Extract relevant subset of the data set:
    data <- data[data$From..m. <= Depth & data$To..m. > Depth, ]
    # Make predictions:
    pred <- backtransform(predict(fit, newdata = data), n = nrow(data))
    meanpred <- mean(pred, na.rm = TRUE)
    return(meanpred)
  }
  depth.cumul <- function(depths, data, fit, verbose = 0) {
    pred <- sapply(depths, the.depthmean, data = data, fit = fit)
    sumpred <- sum(pred, na.rm = TRUE)
    if (verbose >= 1)
      cat("Used ", sum(!is.na(pred)), " predicted values to estimate mean EI up to depth ", 
          max(depths), ", ignored ", sum(is.na(pred)), " NA predictions.\n", sep = "")
    if (any(is.na(pred))) return(NA)
    return(sumpred)
  }
  fit <- estimate_coefficients(data = data, x = x, return_model = TRUE, ...)
  if (is.null(fit)) return(NA)
  depths <- seq(from_Depth + step_Depth / 2, Depth - step_Depth / 2, by = step_Depth)
  res <- try(depth.cumul(depths = depths, data = pred_data, fit = fit, verbose = verbose))
  if (inherits(res, "try-error")) return(NA)
  res <- step_Depth * res
  return(res)
}