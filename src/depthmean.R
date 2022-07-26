# Function: depthmean
#
# This function returns the mean excess ice predictions at each specified depth
#
# Depth: a list of depths (m) 
# data: cryostratigraphic dataset
# fit: a beta regression fit (output of the betareg function; list of betareg fit coefficients, residuals, fitted values, etc.)
# verbose: 0 (default) or 1. If set to 1, the function will print comments as it runs.
#
# Output: This function returns a list of mean predicted excess ice values for each specified depth

depthmean <- function(Depth, data, fit, verbose = 0) {
  # Extract relevant subset of the data set:
  data <- data[data$From..m. <= Depth & data$To..m. > Depth, ]
  # Make predictions:
  pred <- backtransform(predict(fit, newdata = data), n = nrow(data))
  meanpred <- mean(pred, na.rm = TRUE)
  if (verbose >= 1)
    cat("Used ", sum(!is.na(pred)), " predicted values to estimate mean EI at depth ", 
        Depth, ", ignored ", sum(is.na(pred)), " NA predictions.\n", sep = "")
  return(meanpred)
}