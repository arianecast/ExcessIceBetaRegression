estimate_depthmean2 <- function(data, x = NULL, Depth, pred_data = data, verbose = 1, ...) {
  the.depthmean <- function(Depth, data, fit, verbose = 0) {
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
  fit <- estimate_coefficients(data = data, x = x, return_model = TRUE, ...)
  if (is.null(fit)) return(NA)
  res <- try(the.depthmean(Depth = Depth, data = pred_data, fit = fit, verbose = verbose))
  if (inherits(res, "try-error")) return(NA)
  return(res)
}