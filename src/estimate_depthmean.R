estimate_depthmean <- function(data, x = NULL, Depth, pred_data = data, verbose = 1, ...) {
  fit <- estimate_coefficients(data = data, x = x, return_model = TRUE, ...)
  if (is.null(fit)) return(NA)
  res <- try(depthmean(Depth = Depth, data = pred_data, fit = fit, verbose = verbose))
  if (inherits(res, "try-error")) return(NA)
  return(res)
}