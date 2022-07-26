# Function estimate_coefficients
#
# Wrapper function for betareg -> coef to be used for bootstrap estimation
# using the 'boot' package. This function supports regular as well as group-level
# bootstrap resampling. The latter is used to resample observations at the borehole
# level. The function can also be called directly (without going through 'boot')
# to estimate the coefficients on the entire sample, without resampling.
#
# data:   Depends on the type of bootstrap used:
#         (a) Ordinary bootstrap: data.frame with observations (response and 
#             predictors) from which the training data for betareg is sampled.
#         (b) Group-level bootstrap: Vector of type 'factor' of group IDs
# x:      Integer vector received from the boot function, or NULL if the entire
#         data set is used (no bootstrap resampling). (a) Ordinary bootstrap:
#         a row index for the data.frame 'data'. (b) Group-level resampling:
#         an index vector identifying the groups (i.e. boreholes)
# actual_data: Use default ('data') for ordinary bootstrapping.
#         For group-level resampling, a data.frame with observations (response 
#         and predictors) from which the bootstrap sample (identified by the 
#         identifiers in 'x') is drawn.
# group_by: In group-level resampling, the name of the variable in 'data' that
#         is used as a group identifier (in our case, "SiteID"); ignored when
#         using ordinary bootstrap.
# sample_id: Name of the variable identifying individual observations. Only used
#         for reporting purposes.
# names:  Complete set of coefficient names. Needed because in some bootstrap
#         repetitions, a coefficient may be missing because not all factor levels
#         of predictor variable may be present.
# return_model: if FALSE (default), return coefficients; if TRUE, return fitted model
# verbose: Determines the amount of information being reported.
# 
# Inspired by: https://stackoverflow.com/questions/49970248/how-to-bootstrap-predictions-and-levels-of-confidence-for-beta-regression-model)

estimate_coefficients <- function(data, x = NULL, formula, 
                                  group_by = NULL, sample_id = "SampleID",
                                  actual_data = data,
                                  names = COEF_NAMES, 
                                  return_model = FALSE,
                                  verbose = 1) {
  require("betareg")
  if (is.null(x)) {
    ### No resampling, use all data:
    if (verbose >= 2)
      cat("Regular estimation, no bootstrap:\n   ", length(unique(actual_data[,sample_id])), 
          " unique observations in sample of size ", nrow(actual_data), 
          "\n", sep = "")
  } else {
    if (is.null(group_by)) {
      ### Ordinary bootstrap resampling:
      # regular bootstrap, x contains integers:
      actual_data <- actual_data[x, ]
      if (verbose >= 2)
        cat("Regular bootstrap resampling:\n   ", length(unique(actual_data[,sample_id])), 
            " unique observations (from ", length(unique(actual_data[,group_by])), " sites) in bootstrap sample of size ", nrow(actual_data), 
            "\n", sep = "")
    } else {
      ### Bootstrap resampling at the group level:
      # select subset using a grouping variable, e.g. Site.ID;
      data <- data[x] # the selected Site.IDs!
      which <- unlist(lapply(data, function(x) which(actual_data[,group_by] == x)))
      actual_data <- actual_data[which, ]
      if (verbose >= 2)
        cat("Bootstrap resampling at the site level:\n   ", length(unique(actual_data[,sample_id])), 
            " unique observations from ", length(unique(actual_data[,group_by])), " sites in bootstrap sample of size ", 
            nrow(actual_data), "\n", sep = "")
      
      ### All factor variables in the formula must have 
      ### at least two levels!!!
      ### --> check this for Material.simplified and Deposit.type,
      ### and report any issues:
      
      if (length(unique(actual_data$Material.simplified)) < 2) {
        formula <- update(formula, ~ . - Material.simplified)
        if (verbose >= 1)
          cat("Removing predictor Material.simplified: only one factor level present in bootstrap sample.\n")
      }
      if (length(unique(actual_data$Deposit.type)) < 2) {
        formula <- update(formula, ~ . - Deposit.type)
        if (verbose >= 1)
          cat("Removing predictor Deposit.type: only one factor level present in bootstrap sample.\n")
      }
      # Deposit.type.conf is numeric, but it has only up to three unique values,
      # so better check this too:
      if (length(unique(actual_data$Deposit.type.conf)) < 2) {
        formula <- update(formula, ~ . - Deposit.type.conf)
        if (verbose >= 1)
          cat("Removing predictor Deposit.type.conf: only one unique value present in bootstrap sample.\n")
      }
    }
  }
  
  cf <- NULL
  res <- try(fit <- betareg::betareg(formula, data = actual_data))
  
  if (return_model) {
    if (inherits(res, "try-error")) {
      return(NULL)
    } else {
      return(res)
    }
  }
  
  if (!is.null(names)) {
    cf <- rep(NA, length(names))
    names(cf) <- names
    if (!inherits(res, "try-error")) {
      cf[names(cf)] <- coef(fit)[names(cf)]
    }
  } else cf <- coef(fit)
  return(cf)
}