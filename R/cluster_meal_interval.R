#' Determine optimal interval from gaps directly
#'
#' Wrapper function that calculates optimal eps from pre-calculated gaps.
#' This is used internally when gaps have already been calculated properly by animal.
#'
#' @param gaps Numeric vector of inter-visit gaps in minutes
#' @param method Character string specifying the method to use
#' @param percentile Numeric value between 0 and 1 for percentile method
#' @param lower_bound Numeric value for lower bound of the optimal interval
#' @param upper_bound Numeric value for upper bound of the optimal interval
#'
#' @return Optimal eps value in minutes
#' @keywords internal
#' @noRd
optimal_interval_from_gaps <- function(gaps, method = "both", percentile = 0.75, lower_bound = 5, upper_bound = 60) {
  
  if (length(gaps) == 0) {
    warning("There is only 1 row of data in the provided dataframe, no gaps between visits available for analysis, returning default interval of 30 minutes")
    return(30)
  }
  
  # Validate method parameter
  valid_methods <- c("both", "percentile", "gmm")
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
  }
  
  # Validate percentile parameter
  if (!is.numeric(percentile) || length(percentile) != 1 || percentile <= 0 || percentile >= 1) {
    stop("percentile must be a single numeric value between 0 and 1")
  }
  
  # Choose method(s) to use
  if (method == "percentile") {
    optimal_eps <- determine_eps_percentile(gaps, percentile)
  } else if (method == "gmm") {
    optimal_eps <- fit_gmm_to_gaps(gaps, percentile)
  } else { # method == "both"
    percentile_eps <- determine_eps_percentile(gaps, percentile)
    gmm_eps <- fit_gmm_to_gaps(gaps, percentile)
    # Take the minimum of the two methods to be conservative
    optimal_eps <- min(percentile_eps, gmm_eps, na.rm = TRUE)
  }
  
  # Apply reasonable bounds
  if (!is.null(lower_bound) && !is.null(upper_bound)) {
    optimal_eps <- max(lower_bound, min(optimal_eps, upper_bound)) # Between lower_bound and upper_bound
  } else {
    optimal_eps <- optimal_eps
  }
  
  return(as.numeric(optimal_eps))
}

#' Determine eps using percentile-based method
#'
#' @param gaps Numeric vector of inter-visit gaps
#' @param percentile Numeric value between 0 and 1 specifying which percentile to use
#'
#' @return Eps value based on specified percentile of gaps
#' @keywords internal
#' @noRd
determine_eps_percentile <- function(gaps, percentile = 0.75) {
  
  # Use specified percentile of gaps
  percentile_eps <- stats::quantile(gaps, percentile, na.rm = TRUE)
  return(as.numeric(percentile_eps))
}

#' Fit Gaussian mixture model to gaps
#'
#' Fits a 2-component Gaussian mixture model to inter-visit gaps to determine
#' optimal eps for clustering. Falls back to percentile method if GMM fails.
#'
#' @param gaps Numeric vector of inter-visit gaps
#' @param percentile_fallback Numeric value between 0 and 1 for percentile fallback
#'
#' @return Optimal eps value based on GMM intersection or percentile fallback
#' @keywords internal
#' @noRd
fit_gmm_to_gaps <- function(gaps, percentile_fallback = 0.75) {
  if (length(gaps) < 10) {
    # Not enough data for GMM, fall back to percentile
    return(stats::quantile(gaps, percentile_fallback, na.rm = TRUE))
  }
  
  # Try to fit 2-component Gaussian mixture model
  tryCatch({
    # Fit 2-component normal mixture (suppress all output)
    captured_output <- utils::capture.output({
      mix_fit <- mixtools::normalmixEM(gaps, k = 2, verb = FALSE, 
                                      maxit = 1000, epsilon = 1e-08)
    }, type = "output")
    
    # Extract parameters for the two components
    mu1 <- mix_fit$mu[1]
    mu2 <- mix_fit$mu[2]
    sigma1 <- mix_fit$sigma[1]
    sigma2 <- mix_fit$sigma[2]
    lambda1 <- mix_fit$lambda[1]
    lambda2 <- mix_fit$lambda[2]
    
    # Ensure component 1 is the within-meal (smaller mean) distribution
    if (mu1 > mu2) {
      # Swap components
      temp_mu <- mu1; mu1 <- mu2; mu2 <- temp_mu
      temp_sigma <- sigma1; sigma1 <- sigma2; sigma2 <- temp_sigma
      temp_lambda <- lambda1; lambda1 <- lambda2; lambda2 <- temp_lambda
    }
    
    # Find intersection point between the two distributions
    intersection_eps <- find_distribution_intersection(mu1, sigma1, lambda1, 
                                                      mu2, sigma2, lambda2)
    
    return(intersection_eps)
  }, error = function(e) {
    # If GMM fails, fall back to percentile method
    return(stats::quantile(gaps, percentile_fallback, na.rm = TRUE))
  })
}

#' Find intersection point between two normal distributions in a mixture
#'
#' @param mu1 Mean of first distribution (within-meal)
#' @param sigma1 Standard deviation of first distribution
#' @param lambda1 Mixing proportion of first distribution
#' @param mu2 Mean of second distribution (between-meal)
#' @param sigma2 Standard deviation of second distribution  
#' @param lambda2 Mixing proportion of second distribution
#'
#' @return Intersection point between the two weighted distributions
#' @keywords internal
#' @noRd
find_distribution_intersection <- function(mu1, sigma1, lambda1, mu2, sigma2, lambda2) {
  
  # Define the difference function between weighted densities
  diff_func <- function(x) {
    density1 <- lambda1 * stats::dnorm(x, mean = mu1, sd = sigma1)
    density2 <- lambda2 * stats::dnorm(x, mean = mu2, sd = sigma2)
    return(density1 - density2)
  }
  
  # Find intersection point between the distributions
  # Search in the range between the two means
  search_range <- c(mu1, mu2)
  
  tryCatch({
    # Use uniroot to find where the difference is zero
    intersection <- stats::uniroot(diff_func, interval = search_range, 
                                  extendInt = "yes", tol = 1e-6)
    return(intersection$root)
  }, error = function(e) {
    # If uniroot fails, use a conservative estimate
    # Use the point closer to the within-meal distribution
    return(mu1 + 0.5 * (mu2 - mu1))
  })
}
